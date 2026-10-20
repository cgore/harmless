/* Harmless Landlock helper.  Restrict this process, then exec the command.
   Build: gcc -O2 -Wall -Wextra -o src/harmless-landlock src/harmless-landlock.c
   Usage: harmless-landlock --abi
          harmless-landlock --read PATH [--write PATH ...]
                             --network allow|deny -- COMMAND [ARG ...]
   A write of "/" is refused.  A relative path is refused.  Network deny
   handles TCP (and UDP when the kernel supports it) and adds no port
   rules.  Network allow does not handle network access. */

#define _GNU_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <linux/landlock.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#endif
#ifndef __NR_landlock_add_rule
#define __NR_landlock_add_rule 445
#endif
#ifndef __NR_landlock_restrict_self
#define __NR_landlock_restrict_self 446
#endif

struct harmless_ruleset {
	unsigned long long handled_access_fs;
	unsigned long long handled_access_net;
};

static void
fail (const char *text)
{
  fprintf (stderr, "%s\n", text);
}

static int
ll_create (const struct harmless_ruleset *attr, size_t size, unsigned int flags)
{
  return (int) syscall (__NR_landlock_create_ruleset, attr, size, flags);
}

static int
ll_add (int ruleset, const struct landlock_path_beneath_attr *rule)
{
  return (int) syscall (__NR_landlock_add_rule, ruleset,
			LANDLOCK_RULE_PATH_BENEATH, rule, 0);
}

static int
ll_restrict (int ruleset)
{
  return (int) syscall (__NR_landlock_restrict_self, ruleset, 0);
}

static int
abi_version (void)
{
  return ll_create (NULL, 0, LANDLOCK_CREATE_RULESET_VERSION);
}

static unsigned long long
fs_mask (int abi)
{
  unsigned long long mask;

  mask = LANDLOCK_ACCESS_FS_EXECUTE
    | LANDLOCK_ACCESS_FS_WRITE_FILE
    | LANDLOCK_ACCESS_FS_READ_FILE
    | LANDLOCK_ACCESS_FS_READ_DIR
    | LANDLOCK_ACCESS_FS_REMOVE_DIR
    | LANDLOCK_ACCESS_FS_REMOVE_FILE
    | LANDLOCK_ACCESS_FS_MAKE_CHAR
    | LANDLOCK_ACCESS_FS_MAKE_DIR
    | LANDLOCK_ACCESS_FS_MAKE_REG
    | LANDLOCK_ACCESS_FS_MAKE_SOCK
    | LANDLOCK_ACCESS_FS_MAKE_FIFO
    | LANDLOCK_ACCESS_FS_MAKE_BLOCK
    | LANDLOCK_ACCESS_FS_MAKE_SYM;
  if (abi >= 2)
    mask |= LANDLOCK_ACCESS_FS_REFER;
  if (abi >= 3)
    mask |= LANDLOCK_ACCESS_FS_TRUNCATE;
  return mask;
}

static unsigned long long
net_mask (int abi)
{
  unsigned long long mask = 0;

  if (abi >= 4)
    mask |= LANDLOCK_ACCESS_NET_BIND_TCP | LANDLOCK_ACCESS_NET_CONNECT_TCP;
  if (abi >= 10)
    mask |= LANDLOCK_ACCESS_NET_BIND_UDP
      | LANDLOCK_ACCESS_NET_CONNECT_SEND_UDP;
  return mask;
}

static unsigned long long
read_rights (unsigned long long handled)
{
  return handled & (LANDLOCK_ACCESS_FS_EXECUTE
		    | LANDLOCK_ACCESS_FS_READ_FILE
		    | LANDLOCK_ACCESS_FS_READ_DIR);
}

static unsigned long long
write_rights (unsigned long long handled)
{
  return handled & (LANDLOCK_ACCESS_FS_WRITE_FILE
		    | LANDLOCK_ACCESS_FS_REMOVE_DIR
		    | LANDLOCK_ACCESS_FS_REMOVE_FILE
		    | LANDLOCK_ACCESS_FS_MAKE_DIR
		    | LANDLOCK_ACCESS_FS_MAKE_REG
		    | LANDLOCK_ACCESS_FS_MAKE_SOCK
		    | LANDLOCK_ACCESS_FS_MAKE_FIFO
		    | LANDLOCK_ACCESS_FS_MAKE_SYM
		    | LANDLOCK_ACCESS_FS_REFER
		    | LANDLOCK_ACCESS_FS_TRUNCATE);
}

static unsigned long long
device_rights (unsigned long long handled)
{
  return handled & (LANDLOCK_ACCESS_FS_READ_FILE
		    | LANDLOCK_ACCESS_FS_WRITE_FILE
		    | LANDLOCK_ACCESS_FS_TRUNCATE);
}

static int
absolute_p (const char *path)
{
  return path && path[0] == '/';
}

static int
root_p (const char *path)
{
  char buf[PATH_MAX];

  if (!absolute_p (path))
    return 0;
  if (!realpath (path, buf))
    return strcmp (path, "/") == 0;
  return strcmp (buf, "/") == 0;
}

static int
directory_p (const char *path)
{
  struct stat st;

  if (stat (path, &st) != 0)
    return 0;
  return S_ISDIR (st.st_mode);
}

static int
add_path (int ruleset, const char *path, unsigned long long rights)
{
  struct landlock_path_beneath_attr rule;
  int fd;

  fd = open (path, O_PATH | O_CLOEXEC);
  if (fd < 0)
    {
      fprintf (stderr, "Error: Landlock could not open %s\n", path);
      return -1;
    }
  memset (&rule, 0, sizeof rule);
  rule.allowed_access = rights;
  rule.parent_fd = fd;
  if (ll_add (ruleset, &rule) != 0)
    {
      close (fd);
      fail ("Error: Landlock could not restrict this process");
      return -1;
    }
  close (fd);
  return 0;
}

int
main (int argc, char **argv)
{
  int i;
  int abi;
  int ruleset;
  int network_seen = 0;
  int allow_network = 0;
  int reads = 0;
  int cmd = 0;
  unsigned long long handled;
  unsigned long long net;
  size_t size;
  struct harmless_ruleset attr;

  if (argc >= 2 && strcmp (argv[1], "--abi") == 0)
    {
      abi = abi_version ();
      if (abi < 1)
	{
	  fail ("Error: Landlock is not available");
	  return 127;
	}
      printf ("%d\n", abi);
      return 0;
    }

  abi = abi_version ();
  if (abi < 1)
    {
      fail ("Error: Landlock is not available");
      return 127;
    }

  for (i = 1; i < argc; i++)
    {
      if (strcmp (argv[i], "--") == 0)
	{
	  cmd = i + 1;
	  break;
	}
      if (strcmp (argv[i], "--read") == 0 || strcmp (argv[i], "--write") == 0)
	{
	  if (i + 1 >= argc)
	    {
	      fail ("Error: sandbox path must be absolute");
	      return 127;
	    }
	  i++;
	  continue;
	}
      if (strcmp (argv[i], "--network") == 0)
	{
	  if (i + 1 >= argc)
	    {
	      fail ("Error: network must be a boolean");
	      return 127;
	    }
	  i++;
	  if (strcmp (argv[i], "allow") == 0)
	    allow_network = 1;
	  else if (strcmp (argv[i], "deny") == 0)
	    allow_network = 0;
	  else
	    {
	      fail ("Error: network must be a boolean");
	      return 127;
	    }
	  network_seen = 1;
	  continue;
	}
      fail ("Error: Landlock could not restrict this process");
      return 127;
    }

  if (!network_seen)
    {
      fail ("Error: network must be a boolean");
      return 127;
    }
  if (!allow_network && abi < 4)
    {
      fail ("Error: Landlock cannot restrict the network on this kernel");
      return 127;
    }
  if (cmd == 0 || cmd >= argc || !argv[cmd] || !argv[cmd][0])
    {
      fail ("Error: Landlock could not run");
      return 126;
    }

  for (i = 1; i < cmd - 1; i++)
    {
      const char *path;

      if (strcmp (argv[i], "--read") != 0 && strcmp (argv[i], "--write") != 0)
	{
	  if (strcmp (argv[i], "--network") == 0)
	    i++;
	  continue;
	}
      path = argv[++i];
      if (!absolute_p (path))
	{
	  fail ("Error: sandbox path must be absolute");
	  return 127;
	}
      if (strcmp (argv[i - 1], "--write") == 0 && root_p (path))
	{
	  fail ("Error: refusing to sandbox-write the filesystem root");
	  return 127;
	}
      if (strcmp (argv[i - 1], "--read") == 0)
	reads++;
    }
  if (reads < 1)
    {
      fail ("Error: sandbox path must be absolute");
      return 127;
    }

  handled = fs_mask (abi);
  net = allow_network ? 0 : net_mask (abi);
  memset (&attr, 0, sizeof attr);
  attr.handled_access_fs = handled;
  attr.handled_access_net = net;
  size = net ? sizeof attr : sizeof attr.handled_access_fs;
  ruleset = ll_create (&attr, size, 0);
  if (ruleset < 0)
    {
      fail ("Error: Landlock could not restrict this process");
      return 127;
    }

  for (i = 1; i < cmd - 1; i++)
    {
      const char *kind;
      const char *path;
      unsigned long long rights;

      if (strcmp (argv[i], "--network") == 0)
	{
	  i++;
	  continue;
	}
      if (strcmp (argv[i], "--read") != 0 && strcmp (argv[i], "--write") != 0)
	continue;
      kind = argv[i];
      path = argv[++i];
      if (strcmp (kind, "--read") == 0)
	rights = read_rights (handled);
      else if (root_p (path))
	{
	  fail ("Error: refusing to sandbox-write the filesystem root");
	  close (ruleset);
	  return 127;
	}
      else if (directory_p (path))
	rights = write_rights (handled) | read_rights (handled);
      else
	rights = device_rights (handled);
      if (add_path (ruleset, path, rights) != 0)
	{
	  close (ruleset);
	  return 127;
	}
    }

  if (prctl (PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0
      || ll_restrict (ruleset) != 0)
    {
      fail ("Error: Landlock could not restrict this process");
      close (ruleset);
      return 127;
    }
  close (ruleset);
  execvp (argv[cmd], argv + cmd);
  fprintf (stderr, "Error: Landlock could not run %s\n", argv[cmd]);
  return 126;
}
