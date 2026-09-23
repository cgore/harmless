# Harmless

[![Dan Patch, from Types and Breeds of Farm Animals (1906)](dan-patch.jpg)](https://commons.wikimedia.org/wiki/File:Danpatch1.jpg)

Harmless is an AI coding harness that lives entirely inside Emacs.  It is
written in Emacs Lisp.  It is not [gptel](https://github.com/karthink/gptel),
and it does not wrap Grok Build, Claude CLI, or any other external agent.

Conceptually, Harmless is similar to Claude Code, or OpenAI Codex, or Grok
Build, among many other examples, but is not built upon any of those, and lives
inside of your Emacs.

Several sessions can run at once in one Emacs, each pinned to a provider and
model.  Most people will configure a single provider (xAI, OpenAI, Anthropic,
or a local OpenAI-compatible server).  Mixing is optional: one session on Grok
4.6, another on a local LLM, a third on Claude, each in its own project.

Requires **GNU Emacs 31**.

## Install

```elisp
(add-to-list 'load-path "~/programming/emacs/harmless/lisp")
(require 'harmless)
```

## One-provider setup (xAI / Grok)

Browser login is the same flow as Grok Build: Harmless opens `auth.x.ai`,
you sign in, and tokens are stored in `auth.json` under
`harmless-directory` (mode `0600`).  They refresh automatically.

```elisp
(setq harmless-providers (list (harmless-make-xai))
      harmless-default-model "grok-4.6"
      harmless-default-reasoning-effort "xhigh")
```

Then `M-x harmless-login`.  That command is the shared entry for every
provider login.  With more than one named connection it asks which
account.  Prefix argument (`C-u M-x harmless-login`) uses xAI's
device-code flow, which is also used automatically if port 56121 is
already taken (for example by Grok Build itself).

Several accounts on the same vendor are separate connections: give each
a name.  The default names (`xAI`, `Anthropic`, `OpenAI`) keep the
original token files (`auth.json`, `auth-anthropic.json`,
`auth-openai.json`).  Extra connections get `auth-<slug>.json`.

```elisp
(setq harmless-providers
      (list (harmless-make-xai "xAI")
            (harmless-make-xai "xAI work")
            (harmless-make-anthropic "Anthropic")
            (harmless-make-anthropic "Anthropic work")
            (harmless-make-openai))
      harmless-default-provider-name "xAI"
      harmless-default-model "grok-4.6"
      harmless-default-reasoning-effort "xhigh")
```

Then `M-x harmless-login` and pick **xAI work** (or **Anthropic work**)
to sign that account in.  Sessions pin to a connection name, so one
buffer can stay on personal Grok while another uses work Claude.

Anthropic / Claude uses Claude Code's OAuth client.  The browser shows
a code; paste it at the minibuffer prompt.  Tokens go in
`auth-anthropic.json` under `harmless-directory`.  If you already ran
`claude login`, Harmless will reuse a still-valid
`~/.claude/.credentials.json` access token (it never writes or
refreshes that file).

If you already ran `grok login`, Harmless will reuse `~/.grok/auth.json`
until you sign in or out from Emacs.  It never writes that file.

An API key still works as a fallback when no OAuth session is active:
put it in `~/.authinfo` (`machine api.x.ai login apikey password xai-...`)
or export `XAI_API_KEY`.  Settings can live in your init file or in
`harmless/config.el` under `user-emacs-directory` (typically
`~/.emacs.d/harmless/config.el`).

OpenAI:

```elisp
(setq harmless-providers (list (harmless-make-openai))
      harmless-default-model "gpt-5.5")
```

`M-x harmless-login` → **OpenAI** opens ChatGPT OAuth (Codex CLI's
public client, loopback on `localhost:1455`). Prefix argument pastes
the redirect URL instead, which is also used if that port is busy
(for example Codex CLI itself). Tokens go in `auth-openai.json`. A
still-valid `~/.codex/auth.json` is reused (never written). ChatGPT
subscription tokens are sent to the Codex Responses API
(`chatgpt.com/backend-api/codex/responses`), not `api.openai.com`.
An `OPENAI_API_KEY` still works as a usage-based fallback on Chat
Completions.

Anthropic:

```elisp
(setq harmless-providers (list (harmless-make-anthropic "Anthropic"))
      harmless-default-model "claude-sonnet-4-6")
```

Local OpenAI-compatible server (Ollama, llama.cpp, vLLM, …):

```elisp
(setq harmless-providers
      (list (harmless-make-openai-compat
             "local"
             :host "127.0.0.1:11434"
             :protocol "http"
             :endpoint "/v1/chat/completions"
             :key "none"
             :models '("llama3.2"))))
```

If nothing is configured, `M-x harmless` walks through a short setup.

## Commands

| Command | What it does |
|---|---|
| `M-x harmless` | Open or resume this project's session |
| `M-x harmless-new` | Start a new session (picks a model) |
| `M-x harmless-dashboard` | All sessions, grouped by project |
| `M-x harmless-switch` | Jump to a live or saved session |
| `M-x harmless-menu` | Transient: new / switch / model / effort / permissions / abort |
| `M-x harmless-set-reasoning-effort` | Set low / medium / high / xhigh |
| Click `xAI/grok-4.6 (xhigh)` in the session header | Provider, then model, then effort |
| `M-x harmless-login` | Sign in (xAI, Anthropic, or OpenAI) |
| `M-x harmless-logout` | Sign out of a provider |
| `M-x harmless-abort` | Cancel the in-flight turn or shell |

In the prompt window: `C-c C-c` sends.  When a tool needs approval: `y` allow,
`n` deny, `!` always allow that class for the rest of the session.

In the transcript, a tool run is a collapsed line.  `TAB` toggles the
entry at point, `Left` closes it, `Right` opens it, and `S-TAB` toggles
every entry.  A run of reads and searches is one `explored` line.

Permission modes: `ask` (default), `accept-edits`, `always-approve`.  Shell
commands still prompt in `accept-edits`.

## Layout

Each session is a read-only transcript buffer plus a small prompt window at
the bottom.  The dashboard is a tabulated list.  Config and sessions live
under `harmless-directory`, which defaults to `harmless/` in
`user-emacs-directory` (so `~/.emacs.d/harmless/` for a typical setup).
The buffer is a view, not the source of truth.

## Project instructions

Harmless reads `AGENTS.md`, `HARMLESS.md`, and `.harmless/HARMLESS.md`
from the session directory and from each parent directory up through
your home directory.  Each file applies to its directory and everything
under it.  Outer directories come first.  At each directory, `AGENTS.md`
comes before `HARMLESS.md`, which comes before the file inside
`.harmless/`.  When two files disagree, the later one wins.  The text
is sent with every turn and is not stored in the transcript.

## Skills

A skill is a directory with a `SKILL.md` file.  The file starts with
`name` and `description` frontmatter.  Each turn lists the name,
description, and path.  The model reads that file with `read_file`
when the task fits.  The body is not sent until then.

Harmless looks for `skills/` inside these directories, at every level
from your home directory down to the session.  A skill closer to the
session replaces one of the same name farther away.  In a single
directory, later entries in this list replace earlier ones:

| Directory | Whose layout |
|---|---|
| `.agents/skills/`, `.codex/skills/` | Codex |
| `.claude/skills/` | Claude Code |
| `.grok/skills/` | Grok Build |
| `.harmless/skills/` | Harmless |

`/etc/codex/skills` is included when it exists, at the lowest priority.
Codex's own bundled skills are not on disk and are not read.

## Memory

Memory is Markdown under `harmless-directory`, in `memory/`.  Global
notes apply to every project.  Workspace notes belong to one project
directory.  A new fact goes to `observations/_inbox/`.  `M-x
harmless-dream` folds the inbox into `topics/` and moves those files
to `archive/`.  `MEMORY.md` is a generated index.

Each turn sends the index, not the notes.  The model reads a note with
`memory_read`, saves a fact with `memory_remember`, and replaces a
topic with `memory_write_topic`.  `M-x harmless-remember` saves a note
directly.  A prefix argument stores it in global memory.

## Plan mode

`M-x harmless-plan` turns on plan mode for the current session.  A
prefix argument turns it off.  The same command is `P` in the Harmless
menu.  The model may also ask to enter plan mode.

In plan mode, `write_file` and `replace` fail.  The model writes
`plan.md` in the session directory with `write_plan`, then calls
`exit_plan_mode`.  That shows the plan and asks you to approve it,
send revision notes, or quit.  Approving turns plan mode off so the
same turn can start changing project files.  Shell commands are not
inspected for writes.

The session header shows `plan` while the mode is on.  The flag is
saved with the session.

## Usage

`M-x harmless-usage` shows two reports.  The session report is the
prompt and completion tokens Harmless has counted, for the current
session and summed across saved sessions by connection and model.  The
account report is the last rate-limit remainder each provider sent
(requests and tokens still available in the current window, when the
response included those headers).  A connection with no such headers
says so.  This is not an account credit balance.

## Development

```shell
make test      # byte-compile with warnings as errors, then ERT
make compile
make clean
```

## License

3-clause BSD.  See [LICENSE](LICENSE).  Copyright (c) 2026 Christopher Mark
Gore, Soli Deo Gloria.
