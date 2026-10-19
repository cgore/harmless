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
| `M-x harmless-chat` | Start a chat with no project and no working directory |
| `M-x harmless-dashboard` | All sessions, grouped by project |
| `M-x harmless-switch` | Jump to a live or saved session |
| `M-x harmless-menu` | Transient: new / switch / model / effort / permissions / abort |
| `M-x harmless-info` | Open the Info manual (`h` in the transcript, dashboard, usage buffer, or menu) |
| `M-x harmless-set-reasoning-effort` | Set low / medium / high / xhigh |
| Click `xAI/grok-4.6 (xhigh)` in the session header | Provider, then model, then effort |
| `M-x harmless-login` | Sign in (xAI, Anthropic, or OpenAI) |
| `M-x harmless-logout` | Sign out of a provider |
| `M-x harmless-steer` | Queue a follow-up without aborting the turn, including during a permission question |
| `M-x harmless-abort` | Cancel the in-flight turn and any foreground shell. A background shell keeps running |
| `M-x harmless-compact` | Summarize older turns and keep the recent ones (`k` in the transcript). Also runs before a prompt that would fill the context window. |
| `M-x harmless-rewind` | Drop turns after the one at point and save the shorter transcript (`w` in the transcript) |
| `M-x harmless-edit-prompt` | Replace the prompt at point, drop what follows, and send the new text (`E` in the transcript) |
| `M-x harmless-fork` | Copy the transcript through the turn at point into a new session (`f` in the transcript) |
| `M-x harmless-add-directory` | Let this session read and edit a directory outside the project (`+` in the transcript) |
| `M-x harmless-reload-all-harmless` | Reload the Lisp checkout into this Emacs |

In the prompt window: `C-c C-c` sends.  While a turn is streaming, `C-c C-c`
queues that prompt and the turn keeps running.  While a permission question
is showing, `C-c C-c` is refused.  When a tool needs approval: `y` allow,
`n` deny, `!` always allow that class for the rest of the session.

In the transcript, a tool run is a collapsed line.  `TAB` toggles the
entry at point, `Left` closes it, `Right` opens it, and `S-TAB` toggles
every entry.  A run of reads and searches is one `explored` line.

Permission modes: `ask` (default), `accept-edits`, `always-approve`,
and `auto`.  Shell commands still prompt in `accept-edits`.  In `auto`,
a reviewer allows ordinary calls and escalates the rest.

## Layout

Each session is a read-only transcript buffer plus a small prompt window at
the bottom.  The dashboard is a tabulated list.  Config and sessions live
under `harmless-directory`, which defaults to `harmless/` in
`user-emacs-directory` (so `~/.emacs.d/harmless/` for a typical setup).
The buffer is a view, not the source of truth.

## Project instructions

Harmless reads `AGENTS.md`, `HARMLESS.md`, and `.harmless/HARMLESS.md`
from the session directory and from each parent directory up through
your home directory.  `CLAUDE.md` is included too, according to
`harmless-claude-md`: `nil` never, `t` always, and `anthropic` (the
default) only when the session model id starts with `claude-`.  Each
file applies to its directory and everything under it.  Outer
directories come first.  At each directory, `CLAUDE.md` comes first
when it is included, then `AGENTS.md`, then `HARMLESS.md`, then the
file inside `.harmless/`.  When two files disagree, the later one wins.
A file can include another file with `@path`.  The path is relative to
the file that contains it and must stay inside the session directory.
The text is sent with every turn and is not stored in the transcript.

## Path-scoped rules

Harmless reads Markdown files in `.claude/rules/` in the session
directory, including files in subdirectories.  The subdirectory name
does not scope the rule.  A file with no `paths` field in its YAML
frontmatter is sent on every turn.  A file with `paths` is sent only
when a tool call in the transcript names a file that matches one of
those globs.  `*` stays inside one path segment.  `**` crosses
segments, including none.  A rule in a parent directory is not read.
Nil, an empty string, a relative path, and the filesystem root are
not searched.  The text is sent with the turn and is not stored in
the transcript.  When two rules disagree, the later file in sorted
path order wins.

## Standing prompt

Every turn starts with a harness prompt: which tool to use, when to
verify, and when to delegate.  It is sent for a project chat and for
a detached chat.  Project instructions, path-scoped rules, and, in
plan mode, the plan rules are sent after it.  When they disagree with
the harness prompt, they win.  The prompt is not stored in the
transcript.

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

## Subagents

`spawn_agent` starts a child session with its own transcript and waits
for that child's last reply.  `prompt` is the task.  An optional
`agent` name loads `NAME.md` from `.claude/agents/`, `.codex/agents/`,
`.harmless/agents/`, `.grok/agents/`, or `.agents/agents/`, from the
session directory up through your home directory.  A closer file wins.
The file's frontmatter can set `tools`, `model`, and `sandbox`
(`workspace` or `read-only`).  The body is extra instructions for the
child.  A subagent cannot start another subagent.  Aborting the parent
aborts a child that is still running.

## Workflows

`run_workflow` runs one script, starts the child sessions that script
asks for, and returns one string.  Pass `name` to run a fixed
`NAME.hw` file, or `script` to run text written for this task.  Fixed
scripts live in `.claude/workflows/`, `.codex/workflows/`,
`.harmless/workflows/`, `.grok/workflows/`, or `.agents/workflows/`,
from the session directory up through your home directory.  A closer
file wins.  The script is a list of `let` and `complete` forms.  It is
not Emacs Lisp.  A subagent cannot run a workflow.  Aborting the
parent aborts a child the workflow started, and the result is not
added.  An agent form can pass `:worktree NAME` to edit an isolated
checkout.

## Worktrees

`enter_worktree` points the session at a git checkout of the same
repository.  The checkout is stored under `harmless-directory`, outside
the project, on a branch named `harmless/` plus the name.  It starts at
the current `HEAD`.  Uncommitted files are not copied.  The same name
is the same checkout.  `exit_worktree` returns to the previous project
and leaves the checkout on disk.  `M-x harmless-worktree` and `M-x
harmless-worktree-exit` do the same from the minibuffer.  `spawn_agent`
takes `worktree` so the child edits that checkout while the parent
stays put.

## Messages

`list_sessions` lists the sessions that are live in this Emacs.  A
session that is only on disk is omitted.  `send_message` delivers
`text` to one of those ids.  An idle session gets the finding in its
transcript immediately.  A session that is in a turn receives it at
the next safe point.  The finding does not start a turn.  Closing
Emacs before a queued finding is written drops it.

## Web search

`web_search` sends a query to DuckDuckGo over https and returns a
title, a URL, and a snippet for each hit.  The class is `network`, so
accepting edits still asks before a search.  The result is not written
into the project.

## Fetch a page

`web_fetch` reads one http or https page and returns its text.  HTML
is reduced to text.  The class is `network`, so accepting edits still
asks.  The page is not written into the project.

## X search

`x_keyword_search` finds recent posts.  `x_thread_fetch` reads one
post and its conversation.  `x_user_search` finds accounts.  The
class is `network`, so accepting edits still asks.  The bearer
token is `harmless-x-bearer`, `X_BEARER_TOKEN`, or an auth-source
entry for `api.x.com`.  A missing token is refused before a
request.  Nothing is written into the project.

## MCP

`mcp.json` names local MCP servers.  The project file is
`.harmless/mcp.json`.  The global file is `mcp.json` under the
Harmless data directory.  A server tool is offered as
`mcp__SERVER__TOOL`.  Its schema is loaded when the tool is about
to be called.  `mcp_list_resources` and `mcp_read_resource` read
resources.  The class is `network`, so accepting edits still asks.
A read-only subagent may read resources.  Nothing is written into
the project.

## Computer use

`computer_screenshot` returns the size and a PNG path under the
Harmless data directory.  `computer_click`, `computer_type`, and
`computer_key` act on the desktop.  `browser_open` opens an http
or https URL in the desktop browser.  The class is `shell`, so
accepting edits still asks.  Plan mode refuses a click, typed
text, a key, and the browser.  A screenshot still runs.  A
read-only subagent cannot use these tools.  Nothing is written
into the project.

## Monitors

`monitor` watches a file in the project or an added directory, a
command, or a background shell.  A line that matches `pattern` wakes the
session.  A file watch does not ask.  A command that can write
still asks, and plan mode refuses it.  `monitor_stop` stops the
watch.  Nothing is written into the project.

## Schedules

`schedule` sends a prompt once, after a delay or at a UTC time.
The prompt and the time are saved with the session.  Loading the
session arms a schedule that is still in the future and drops one
that has expired without sending it.  `schedule_cancel` removes a
waiting prompt.  The class is `edit`, so accepting edits does not
ask.  Plan mode does not refuse it.  A read-only subagent cannot
use it.  Nothing is written into the project.

## Todos

`todo_write` keeps a list of work on the session.  Each item has
an id, content, a status (`pending`, `in_progress`, `completed`,
or `cancelled`), and the ids it depends on.  `merge` updates by
id.  False replaces the list.  The list is saved with the
session.  A fork copies it.  A subagent starts empty.  The class
is `read`, so it does not ask.  Plan mode does not refuse it.
Nothing is written into the project.

## Review

`review` reads the working tree, a base branch, or one commit.
`M-x harmless-review` does the same.  The menu key is `R`.
Omit `base` and `commit` for the working tree.  `findings`
records the result.  Omit findings and the tool returns the
diff.  The list is saved with the session.  A fork copies it.
A subagent starts without one.  The class is `read`, so it does
not ask.  Plan mode does not refuse it.  A finding is not
applied as an edit.  Nothing is written into the project.

## Language server

`lsp` asks a language server that is already running in Emacs.
The operation is `definition`, `references`, or `diagnostics`.
`M-x harmless-lsp` does the same.  The menu key is `L`.
Line and column start at 1.  Diagnostics read the whole file.
Harmless does not start the server.  The class is `read`, so it
does not ask.  Plan mode does not refuse it.  Nothing is written
into the project.

## Images, PDFs, and notebooks

`read_file` reads an image by its bytes, a PDF by the literal
strings in the file, and a notebook by its cells.  `offset` and
`limit` apply only to text.  `notebook_edit` replaces one cell
in an existing `.ipynb` file.  The file is not created.  The
class is `edit`, so it asks in ask mode.  Plan mode refuses it.
`M-x harmless-attach-image` queues an image on the next prompt.
The menu key is `I`.  The queue is not saved.  A read does not
change the file.

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

## Permissions

`harmless-permission-rules` and `harmless-permission-denies` are
copied onto a session when it is created and when it is loaded.
A rule is `Bash(PATTERN)`, `Edit(PATTERN)`, or
`WebFetch(domain:HOST)`.  An allow skips the question.  A deny
refuses before the question, including in accept-edits and
always-approve, and a deny wins.  Plan mode still refuses a shell
command that can write.  `/` and `c:/` are not edit paths.  A rule
set only on the session is not saved.  `.harmless/permissions` is
a per-project allow list that survives the session.  One rule per
line.  A missing file creates nothing.
`harmless-dangerous-commands` refuses those programs before the
question.  An allow rule does not override it.  `rm` of `/`, a
drive root, `~`, or `/*` is refused too.  `auto` mode asks a
classifier, or the reviewer model when the classifier is nil,
before a call that would otherwise prompt.  `allow` skips the
question.  Anything else asks.  An allow rule skips the reviewer.
A deny, a dangerous command, and plan mode still refuse first.
`harmless-hooks` can block a tool call before the question, or
replace the result the model sees after the tool runs.  A hook
does not override plan mode, a deny rule, or the
dangerous-command list, and it does not skip the question.  A
hook is a function, a command, an HTTP request, an MCP call, a
prompt, or a subagent.  The list is not saved on the session.
`M-x harmless-add-directory` lets the session read and edit a
directory outside the project.  Nil, `""`, a relative path, `/`,
and `c:/` are refused and are not expanded.  The list is saved
with the session.  A path outside the project and outside every
added directory is still refused.  The shell's remembered
directory stays inside the project.

## Plan mode

`M-x harmless-plan` turns on plan mode for the current session.  A
prefix argument turns it off.  The same command is `P` in the Harmless
menu.  The model may also ask to enter plan mode.

In plan mode, `write_file`, `replace`, and `apply_patch` fail.  A shell
command that can write is refused before any permission question.  A
read-only shell command still runs.  The model writes `plan.md` in the
session directory with `write_plan`, then calls `exit_plan_mode`.  That
shows the plan and asks you to approve it, comment on a line or
range, send revision notes, or quit.  A comment does not change
`plan.md`.  Approving turns plan mode off so the same turn can start
changing project files.

The session header shows `plan` while the mode is on.  The flag is
saved with the session.

## Usage

`M-x harmless-usage` shows the current session and one row per
connection.  The session block is the model's context window, the last
request's prompt size, and what remains in the window, then the running
prompt and completion totals for this chat.  Window sizes live in
`harmless-usage-context-windows`.  The account block is the last
rate-limit remainder each provider sent.  A connection with no such
headers says so.  This is not an account credit balance.

## Development

```shell
make test      # byte-compile with warnings as errors, then ERT
make compile
make clean
```

## License

3-clause BSD.  See [LICENSE](LICENSE).  Copyright (c) 2026 Christopher Mark
Gore, Soli Deo Gloria.
