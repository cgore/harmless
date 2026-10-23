# TODO

Gaps versus Grok Build, Claude Code, the Codex CLI, [OpenRig](https://openrig.dev/) 0.5.14, and [Hermes Agent](https://hermes-agent.nousresearch.com/), in the order that changes whether the work can finish. Sections 1–3 are one session. Section 4 is the lasting team OpenRig runs above sessions like these. Section 10 is what Hermes does that those sections do not. Section 11 is the editor Harmless is running in. An item is listed once. A parenthetical names the products that already have it when that is not all three.

Harmless already has the core loop: sessions, streaming turns, `harmless-tool-read-file`, `harmless-tool-write-file`, `harmless-tool-replace`, `harmless-tool-list-dir`, `harmless-tool-glob`, `harmless-tool-grep`, `harmless-tool-run-shell`, permission classes, project instructions, a skill catalog, memory, and plan mode.

## 1. Delegation

The model cannot start work that runs beside the current turn.

- [x] Run tool calls from one turn in parallel. Permission is still one question at a time. An allowed call starts before the next question. Results are recorded in the order the model requested them.
- [x] Subagents. `harmless-tool-spawn-agent` starts a child session with its own transcript and returns that child's last reply. A definition file (`.claude/agents/`, `.codex/agents/`, `.harmless/agents/`, `.grok/agents/`, `.agents/agents/`) can set the child's prompt, tools, model, and sandbox (`workspace` or `read-only`). A subagent cannot start another subagent. Aborting the parent aborts a child that is still running.
- [x] Workflows. One job that runs many child sessions and returns one result: a fixed script (Grok Build), or a script the model writes for this task (Claude Code dynamic workflows, Codex subagent workflows).
- [x] Git worktrees, so parallel sessions edit isolated checkouts of the same repository (Claude Code `EnterWorktree`, Codex, Grok Build subagent worktrees).
- [x] Cross-session messaging (Claude Code). One live session can send a finding to another session on this machine. A message that obligates another seat, and survives that seat being replaced, is section 4.

## 2. Network

The shell is the only way out of the project. A skill that needs any of these is only a `SKILL.md` the model can read.

- [x] Web search.
- [x] Fetch a page.
- [x] X search (Grok Build): posts, threads, and users.
- [x] MCP client, so configured servers show up next to the built-in tools. Load a tool's schema when it is about to be called, and read MCP resources, not only call tools (Claude Code).
- [x] Computer use and browser control (Claude Code, Codex). See the screen, click, and type, including a browser the user can watch.

## 3. Long sessions

Every turn resends the full transcript. Editing an earlier prompt still aborts a turn that is streaming or waiting on permission.

- [x] Compact the transcript. `M-x harmless-compact` summarizes older turns, keeps the recent ones, and saves the shorter transcript. A tool call stays with its results. An empty summary leaves the transcript unchanged.
- [x] Compact automatically when the context window fills. `harmless-compact-threshold` is a percent of that model's window (default 80). Nil disables it. The same percent applies to every model.
- [x] Rewind to an earlier turn and drop what follows. `M-x harmless-rewind` keeps that turn and everything before it. A tool call stays with its turn. The dropped turns are not saved.
- [x] Edit an earlier prompt and rerun from there. `M-x harmless-edit-prompt` replaces that prompt, drops its reply and every later turn, and sends the new prompt. The dropped turns are not saved.
- [x] Fork a session, keeping history up to this point. `M-x harmless-fork` copies the transcript through that turn into a new session and leaves the original alone. Nothing is sent.
- [x] Steer a running turn. A follow-up arrives without aborting the turn. `C-c C-c` while streaming queues the prompt. `M-x harmless-steer` queues from the minibuffer, including while a permission question is showing. `C-c C-c` during that question stays refused. Nil, `""`, and whitespace are refused: `Prompt is empty`. The follow-up is sent after the current tool batch, before the next provider call. Several follow-ups keep their order.

## 4. A lasting team

[OpenRig](https://openrig.dev/) 0.5.14 is a local control plane over Claude Code, Codex, and Pi. It keeps a named team at work for weeks. Harmless can open several sessions. It has no seats, no owned work, and no way to bring that team back. Compact, fork, and a one-off message are already listed above. A declared workflow here is durable team state. The one-shot workflow in section 1 is a script that returns one result.

A message informs. A queue row obligates. A seat outlives the session occupying it.

- [x] A rig of pods of named seats. A seat has a role and an address (`pod-member@rig`). The session in it can be replaced. The seat keeps its name, its role, and its inbound work.
- [x] Edges between seats. `delegates_to` and `spawned_by` decide launch order. `can_observe`, `collaborates_with`, and `escalates_to` are recorded and shown.
- [x] Boot a team from a spec, a named starter, or a portable bundle. Preview the boot before it runs. Grow, shrink, add, and remove seats while the team runs. Removing a seat that still holds work requires a fallback seat, or the removal is refused.
- [x] Adopt a session that is already running, including one started by hand. Release it without killing it. A seat can be a Harmless session, a Claude Code session, a Codex session, a Pi session, or a plain terminal for a dev server or a log.
- [x] Send to a seat, several seats, a pod, or the whole rig. Check that the text appeared. Refuse to type into a seat that is sitting on a prompt or a permission question. Capture what is on a seat's screen, and read what it has said across every session that has occupied it.
- [x] A durable chatroom for the rig, with history, topics, and a wait that blocks until someone speaks.
- [x] Walk a paced sequence of context into a seat, one piece at a time, so onboarding is not one dump.
- [x] A queue row: one unit of owned work, with a destination seat, a body, a state, and an append-only history. It survives the owner's turn ending, the owner compacting, and the owner being replaced.
- [x] Claim a row, append notes, and close it only with a reason: `handed_off_to`, `blocked_on`, `denied`, `canceled`, `no-follow-on`, or `escalation`. Closing records delivery. Acceptance is the next seat's own verdict.
- [x] Hand work to another seat in one transaction: the source row closes and the destination row opens together.
- [x] Park a row on a real blocker, with one wake. Resolving it records the decision and wakes the owner. A park that asks a person includes a plain-language summary and a pointer to the evidence.
- [x] An intake stream for observations nobody owns yet, and views over the board: what is held, what is stalled after a claim, and what is waiting on a person.
- [x] A declared workflow. When the seat that owns a step closes it, the next step's row is created in the same transaction. The owner decides that the step is done.
- [x] Watchdog jobs that survive a restart: a reminder, a pool of artifacts, a nudge to a workflow, an idle queue, or a seat approaching its context wall.
- [x] Snapshot a rig and restore it by name. The report says, per seat, whether it resumed, was rebuilt, started fresh, is waiting on a decision, or failed. A fresh start is a choice, and it is labeled as one. Check whether a restore can succeed before running it.
- [x] Hand one seat to a successor and keep its name, its edges, and its inbound work. Write that seat's recap of decisions at the boundary.
- [x] An agent image: capture a productive seat and start another seat from it. A restore packet carries what a seat knows across a change of runtime.
- [x] Plan compaction for seats near the context wall, then compact deliberately and tell the rest of the team. The compact itself is section 3.
- [x] Work on disk as a tree: a project holds missions, a mission holds slices. Intent composes down the tree. The slice is the only unit that is specified and proved. Proof is evidence in the tree, with attributed judgments. Progress is derived from that evidence.
- [x] Context packs addressed by ref, including one section of a file. Compose a profile for a fresh seat, a handover, or a return from compaction, and report the token total instead of truncating it.
- [x] Reconcile the skill loadout for one working directory, and inspect a plugin's skills, hooks, and MCP servers. Installing a plugin stays an explicit copy.
- [x] A registry of other machines. The same team verbs run against a registered host, and one file can be copied between hosts.
- [x] A person is an address. A queue row to that address is how the team asks for a decision, and the row's history is the receipt. Slack is the first connector. Secrets stay out of the team config.
- [x] The model can boot, inspect, message, snapshot, and restore the team through tools, with the same guards as the commands a person runs.
- [x] Named auth profiles per runtime. Switch a seat to another account without stranding its conversation. The team record never stores the token.
- [x] A health surface that says what is true, and a recovery command when a live session and the record disagree. A rig can also run services beside its seats and report those services' health separately.
- [x] One fleet view: topology, health, and the rows that need a person. The dashboard today lists sessions by project.

## 5. Processes

`harmless-tool-run-shell` waits up to `harmless-shell-timeout-seconds` (120), keeps one process on the session, and then returns. Each command starts in the session cwd.

- [x] Leave a command running, read its output later, and kill it. A command that hits its timeout moves to the background instead of dying, and the model can raise that command's timeout (Claude Code). `harmless-tool-run-shell` takes `timeout` and `background`. `harmless-tool-shell-output` reads one. `harmless-tool-shell-kill` stops one.
- [x] Watch a log, a file, or another stream and wake the session when a line matters (Grok Build monitors, Claude Code `Monitor`).
- [x] Schedule a prompt to run later. Claude Code restores an unexpired session schedule on resume. A watchdog that survives a restart and can watch a queue or a context wall is section 4.
- [x] Remember a `cd` that stays inside the project for later commands (Claude Code). A directory outside the project, the filesystem root, nil, and `""` are not kept. The remembered directory is saved with the session.
- [x] Notify through Emacs when a background task finishes. A background shell posts a message and a transcript note. The note is not a message in the transcript the model sees.
- [x] In plan mode, treat a shell command that writes as a write. `harmless-tool-write-file` and `harmless-tool-replace` are already blocked. A shell command that can write is refused before the permission question: `Plan mode is on. A shell command that can write is refused`. A read-only command still runs.

## 6. Permissions and safety

The policy is three classes (`read`, `edit`, `shell`) and three modes (`ask`, `accept-edits`, `always-approve`). "Always" lasts for the session. File tools stay inside the project. The shell does not, unless that session's Landlock sandbox is on.

- [x] Allow and deny rules for commands, paths, and fetch domains. Claude Code writes these as `Bash(npm run *)` and `Edit(/src/**)`.
- [x] A per-project allow list that survives the session.
- [x] A denylist for dangerous commands.
- [x] Classify read-only shell commands so they can skip a prompt. A command Harmless does not recognize still asks. A redirect, a command substitution, and a subshell still ask.
- [x] Auto-review. A reviewer model or a classifier decides an escalation (Codex auto-review, Claude Code auto mode, Grok Build auto-review).
- [x] Hooks that can block a tool call, or replace the result the model sees. Claude Code also runs a hook as an HTTP request, an MCP call, a prompt, or a subagent.
- [x] An OS sandbox for the shell (Landlock on Linux). The default is `off`. `read-only` and `workspace-write` use Landlock. `network` true asks before a connection is allowed. A read-only subagent is forced to read-only Landlock when the session sandbox is on.
- [x] Extra directories, outside the project root, that this session may read or edit (Claude Code `--add-dir`).

## 7. Planning and tracking

Plan mode already has `harmless-tool-enter-plan-mode`, `harmless-tool-write-plan`, and `harmless-tool-exit-plan-mode`, with approve, revise, or quit.

- [x] Comment on a line or range of `plan.md` during review (Grok Build).
- [x] A todo list the model updates while it works, with status and dependencies (Claude Code tasks, Grok Build todos).
- [x] A read-only review of the working tree, a base branch, or one commit. The result is a list of findings the session does not apply as edits (Codex `/review`, Claude Code `ReportFindings`).

## 8. Search, reading, and the harness prompt

- [x] Code intelligence through a language server: definition, references, and diagnostics after an edit (Claude Code `LSP`). Emacs already has the server. The model has no tool for it.
- [x] Let `harmless-tool-read-file` read images, PDFs, and notebooks, not only text with an offset, a limit, and a byte cap. Edit a notebook cell (Claude Code `NotebookEdit`). Accept an image attached to the user prompt (Claude Code, Codex).
- [x] Apply a patch: several hunks across files in one call. Each hunk replaces one unique string. Later hunks on the same file see earlier replacements. If any hunk fails, no file is changed. `harmless-tool-replace` still changes one unique string.
- [x] Expand `@path` imports in `AGENTS.md`, `HARMLESS.md`, `.harmless/HARMLESS.md`, and `CLAUDE.md`. The path is relative to the file that contains it and must stay inside the session directory. A cycle, a missing file, a directory, an empty path, and a path that leaves the session are refused.
- [x] Read path-scoped rules (Claude Code `.claude/rules/`).
- [x] A standing harness prompt on every turn: tool policy, when to verify, and when to delegate. Today a turn sends project instructions, the skill catalog, the memory index, and, in plan mode, the plan rules.

## 9. Later

- [x] Load a skill into the turn when the user invokes it or the model selects it, including an `allowed-tools` list. Today the catalog lists the name, description, and path, and the model is told to `harmless-tool-read-file` the skill.
- [x] Plugins and marketplaces, so a bundle of skills, hooks, agents, and MCP servers installs as one unit.
- [x] Output styles (Claude Code): a session-wide role, tone, and format that can be switched without editing project instructions.
- [x] Image generation (Grok Build, Codex).
- [x] Capture a memory observation after a turn. Today that happens only through `harmless-tool-memory-remember` or `M-x harmless-remember`. Codex and Grok Build can generate memories. Claude Code keeps auto memory alongside `CLAUDE.md`.
- [x] Rename, delete, and export a session.
- [x] A headless mode, and a remote task that keeps running while Emacs is closed (Codex cloud, Claude Code on the web and remote control). A registry of other machines, with the same team verbs on each, is section 4.
- [x] Publish a Markdown or HTML artifact as a page (Claude Code).

## 10. Hermes

[Hermes Agent](https://hermes-agent.nousresearch.com/) already overlaps the checked items: tools, a skill catalog, memory notes, subagents, schedules, browser control, vision, image generation, and an MCP client. These are the pieces it has on top of that.

- [x] Expand `@file:`, `@folder:`, `@diff`, `@staged`, `@git:N`, and `@url:` in a prompt (Hermes context references). The user's text stays, and the attachment follows under `--- Attached Context ---`. A file or folder stays inside the project or an extra directory. Nil, `""`, a relative path that leaves the project, `@file:` of a directory (a trailing slash that names one), and the filesystem root are refused. A line range is 1-based and inclusive. A folder lists at most 200 entries. `@git:` is from 1 to 10 commits. An attachment over half the context window is left off and the prompt is unchanged. One over a quarter keeps the text and says so.
- [x] Snapshot the working tree in a shadow git store before `harmless-tool-write-file`, `harmless-tool-replace`, a patch, or a destructive shell command, at most once per directory per turn (Hermes checkpoints). Restore one checkpoint, one file from it, or show the diff, without touching the project's `.git`. A hand-edit made after the snapshot is kept unless the restore asks for every file. Skip the filesystem root, a missing directory, and a file over the size cap. Nil, `""`, and a snapshot that is not in the store are refused. A snapshot that fails does not block the tool. Checkpoints are off until `harmless-checkpoints` is set. Each snapshotted directory has its own store. A file over 1000000 bytes is left out. A tree with more than 5000 files is not snapshotted.
- [x] `harmless-tool-execute-code` (Hermes). The model writes an Emacs Lisp script that calls Harmless tools. The script runs in a separate Emacs process. Only what the script prints is returned. Intermediate tool results stay out of the transcript. The script cannot call `harmless-tool-execute-code`, `harmless-tool-spawn-agent`, `harmless-tool-run-workflow`, or an MCP tool. Names that look like secrets are stripped from the environment. The default timeout is 300 seconds. One script may make 50 tool calls. Printed output is cut at 50000 bytes. Nil, `""`, a timeout, and a call past the tool cap are refused with the error text. A later call in the same session may reuse the interpreter until `reset` or a timeout.
- [ ] The model can create, patch, and delete a skill, and read one file inside it (Hermes `skill_manage` and `skill_view`). A new skill is a `SKILL.md` under the user skill directory. A path that leaves that directory, the filesystem root, nil, and `""` are refused. An idle curator can patch or archive an agent-written skill. It does not edit a project skill.
- [x] Search past session transcripts on this machine and return a short recall (Hermes `harmless-tool-session-search`). Nil, `""`, and a query that matches nothing return an empty recall, not an error. The search does not load another session's tools. `""` and the filesystem root are refused as the data directory. Nil uses the Harmless data directory. A transcript is searched through its first 100000 characters. At most five sessions are named.
- [ ] When the primary provider errors, retry that turn once on a configured fallback provider and model (Hermes fallback providers). A fallback that names the same connection is refused. The transcript records which connection answered.
- [ ] A pool of credentials for one provider (Hermes credential pools). A rate limit or an auth failure tries the next key. Nil, `""`, and an empty pool are refused. The pool is not written into the team record or the transcript.
- [x] `USER.md`, a profile of the person, kept apart from `MEMORY.md` (Hermes). The memory tool can write it. It is injected with memory. It is not project instructions. `""` and the filesystem root are refused as its path. Nil uses the Harmless data directory.
- [x] Read `.cursorrules` and `.cursor/rules/*.mdc` with the other project instruction files (Hermes context files). They are read before `CLAUDE.md`, `AGENTS.md`, and `HARMLESS.md`, so those later files still win. A nested rule is not read. Nothing is created.
- [x] `SOUL.md` is the identity block at the front of the system prompt (Hermes personality). It is read from the Harmless data directory, not from the project. A missing file leaves the built-in identity. A directory is refused. `""` and the filesystem root are refused as its path. Nil uses the Harmless data directory. A relative path is refused and is not expanded.
- [ ] Send an Anthropic cache breakpoint on the stable prefix: identity, tools, the memory snapshot, the skill index, and project instructions (Hermes prompt caching).
- [ ] Pause, resume, and edit a scheduled prompt, and accept a cron expression as well as a delay (Hermes cron). A bad expression is refused with the error text. A schedule still ends when Emacs does, unless it is a section 4 watchdog.
- [ ] Choose which tools one MCP server exposes, and answer an MCP sampling request with the session's model (Hermes MCP). A server that is not configured is refused.
- [ ] Run one prompt across many inputs and write a trajectory file (Hermes batch processing). This is an evaluation run, not a team workflow. Nil, `""`, and the filesystem root are refused as the output path.
- [ ] One memory-provider slot and one context-engine slot (Hermes plugins). The provider can recall text before a turn. The engine can replace compact. The built-in memory and compact stay the defaults. Two providers, or two engines, are refused.

## 11. The editor

Harmless already has a transcript buffer, a prompt buffer, a dashboard, the menu, and Info. `project-current` chooses the project. `harmless-tool-lsp` asks an Eglot server that is already running and does not start one. When a buffer visits a file, `harmless-tool-read-file` returns that buffer's text, and `harmless-tool-write-file`, `harmless-tool-replace`, and a patch edit that buffer as one undo entry and save it. These items use the buffer, the window, and the command the user already has. A script stays in the separate Emacs from section 10. Magit is not a dependency. A second language server is not added.

- [x] Read and edit a visiting buffer. When a buffer visits the file, `harmless-tool-read-file` returns that buffer's accessible text. Unsaved changes are labeled unsaved. `harmless-tool-write-file`, `harmless-tool-replace`, and a patch then change that buffer as one undo entry and save it, so the file matches the buffer. The buffer's coding system and newline convention are what get saved. A window already showing the buffer displays the first change, and the transcript keeps the selection. `vc` is refreshed for that file after a save. No visiting buffer reads and writes the file as today. A checkpoint still runs before the edit when checkpoints are on. Plan mode still refuses every edit except `plan.md`. A buffer that changed after it was read is refused, and the file is left as it was: `buffer changed since it was read`. A read-only buffer, or read-only text inside the replacement, is refused: `buffer is read-only`. The transcript, the prompt, the minibuffer, and a buffer whose name starts with a space are refused: `buffer is not a file`. An indirect buffer is edited through its base buffer. Narrowing is restored when the tool returns, and the report says the buffer was narrowed. A `before-save-hook` or `after-save-hook` that signals restores the buffer text and is refused: `buffer save failed`, followed by that message. Nil, `""`, a trailing slash, and the filesystem root stay the path refusals those tools already use.

- [x] Attach the editor to a prompt sent from a file buffer. The attachment names the buffer, the file, the major mode, the line and column at point, and whether the buffer is modified or narrowed. The line is counted from the start of the buffer. The column starts at 1. An active region is included, cut at `harmless-read-file-max-bytes` the same way `harmless-tool-read-file` cuts a file. No region sends the line at point and the name `which-function` can find. A prompt sent from the transcript or the prompt window attaches the other window in the selected frame when that window shows a file buffer, and attaches no file when it does not. A non-file buffer is named, with its major mode, and contributes no file text. The minibuffer and a buffer whose name starts with a space are not attached. `harmless-steer` from a file buffer attaches that buffer. The file and the transcript keep their text. The prompt buffer is what gets cleared. Nil, `""`, and whitespace stay `Prompt is empty`.

- [x] Visit `FILE:LINE` and `FILE:LINE:COLUMN` from the transcript. A button on a path in tool output or a reply opens that file at that position in the other window. The column starts at 1. These buttons stay in the transcript and do not take `next-error` from another buffer. A path outside the project and outside an extra directory is refused: `Path escapes project: `, followed by the path. Nil, `""`, and the filesystem root are refused and do not visit a buffer. A missing file is refused: `Cannot read `, followed by the path. A directory and a trailing slash are refused: `path is not a file`. Line 0, a negative line, and a column that is not a positive integer are refused: `line is not a line` or `column is not a column`.

- [x] Ask from a file buffer, and replace that text with the reply. A minor mode, off until it is enabled, binds the commands under its own prefix and does not rebind `C-c C-c` in other modes. The same commands have `M-x` names. The command sends the active region, or the defun at point when the region is inactive, to the live session for that project. None starts a second session; with no session it opens one the way `harmless-current` does. A prefix argument replaces that region with the reply, as one undo entry, and does not save the buffer. An empty region and a point with no defun are refused: `region is empty`. A read-only buffer is refused: `buffer is read-only`. The transcript, the prompt, and the minibuffer are refused: `buffer is not a file`. An empty reply inserts nothing and leaves the region.

- [x] Read one buffer the user can see. The model names it and receives its accessible text, cut at `harmless-read-file-max-bytes`. A missing buffer is refused: `buffer is not live`. Nil, `""`, and whitespace are refused: `buffer name is empty`. A value that is not text is refused: `buffer name must be a string`. The minibuffer, a buffer whose name starts with a space, and a Harmless process buffer are refused: `buffer is not readable`. The transcript of another session is refused: `buffer is another session`.

- [x] Attach Dired marks or the Org subtree at point. From Dired, the attachment is the marked file names, at most 200, the same cap as `@folder:`. No mark uses the file at point. A mark outside the project is refused and nothing is attached: `Path escapes project: `, followed by the path. From an Org buffer, the current subtree is the attachment, cut at `harmless-read-file-max-bytes`. An empty subtree is refused: `region is empty`. The filesystem root, nil, and `""` are refused and contribute no attachment.

- [ ] Complete at point with an overlay, off until `harmless-complete` is enabled. Tab accepts the overlay and another key dismisses it. The request sends the text around point, cut at `harmless-read-file-max-bytes`, and does not send the session transcript or call a tool. A read-only buffer, the minibuffer, and a buffer whose name starts with a space show no overlay. Nil and `""` from the model insert nothing.

## Not planned

Theming, voice, wake word, text-to-speech, skins, and the status line belong to the terminal UIs. Voice is in Grok Build, Codex, and Hermes. Hermes also runs one agent from Telegram, Discord, Slack, WhatsApp, and the other gateway chats, and it ships a desktop app. A Slack receipt for a person is section 4. A gateway that is the session stays out of this Emacs package. Vendor-hosted phone push, claude.ai routines, and a separate PowerShell tool are out of scope for this Emacs package.
