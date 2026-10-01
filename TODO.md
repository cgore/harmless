# TODO

Gaps versus Grok Build, Claude Code, the Codex CLI, and [OpenRig](https://openrig.dev/) 0.5.14, in the order that changes whether the work can finish. Sections 1–3 are one session. Section 4 is the lasting team OpenRig runs above sessions like these. An item is listed once. A parenthetical names the products that already have it when that is not all three.

Harmless already has the core loop: sessions, streaming turns, `read_file`, `write_file`, `replace`, `list_dir`, `glob`, `grep`, `run_shell`, permission classes, project instructions, a skill catalog, memory, and plan mode.

## 1. Delegation

The model cannot start work that runs beside the current turn.

- [ ] Run tool calls from one turn in parallel. Today `harmless-turn--run-tools` runs them one after another.
- [ ] Subagents. A child session with its own context, started by the model, that reports a summary back. A definition file can give that child its own prompt, tools, model, and sandbox (Claude Code `.claude/agents/`, Codex custom agents).
- [ ] Workflows. One job that runs many child sessions and returns one result: a fixed script (Grok Build), or a script the model writes for this task (Claude Code dynamic workflows, Codex subagent workflows).
- [ ] Git worktrees, so parallel sessions edit isolated checkouts of the same repository (Claude Code `EnterWorktree`, Codex, Grok Build subagent worktrees).
- [ ] Cross-session messaging (Claude Code). One live session can send a finding to another session on this machine. A message that obligates another seat, and survives that seat being replaced, is section 4.

## 2. Network

The shell is the only way out of the project. A skill that needs any of these is only a `SKILL.md` the model can read.

- [ ] Web search.
- [ ] Fetch a page.
- [ ] X search (Grok Build): posts, threads, and users.
- [ ] MCP client, so configured servers show up next to the built-in tools. Load a tool's schema when it is about to be called, and read MCP resources, not only call tools (Claude Code).
- [ ] Computer use and browser control (Claude Code, Codex). See the screen, click, and type, including a browser the user can watch.

## 3. Long sessions

Every turn resends the full transcript. Sending a new prompt aborts a turn that is streaming or waiting on permission.

- [ ] Compact the transcript, including an automatic compact as the context window fills.
- [ ] Rewind to an earlier turn and drop what follows. Codex also edits an earlier prompt and reruns from there.
- [ ] Fork a session, keeping history up to this point (Grok Build, Codex, and Claude Code fork-mode subagents).
- [ ] Steer a running turn. A follow-up arrives without aborting the turn.

## 4. A lasting team

[OpenRig](https://openrig.dev/) 0.5.14 is a local control plane over Claude Code, Codex, and Pi. It keeps a named team at work for weeks. Harmless can open several sessions. It has no seats, no owned work, and no way to bring that team back. Compact, fork, and a one-off message are already listed above. A declared workflow here is durable team state. The one-shot workflow in section 1 is a script that returns one result.

A message informs. A queue row obligates. A seat outlives the session occupying it.

- [ ] A rig of pods of named seats. A seat has a role and an address (`pod-member@rig`). The session in it can be replaced. The seat keeps its name, its role, and its inbound work.
- [ ] Edges between seats. `delegates_to` and `spawned_by` decide launch order. `can_observe`, `collaborates_with`, and `escalates_to` are recorded and shown.
- [ ] Boot a team from a spec, a named starter, or a portable bundle. Preview the boot before it runs. Grow, shrink, add, and remove seats while the team runs. Removing a seat that still holds work requires a fallback seat, or the removal is refused.
- [ ] Adopt a session that is already running, including one started by hand. Release it without killing it. A seat can be a Harmless session, a Claude Code session, a Codex session, a Pi session, or a plain terminal for a dev server or a log.
- [ ] Send to a seat, several seats, a pod, or the whole rig. Check that the text appeared. Refuse to type into a seat that is sitting on a prompt or a permission question. Capture what is on a seat's screen, and read what it has said across every session that has occupied it.
- [ ] A durable chatroom for the rig, with history, topics, and a wait that blocks until someone speaks.
- [ ] Walk a paced sequence of context into a seat, one piece at a time, so onboarding is not one dump.
- [ ] A queue row: one unit of owned work, with a destination seat, a body, a state, and an append-only history. It survives the owner's turn ending, the owner compacting, and the owner being replaced.
- [ ] Claim a row, append notes, and close it only with a reason: `handed_off_to`, `blocked_on`, `denied`, `canceled`, `no-follow-on`, or `escalation`. Closing records delivery. Acceptance is the next seat's own verdict.
- [ ] Hand work to another seat in one transaction: the source row closes and the destination row opens together.
- [ ] Park a row on a real blocker, with one wake. Resolving it records the decision and wakes the owner. A park that asks a person includes a plain-language summary and a pointer to the evidence.
- [ ] An intake stream for observations nobody owns yet, and views over the board: what is held, what is stalled after a claim, and what is waiting on a person.
- [ ] A declared workflow. When the seat that owns a step closes it, the next step's row is created in the same transaction. The owner decides that the step is done.
- [ ] Watchdog jobs that survive a restart: a reminder, a pool of artifacts, a nudge to a workflow, an idle queue, or a seat approaching its context wall.
- [ ] Snapshot a rig and restore it by name. The report says, per seat, whether it resumed, was rebuilt, started fresh, is waiting on a decision, or failed. A fresh start is a choice, and it is labeled as one. Check whether a restore can succeed before running it.
- [ ] Hand one seat to a successor and keep its name, its edges, and its inbound work. Write that seat's recap of decisions at the boundary.
- [ ] An agent image: capture a productive seat and start another seat from it. A restore packet carries what a seat knows across a change of runtime.
- [ ] Plan compaction for seats near the context wall, then compact deliberately and tell the rest of the team. The compact itself is section 3.
- [ ] Work on disk as a tree: a project holds missions, a mission holds slices. Intent composes down the tree. The slice is the only unit that is specified and proved. Proof is evidence in the tree, with attributed judgments. Progress is derived from that evidence.
- [ ] Context packs addressed by ref, including one section of a file. Compose a profile for a fresh seat, a handover, or a return from compaction, and report the token total instead of truncating it.
- [ ] Reconcile the skill loadout for one working directory, and inspect a plugin's skills, hooks, and MCP servers. Installing a plugin stays an explicit copy.
- [ ] A registry of other machines. The same team verbs run against a registered host, and one file can be copied between hosts.
- [ ] A person is an address. A queue row to that address is how the team asks for a decision, and the row's history is the receipt. Slack is the first connector. Secrets stay out of the team config.
- [ ] The model can boot, inspect, message, snapshot, and restore the team through tools, with the same guards as the commands a person runs.
- [ ] Named auth profiles per runtime. Switch a seat to another account without stranding its conversation. The team record never stores the token.
- [ ] A health surface that says what is true, and a recovery command when a live session and the record disagree. A rig can also run services beside its seats and report those services' health separately.
- [ ] One fleet view: topology, health, and the rows that need a person. The dashboard today lists sessions by project.

## 5. Processes

`run_shell` waits up to `harmless-shell-timeout-seconds` (120), keeps one process on the session, and then returns. Each command starts in the session cwd.

- [ ] Leave a command running, read its output later, and kill it. A command that hits its timeout moves to the background instead of dying, and the model can raise that command's timeout (Claude Code).
- [ ] Watch a log, a file, or another stream and wake the session when a line matters (Grok Build monitors, Claude Code `Monitor`).
- [ ] Schedule a prompt to run later. Claude Code restores an unexpired session schedule on resume. A watchdog that survives a restart and can watch a queue or a context wall is section 4.
- [ ] Remember a `cd` that stays inside the project for later commands (Claude Code).
- [ ] Notify through Emacs when a background task finishes.
- [ ] In plan mode, treat a shell command that writes as a write. `write_file` and `replace` are already blocked.

## 6. Permissions and safety

The policy is three classes (`read`, `edit`, `shell`) and three modes (`ask`, `accept-edits`, `always-approve`). "Always" lasts for the session. File tools stay inside the project. The shell does not.

- [ ] Allow and deny rules for commands, paths, and fetch domains. Claude Code writes these as `Bash(npm run *)` and `Edit(/src/**)`.
- [ ] A per-project allow list that survives the session.
- [ ] A denylist for dangerous commands.
- [ ] Classify read-only shell commands so they can skip a prompt.
- [ ] Auto-review. A reviewer model or a classifier decides an escalation (Codex auto-review, Claude Code auto mode, Grok Build auto-review).
- [ ] Hooks that can block a tool call, or replace the result the model sees. Claude Code also runs a hook as an HTTP request, an MCP call, a prompt, or a subagent.
- [ ] An OS sandbox for the shell (Landlock on Linux). Codex defaults to workspace-write and asks before the network. Grok Build's sandbox is off unless asked.
- [ ] Extra directories, outside the project root, that this session may read or edit (Claude Code `--add-dir`).

## 7. Planning and tracking

Plan mode already has `enter_plan_mode`, `write_plan`, and `exit_plan_mode`, with approve, revise, or quit.

- [ ] Comment on a line or range of `plan.md` during review (Grok Build).
- [ ] A todo list the model updates while it works, with status and dependencies (Claude Code tasks, Grok Build todos).
- [ ] A read-only review of the working tree, a base branch, or one commit. The result is a list of findings the session does not apply as edits (Codex `/review`, Claude Code `ReportFindings`).

## 8. Search, reading, and the harness prompt

- [ ] Code intelligence through a language server: definition, references, and diagnostics after an edit (Claude Code `LSP`). Emacs already has the server. The model has no tool for it.
- [ ] Let `read_file` read images, PDFs, and notebooks, not only text with an offset, a limit, and a byte cap. Edit a notebook cell (Claude Code `NotebookEdit`). Accept an image attached to the user prompt (Claude Code, Codex).
- [ ] Apply a patch: several hunks across files in one call (Codex `apply_patch`, Claude Code `Edit`). `replace` changes one unique string.
- [ ] Read `CLAUDE.md` and path-scoped rules (Claude Code `.claude/rules/`), and expand `@path` imports in instruction files. Harmless already reads `AGENTS.md`, `HARMLESS.md`, and `.harmless/HARMLESS.md`.
- [ ] A standing harness prompt on every turn: tool policy, when to verify, and when to delegate. Today a turn sends project instructions, the skill catalog, the memory index, and, in plan mode, the plan rules.

## 9. Later

- [ ] Load a skill into the turn when the user invokes it or the model selects it, including an `allowed-tools` list. Today the catalog lists the name, description, and path, and the model is told to `read_file` the skill.
- [ ] Plugins and marketplaces, so a bundle of skills, hooks, agents, and MCP servers installs as one unit.
- [ ] Output styles (Claude Code): a session-wide role, tone, and format that can be switched without editing project instructions.
- [ ] Image generation (Grok Build, Codex).
- [ ] Capture a memory observation after a turn. Today that happens only through `memory_remember` or `M-x harmless-remember`. Codex and Grok Build can generate memories. Claude Code keeps auto memory alongside `CLAUDE.md`.
- [ ] Rename, delete, and export a session.
- [ ] A headless mode, and a remote task that keeps running while Emacs is closed (Codex cloud, Claude Code on the web and remote control). A registry of other machines, with the same team verbs on each, is section 4.
- [ ] Publish a Markdown or HTML artifact as a page (Claude Code).

## Not planned

Theming, voice, and the status line belong to the terminal UIs. Voice is in Grok Build and Codex. Vendor-hosted phone push, claude.ai routines, and a separate PowerShell tool are out of scope for this Emacs package.
