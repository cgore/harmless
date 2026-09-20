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

Requires **GNU Emacs 32**.

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
      harmless-default-model "grok-4.6")
```

Then `M-x harmless-login`.  That command is the shared entry for every
provider login we add; with only xAI registered it goes straight there.
Prefix argument (`C-u M-x harmless-login`) uses xAI's device-code flow,
which is also used automatically if port 56121 is already taken (for
example by Grok Build itself).

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
      harmless-default-model "gpt-4o")
```

Anthropic:

```elisp
(setq harmless-providers (list (harmless-make-anthropic "Anthropic"))
      harmless-default-model "claude-sonnet-4-5")
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
| `M-x harmless-menu` | Transient: new / switch / model / permissions / abort |
| `M-x harmless-login` | Sign in (choose a provider once several exist; xAI today) |
| `M-x harmless-logout` | Sign out of a provider |
| `M-x harmless-abort` | Cancel the in-flight turn or shell |

In the prompt window: `C-c C-c` sends.  When a tool needs approval: `y` allow,
`n` deny, `!` always allow that class for the rest of the session.

Permission modes: `ask` (default), `accept-edits`, `always-approve`.  Shell
commands still prompt in `accept-edits`.

## Layout

Each session is a read-only transcript buffer plus a small prompt window at
the bottom.  The dashboard is a tabulated list.  Config and sessions live
under `harmless-directory`, which defaults to `harmless/` in
`user-emacs-directory` (so `~/.emacs.d/harmless/` for a typical setup).
The buffer is a view, not the source of truth.

## Development

```shell
make test      # byte-compile with warnings as errors, then ERT
make compile
make clean
```

## License

3-clause BSD.  See [LICENSE](LICENSE).  Copyright (c) 2026 Christopher Mark
Gore, Soli Deo Gloria.
