# Harmless

Harmless is an AI coding harness that lives entirely inside Emacs.  It is
written in Emacs Lisp.  It is not [gptel](https://github.com/karthink/gptel),
and it does not wrap Grok Build, Claude CLI, or any other external agent.

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

Put the API key in `~/.authinfo` or `~/.authinfo.gpg`:

```
machine api.x.ai login apikey password xai-...
```

Or export `XAI_API_KEY`.  Then in your init file or `~/.harmless/config.el`:

```elisp
(setq harmless-providers (list (harmless-make-xai))
      harmless-default-model "grok-4.6")
```

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
| `M-x harmless-abort` | Cancel the in-flight turn or shell |

In the prompt window: `C-c C-c` sends.  When a tool needs approval: `y` allow,
`n` deny, `!` always allow that class for the rest of the session.

Permission modes: `ask` (default), `accept-edits`, `always-approve`.  Shell
commands still prompt in `accept-edits`.

## Layout

Each session is a read-only transcript buffer plus a small prompt window at
the bottom.  The dashboard is a tabulated list.  Sessions are stored under
`~/.harmless/sessions/`; the buffer is a view, not the source of truth.

## Development

```shell
make test      # byte-compile with warnings as errors, then ERT
make compile
make clean
```

## License

3-clause BSD.  See [LICENSE](LICENSE).  Copyright (c) 2026 Christopher Mark
Gore, Soli Deo Gloria.
