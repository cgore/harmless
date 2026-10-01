# Harmless

## Tests

New and changed behavior needs tests that fail if that behavior regresses. Run `make test` before you finish.

Cover the edges, not only the happy path. That includes nil, `""`, relative paths, a trailing slash, and the filesystem root. When the code writes a file or chooses a path, assert that path and the file contents. When it refuses, assert the error text. A bare `should-error` is not enough, because a different signal would still pass.

Do not weaken or delete a test to make a change pass. Fix the code, or change the test when the intended behavior changed.

Harmless state (sessions, memory, auth, config, usage) must not be written at `/`. A missing directory is not the filesystem root.

## Manual

The user manual is the Info file `doc/harmless.info`, built from `doc/harmless.texi`. Users open it with `M-x harmless-info`, with `h` in a session, dashboard, or usage buffer, or from the Harmless menu.

When you change a command, a key, a tool, a permission, or any behavior the manual describes, update `doc/harmless.texi` in the same change. Regenerate the Info file with `make manual`. Keep the Texinfo source and the generated Info file together. Do not describe a feature the code does not have. `doc/dir` is the portable directory entry: it names `(harmless)` and contains no machine path.
