EMACS ?= emacs
CC ?= gcc
UNAME_S := $(shell uname -s)
LISPDIR := lisp
TESTDIR := test

LISP_FILES := $(wildcard $(LISPDIR)/*.el)
TEST_FILES := $(wildcard $(TESTDIR)/*-tests.el)

BATCH := $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)"

.PHONY: all compile test autoloads clean version-check manual

src/harmless-landlock: src/harmless-landlock.c
	$(CC) -O2 -Wall -Wextra -o $@ $<

all: test

version-check:
	@$(BATCH) --eval "(unless (>= emacs-major-version 31) (error \"Emacs 31 required, got %s\" emacs-version))"

compile: version-check
	$(BATCH) -L $(LISPDIR) \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $(LISP_FILES)

autoloads: version-check
	$(BATCH) -L $(LISPDIR) \
	  --eval "(loaddefs-generate \"$(LISPDIR)\" \"$(LISPDIR)/harmless-autoloads.el\" nil nil nil t)"

test: compile
ifeq ($(UNAME_S),Linux)
test: src/harmless-landlock
endif
	$(BATCH) -L $(LISPDIR) -L $(TESTDIR) \
	  $(foreach f,$(TEST_FILES),-l $(notdir $(f))) \
	  -f ert-run-tests-batch-and-exit

manual:
	makeinfo --no-split -o doc/harmless.info doc/harmless.texi

clean:
	rm -f $(LISPDIR)/*.elc $(TESTDIR)/*.elc $(LISPDIR)/harmless-autoloads.el \
	  src/harmless-landlock
