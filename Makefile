EMACS ?= emacs
LISPDIR := lisp
TESTDIR := test

LISP_FILES := $(wildcard $(LISPDIR)/*.el)
TEST_FILES := $(wildcard $(TESTDIR)/*-tests.el)

BATCH := $(EMACS) -Q --batch --eval "(setq load-prefer-newer t)"

.PHONY: all compile test autoloads clean version-check

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
	$(BATCH) -L $(LISPDIR) -L $(TESTDIR) \
	  $(foreach f,$(TEST_FILES),-l $(notdir $(f))) \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f $(LISPDIR)/*.elc $(TESTDIR)/*.elc $(LISPDIR)/harmless-autoloads.el
