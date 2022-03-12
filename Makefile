SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

PKG-FILES := watch-cursor.el

TEST-FILES := $(shell ls test/watch-cursor-*.el)

.PHONY: clean checkdoc lint build compile unix-test

ci: clean build compile

build:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask build
	EMACS=$(EMACS) cask clean-elc

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(PKG-FILES)

unix-test:
	@echo "Testing..."
	$(CASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
