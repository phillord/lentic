EMACS ?= emacs
CASK ?= cask

all: install test

install:
	cask install

test:
	cask exec ert-runner


.PHONY: test
