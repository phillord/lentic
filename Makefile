EMACS ?= /usr/local/bin/emacs
CASK ?= cask

all: install test

install:
	cask install

test:
	cask exec ert-runner

org:
	cask exec emacs --debug --script build.el -- gen-org

texinfo: org
	cask exec emacs --debug --script build.el -- gen-texinfo

info: texinfo
	makeinfo lentic.texi

html: org
	cask exec emacs --debug --script build.el -- gen-html


COMMIT_DATE = $(shell date +%y-%m-%d-%H-%m)
DISTRIB-LENTIC=../distrib-lentic

# commit-distrib: info
# 	cp lentic*.info $(DISTRIB-LENTIC)
# 	cd $(DISTRIB-LENTIC);git pull;git add -A;git commit -m "automated-commit $(COMMIT_DATE)"

infoclean: info clean-butinfo

clean-butinfo:
	-rm lentic.org
	-rm lentic-*.org
	-rm *.texi

clean: clean-butinfo
	-rm *.info

-include Makefile-local

.PHONY: test org

