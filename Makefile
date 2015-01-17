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

COMMIT_DATE = $(shell date +%y-%m-%d-%H-%m)

commit-distrib: info
	cp lentic*.el ../lentic-distrib
	cp lentic*.info ../lentic-distrib
	cp dir ../lentic-distrib
	cd ../lentic-distrib;git add -A;git commit -m "automated-commit $(COMMIT_DATE)"

infoclean: info clean-butinfo

clean-butinfo:
	-rm *.org
	-rm *.texi

clean: clean-butinfo
	-rm *.info

.PHONY: test org
