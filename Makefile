
EMACS ?= /usr/local/bin/emacs
CASK ?= cask

all: install test

install:
	cask install

test: install just-test

just-test:
	cask exec ert-runner $(TESTS)

org:
	cask exec emacs --debug --script build.el -- gen-org

html: org
	cask exec emacs --debug --script build.el -- gen-html

install-test:
	echo [install] Installation Test Starting
	$(MAKE) -C test/install-test/ test

travis: test install-test html

COMMIT_DATE = $(shell date +%y-%m-%d-%H-%m)
DISTRIB-LENTIC=../distrib-lentic

# commit-distrib: info
# 	cp lentic*.info $(DISTRIB-LENTIC)
# 	cd $(DISTRIB-LENTIC);git pull;git add -A;git commit -m "automated-commit $(COMMIT_DATE)"

clean:
	-rm lentic.org
	-rm lentic-*.org
	-rm lenticular.html


-include Makefile-local

.PHONY: test org
