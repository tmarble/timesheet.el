# Makefile for timesheet.el
# This Makefile is only used during development to create a tarball
# for rapid prototyping

ORIGIN := $(shell git remote -v | awk '{print $$2; exit;}' | sed 's/.git$$//' )
REPO := $(shell basename $(ORIGIN))
NAME := $(subst .el,,$(REPO))
PKG := $(NAME)-pkg.el
DESC := $(shell head -1 $(REPO) | sed 's/^.*--- \(.*\)$$/\1/' )
VERSION := $(shell awk '/Version:/ { print $$3; }' $(REPO))
TIMESHEET_VERSION := $(shell awk 'BEGIN {FS="\"";} /defconst timesheet-version/ {print $$2;}' $(REPO))
REQUIRES := '(quote $(shell awk '/Package-Requires:/ { for (i = 3; i <= NF; i++) printf("%s ", $$i); print ""; }' $(REPO)) )'
TARBALL := $(NAME)-$(VERSION).tar

all: tarball

check-version:
	@if [ "$(TIMESHEET_VERSION)" != "$(VERSION)" ]; then echo "error: update timesheet-version to match ;; Version:"; exit 1; fi
	@echo building $(REPO) version $(VERSION)

$(PKG): check-version $(REPO)
	@echo updating package meta information from $< to $@
	printf "(define-package \"%s\" \"%s\" \"%s\" %s)" $(NAME) $(VERSION) "$(DESC)" $(REQUIRES) > $@

pkg: $(PKG)

tarball: pkg
	rm -f *.tar
	tar cf $(TARBALL) --exclude='*.tar' --exclude='*~' --exclude=auto --exclude=example.emacs.d --transform "s%^./%$(NAME)-$(VERSION)/%" ./*

vars:
	@echo ORIGIN=$(ORIGIN)
	@echo REPO=$(REPO)
	@echo NAME=$(NAME)
	@echo PKG=$(PKG)
	@echo DESC=$(DESC)
	@echo VERSION=$(VERSION)
	@echo TIMESHEET_VERSION=$(TIMESHEET_VERSION)
	@echo REQUIRES=$(REQUIRES)
	@echo TARBALL=$(TARBALL)
