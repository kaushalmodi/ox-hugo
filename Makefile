# Time-stamp: <2018-02-01 00:29:00 kmodi>

# Makefile to export org documents to md for Hugo from the command line
# Run just "make" to see usage examples.

MAKE_ := $(MAKE) --no-print-directory

EMACS ?= emacs
EMACS_exists := $(shell command -v $(EMACS) 2> /dev/null)
ifeq ("$(EMACS_exists)","")
	EMACS := /tmp/emacs/bin/emacs
endif

# EMACS_BIN_SOURCE and EMACS_BIN_VERSION are used later in the vcheck rule
# only if EMACS_exists has evaluated to "".
EMACS_BIN_SOURCE ?= https://github.com/npostavs/emacs-travis/releases/download/bins
EMACS_BIN_VERSION ?= 26

HUGO ?= hugo
HUGO_exists := $(shell command -v $(HUGO) 2> /dev/null)
ifeq ("$(HUGO_exists)","")
	HUGO := /tmp/hugo/bin/hugo
endif

# HUGO_BIN_SOURCE and HUGO_VERSION are used later in the vcheck rule
# only if HUGO_exists has evaluated to "".
HUGO_BIN_SOURCE ?= https://gitlab.com/kaushalmodi/unofficial-hugo-dev-builds.git
HUGO_VERSION ?= DEV

# baseURL value set via environment variable HUGO_BASEURL
HUGO_BASEURL ?= http://localhost

# Directory containing the Hugo site's config.toml
HUGO_BASE_DIR=./
# Other hugo arguments
HUGO_ARGS=

# Set TIMEZONE to the TZ environment variable. If TZ is unset, Emacs
# uses system wall clock time, which is a platform-dependent default
# time zone --
# https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Zone-Rules.html
TIMEZONE=${TZ}

# Port for hugo server
PORT=1337

# Directory where the required elisp packages are auto-installed
TMPDIR ?= /tmp
OX_HUGO_ELPA=$(TMPDIR)/$(USER)/ox-hugo-dev/

# ox-hugo test directory; also contains the setup-ox-hugo.el
OX_HUGO_TEST_DIR=$(shell pwd)/test

# Base directory for the Hugo example site
OX_HUGO_TEST_SITE_DIR=$(OX_HUGO_TEST_DIR)/site

# Directory containing Org files for the test site
OX_HUGO_TEST_ORG_DIR=$(OX_HUGO_TEST_SITE_DIR)/content-org
# https://stackoverflow.com/a/3774731/1219634
# Note that the use of immediate assignment := rather than recursive
# assignment = is important here: you do not want to be running the
# shell escape every time SOURCES is inspected by make.
test_org_files := $(shell find ${OX_HUGO_TEST_ORG_DIR} -type f -name '*.org')

# Path to the Org file (relative to pwd, or absolute)
ORG_FILE=

# Function to be run in emacs --batch
FUNC=

test_check=1

.PHONY: help emacs-batch md1 vcheck hugo hugo_doc hugo_test serve server diff \
	test md testmkgold \
	do_test $(test_org_files) \
	doc_md doc_gh doc \
	ctemp diffgolden clean

help:
	@echo "Help for command-line Org->Markdown for Hugo Exporter"
	@echo "====================================================="
	@echo " make test          <-- Run test with checks enabled"
	@echo " make md            <-- Only export the test Org files to Markdown, no checks"
	@echo " make doc           <-- Build both Doc Site contents and GitHub docs"
	@echo " make FOO.org       <-- Export the FOO.org file from content-org/ dir to Markdown file(s)"
	@echo " make vcheck        <-- Print emacs and Org versions"
	@echo " make hugo          <-- Run hugo"
	@echo " make serve         <-- Run the hugo server on http://localhost:$(PORT)"
	@echo " make diff          <-- Run git diff"
	@echo " make doc_md        <-- Build the Markdown content for the documentation site"
	@echo " make hugo_doc      <-- Build the documentation site using Hugo"
	@echo " make doc_gh        <-- Build README.org and CONTRIBUTING.org for GitHub"
	@echo " make hugo_test     <-- Build the test site using Hugo"
	@echo " make clean         <-- Delete the Hugo public/ directory and auto-installed elisp packages"
	@echo " make               <-- Show this help"

# Note: The Org file from $(ORG_FILE) is loaded *after* the --eval
# section gets evaluated i.e. --eval '(progn ..)' $(ORG_FILE) If the
# order is reversed i.e. i.e.$(ORG_FILE) --eval '(progn ..)', the act
# of loading the $(ORG_FILE) file first will load the older Org
# version that ships with Emacs and then run the stuff in --eval that
# loads the new Org version.. and thus we'll end up with mixed Org in
# the load-path.
emacs-batch:
	@echo ""
	@echo "$(ORG_FILE) ::"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_ELPA\" \"$(OX_HUGO_ELPA)\")\
	(when (> (length \"$(TIMEZONE)\") 0) (setenv \"TZ\" \"$(TIMEZONE)\"))\
	(setq-default make-backup-files nil)\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(OX_HUGO_TEST_DIR)\"))\
	)" $(ORG_FILE) \
	-f $(FUNC) \
	--kill

md1:
	@$(MAKE_) emacs-batch FUNC=org-hugo-export-all-wim-to-md

vcheck:
ifeq ("$(EMACS_exists)","")
	@curl -fsSkL --retry 9 --retry-delay 9 -O $(EMACS_BIN_SOURCE)/emacs-bin-$(EMACS_BIN_VERSION).tar.gz
	@tar xf emacs-bin-$(EMACS_BIN_VERSION).tar.gz -C /
endif
	@echo "Emacs binary used: $(EMACS)"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_ELPA\" \"$(OX_HUGO_ELPA)\")\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(OX_HUGO_TEST_DIR)\"))\
	(message \"[Version check] Emacs %s\" emacs-version)\
	(message \"[Version check] %s\" (org-version nil :full))\
	)" \
	--kill
ifeq ("$(HUGO_exists)","")
	@mkdir -p /tmp/hugo
	@find /tmp/hugo -maxdepth 1 -type d -name bin -exec rm -rf "{}" \;
	@git clone $(HUGO_BIN_SOURCE) /tmp/hugo/bin
	@tar xf /tmp/hugo/bin/hugo_DEV-Linux-64bit.tar.xz -C /tmp/hugo/bin
endif
	@$(HUGO) version

hugo: vcheck
	@cd $(HUGO_BASE_DIR) && $(HUGO) $(HUGO_ARGS)

hugo_doc:
	@$(MAKE_) hugo HUGO_BASE_DIR=./doc HUGO_BASEURL=https://ox-hugo.scripter.co/

hugo_test:
	@$(MAKE_) hugo HUGO=/tmp/hugo/bin/hugo HUGO_BASE_DIR=./test/site HUGO_BASEURL=https://ox-hugo.scripter.co/test/ HUGO_ARGS=--buildDrafts

serve server: vcheck
	@echo "Serving the site on $(HUGO_BASEURL):$(PORT) .."
	@cd $(HUGO_BASE_DIR) && $(HUGO) server --port $(PORT) --buildDrafts --buildFuture --navigateToChanged

diff:
	@git diff

test: vcheck testmkgold do_test

md:
	@$(MAKE_) do_test test_check=0

# Get rid of all changes in $(OX_HUGO_TEST_SITE_DIR)/content.
# https://stackoverflow.com/a/16589534/1219634
testmkgold:
	@git checkout --ignore-skip-worktree-bits -- $(OX_HUGO_TEST_SITE_DIR)/content
	@rm -rf $(OX_HUGO_TEST_SITE_DIR)/content-golden
	@cp -rf $(OX_HUGO_TEST_SITE_DIR)/content $(OX_HUGO_TEST_SITE_DIR)/content-golden

# Run the md1 + diffgolden rules in loop on all of $(test_org_files)
# https://stackoverflow.com/a/37748952/1219634
do_test: $(test_org_files)
$(test_org_files):
	@$(MAKE_) md1 ORG_FILE=$@ TIMEZONE=UTC # Use UTC/Universal time zone for tests
ifeq ($(test_check),1)
	@$(MAKE_) diffgolden
endif

doc_md:
	@echo "[Doc Site] Generating ox-hugo Documentation Site content .."
	@$(MAKE_) md1 ORG_FILE=./doc/ox-hugo-manual.org
	@echo "[Doc Site] Done"

doc_gh:
	@echo "[GitHub Docs] Generating README.org and CONTRIBUTING.org for GitHub .."
	@$(MAKE_) emacs-batch FUNC=ox-hugo-export-gh-doc ORG_FILE=./doc/github-files.org
	@echo "[GitHub Docs] Done"

doc: doc_md hugo_doc doc_gh

ctemp:
	@find $(OX_HUGO_TEST_SITE_DIR)/content -name "*.*~" -delete
	@find ./doc/content -name "*.*~" -delete

# Before doing the diff, replace the randomly generated org reference
# id's like "org123abcd" with "orgxxxxxxx" so that the diff doesn't
# fail on those id mismatches.
# Also get rid of all changes in $(OX_HUGO_TEST_SITE_DIR)/content
# after making a copy to $(OX_HUGO_TEST_SITE_DIR)/content-modified.
# https://stackoverflow.com/a/16589534/1219634
diffgolden:
	@rm -rf $(OX_HUGO_TEST_SITE_DIR)/content-modified
	@cp -rf $(OX_HUGO_TEST_SITE_DIR)/content $(OX_HUGO_TEST_SITE_DIR)/content-modified
	@git checkout --ignore-skip-worktree-bits -- $(OX_HUGO_TEST_SITE_DIR)/content
	@find $(OX_HUGO_TEST_SITE_DIR)/content-modified -name "*.md" | xargs sed -r -i 's/(["#]org)([a-f0-9]{7})/\1xxxxxxx/'
	@find $(OX_HUGO_TEST_SITE_DIR)/content-golden -name "*.md" | xargs sed -r -i 's/(["#]org)([a-f0-9]{7})/\1xxxxxxx/'
	@diff -r $(OX_HUGO_TEST_SITE_DIR)/content-modified $(OX_HUGO_TEST_SITE_DIR)/content-golden

clean: ctemp
	@find ./doc/content -name "*.md" -delete
	@rm -rf $(OX_HUGO_TEST_SITE_DIR)/public $(OX_HUGO_TEST_SITE_DIR)/content-golden
	@rm -rf $(OX_HUGO_ELPA)
	@rm -rf ./doc/public
	@rm -rf /tmp/hugo/bin

# Set a make variable during rule execution
# https://stackoverflow.com/a/1909390/1219634

# Check if an executable exists
# https://stackoverflow.com/a/34756868/1219634
