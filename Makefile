# Makefile to export org documents to md for Hugo from the command
# line.
#
# Run "make" to see the help for this Makefile.

MAKE_ := $(MAKE) -j1 --no-print-directory

CURL ?= curl -fsSkL --retry 9 --retry-delay 9

USER ?= me
OX_HUGO_TMP_DIR_BASE ?= /tmp/$(USER)
ox_hugo_tmp_dir ?= $(OX_HUGO_TMP_DIR_BASE)/ox-hugo-dev

ifdef INSIDE_EMACS
	EMACS := $(shell which emacs)
else
	EMACS ?= emacs
endif

EMACS_exists := $(shell command -v $(EMACS) 2> /dev/null)
ifeq ("$(EMACS_exists)","")
	EMACS := /tmp/emacs/bin/emacs
endif

EMACS_BIN_SOURCE ?= https://github.com/npostavs/emacs-travis/releases/download/bins
EMACS_BIN_VERSION ?= 27

# In Netlify, set HUGO env var to something like "/tmp/me/ox-hugo-dev/hugo/bin/hugo"
# to use the custom-built hugo dev binary.
HUGO ?= hugo
HUGO_exists := $(shell command -v $(HUGO) 2> /dev/null)
ifeq ("$(HUGO_exists)","")
	HUGO := $(ox_hugo_tmp_dir)/hugo/bin/hugo
endif

HTMLTEST ?= htmltest
HTMLTEST_exists := $(shell command -v $(HTMLTEST) 2> /dev/null)
ifeq ("$(HTMLTEST_exists)","")
	HTMLTEST := $(ox_hugo_tmp_dir)/htmltest/bin/htmltest
endif

PANDOC ?= pandoc
PANDOC_exists := $(shell command -v $(PANDOC) 2> /dev/null)
ifeq ("$(PANDOC_exists)","")
	PANDOC := $(ox_hugo_tmp_dir)/pandoc/bin/pandoc
endif

HUGO_BIN_SOURCE ?= https://gitlab.com/kaushalmodi/unofficial-hugo-dev-builds.git
HUGO_VERSION ?= DEV

PANDOC_BIN_VERSION ?= 2.16.2
PANDOC_ARCHIVE_NAME ?= pandoc-$(PANDOC_BIN_VERSION)-linux-amd64.tar.gz
PANDOC_BIN_SOURCE ?= https://github.com/jgm/pandoc/releases/download/$(PANDOC_BIN_VERSION)

# baseURL value set via environment variable HUGO_BASEURL
HUGO_BASEURL ?= http://localhost

# Directory containing the Hugo site's config.toml
HUGO_BASE_DIR=./
# Other hugo arguments
HUGO_ARGS=

# Port for hugo server
PORT=1337

# Below is set to 1 during "make test"
TEST_ENABLED=0

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

.PHONY: help emacs-batch md1 \
	vcheck_emacs vcheck_hugo vcheck_pandoc vcheck \
	hugo hugo_doc hugo_test serve server diff \
	test md testmkgold \
	do_test $(test_org_files) \
	doc_md doc_gh doc doc_htmltest doc_test \
	ctemp diffgolden clean

help:
	@echo "Help for command-line Org->Markdown for Hugo Exporter"
	@echo "====================================================="
	@echo " make -j1 test      <-- Run test with checks enabled"
	@echo " make md            <-- Only export the test Org files to Markdown, no checks"
	@echo " make doc           <-- Build both Doc Site contents and GitHub docs"
	@echo " make FOO.org       <-- Export the FOO.org file from content-org/ dir to Markdown file(s)"
	@echo " make vcheck        <-- Print Emacs, Org and Pandoc versions"
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
	@env HOME=$(shell pwd)/test $(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_TMP_DIR\" \"$(ox_hugo_tmp_dir)\")\
	(setenv \"TEST_ENABLED\" \"$(TEST_ENABLED)\")\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(OX_HUGO_TEST_DIR)\"))\
	)" $(ORG_FILE) \
	-f $(FUNC) \
	--kill

md1:
	@$(MAKE_) emacs-batch FUNC=org-hugo-export-all-wim-to-md

vcheck_emacs:
	@mkdir -p $(ox_hugo_tmp_dir)
ifeq ("$(EMACS_exists)","")
	@$(CURL) -O $(EMACS_BIN_SOURCE)/emacs-bin-$(EMACS_BIN_VERSION).tar.gz
	@tar xf emacs-bin-$(EMACS_BIN_VERSION).tar.gz -C /
endif
	@echo "Emacs binary used: $(EMACS)"
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_TMP_DIR\" \"$(ox_hugo_tmp_dir)\")\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(OX_HUGO_TEST_DIR)\"))\
	(message \"[Version check] Emacs %s\" emacs-version)\
	(message \"[Version check] %s\" (org-version nil :full))\
	)" \
	--kill

vcheck_hugo:
	@mkdir -p $(ox_hugo_tmp_dir)
ifeq ("$(HUGO_exists)","")
	@mkdir -p $(ox_hugo_tmp_dir)/hugo
	@find $(ox_hugo_tmp_dir)/hugo -maxdepth 1 -type d -name bin -exec rm -rf "{}" \;
	@git clone $(HUGO_BIN_SOURCE) $(ox_hugo_tmp_dir)/hugo/bin
	@tar xf $(ox_hugo_tmp_dir)/hugo/bin/hugo_DEV-Linux-64bit.tar.xz -C $(ox_hugo_tmp_dir)/hugo/bin
endif
	$(HUGO) version

vcheck_pandoc:
	@mkdir -p $(ox_hugo_tmp_dir)
ifeq ("$(PANDOC_exists)","")
	@mkdir -p $(ox_hugo_tmp_dir)/pandoc
	@find $(ox_hugo_tmp_dir)/pandoc -maxdepth 1 -type d -name bin -exec rm -rf "{}" \;
	@$(CURL) -O $(PANDOC_BIN_SOURCE)/$(PANDOC_ARCHIVE_NAME)
	@tar xf $(PANDOC_ARCHIVE_NAME)
	@mv pandoc-$(PANDOC_BIN_VERSION)/bin $(ox_hugo_tmp_dir)/pandoc/.
	@rm -rf pandoc-$(PANDOC_BIN_VERSION)
endif
	$(PANDOC) --version

vcheck: vcheck_emacs vcheck_hugo vcheck_pandoc

hugo: vcheck_hugo
	@cd $(HUGO_BASE_DIR) && $(HUGO) $(HUGO_ARGS)

hugo_doc:
	@$(MAKE_) hugo HUGO_BASE_DIR=./doc HUGO_BASEURL=https://ox-hugo.scripter.co/

hugo_test:
	@$(MAKE_) hugo HUGO_BASE_DIR=./test/site HUGO_BASEURL=https://ox-hugo.scripter.co/test/ HUGO_ARGS=--buildDrafts

serve server: vcheck_hugo
	@echo "Serving the site on $(HUGO_BASEURL):$(PORT) .."
	@cd $(HUGO_BASE_DIR) && $(HUGO) server --port $(PORT) --buildDrafts --buildFuture --navigateToChanged

diff:
	@git diff

test: vcheck_emacs vcheck_pandoc testmkgold do_test

md: vcheck_emacs vcheck_pandoc
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
	@$(MAKE_) md1 ORG_FILE=$@ TEST_ENABLED=1
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

doc: vcheck_emacs doc_md hugo_doc doc_gh

doc_htmltest:
ifeq ("$(HTMLTEST_exists)","")
	@mkdir -p $(ox_hugo_tmp_dir)/htmltest
	@find $(ox_hugo_tmp_dir)/htmltest -maxdepth 1 -type d -name bin -exec rm -rf "{}" \;
	@git clone $(HUGO_BIN_SOURCE) $(ox_hugo_tmp_dir)/htmltest/bin
	@tar xf $(ox_hugo_tmp_dir)/htmltest/bin/htmltest_DEV-Linux-64bit.tar.xz -C $(ox_hugo_tmp_dir)/htmltest/bin
endif
	@cd doc && $(HTMLTEST)
doc_test: doc doc_htmltest

ctemp:
	@find $(OX_HUGO_TEST_SITE_DIR)/content -name "*.*~" -delete
	@find ./doc/content -name "*.*~" -delete

# Get rid of all changes in $(OX_HUGO_TEST_SITE_DIR)/content after
# making a copy to $(OX_HUGO_TEST_SITE_DIR)/content-modified.
# https://stackoverflow.com/a/16589534/1219634
diffgolden:
	@rm -rf $(OX_HUGO_TEST_SITE_DIR)/content-modified
	@cp -rf $(OX_HUGO_TEST_SITE_DIR)/content $(OX_HUGO_TEST_SITE_DIR)/content-modified
	@git checkout --ignore-skip-worktree-bits -- $(OX_HUGO_TEST_SITE_DIR)/content
	@diff -r $(OX_HUGO_TEST_SITE_DIR)/content-modified $(OX_HUGO_TEST_SITE_DIR)/content-golden

clean: ctemp
	find $(OX_HUGO_TMP_DIR_BASE)  -maxdepth 1 -type d -name ox-hugo-dev -exec rm -rf "{}" \;
	find $(OX_HUGO_TEST_SITE_DIR) -maxdepth 1 -type d \( -name public -o -name content-golden -o -name content-modified \) -exec rm -rf "{}" \;
	find ./doc -maxdepth 1 -type d -name public -exec rm -rf "{}" \;
	find ./doc/content -name "*.md" -delete
	rm -f ox-hugo-autoloads.el

# Set a make variable during rule execution
# https://stackoverflow.com/a/1909390/1219634

# Check if an executable exists
# https://stackoverflow.com/a/34756868/1219634
