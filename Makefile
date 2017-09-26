# Time-stamp: <2017-09-25 21:44:59 kmodi>

# Makefile to export org documents to md for Hugo from the command line
# Run just "make" to see usage examples.

EMACS ?= emacs

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

# Path to the Org file relative to $(OX_HUGO_TEST_ORG_DIR)
ORG_FILE=

# Function to be run in emacs --batch
FUNC=

subtree_test_files = all-posts.org \
	construct-hugo-front-matter-from-menu-meta-data.org \
	src-blocks-with-highlight-shortcode.org \
	mandatory-EXPORT_FILE_NAME-for-subtree-export.org \
	hugo-menu-as-keyword.org \
	tags-keyword.org \
	hugo-weight-as-keyword-auto-calc.org

file_test_files = single-posts/post-toml.org \
	single-posts/post-yaml.org \
	single-posts/post-draft.org

# Cannot run tests on the following files, because:
# - auto-set-lastmod.org - the lastmod field will always get updated.
# - screenshot-subtree-export-example.org - sets the org-hugo-footer using Local Variables.
# - writing-hugo-blog-in-org-file-export.org - sets the org-hugo-footer using Local Variables.

.PHONY: help mdtree mdfile vcheck hugo serve server diff \
	test vcheck testmkgold \
	test_subtree $(subtree_test_files) \
	test_file $(file_test_files) \
	doc_site gh_docs doc \
	ctemp diffgolden clean

help:
	@echo "Help for command-line Org->Markdown for Hugo Exporter"
	@echo "====================================================="
	@echo " make example.org   <-- Export the .org file from content-org/ dir to Markdown file(s)"
	@echo " make vcheck        <-- Print emacs and Org versions"
	@echo " make hugo          <-- Run hugo"
	@echo " make serve         <-- Run the hugo server on http://localhost:$(PORT)"
	@echo " make diff          <-- Run git diff"
	@echo " make doc_site      <-- Build the Markdown content for the documentation site"
	@echo " make gh_docs       <-- Build README.org and CONTRIBUTING.org for GitHub"
	@echo " make doc           <-- Build both Doc Site contents and GitHub docs"
	@echo " make test          <-- Run test"
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
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_ELPA\" \"$(OX_HUGO_ELPA)\")\
	(when (> (length \"$(TIMEZONE)\") 0) (setenv \"TZ\" \"$(TIMEZONE)\"))\
	(setq-default make-backup-files nil)\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(OX_HUGO_TEST_DIR)\"))\
	)" $(ORG_FILE_DIR)/$(ORG_FILE) \
	-f $(FUNC) \
	--kill

mdtree:
	@echo "[ox-hugo] Exporting Org to Md in 'Subtree' mode .."
	@$(MAKE) emacs-batch FUNC=org-hugo-export-all-subtrees-to-md
	@echo "[ox-hugo] Done"

mdfile:
	@echo "[ox-hugo] Exporting Org to Md in 'File' mode .."
	@$(MAKE) emacs-batch FUNC=org-hugo-export-to-md
	@echo "[ox-hugo] Done"

vcheck:
	@$(EMACS) --batch --eval "(progn\
	(setenv \"OX_HUGO_ELPA\" \"$(OX_HUGO_ELPA)\")\
	(load-file (expand-file-name \"setup-ox-hugo.el\" \"$(OX_HUGO_TEST_DIR)\"))\
	(message \"[Version check] Emacs %s\" emacs-version)\
	(message \"[Version check] %s\" (org-version nil :full))\
	)" \
	--kill
# Thu Sep 21 00:36:23 EDT 2017 - kmodi
# Don't check hugo version for now, as Travis fails
#	@hugo version

hugo: vcheck
	@hugo

serve server: vcheck
	@echo "Serving the site on http://localhost:$(PORT) .."
	@hugo server \
	--buildDrafts \
	--buildFuture \
	--navigateToChanged \
	--baseURL http://localhost \
	--port $(PORT)

diff:
	@git diff

test: vcheck testmkgold test_subtree test_file

# https://stackoverflow.com/a/16589534/1219634
testmkgold:
	@git checkout --ignore-skip-worktree-bits -- $(OX_HUGO_TEST_SITE_DIR)/content
	@rm -rf $(OX_HUGO_TEST_SITE_DIR)/content-golden
	@cp -rf $(OX_HUGO_TEST_SITE_DIR)/content $(OX_HUGO_TEST_SITE_DIR)/content-golden

# Run the mdtree + diffgolden rules in loop on all of $(subtree_test_files)
# https://stackoverflow.com/a/37748952/1219634
test_subtree: $(subtree_test_files)
$(subtree_test_files):
	@$(MAKE) mdtree ORG_FILE=$@ \
	                ORG_FILE_DIR=$(OX_HUGO_TEST_ORG_DIR) \
	                TIMEZONE=UTC # Use UTC/Universal time zone for tests
	@$(MAKE) diffgolden

# Run the mdfile + diffgolden rules in loop on all of $(file_test_files)
test_file: $(file_test_files)
$(file_test_files):
	@$(MAKE) mdfile ORG_FILE=$@ \
	                ORG_FILE_DIR=$(OX_HUGO_TEST_ORG_DIR) \
	                TIMEZONE=UTC # Use UTC/Universal time zone for tests
	@$(MAKE) diffgolden

doc_site:
	@echo "[Doc Site] Generating ox-hugo Documentation Site content .."
	@$(MAKE) mdtree ORG_FILE=ox-hugo-manual.org ORG_FILE_DIR=./doc
	@echo "[Doc Site] Done"

gh_docs:
	@echo "[GitHub Docs] Generating README.org and CONTRIBUTING.org for GitHub .."
	@$(MAKE) emacs-batch FUNC=ox-hugo-export-gh-doc ORG_FILE=github-files.org ORG_FILE_DIR=./doc
	@echo "[GitHub Docs] Done"

doc: doc_site gh_docs

ctemp:
	@find $(OX_HUGO_TEST_SITE_DIR)/content -name "*.*~" -delete
	@find ./doc/content -name "*.*~" -delete

diffgolden:
	@diff -r $(OX_HUGO_TEST_SITE_DIR)/content $(OX_HUGO_TEST_SITE_DIR)/content-golden

clean: ctemp
	@rm -rf $(OX_HUGO_TEST_SITE_DIR)/public $(OX_HUGO_TEST_SITE_DIR)/content-golden
	@rm -rf $(OX_HUGO_ELPA)
