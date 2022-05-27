#!/usr/bin/env bash

TEST_ENABLED=1
export TEST_ENABLED

emacs -Q \
      -l "setup-ox-hugo.el" \
      site/content-org/temp.org \
      --eval "(org-hugo-export-wim-to-md :all-subtrees)" &
