#!/usr/bin/env bash

HOME="$(pwd)"
TEST_ENABLED=1

export HOME
export TEST_ENABLED

emacs -Q -l "setup-ox-hugo.el"
