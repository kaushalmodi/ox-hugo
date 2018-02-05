+++
title = "Single Post, but draft"
date = 2017-07-20
tags = ["single", "toml"]
categories = ["cat1", "cat2"]
draft = true
[menu.foo]
  identifier = "single-post-but-draft"
  parent = "main"
  weight = 10
+++

This is a single post. You do not need to set the `EXPORT_FILE_NAME`
property in here. But then you also lose the tag and property
inheritance, TODO state, etc. Org awesomeness.


## First heading in this post {#first-heading-in-this-post}

This is a under first heading.


## Second heading in this post {#second-heading-in-this-post}

This is a under second heading.
