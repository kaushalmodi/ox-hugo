+++
title = "HUGO_DRAFT true, DRAFT state"
tags = ["draft", "todo"]
draft = true
+++

If a post has the Org TODO state set to `DRAFT`, the `draft` front
matter variable should be set to `true` regardless of the value of
`EXPORT_HUGO_DRAFT` property.

Idea is to mark a post as `DRAFT` that you have already started
writing, or are in the process at the moment, but it is not yet ready
to be published
