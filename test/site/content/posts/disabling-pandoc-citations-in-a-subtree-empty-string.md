+++
title = "Disabling Pandoc citations in a subtree (empty string)"
description = """
  Test that `:EXPORT_HUGO_PANDOC_CITATIONS:` disables Pandoc citations
  in a subtree.
  """
tags = ["citations", "pandoc", "disable"]
draft = false
nocite = ["@giovanelli2016", "@eilan2016"]
+++

The Nocites in the front-matter do not get rendered as citations.
