+++
title = "Dealing with underscores"
date = 2017-07-21
tags = ["body"]
draft = false
+++

## Cases where the underscores should be escaped {#cases-where-the-underscores-should-be-escaped}

\_something

foo bar \_something foo bar

something\_

foo bar something\_ foo bar


## Cases when the underscores should not be escaped {#cases-when-the-underscores-should-not-be-escaped}

By itself
: _

In a verbatim block
: `_`

In an emoji
: :raised_hands:, :white_check_mark:

In a citation
: @abc_def
