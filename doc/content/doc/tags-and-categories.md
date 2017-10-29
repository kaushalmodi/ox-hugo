+++
title = "Tags and Categories"
draft = false
[menu."org_to_hugo"]
  weight = 3005
  identifier = "tags-and-categories"
+++

## Subtree-based Export {#subtree-based-export}


### Tags {#tags}

For subtree-based exports, the Hugo front-matter `tags` values are
derived from Org tags set for the post subtree headline.

Example:

```org
* My post                                                         :tag1:tag2:
```

By default, Org tags from parent headlines, and the tags set in the
`#+FILETAGS` keyword get inherited (as the default value of
`org-use-tag-inheritance` is `t`). If the tag inheritance doesn't work
as expected, check that the value of that variable is set as required.

If the `EXPORT_HUGO_TAGS` property is set for a valid Hugo post
subtree, the value of that property will **completely override** the Org
tags set even on that subtree, the inherited values of Org-style tags
from parent headlines and even `#+FILETAGS`.


#### Why use `#+FILETAGS` and not `#+TAGS`? {#why-use-filetags-and-not-tags}

-   About `#+FILETAGS` -- [Tag Inheritance](http://orgmode.org/manual/Tag-inheritance.html) or `C-h i g (org) Tag
      inheritance`
-   About `#+TAGS` -- [Setting Tags](http://orgmode.org/manual/Setting-tags.html) or `C-h i g (org) Setting tags`


### Categories {#categories}

For subtree-based exports, the Hugo front-matter `categories` values
are derived from Org tags set for the post subtree headline, but only
the ones prefixed with **@**.

Example:

```org
* My post                                                       :@cat1:@cat2:
```

As with the tags, by default, the categories (Org tags with "@"
prefix) from parent headlines, and the ones set in the `#+FILETAGS`
keyword too get inherited (as the default value of
`org-use-tag-inheritance` is `t`). If the tag inheritance doesn't work
as expected, check that the value of that variable is set as required.

If the `EXPORT_HUGO_CATEGORIES` property is set for a valid Hugo post
subtree, the value of that property will **completely override** the
categories set even on that subtree, the inherited values of
categories from parent headlines and even `#+FILETAGS`.


## File-based Export {#file-based-export}

The tag (and category) inheritance does not apply to the file-based
export flow. So `#+FILETAGS` will have no effect in this flow.

-   To set tags, use `#+HUGO_TAGS`.
-   To set categories, use `#+HUGO_CATEGORIES`.


## Hyphens in tags (and categories) {#hyphens-in-tags--and-categories}

Hyphens are not allowed in Org tags. So `ox-hugo` converts **single
underscores** to hyphens if `org-hugo-prefer-hyphen-in-tags` is set to
non-nil (default). So an Org tag **abc\_def** will be exported as _tag_
**"abc-def"**. Similarly an Org tag **@abc\_def** will be exported as
_category_ **"abc-def"**.

To export a tag or category with an underscore, use 3 consecutive
underscores. So an Org tag **abc\_\_\_def** will be exported as _tag_
**"abc\_def"**. If you rather prefer to always export single underscores
as underscores, set `org-hugo-prefer-hyphen-in-tags` to nil.

This variable does not affect the tags set via `#+HUGO_TAGS` keyword
or the `EXPORT_HUGO_TAGS` property, because Org keywords and
properties allow using the hyphen character. So underscores and
hyphens in tags (or categories in `#+HUGO_CATEGORIES` /
`EXPORT_HUGO_CATEGORIES`) remain untransformed on export.


## Spaces in tags (and categories) {#spaces-in-tags--and-categories}

Spaces are not allowed in Org tags. So `ox-hugo` converts **double
underscores** to spaces if `org-hugo-allow-spaces-in-tags` is set to
non-nil (default). So an Org tag **abc\_\_def** will be exported as _tag_
**"abc def"**. Similarly an Org tag **@abc\_\_def** will be exported as
_category_ **"abc def"**.

This variable **also affects** the tags set via `#+HUGO_TAGS` keyword or
the `EXPORT_HUGO_TAGS` property, because it is not possible to
distinguish in Org keywords and properties whether the space is part
of the tag or used to separate two tags. The same applies to
categories set via `#+HUGO_CATEGORIES` / `EXPORT_HUGO_CATEGORIES`.


## Examples {#examples}

-   [Org source](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/tags-and-categories.org)
-   Exported Markdown -- [`inheriting-tags.md`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/posts/inheriting-tags.md), [`overriding-tags.md`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/posts/overriding-tags.md)
-   Hugo-generated HTML -- [Inheriting tags](https://ox-hugo.scripter.co/test/posts/inheriting-tags/), [Overriding tags](https://ox-hugo.scripter.co/test/posts/overriding-tags/)
