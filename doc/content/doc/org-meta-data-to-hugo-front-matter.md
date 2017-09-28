+++
title = "Org meta-data to Hugo front-matter"
draft = false
[menu."org_to_hugo"]
  weight = 3001
  identifier = "org-meta-data-to-hugo-front-matter"
+++

## For subtree exports (`C-c C-e H H` or `C-c C-e H A`) {#for-subtree-exports--c-c-c-e-h-h-or-c-c-c-e-h-a}

When organizing the posts as Org **subtrees**, many Hugo front-matter
variables get set implicitly using the meta-data parsed from the posts
in Org.

Below, where _subtree_ is mentioned, it implies a **valid Hugo-post
subtree** i.e. an Org subtree that has the `EXPORT_FILE_NAME` property
set.

Hugo front-matter (TOML)           | Org                                | Org description
-----------------------------------|------------------------------------|--------------------------------------------------------------------------
`title = "foo"​`                    | `* foo`                            | Subtree heading
`date = 2017-09-11T14:32:00-04:00` | `CLOSED: [2017-09-11 Mon 14:32]`   | Auto-inserted `CLOSED` subtree property when switch to Org **DONE** state
`date = 2017-07-24`                | `:EXPORT_DATE: 2017-07-24`         | Subtree property
`lastmod = <current date>`         | `:EXPORT_HUGO_AUTO_SET_LASTMOD: t` | Subtree property
`lastmod = <current date>`         | `#+HUGO_AUTO_SET_LASTMOD: t`       | Org keyword
`tags = ["abc", "def"]`            | `* foo :abc:def:`                  | Subtree heading tags
`categories = ["x", "y"]`          | `* foo :@x:@y:`                    | Subtree heading tags with `@` prefix
`draft = true`                     | `* TODO foo`                       | Subtree heading Org Todo state set to `TODO` (or `DRAFT`)
`draft = false`                    | `* foo`                            | Subtree heading Org Todo state **not** set to `TODO` (or `DRAFT`)
`weight = 123`                     | `:EXPORT_HUGO_WEIGHT: auto`        | When set to `auto`, weight is auto-calculated
`weight = 123` (in `[menu.foo]`)   | `:EXPORT_HUGO_MENU: :menu foo`     | Menu weight is auto-calculated unless specified


### Notes {#notes}

-   Precedence for `date` parsing: `CLOSED` subtree property _more than_
    `EXPORT_DATE` subtree property _more than_ `#+DATE:` keyword.


## For complete-file exports (`C-c C-e H h`) {#for-complete-file-exports--c-c-c-e-h-h}

Hugo front-matter (TOML)         | Org
---------------------------------|-------------------------------------
`title = "foo"​`                  | `#+TITLE: foo`
`date = 2017-07-24`              | `#+DATE: 2017-07-24`
`lastmod = <current date>`       | `#+HUGO_AUTO_SET_LASTMOD: t`
`tags = ["abc", "def"]`          | `#+HUGO_TAGS: abc def`
`categories = ["x", "y"]`        | `#+HUGO_CATEGORIES: x y`
`draft = true`                   | `#+HUGO_DRAFT: true`
`draft = false`                  | `#+HUGO_DRAFT: false` (default)
`weight = 123`                   | `#+HUGO_WEIGHT: 123`
`weight = 123` (in `[menu.foo]`) | `#+HUGO_MENU: :menu foo :weight 123`


### Notes {#notes}

-   The auto weight calculation for posts and menu items works **only**
    for subtree exports. For the complete-file export flow, one needs to
    specify the weights manually. The value of _weight_ set to `"auto"`
    will be equivalent to _nil_ for the complete-file export flow.
