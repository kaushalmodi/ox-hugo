+++
title = "Changelog"
draft = false
[menu.meta]
  weight = 3002
  identifier = "changelog"
+++

## v0.4 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-28 Sat&gt;</span></span> {#v0-dot-4}


### Backward-incompatible changes {#backward-incompatible-changes}

-   Restore the default Org behavior of `#+TAGS`. Now that keyword (and
    the `EXPORT_TAGS` property) is **not** used by `ox-hugo`. Fixes
    [[89](https://github.com/kaushalmodi/ox-hugo/issues/89)].
-   File-based exports must now use `#+HUGO_TAGS` to set the post tags.
-   Subtree-based exports can use the `EXPORT_HUGO_TAGS` property to
    override Org-style tags on the same headline (and the ones inherited
    from Org-style tags from any of the parent subtrees and
    `#+FILETAGS`).
    -   Note that for subtree-based exports, `#+FILETAGS` can be used to
        set tags globally in the file. Earlier `#+TAGS` was used for that
        purpose.
-   Subtree-based exports can use the `EXPORT_HUGO_CATEGORIES` property
    to override Org-style categories (tags with "@" prefix) on the same
    headline (and the ones inherited from Org-style categories from any
    of the parent subtrees and `#+FILETAGS`).
    -   Note that for subtree-based exports, `#+FILETAGS` can be used to
        set categories (tags with "@") globally in the file.

See the new section added to documentation:
[**Tags and Categories**](/doc/tags-and-categories)


## v0.3.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-24 Tue&gt;</span></span> {#v0-dot-3-dot-2}


### Fixes {#fixes}

-   Fix issue with headline metadata parsing (ALLTAGS, CLOSED, TODO)
    when a post Org heading was immediately followed by that post's
    sub-heading.  This issue was seen in subtree-based exports
    [[87](https://github.com/kaushalmodi/ox-hugo/issues/87)].


## v0.3.1 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-19 Thu&gt;</span></span> {#v0-dot-3-dot-1}


### Fixes {#fixes}

-   Fix the source block line number annotation when the line numbers
    increased in number of digits in the same code block.


## v0.3 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-18 Wed&gt;</span></span> {#v0-dot-3}


### Features {#features}

-   Source blocks can now be exported with line numbers and/or
    highlighting!

    See [Source Blocks](/doc/source-blocks) for details.


## v0.2.3 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-11 Wed&gt;</span></span> {#v0-dot-2-dot-3}


### Fixes {#fixes}

-   `org-hugo-slug` earlier stripped off only the `code` HTML tag
    (`<code> .. </code>`) from the input string, if present. Now it does
    that for **any** HTML tag, like `span`. For example, this HTML gets
    stripped off from the above heading (only inside `org-hugo-slug`
    when deriving the slug string): `<span
      class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-11
      Wed&gt;</span></span>`.


## v0.2.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-10-10 Tue&gt;</span></span> {#v0-dot-2-dot-2}


### Backward-incompatible changes {#backward-incompatible-changes}

-   Now `ox-hugo` by default requires text, to be sub/super-scripted, to
    be wrapped in `{}`. So now `a_b` will be exported as `a_b`, but
    `a_{b}` will be exported as `a<sub>b</sub>`. To revert back to the
    earlier behavior, user needs to add `#+OPTIONS: ^:t` to their Org
    file.


## v0.2.1 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-28 Thu&gt;</span></span> {#v0-dot-2-dot-1}


### Fixes {#fixes}

-   Single column tables now export correctly [[84](https://github.com/kaushalmodi/ox-hugo/issues/84)].
-   Ignore `HUGO_WEIGHT` set to `auto` for _per-file_ exports
    [[83](https://github.com/kaushalmodi/ox-hugo/issues/83)].


## v0.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-27 Wed&gt;</span></span> {#v0-dot-2}


### Features {#features}

-   Add support for all Hugo `figure` shortcode parameters
    [[79](https://github.com/kaushalmodi/ox-hugo/issues/79)].
-   New option `org-hugo-delete-trailing-ws` defaults to `t`; now Hugo
    deletes trailing white-spaces by default.
-   New options `org-hugo-default-static-subdirectory-for-externals` and
    `org-hugo-external-file-extensions-allowed-for-copying` (related to
    [[69](https://github.com/kaushalmodi/ox-hugo/issues/69)]).


### Fixes {#fixes}

-   Remove `HUGO_STATIC_IMAGE` option; fix attachment re-write
    [[69](https://github.com/kaushalmodi/ox-hugo/issues/69)].
-   Fix incorrectly inserted hard line-breaks [[72](https://github.com/kaushalmodi/ox-hugo/issues/72)]. Added a
    new option `HUGO_PRESERVE_FILLING`.
-   Fix error happening when a post title was set to an empty string
    [[ba9e8365](https://github.com/kaushalmodi/ox-hugo/commit/ba9e8365f6ee42f030ed806bf5ec42d6acce4c76)].


### Backward-incompatible changes {#backward-incompatible-changes}

-   Switch the default value of `org-hugo-use-code-for-kbd` option to
    `nil` [[88ba15ae](https://github.com/kaushalmodi/ox-hugo/commit/88ba15ae9bc809b0983315446c88fecfda3534e5)].


## v0.1.3 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-13 Wed&gt;</span></span> {#v0-dot-1-dot-3}

-   Now a HUGO key value set to `"nil"`, like `#+HUGO_CODE_FENCE: nil`,
    will evaluate as _nil_ instead of _t_, as now
    `org-hugo--plist-get-true-p` is used to parse boolean keys instead
    of `plist-get`.


## v0.1.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-12 Tue&gt;</span></span> {#v0-dot-1-dot-2}

-   Make DateTime matching better; new internal variable
    `org-hugo--date-time-regexp`. Earlier time zones ahead of UTC (with
    `+` sign) were not detected as dates in `org-hugo--quote-string` and
    thus were unnecessarily quoted.


## v0.1.1 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-11 Mon&gt;</span></span> {#v0-dot-1-dot-1}

-   Use CLOSED log drawer info if available to set the date in
    front-matter [[68](https://github.com/kaushalmodi/ox-hugo/issues/68)].
-   Code optimization: Use of `org-entry-get` at places instead of
    maintaining global variables.
