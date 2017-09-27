+++
title = "Changelog"
draft = false
[menu.meta]
  weight = 3002
  identifier = "changelog"
+++

## 0.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-27 Wed&gt;</span></span> {#0-dot-2-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-27-wed-and-gt-span-span}


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


## 0.1.3 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-13 Wed&gt;</span></span> {#0-dot-1-dot-3-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-13-wed-and-gt-span-span}

-   Now a HUGO key value set to `"nil"`, like `#+HUGO_CODE_FENCE: nil`,
    will evaluate as _nil_ instead of _t_, as now
    `org-hugo--plist-get-true-p` is used to parse boolean keys instead
    of `plist-get`.


## 0.1.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-12 Tue&gt;</span></span> {#0-dot-1-dot-2-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-12-tue-and-gt-span-span}

-   Make DateTime matching better; new internal variable
    `org-hugo--date-time-regexp`. Earlier time zones ahead of UTC (with
    `+` sign) were not detected as dates in `org-hugo--quote-string` and
    thus were unnecessarily quoted.


## 0.1.1 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-11 Mon&gt;</span></span> {#0-dot-1-dot-1-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-11-mon-and-gt-span-span}

-   Use CLOSED log drawer info if available to set the date in
    front-matter [[68](https://github.com/kaushalmodi/ox-hugo/issues/68)].
-   Code optimization: Use of `org-entry-get` at places instead of
    maintaining global variables.
