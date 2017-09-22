+++
title = "Changelog"
draft = false
[menu.meta]
  weight = 3002
  identifier = "changelog"
+++

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
