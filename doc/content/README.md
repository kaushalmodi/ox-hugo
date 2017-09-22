+++
title = "Readme"
tags = ["readme"]
type = "homepage"
draft = false
[menu.main]
  weight = 2001
  identifier = "readme"
+++

[{{<figure src="//travis-ci.org/kaushalmodi/ox-hugo.svg">}}](https://travis-ci.org/kaushalmodi/ox-hugo) [{{<figure src="https://melpa.org/packages/ox-hugo-badge.svg">}}](https://melpa.org/#/ox-hugo) [{{<figure src="//img.shields.io/badge/License-GPL%20v3-blue.svg">}}](https://www.gnu.org/licenses/gpl-3.0)

`ox-hugo` is an Org exporter backend that exports Org to
[Hugo](https://gohugo.io/)-compatible Markdown ([Blackfriday](https://github.com/russross/blackfriday)) and also generates the
front-matter (in TOML or YAML format).

This project consists of `ox-blackfriday.el` too. It is a derivation
of [`ox-gfm`](https://github.com/larstvei/ox-gfm) with support added for Blackfriday Markdown tables and
many other tweaks. `ox-hugo` backend extends from this.

There are 2 major blogging flows that can be used with this package:

1.  One post per Org subtree (preferred)
    -   Export only the **current** post Org subtree, or
    -   Export all valid Hugo post subtrees in a loop.
2.  One post per Org file
    -   This works but you won't be able to leverage Org-specific
        benefits like tag and property inheritance, use of TODO states to
        translate to post `draft` state, auto weight calculation for
        posts and menu items, etc.

See the [Org Capture Setup](https://github.com/kaushalmodi/ox-hugo/wiki/Org-Capture-Setup) Wiki page to see how to quickly create new
posts.

See the [Auto-export on Saving](https://github.com/kaushalmodi/ox-hugo/wiki/Auto-export-on-Saving) Wiki page to learn how to setup up
seeing live-preview of the Hugo-rendered HTML each time you do `C-x
C-s` in the Org file --- For now, this works only with the **Subtree
export flow**.


## Demo {#demo}

[Org source](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site/content-org) → [Exported Markdown](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site/content) → [**Hugo published test site**](../test)

Now, the test site doesn't look pretty, I know :) .. because:

-   It is designed to verify if all the content translates from Org to
    Markdown as expected.
-   It uses a [**bare\_min**](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site/themes/bare_min/) _theme_ written just for the debug purpose (not
    presentation).

_See [Hugo Themes](https://themes.gohugo.io/) for examples of really good site prettification and
presentation styles._


### Actual usage examples {#actual-usage-examples}

-   <https://scripter.co> -- My blog


## Documentation {#documentation}

See this README and the [**Wiki**](https://github.com/kaushalmodi/ox-hugo/wiki) for more!


## Table of Contents {#table-of-contents}


## Screenshots {#screenshots}

Before you read further, you can see below how `ox-hugo` translates
Org to Markdown (Org on the left; exported Markdown with Hugo
front-matter on the right).

**One post per Org subtree --**
[{{<figure src="/images/one-post-per-subtree.png">}}](/images/one-post-per-subtree.png)

-   **Files in above screenshot:** [Org](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/screenshot-subtree-export-example.org) -> [Markdown](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/writing-hugo-blog-in-org-subtree-export.md)

**One post per Org file --**
[{{<figure src="/images/one-post-per-file.png">}}](/images/one-post-per-file.png)

-   **Files in above screenshot:** [Org](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/writing-hugo-blog-in-org-file-export.org) -> [Markdown](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/writing-hugo-blog-in-org-file-export.md)

The preferred way to organize the posts is as Org subtrees (also the
main reason to write this package, as nothing like that was out there)
as it makes the meta-data management for Hugo front-matter pretty
effortless.

If you are a _one Org-file per post_ type of a person, that flow works
too! Just note that in this flow many of those `#+HUGO_` properties
need to be managed manually.. just as one would manage the front-matter
in Markdown files --- See the Org versions in the above screenshots for
comparison.


## Why `ox-hugo`? {#why-ox-hugo}

Using Org just as a markup like Markdown is a miniscule part of its
complete feature-set. Org also allows stuff like:

-   Easy ordering/manipulation/commenting of subtrees
-   Creating tables (with even formulas like in Excel)
-   Directly including source code snippets from external files (instead
    of having to copy/paste them in)
-   Running code snippets within the Org file and embedding the results
    (Org Babel)
-   ..

Using Org for content writing allows using in-built Org features to
translate to Hugo front-matter:

-   Org uses an outline structure and can inherit meta data (tags and
    properties) from one subtree to children subtrees.
-   Using that feature, one can tag one tree as _emacs_, and everything
    under that tree (all posts under that) will get that tag
    automatically.
-   The same concept applies to inheriting any Org _property_ meta data
    like menu entry, category, section name, etc.
-   A subtree can be quickly marked to be in TODO state (default binding
    `C-c C-t`). A **TODO** post is marked as a _draft_ Hugo post.
-   The _menu-item weights_ and/or _post weights_ can be set to be
    auto-calculated so that the menu items or post order in the final
    HTML appear in the same order as the respective subtrees in Org.

    If the subtrees are re-ordered in Org, the weights get changed too.
-   One can have a subtree with section property set to "posts" and all
    post subtrees under that will go to that section. Similarly another
    parent subtree can have that property set to "articles", and so on.
-   Images can be displayed inline in the Org buffer.
-   After save hooks can be set up in Emacs so that each time I save the
    file, only the current subtree in Org gets exported to
    Markdown. With the Hugo server running with the new switch that auto
    changes the preview to the last changed post (`--navigateToChanged`
    introduced in Hugo 0.25), the flow is seamless -- Save the Org file
    and see the exact changed post in browser.
-   **All** posts can simply be subtrees in a single Org file. That way
    one can take advantage of Org subtree filtering and searching
    functions (`org-sparse-tree` bound to `C-c /` by default).
-   (and much more..)


## Installation {#installation}

This package requires emacs 24.5+ and Org 9.0+. It is available on Melpa.


## Usage {#usage}

Once the package is installed, you will need to require it so that the
`ox-hugo` export options are available in the _Org Export Dispatcher_
menu (the one you see when you hit `C-c C-e` to initiate any export).

You can do that by adding the below to your config:

```emacs-lisp
(with-eval-after-load 'ox
  (require 'ox-hugo))
```

If you use `use-package`, you can do the below instead:

```emacs-lisp
(use-package ox-hugo
  :after ox)
```


### Before you export {#before-you-export}

Before you export check that these properties are set as you need:

-   **HUGO\_SECTION:** The default Hugo section name for all the posts.  See
    [here](http://gohugo.io/content/sections/) for more information on Hugo sections.  It is
    common for this property to be set to `posts` or
    `blog`.  The default value is set using
    `org-hugo-default-section-directory`.
-   **HUGO\_BASE\_DIR:** Root directory of the source for the Hugo site. If
    this is set to `~/hugo/`, the exported Markdown
    files will be saved to
    `~/hugo/content/<HUGO_SECTION>/` directory.  By
    default, the Markdown files reside in a hierarchy
    under the `content/` directory in the site root
    directory ([ref](http://gohugo.io/content/organization/)). If you try to export without
    setting this property, you will get this error:

    ```text
    user-error: It is mandatory to set the HUGO_BASE_DIR property
    ```

**Important**: If you choose to export an Org subtree as a post, you
 need to set the `EXPORT_FILE_NAME` subtree property. That property is
 used by this package to figure out where the current post starts.


### Export bindings {#export-bindings}

The common `ox-hugo` export bindings are:

Binding       | Description
--------------|--------------------------------------------------------------------------------------
`C-c C-e H H` | Export only the current _valid_ subtree (has the `EXPORT_FILE_NAME` property set)
`C-c C-e H A` | Export **all** _valid_ subtrees (those that have the `EXPORT_FILE_NAME` property set)
`C-c C-e H h` | Export the whole Org file to a single post


## Hugo test site for this package {#hugo-test-site-for-this-package}

An [site](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site) with bare-bones "theme" is used to live-test the
package --- you'll know why theme is double-quoted once you try out the
site on `hugo`.

Check out the [example single Org file](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/all-posts.org). That is created for testing various
Org->Hugo content and meta-data translation features. [Here](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site/content/posts) are the
exported Markdown files.


### How to try `ox-hugo` on that site? {#how-to-try-ox-hugo-on-that-site}

1.  Clone this repo.
2.  `cd` to the `test/site/` directory and do:

    ```text
    make serve
    ```

    -   **Requires Hugo 0.25+**
3.  Open `http://localhost:1337` in your browser.
4.  In a different terminal, `cd` to the same `test/site/` directory.
5.  Run:

    ```text
    make mdtree ORG=content-org/all-posts.org
    ```
6.  In few seconds, dozens of test posts will get created, with the
    `hugo server` aided preview in the browser zapping through each new
    created post.


### Alternative way {#alternative-way}

1.  Clone this repo.
2.  `cd` to the `test/site/` directory and do:

    ```text
    hugo server -D --navigateToChanged
    ```

    -   `--navigateToChanged` requires Hugo 0.25+.
3.  Above command will mention the localhost where the site is
    served. Open that in your browser.
4.  In emacs, `(require  'ox-hugo)` or evaluate the `ox-hugo.el` from the
    cloned repo.
5.  Open the [`all-posts.org`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/all-posts.org) file.
6.  `C-c C-e H A` -- That will export **all** subtrees in the file to
    Markdown files.
7.  In few seconds, dozens of test posts will get created, with the
    `hugo server` aided preview in the browser zapping through each new
    created post (needs that new feature `--navigateToChanged`
    introduced in Hugo 0.25).


## How do I try `ox-hugo` on my site? {#how-do-i-try-ox-hugo-on-my-site}

1.  `cd` to your Hugo site base directory -- the one that contains the
    `config.toml` (or `config.yaml` or `config.json`).
2.  Start the `hugo server` in that directory:

    ```text
    hugo server -D --navigateToChanged
    ```

    -   `--navigateToChanged` requires Hugo 0.25+.
3.  Above command will mention the localhost where the site is
    served. Open that in your browser.
4.  Create a separate directory for Org content in the Hugo site base
    directory. You can name it anything, but I prefer to name it
    `content-org` ([Example 1 -- `ox-hugo` test site](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site), [Example 2 -- My
    blog](https://gitlab.com/kaushalmodi/kaushalmodi.gitlab.io)).
5.  Create an Org file in there and follow the **Usage** section in the
    [README](https://github.com/kaushalmodi/ox-hugo#usage) or [Wiki](https://github.com/kaushalmodi/ox-hugo/wiki/Usage) to export it.


## Translation of Org meta-data to Hugo front-matter {#translation-of-org-meta-data-to-hugo-front-matter}


### For subtree exports (`C-c C-e H H` or `C-c C-e H A`) {#for-subtree-exports--c-c-c-e-h-h-or-c-c-c-e-h-a}

When organizing the posts as Org **subtrees**, many Hugo front-matter
variables get set implicitly using the meta-data parsed from the posts
in Org.

Below, where _subtree_ is mentioned, it implies a **valid Hugo-post
subtree** i.e. an Org subtree that has the `EXPORT_FILE_NAME` property
set.

Hugo front-matter (TOML)           | Org                                | Org description
-----------------------------------|------------------------------------|--------------------------------------------------------------------------
=title = "foo"​=                    | `* foo`                            | Subtree heading
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


#### Notes {#notes}

-   Precedence for `date` parsing: `CLOSED` subtree property _more than_
    `EXPORT_DATE` subtree property _more than_ `#+DATE:` keyword.


### For complete-file exports (`C-c C-e H h`) {#for-complete-file-exports--c-c-c-e-h-h}

Hugo front-matter (TOML)         | Org
---------------------------------|-------------------------------------
=title = "foo"​=                  | `#+TITLE: foo`
`date = 2017-07-24`              | `#+DATE: 2017-07-24`
`lastmod = <current date>`       | `#+HUGO_AUTO_SET_LASTMOD: t`
`tags = ["abc", "def"]`          | `#+HUGO_TAGS: abc def`
`categories = ["x", "y"]`        | `#+HUGO_CATEGORIES: x y`
`draft = true`                   | `#+HUGO_DRAFT: true`
`draft = false`                  | `#+HUGO_DRAFT: false` (default)
`weight = 123`                   | `#+HUGO_WEIGHT: 123`
`weight = 123` (in `[menu.foo]`) | `#+HUGO_MENU: :menu foo :weight 123`


#### Notes {#notes}

-   The auto weight calculation for posts and menu items works **only**
    for subtree exports. For the complete-file export flow, one needs to
    specify the weights manually if needed.


## Formatting {#formatting}

Below table shows the translation of Org markup to Markdown markup in
the exported `.md` files.

See the Org source in [`all-posts.org`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/all-posts.org) under _Formatting_ -> _General_
heading and how it exports to Markdown in [`general-formatting.md`](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/posts/general-formatting.md).

Org                | Markdown
-------------------|---------------------------------------------------------------------
`*bold*`           | `**bold**`
`/italics/`        | `_italics_`
`=monospace=`      | `` `monospace` ``
`~key-binding~`    | `<kbd>key-binding</kbd>`
                   | - if `org-hugo-use-code-for-kbd` is non-nil [default]
                   | - Requires **CSS** to render the `<kbd>` tag as something special.
`~key-binding~`    | `` `key-binding` ``
                   | - if `org-hugo-use-code-for-kbd` is nil
`+strike-through+` | `~~strike-through~~`
`_underline_`      | `<span class = "underline">underline</span>`
                   | - Requires **CSS** to render this `underline` class as an underline.

(Note: If you see two equal signs on each side of _monospace_ in the
_Org_ column in the table above, it is a bug with GitHub's Org
renderer.. just see those as **single** equal signs on each side of
_monospace_ instead.)


## Do I need to re-write my whole blog in Org? {#do-i-need-to-re-write-my-whole-blog-in-org}

If you are considering to try out `ox-hugo`, and if you have already
been using Hugo, it is normal for this thought to cross your mind:

> I already have dozens or hundreds of posts written in Markdown. Do I
> need to convert them to Org if I want to start using `ox-hugo`?

The answer is **No**.

This package will export your future posts written in Org to
Markdown. And those files will live along with your already written
Markdown posts. So converting existing Markdown files to Org would be
purely the user's choice, your choice -- but that's by no means a
necessity if you want to start using `ox-hugo`.

.. And if at some point, you want to stop using `ox-hugo`, you still
have the exported Markdown files.


## Meta Features {#meta-features}

-   `[✓]` Extensive tests! -- [test/site/content-org](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site/content-org)
-   `[✓]` Travis CI Integration -- Checks with emacs versions starting from
    24.4->25.2, with the latest stable version of `org-plus-contrib`.
-   `[✓]` [Test site](../test) -- Regenerated after each commit to this repo
-   `[✓]` Documentation -- README, Changelog, Wiki **all** generated from a
    single [ox-hugo-manual.org](https://github.com/kaushalmodi/ox-hugo/raw/master/doc/ox-hugo-manual.org)
-   `[✓]` Passes `checkdoc` and `package-lint` ([_almost_](https://github.com/purcell/package-lint/issues/89)).


## Changelog {#changelog}


### 0.1.3 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-13 Wed&gt;</span></span> {#0-dot-1-dot-3-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-13-wed-and-gt-span-span}

-   Now a HUGO key value set to `"nil"`, like `#+HUGO_CODE_FENCE: nil`,
    will evaluate as _nil_ instead of _t_, as now
    `org-hugo--plist-get-true-p` is used to parse boolean keys instead
    of `plist-get`.


### 0.1.2 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-12 Tue&gt;</span></span> {#0-dot-1-dot-2-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-12-tue-and-gt-span-span}

-   Make DateTime matching better; new internal variable
    `org-hugo--date-time-regexp`. Earlier time zones ahead of UTC (with
    `+` sign) were not detected as dates in `org-hugo--quote-string` and
    thus were unnecessarily quoted.


### 0.1.1 <span class="timestamp-wrapper"><span class="timestamp">&lt;2017-09-11 Mon&gt;</span></span> {#0-dot-1-dot-1-span-class-timestamp-wrapper-span-class-timestamp-and-lt-2017-09-11-mon-and-gt-span-span}

-   Use CLOSED log drawer info if available to set the date in
    front-matter [[68](https://github.com/kaushalmodi/ox-hugo/issues/68)].
-   Code optimization: Use of `org-entry-get` at places instead of
    maintaining global variables.


## Debug {#debug}

If the `ox-hugo` exports do not work as expected, or if you get an
error backtrace,

1.  Open an [Issue](https://github.com/kaushalmodi/ox-hugo/issues).
2.  Describe the problem you are seeing.
3.  Provide the debug info using `org-hugo-debug-info`:
    -   `M-x org-hugo-debug-info` (that will copy the debug info in
        Markdown format to the kill ring)
    -   Paste the Markdown contents in the GitHub issue.
        -   You can still hit the _Preview_ tab of the Issue before
            submitting it.


## Test {#test}

1.  Clone this repo.
2.  `cd` to the `test/site/` directory and do:

    ```text
    make test
    ```


## Thanks {#thanks}

-   Matt Price (@titaniumbones)
-   Puneeth Chaganti (@punchagan)
-   Also thanks to [holgerschurig.de](http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/), [whyarethingsthewaytheyare.com](http://whyarethingsthewaytheyare.com/setting-up-the-blog/) and
    the [`goorgoeous`](https://github.com/chaseadamsio/goorgeous) project by Chase Adams (@chaseadamsio) for
    inspiration to start this project.
