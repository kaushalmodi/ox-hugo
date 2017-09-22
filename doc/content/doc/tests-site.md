+++
title = "Hugo test site for this package"
slug = "tests-site"
draft = false
[menu.example]
  weight = 3002
  identifier = "hugo-test-site-for-this-package"
+++

A [site](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site) with bare-bones Hugo "theme" is used to live-test the
package --- you'll know why theme is double-quoted once you try out the
site on `hugo`.

Check out the [example single Org file](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/all-posts.org). That is created for testing various
Org->Hugo content and meta-data translation features. [Here](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site/content/posts) are the
exported Markdown files.


## How to try `ox-hugo` on that site? {#how-to-try-ox-hugo-on-that-site}

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


## Alternative way {#alternative-way}

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
