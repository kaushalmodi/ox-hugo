+++
title = "Org to Markdown for Hugo"
type = "homepage"
draft = false
[menu.main]
  weight = 1001
  identifier = "org-to-markdown-for-hugo"
+++

`ox-hugo` is an Org exporter backend that exports Org to
[Hugo](https://gohugo.io/)-compatible Markdown ([Blackfriday](https://github.com/russross/blackfriday)) and also generates the
front-matter (in TOML or YAML format).

This project consists of `ox-blackfriday.el` too. It is a derivation
of [`ox-gfm`](https://github.com/larstvei/ox-gfm) with support added for Blackfriday Markdown tables and
many other tweaks. `ox-hugo` backend extends from this.


## Screenshots {#screenshots}

Before you read further, you can see below how `ox-hugo` translates
Org to Markdown (Org on the left; exported Markdown with Hugo
front-matter on the right).


### One post per Org subtree (preferred) {#screenshot-one-post-per-subtree}

[{{<figure src="/images/one-post-per-subtree.png">}}](/images/one-post-per-subtree.png)

-   **Files in above screenshot:** [Org](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/screenshot-subtree-export-example.org) -> [Markdown](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/writing-hugo-blog-in-org-subtree-export.md)


### One post per Org file {#screenshot-one-post-per-file}

[{{<figure src="/images/one-post-per-file.png">}}](/images/one-post-per-file.png)

-   **Files in above screenshot:** [Org](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content-org/writing-hugo-blog-in-org-file-export.org) -> [Markdown](https://raw.githubusercontent.com/kaushalmodi/ox-hugo/master/test/site/content/writing-hugo-blog-in-org-file-export.md)


### Editorial {#org-blogging-flow-editorial}

The preferred way to organize the posts is as Org subtrees (also the
main reason to write this package, as nothing like that was out there)
as it makes the meta-data management for Hugo front-matter pretty
effortless.

If you are a _one Org-file per post_ type of a person, that flow works
too! Just note that in this flow many of those `#+HUGO_` properties
need to be managed manually.. just as one would manage the front-matter
in Markdown files --- See the Org versions in the above screenshots for
comparison.


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

-   <https://ox-hugo.scripter.co> -- `ox-hugo` Documentation Site
-   <https://scripter.co> -- My blog


## Installation {#installation}

This package requires emacs 24.5+ and Org 9.0+. It is available on
Melpa (<https://melpa.org/#/ox-hugo>).


## Usage {#usage}

Once the package is installed, you will need to _require_ it so that
the `ox-hugo` export options are available in the _Org Export
Dispatcher_ menu (the one you see when you hit `C-c C-e` to initiate
any export).

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

**Spacemacs**

Spacemacs users can choose to add this snippet to their
`dotspacemacs/user-config` function in `.spacemacs`:

```emacs-lisp
(defun dotspacemacs/user-config ()
  ;; Other stuff
  ;; ..

  ;; ox-hugo config
  (use-package ox-hugo
    :ensure t                           ;Auto-install the package from Melpa
    :after ox))
```

If you do so, you **also need to** add `ox-hugo` to
`dotspacemacs-additional-packages`.

_Verified to work on Spacemacs `develop` branch with `spacemacs-base`
distribution, `emacs` editing style._


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

| Binding       | Description                                                                           |
|---------------|---------------------------------------------------------------------------------------|
| `C-c C-e H H` | Export only the current _valid_ subtree (has the `EXPORT_FILE_NAME` property set)     |
| `C-c C-e H A` | Export **all** _valid_ subtrees (those that have the `EXPORT_FILE_NAME` property set) |
| `C-c C-e H h` | Export the whole Org file to a single post                                            |


### Customization Options {#customization-options}

Do `M-x customize-group`, and select `org-export-hugo` to see the
available customization options for this package.


## Thanks {#thanks}

-   Matt Price ([@titaniumbones](https://github.com/titaniumbones))
-   Puneeth Chaganti ([@punchagan](https://github.com/punchagan))
-   Also thanks to [holgerschurig.de](http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/), [whyarethingsthewaytheyare.com](http://whyarethingsthewaytheyare.com/setting-up-the-blog/) and
    the [`goorgeous`](https://github.com/chaseadamsio/goorgeous) project by Chase Adams ([@chaseadamsio](https://github.com/chaseadamsio)) for
    inspiration to start this project.
