+++
title = "Usage"
draft = false
[menu."getting_started"]
  weight = 3002
  identifier = "usage"
+++

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


## Before you export {#before-you-export}

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


## Export bindings {#export-bindings}

The common `ox-hugo` export bindings are:

| Binding       | Description                                                                           |
|---------------|---------------------------------------------------------------------------------------|
| `C-c C-e H H` | Export only the current _valid_ subtree (has the `EXPORT_FILE_NAME` property set)     |
| `C-c C-e H A` | Export **all** _valid_ subtrees (those that have the `EXPORT_FILE_NAME` property set) |
| `C-c C-e H h` | Export the whole Org file to a single post                                            |


## Customization Options {#customization-options}

Do `M-x customize-group`, and select `org-export-hugo` to see the
available customization options for this package.
