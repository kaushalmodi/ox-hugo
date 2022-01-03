+++
title = "Source blocks with Hugo highlight shortcode"
date = 2017-07-13T17:57:58-04:00
draft = false
+++

Here are few variables that you might like to change in the `local.mk`:

`prefix`
: Org installation directory

    {{< highlight makefile >}}
    prefix = /dir/where/you/want/to/install/org # Default: /usr/share
    {{< /highlight >}}

    The `.el` files will go to `$(prefix)/emacs/site-lisp/org` by
                 default. If you'd like to change that, you can tweak the
                 `lispdir` variable.

`infodir`
: Org Info installation directory. I like to keep the
    Info file for development version of Org in a separate
    directory.

    {{< highlight makefile >}}
    infodir = $(prefix)/org/info # Default: $(prefix)/info
    {{< /highlight >}}

`ORG_MAKE_DOC`
: Types of Org documentation you'd like to build by
    default.

    {{< highlight makefile >}}
    # Define below you only need info documentation, the default includes html and pdf
    ORG_MAKE_DOC = info pdf card # html
    {{< /highlight >}}

`ORG_ADD_CONTRIB`
: Packages from the `contrib/` directory that
    you'd like to build along with Org. Below are the ones on my
    _must-have_ list.

    {{< highlight makefile >}}
    # Define if you want to include some (or all) files from contrib/lisp
    # just the filename please (no path prefix, no .el suffix), maybe with globbing
    #   org-eldoc - Heading breadcrumb trail in minibuffer
    #   ox-extra - Allow ignoring just the heading, but still export the body of those headings
    #   org-mime - Convert org buffer to htmlized format for email
    ORG_ADD_CONTRIB = org-eldoc ox-extra org-mime
    {{< /highlight >}}
