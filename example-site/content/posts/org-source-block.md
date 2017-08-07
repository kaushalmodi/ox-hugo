+++
title = "Org Source Block"
tags = ["src-block"]
draft = false
+++

Test case for the case where user has set
`org-hugo-langs-no-descr-in-code-fences` to a list containing the
element `org`.

_As this variable is dependent on user's config, this post is not set
to be exported by default._

The [issue](https://discourse.gohugo.io/t/fenced-code-block-with-language-unsupported-by-pygments/7710) with Hugo will be seen if:

-   `pygmentsCodeFences = true` is set in the Hugo site `config.toml`,
-   a source block's language is set to one that's not supported by
    Pygments (like [org](https://bitbucket.org/birkenfeld/pygments-main/issues/719/wishlist-support-org), and thus the below example with source code
    language set to `org`), and
-   `org-hugo-langs-no-descr-in-code-fences` is set to a value not
    containing that lanaguage descriptor (`org` in this case).

```
# Org comment
Export this post after setting
=org-hugo-langs-no-descr-in-code-fences= to =(org)= and temporarily
removing the =noexport= tag.
```
