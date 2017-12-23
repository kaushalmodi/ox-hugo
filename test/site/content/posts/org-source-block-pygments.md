+++
title = "Org Source Block via Pygments"
tags = ["src-block", "pygments"]
draft = false
+++

_This test case is not important after Hugo 0.28 as this was a
workaround for a limitation in the Pygments syntax highlighter. Hugo
0.28 onwards, the default syntax highlighter is Chroma, which does not
have that limitation, and thus no need for this workaround._

---

Test case for the case where user has set
`org-hugo-langs-no-descr-in-code-fences` to a list containing the
element `org`.

_As this variable is dependent on user's config, this post is not set
to be exported by default._

The [issue](https://discourse.gohugo.io/t/fenced-code-block-with-language-unsupported-by-pygments/7710) with Hugo will be seen if <span class="underline">all</span> of the below are true:

-   `pygmentsCodeFences = true` **and** `pygmentsUseClassic = true` (Hugo
    ≥ 0.28 -- default value is `false`) are set in the Hugo site
    `config.toml`.
-   A source block's language is set to one that's not supported by
    Pygments (like [org](https://bitbucket.org/birkenfeld/pygments-main/issues/719/wishlist-support-org), and thus the below example with source code
    language set to `org`).
-   `org-hugo-langs-no-descr-in-code-fences` is set to a value not
    containing that language descriptor (`org` in this case).

```
# Org comment
Export this post after setting
=org-hugo-langs-no-descr-in-code-fences= to =(org)= and temporarily
removing the =noexport= tag.
```
