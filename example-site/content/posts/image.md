+++
title = "Image"
date = 2017-07-07T18:44:51-04:00
tags = []
+++

**To be fixed**: At the moment, you need to place the point here and do `C-c C-e H H`. If the point is under any of the nested sub-trees in here where you do that, only that subtree gets exported.

*Need to add smarts that determines which subtree contains the whole post.. like look for `EXPORT_FILE_NAME`?*


# Unclickable image (works!)

![img](/images/org-mode-unicorn-logo.png)

**To be fixed**: The sub-headings in a post get exported as *Heading 1* instead of *Heading 2*.

For example, this sub-section&rsquo;s heading is exported as:

    # Unclickable image

instead of

    ## Unclickable image

So the sub-heading title and the post title both get the *Heading 1* tag and look the same size.


# Clickable link that opens the image (works!)

[Click here to see the unicorn](/images/org-mode-unicorn-logo.png)


# Clickable image that opens the image (works!)

Click below image to jump to the unicorn image. [![img](/images/org-mode-unicorn-logo.png)](/images/org-mode-unicorn-logo.png)

-   **NOTE:** `file:` has to be used in both Link and Description components of the Org link.


# Image with `ATTR_HTML` [Issue # 17](https://github.com/kaushalmodi/ox-hugo/issues/17)

![img](/images/org-mode-unicorn-logo.png)

[Discussion](https://github.com/kaushalmodi/ox-hugo/issues/17#issuecomment-313627728)


## Below will not work!

You cannot wrap markdown code inside HTML.

As *rdwatters* says [here](https://discourse.gohugo.io/t/is-it-possible-to-insert-html-code-in-markdown-content/4867/4?u=kaushalmodi),

> HTML can be part of markdown because HTML-inside-markdown is part of the spec. That said, remember that the spec disallows markdown nested inside of HTML. So if you create a div, just make sure everything inside that div is valid HTML.

<div class="inset">

![img](/images/org-mode-unicorn-logo.png)

</div>
