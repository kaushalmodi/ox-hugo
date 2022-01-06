+++
title = "Inline hyperlinked and non-hyperlinked images"
tags = ["image", "inline"]
draft = false
+++

This tests the feature added to support proper export syntax for
inline images -- `ox-hugo` Issue #[125](https://github.com/kaushalmodi/ox-hugo/issues/125).


## Inline non-hyperlinked image {#inline-non-hyperlinked-image}


### No Alt text or any other attributes {#no-alt-text-or-any-other-attributes}

This is an inline non-hyperlinked image without alt text or other
attributes: ![](/images/org-mode-unicorn-logo.png).


### With Alt text {#with-alt-text}

This is an inline non-hyperlinked image with alt text:
![Inline Non-hyperlinked Image](/images/org-mode-unicorn-logo.png).


### With other HTML attributes {#with-other-html-attributes}

This is an inline non-hyperlinked image with the width attribute:
<img src="/images/org-mode-unicorn-logo.png" alt="org-mode-unicorn-logo.png" width="30" />.


## Non-inline non-hyperlinked image {#non-inline-non-hyperlinked-image}

{{< figure src="/images/org-mode-unicorn-logo.png" >}}


## Inline hyperlinked image {#inline-hyperlinked-image}


### No Alt text or any other attributes {#inline-image-to-be-copied-to-static--no-attr}

This is an inline non-hyperlinked image without alt text or other
attributes:
[![](/ox-hugo/copy-2-of-unicorn-logo.png)](/ox-hugo/copy-2-of-unicorn-logo.png).


### With Alt text {#with-alt-text}

This is an inline non-hyperlinked image with alt text:
[![Inline Non-hyperlinked Image](/ox-hugo/copy-2-of-unicorn-logo.png)](/ox-hugo/copy-2-of-unicorn-logo.png).


### With other HTML attributes {#with-other-html-attributes}

This is an inline non-hyperlinked image with the width attribute:
[<img src="/ox-hugo/copy-2-of-unicorn-logo.png" alt="copy-2-of-unicorn-logo.png" width="30" />](/ox-hugo/copy-2-of-unicorn-logo.png).


### With space in the filename {#with-space-in-the-filename}

This is an inline hyperlinked image with a space in the filename:
`ox-hugo` Issue #[277](https://github.com/kaushalmodi/ox-hugo/issues/277)
[![](/ox-hugo/unicorn logo.png)](/ox-hugo/copy-2-of-unicorn-logo.png).


## Non-inline hyperlinked image {#non-inline-hyperlinked-image}

{{< figure src="/ox-hugo/copy-2-of-unicorn-logo.png" link="/ox-hugo/copy-2-of-unicorn-logo.png" >}}
