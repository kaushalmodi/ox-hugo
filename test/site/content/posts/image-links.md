+++
title = "Image links"
date = 2017-07-15T07:49:44-04:00
tags = ["image"]
draft = false
+++

This is some text before the first heading of this post.


## Unclickable image (works!) {#unclickable-image--works}

```org
# Org source
#+name: fig__unclickable_image
#+caption: Don't click this image -- It's not a hyperlink
[[/images/org-mode-unicorn-logo.png]]
```

<a id="figure--unclickable-image"></a>

{{< figure src="/images/org-mode-unicorn-logo.png" caption="<span class=\"figure-number\">Figure 1: </span>Don't click this image -- It's not a hyperlink" >}}

---

~~To be fixed~~ (**Now fixed**): The sub-headings in a post get exported as _Heading 1_
 instead of _Heading 2_.

For example, this sub-section's heading is exported as:

```text
# Unclickable image
```

instead of

```text
## Unclickable image
```

---

**Solution**: Above is fixed by setting `HUGO_OFFSET_LEVEL` to 1.

So the sub-heading title and the post title both get the _Heading 1_
tag and look the same size.


## Clickable link that opens the image (works!) {#clickable-link-that-opens-the-image--works}

[Click here to see the unicorn](/images/org-mode-unicorn-logo.png)

Do **not** use `file:` prefix in the Description if you want the image
link to show up as-is i.e. not be replaced by the image-at-link like
in [2](#figure--clickable-image-that-opens-the-image):

```org
# Org source
[[/images/org-mode-unicorn-logo.png][/images/org-mode-unicorn-logo.png]]
```

[/images/org-mode-unicorn-logo.png](/images/org-mode-unicorn-logo.png)


## Clickable image that opens the image (works!) {#clickable-image-that-opens-the-image--works}

Click below image to jump to the unicorn image.

```org
# Org source
#+name: fig__clickable_image_that_opens_the_image
#+caption: Click this image -- It's a hyperlink
[[/images/org-mode-unicorn-logo.png][file:../files-to-be-copied-to-static/static/images/unicorn-logo-small.png]]
```

<a id="figure--clickable-image-that-opens-the-image"></a>

{{< figure src="/images/unicorn-logo-small.png" caption="<span class=\"figure-number\">Figure 2: </span>Click this image -- It's a hyperlink" link="/images/org-mode-unicorn-logo.png" >}}

NOTE
: `file:` has to be used in the **Description component** of the
    Org link.


### Similar link with `#+name` specified {#similar-link-with-plus-name-specified}

Here's a similar link with `#+name` specified.. which should also be
clickable.

<a id="figure--unicorn"></a>

{{< figure src="/images/org-mode-unicorn-logo.png" link="/images/org-mode-unicorn-logo.png" >}}


### Same link with `file:` in "link" portion of the Org link too {#same-link-with-file-in-link-portion-of-the-org-link-too}

_Note that the `file:` is needed only in the "description" portion to
create a hyperlinked image that links to an image. But having `file:`
in the "link" portion of the Org link too shouldn't hurt._

Click below image to jump to the unicorn image.

{{< figure src="/images/org-mode-unicorn-logo.png" link="/images/org-mode-unicorn-logo.png" >}}


## Link to image outside of standard Hugo `static` directory {#path-containing-static}

{{< figure src="/images/copy-of-unicorn-logo.png" >}}

If you link to files outside of the Hugo `static` directory, ensure
that the path contains `/static/` if you would like to preserve the
directory structure.

Example translations between outside `static` directory paths to the
copied location inside `static`:

| Outside `static`                 | Copied-to location inside `static`        | Explanation                                                                                                |
|----------------------------------|-------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `~/temp/static/images/foo.png`   | `<HUGO_BASE_DIR>/static/images/foo.png`   | If the **outside** path has `/static/` in it, the directory structure after that is preserved when copied. |
| `~/temp/static/img/foo.png`      | `<HUGO_BASE_DIR>/static/img/foo.png`      | (same as above)                                                                                            |
| `~/temp/static/foo.png`          | `<HUGO_BASE_DIR>/static/foo.png`          | (same as above)                                                                                            |
| `~/temp/static/articles/zoo.pdf` | `<HUGO_BASE_DIR>/static/articles/zoo.pdf` | (same as above)                                                                                            |

The above path translations (when path contains `static`) hold true
even when inside page bundle pages.


### Source path does not contain `/static/` {#source-path-does-not-contain-static}

{{< figure src="/ox-hugo/copy-2-of-unicorn-logo.png" >}}

| Outside `static`         | Copied-to location inside `static`       | Explanation                                                                                                             |
|--------------------------|------------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| `~/temp/bar/baz/foo.png` | `<HUGO_BASE_DIR>/static/ox-hugo/foo.png` | Here, as the **outside** path does not have `/static/`, the file is copied to the `ox-hugo/` dir in Hugo `static/` dir. |

Note
: The `ox-hugo` sub-directory name is because of the default
    value of
    `org-hugo-default-static-subdirectory-for-externals`.


#### Same image, but hyperlinked to itself {#same-image-but-hyperlinked-to-itself}

{{< figure src="/ox-hugo/copy-2-of-unicorn-logo.png" link="/ox-hugo/copy-2-of-unicorn-logo.png" >}}


## Link to an image with space in filename {#link-to-an-image-with-space-in-filename}

This is a link to a file with a space in the filename:
`ox-hugo` Issue #[376](https://github.com/kaushalmodi/ox-hugo/issues/376)
[Link to `unicorn logo.png`](</ox-hugo/unicorn logo.png>)
