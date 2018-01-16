+++
title = "Image links"
date = 2017-07-15T07:49:44-04:00
tags = ["image"]
draft = false
+++

This is some text before the first heading of this post.


## Unclickable image (works!) {#unclickable-image--works}

{{<figure src="/images/org-mode-unicorn-logo.png">}}

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


## Clickable image that opens the image (works!) {#clickable-image-that-opens-the-image--works}

Click below image to jump to the unicorn image.
[{{<figure src="/images/org-mode-unicorn-logo.png">}}](/images/org-mode-unicorn-logo.png)

NOTE
: `file:` has to be used in the **Description component** of the
    Org link.

---

Here's the same link with `#+NAME` specified.. which should also be
clickable.

<a id="orga7fe1df"></a>
[{{<figure src="/images/org-mode-unicorn-logo.png">}}](/images/org-mode-unicorn-logo.png)


## Link to image outside of standard Hugo `static` directory {#link-to-image-outside-of-standard-hugo-static-directory}

{{<figure src="/images/copy-of-unicorn-logo.png">}}

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


### Source path does not contain `/static/` {#source-path-does-not-contain-static}

{{<figure src="/ox-hugo/copy-2-of-unicorn-logo.png">}}

| Outside `static`         | Copied-to location inside `static`       | Explanation                                                                                                             |
|--------------------------|------------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| `~/temp/bar/baz/foo.png` | `<HUGO_BASE_DIR>/static/ox-hugo/foo.png` | Here, as the **outside** path does not have `/static/`, the file is copied to the `ox-hugo/` dir in Hugo `static/` dir. |

Note
: The `ox-hugo` sub-directory name is because of the default
    value of
    `org-hugo-default-static-subdirectory-for-externals`.
