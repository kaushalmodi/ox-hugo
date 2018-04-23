+++
title = "Page Bundle A"
tags = ["page-bundles"]
draft = false
[[resources]]
  src = "images/copy-of-*.png"
  title = "First copy of Org mode logo"
[[resources]]
  src = "copy-2-*.png"
  title = "Second copy of Org mode logo"
+++

Index page of _Page Bundle A_.


## Link to images not in the current directory {#link-to-images-not-in-the-current-directory}


### Source path contains `/static/` {#source-path-contains-static}

When path contains `/static/`, the path translations are the exact
same as those for non-bundle cases.

[More details](/posts/image-links/#path-containing-static)

{{< figure src="/images/copy-of-unicorn-logo-page-bundle.png" >}}


### Source path contains the **bundle name** {#source-path-contains-the-bundle-name}

See [this other test](/images-in-content/page-bundle-images-in-same-dir/) for examples.

| Inside `<ORG_FILE_DIR>`                   | Copied-to location inside BUNDLE                         | Explanation                                                                                                             |
|-------------------------------------------|----------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------|
| `<ORG_FILE_DIR>/bar/<BUNDLE>/baz/foo.png` | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/baz/foo.png` | If the file directory path contains `"/<BUNDLE>/"`, the directory structure following that `"/<BUNDLE>/"` is preserved. |


#### Special case: Home page branch bundle {#special-case-home-page-branch-bundle}

In this case, both `HUGO_SECTION` and `HUGO_BUNDLE` values will be
`/`.

So the images to be copied to the **home page branch bundle** i.e. the
`content/` dir must be placed in a special `_home/` directory. Here
are some examples:

| Inside `<ORG_FILE_DIR>`                | Copied-to location inside BUNDLE      | Explanation                                                                                                                                                    |
|----------------------------------------|---------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<ORG_FILE_DIR>/bar/_home/baz/foo.png` | `<HUGO_BASE_DIR>/content/baz/foo.png` | If the page is the home page branch bundle, and the file directory path contains `"/_home/"`, the directory structure following that `"/_home/"` is preserved. |
| `<ORG_FILE_DIR>/bar/_home/foo.png`     | `<HUGO_BASE_DIR>/content/foo.png`     |                                                                                                                                                                |


### Source path contains neither `/static/` nor the **bundle name** {#source-path-contains-neither-static-nor-the-bundle-name}

{{< figure src="copy-2-of-unicorn-logo.png" >}}

| Outside `static`         | Copied-to location inside BUNDLE                     | Explanation                                                                                            |
|--------------------------|------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `~/temp/bar/baz/foo.png` | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/foo.png` | Here, as the **outside** path does not have `/static/`, the file is copied directly to the BUNDLE dir. |


#### Same image, but hyperlinked {#same-image-but-hyperlinked}

{{< figure src="copy-2-of-unicorn-logo.png" link="copy-2-of-unicorn-logo.png" >}}


### Page Bundles with images in the same dir as content Org file {#page-bundles-with-images-in-the-same-dir-as-content-org-file}

| Inside `<ORG_FILE_DIR>`          | Copied-to location inside BUNDLE                             | Explanation                                                                                                                                      |
|----------------------------------|--------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| `<ORG_FILE_DIR>/bar/baz/foo.png` | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/bar/baz/foo.png` | Even if the **outside** path does not have `/static/`, it is still inside the same dir as the Org file, so the directory structure is preserved. |

See [this other test](/images-in-content/page-bundle-images-in-same-dir/) for an example.
