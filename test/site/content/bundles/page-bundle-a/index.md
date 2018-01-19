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

{{<figure src="images/copy-of-unicorn-logo.png">}}

If you link to files not in the **current directory**, ensure that the
path contains `/static/` if you would like to preserve the directory
structure.

Example translations between the `static`-containing image paths to
the copied location inside the bundle:

| Outside `static`                 | Copied-to location inside BUNDLE                              | Explanation                                                                                                |
|----------------------------------|---------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `~/temp/static/images/foo.png`   | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/images/foo.png`   | If the **outside** path has `/static/` in it, the directory structure after that is preserved when copied. |
| `~/temp/static/img/foo.png`      | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/img/foo.png`      | (same as above)                                                                                            |
| `~/temp/static/foo.png`          | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/foo.png`          | (same as above)                                                                                            |
| `~/temp/static/articles/zoo.pdf` | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/articles/zoo.pdf` | (same as above)                                                                                            |


### Source path does not contain `/static/` {#source-path-does-not-contain-static}

{{<figure src="copy-2-of-unicorn-logo.png">}}

| Outside `static`         | Copied-to location inside BUNDLE                     | Explanation                                                                                            |
|--------------------------|------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `~/temp/bar/baz/foo.png` | `<HUGO_BASE_DIR>/content/<SECTION>/<BUNDLE>/foo.png` | Here, as the **outside** path does not have `/static/`, the file is copied directly to the BUNDLE dir. |
