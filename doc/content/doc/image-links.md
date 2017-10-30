+++
title = "Image Links"
draft = false
[menu."org_to_hugo"]
  weight = 3003
  identifier = "image-links"
+++

This section will provides few alternatives for linking to images in
Org files in a way that's compatible with `ox-hugo` and Hugo.

For the sake of the below explanation, let's have the _HUGO\_BASE\_DIR_
(the directory containing the Hugo site's `config.toml` file) be
`~/hugo/`.

In that case, the Hugo _static_ directory will be `~/hugo/static/`.


## References to files in the _static_ directory {#references-to-files-in-the-static-directory}

Now if you have a file `~/hugo/static/images/foo.png`, `ox-hugo` makes
it convenient for you to reference that image by simply
`/images/foo.png`. Note that this is **not** the default behavior of
other Org exporters.

But in the case of Hugo, as the referenced file will _normally_ be in
the Hugo _static_ directory, `ox-hugo` allows the `/images/foo.png`
style of short reference **if** that is a valid path under that Hugo
site's _static_ directory.

See the below examples on how to reference images in different ways:


### Inline image (Unhyperlinked) {#inline-image--unhyperlinked}

```org
[[/images/foo.png]]
```


### Inline image (Hyperlinked to the image itself) {#inline-image--hyperlinked-to-the-image-itself}

```org
[[file:/images/foo.png][file:/images/foo.png]]
```

-   **NOTE:** The `file:` prefix **has to be used** in both Link and
    Description components of the Org link.


### Link to an image (Image not inlined) {#link-to-an-image--image-not-inlined}

```org
[[/images/foo.png][Click here to see foo.png]]
```

This style of linking will work for references to non-image files in
the _static_ directory too.


## References to files **outside** the _static_ directory {#references-to-files-outside-the-static-directory}

This is a unique feature of `ox-hugo`.

(i) If a reference is made to a file outside the Hugo _static_
directory **and** (ii) if it has one of the extensions listed in
`org-hugo-external-file-extensions-allowed-for-copying`, then that
file is copied by `ox-hugo` to the _static_ directory.

Here is an example link:

```org
[[~/some-dir/static/images/foo.png]]
```


### Source path contains `/static/` {#source-path-contains-static}

If you link to files outside of the Hugo `static` directory, just
ensure that the path contains the string `/static/` _if you like to
preserve the directory structure_. Necessary directories are then
created inside the _static_ directory to preserve the structure.

Example translations between outside `static` directory paths to the
copied location inside `static`:

| File location outside `static`   | Copied-to location inside `static`        | Explanation                                                                                                |
|----------------------------------|-------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `~/temp/static/images/foo.png`   | `<HUGO_BASE_DIR>/static/images/foo.png`   | If the **outside** path has `/static/` in it, the directory structure after that is preserved when copied. |
| `~/temp/static/img/foo.png`      | `<HUGO_BASE_DIR>/static/img/foo.png`      | (same as above)                                                                                            |
| `~/temp/static/foo.png`          | `<HUGO_BASE_DIR>/static/foo.png`          | (same as above)                                                                                            |
| `~/temp/static/articles/zoo.pdf` | `<HUGO_BASE_DIR>/static/articles/zoo.pdf` | (same as above)                                                                                            |

See the [Usage → Before you export](/doc/usage#before-you-export)
section to learn how to set the **HUGO\_BASE\_DIR**.


### Source path does not contain `/static/` {#source-path-does-not-contain-static}

Here is an example link where the source path does not contain
`/static/`:

```org
[[~/some-dir/bar/foo.png]]
```

In this case, that file is copied directly to the
`org-hugo-default-static-subdirectory-for-externals` sub-directory
(`ox-hugo/` by default) within the Hugo static directory. No directory
structure generation happens in this case.

| File location outside `static` | Copied-to location inside `static`       | Explanation                                                                                                                   |
|--------------------------------|------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `~/temp/bar/baz/foo.png`       | `<HUGO_BASE_DIR>/static/ox-hugo/foo.png` | Here, as the **outside** path does not have `/static/` in it, the file is copied to the `ox-hugo/` dir in Hugo `static/` dir. |


### Disable auto-copying {#disable-auto-copying}

This auto-copying behavior can be disabled completely by setting
`org-hugo-external-file-extensions-allowed-for-copying` to _nil_.. but
you might not want that if you keep your files **outside** the Hugo
static directory.
