+++
title = "Image Links"
draft = false
[menu."org_to_hugo"]
  weight = 3003
  identifier = "image-links"
+++

For the sake of the below explanation, let's have the _HUGO\_BASE\_DIR_
(the directory containing the Hugo site's `config.toml` file) be
`~/hugo/`.

In that case, the Hugo _static_ directory will be `~/hugo/static/`.


## References to files in the _static_ directory {#references-to-files-in-the-static-directory}

Now if you have a file `~/hugo/static/images/foo.png`, `ox-hugo` makes
it convenient for you to reference that image by simply
`/images/foo.png`. **This is the default behavior of other Org
exporters.** But in the case of Hugo, as the referenced file will
normally be in the Hugo _static_ directory, the `/images/foo.png`
style of short reference is allowed **if** that is a valid path under
the _static_ directory.

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

This applies to references to non-image files in the _static_
directory too.


## Having references to files **outside** the _static_ directory {#having-references-to-files-outside-the-static-directory}

This is a unique feature of `ox-hugo`.

If a reference is made to a file outside the Hugo _static_ directory
and if it has one of these extensions listed in
`org-hugo-external-file-extensions-allowed-for-copying`, then that
file is copied by `ox-hugo` to the _static_ directory.

Here is an example link:

```org
[[../files-to-be-copied-to-static/static/images/copy-of-unicorn-logo.png]]
```

**Note**: If you link to files outside of the Hugo `static` directory,
just ensure that that path contains `/static/` if you would like to
preserve the directory structure. _The necessary directories are also
created inside the /static_ directory to preserve the structure./

Example translations between outside `static` directory paths to the
copied location inside `static`:

| File location outside `static`   | Copied-to location inside `static`        | Explanation                                                                                                |
|----------------------------------|-------------------------------------------|------------------------------------------------------------------------------------------------------------|
| `~/temp/static/images/foo.png`   | `<HUGO_BASE_DIR>/static/images/foo.png`   | If the **outside** path has `/static/` in it, the directory structure after that is preserved when copied. |
| `~/temp/static/img/foo.png`      | `<HUGO_BASE_DIR>/static/img/foo.png`      | (same as above)                                                                                            |
| `~/temp/static/foo.png`          | `<HUGO_BASE_DIR>/static/foo.png`          | (same as above)                                                                                            |
| `~/temp/static/articles/zoo.pdf` | `<HUGO_BASE_DIR>/static/articles/zoo.pdf` | (same as above)                                                                                            |

See the [Usage → Before you export](/doc/usage#before-you-export) section to learn how to set the
**HUGO\_BASE\_DIR**.


### Source path does not contain `/static/` {#source-path-does-not-contain-static}

Here is an example link where the source path does not contain
`/static/`:

```org
[[../files-to-be-copied-to-static/foo/copy-2-of-unicorn-logo.png]]
```

In that case, that file is copied to the
`org-hugo-default-static-subdirectory-for-externals` sub-directory
(`ox-hugo/` by default) within the Hugo static directory.

| Outside `static`         | Copied-to location inside `static`       | Explanation                                                                                                                   |
|--------------------------|------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `~/temp/bar/baz/foo.png` | `<HUGO_BASE_DIR>/static/ox-hugo/foo.png` | Here, as the **outside** path does not have `/static/` in it, the file is copied to the `ox-hugo/` dir in Hugo `static/` dir. |

-   **Note:** This auto-copying behavior can be disabled by setting
    `org-hugo-external-file-extensions-allowed-for-copying` to
    _nil_.
