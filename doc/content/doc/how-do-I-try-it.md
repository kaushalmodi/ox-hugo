+++
title = "How do I try ox-hugo on my site?"
draft = false
[menu."getting_started"]
  weight = 3003
  identifier = "how-do-i-try-ox-hugo-on-my-site"
+++

1.  `cd` to your Hugo site base directory -- the one that contains the
    `config.toml` (or `config.yaml` or `config.json`).
2.  Start the `hugo server` in that directory:

    ```text
    hugo server -D --navigateToChanged
    ```

    -   `--navigateToChanged` requires Hugo 0.25+.
3.  Above command will mention the localhost where the site is
    served. Open that in your browser.
4.  Create a separate directory for Org content in the Hugo site base
    directory. You can name it anything, but I prefer to name it
    `content-org`

    Examples ---

    -   [`ox-hugo` documentation site (you're reading it)](https://github.com/kaushalmodi/ox-hugo/tree/master/doc)
    -   [`ox-hugo` test site](https://github.com/kaushalmodi/ox-hugo/tree/master/test/site)
    -   [_scripter.co_ -- My blog](https://gitlab.com/kaushalmodi/kaushalmodi.gitlab.io)
5.  Create an Org file in there and follow the [**Usage**](/doc/usage)
    section to export it.
