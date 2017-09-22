+++
title = "Auto-export on Saving"
draft = false
[menu.enhancements]
  weight = 3001
  identifier = "auto-export-on-saving"
+++

Wouldn't it be awesome if you can see the live-preview of your
Hugo-rendered post each time you saved your post in Org?

Well.. you can do that with these steps, though, this works **only with
subtree-export flow** at the moment.


## First time setup {#first-time-setup}


### Step 1: Set up the `after-save-hook` {#step-1-set-up-the-after-save-hook}

1.  Add below to the very-end of your posts Org file:

    ```
    ​* Footnotes
    ​* COMMENT Local Variables                                           :ARCHIVE:
    # Local Variables:
    # eval: (add-hook 'after-save-hook #'org-hugo-export-subtree-to-md-after-save :append :local)
    # End:
    ```

    Here I recommend adding the `* Footnotes` header too so that in
    case you add any Org footnotes, they go directly to that section
    you created. Otherwise, in the absence of an existing _Footnotes_
    heading, Org would create a new _Footnotes_ heading **at the end of
    the file** -- so the _Local Variables_ heading will then no longer be
    at the end of the file.
2.  Then save the file, and do `revert-buffer`.
3.  You will be prompted to add that `eval` line to your _Customize_
    setup, hit `!` to permanently save that setting and prevent future
    prompts.


### Step 2: Prevent auto-export during Org Capture {#step-2-prevent-auto-export-during-org-capture}

You might find this step useful if you choose to write new posts using
`org-capture` as explained in the [_Org Capture Setup_](/doc/org-capture-setup) section.

After saving the below to your emacs config and evaluating it,
auto-exports will be prevented when saving a new post created using
Org Capture.

```emacs-lisp
(with-eval-after-load 'org-capture
  ;; Do not cause auto Org->Hugo export to happen when saving captures
  (defun modi/org-capture--remove-auto-org-to-hugo-export-maybe ()
    "Function for `org-capture-before-finalize-hook'.
Disable `org-hugo-export-subtree-to-md-after-save'."
    (setq org-hugo-allow-export-after-save nil))

  (defun modi/org-capture--add-auto-org-to-hugo-export-maybe ()
    "Function for `org-capture-after-finalize-hook'.
Enable `org-hugo-export-subtree-to-md-after-save'."
    (setq org-hugo-allow-export-after-save t))

  (add-hook 'org-capture-before-finalize-hook #'modi/org-capture--remove-auto-org-to-hugo-export-maybe)
  (add-hook 'org-capture-after-finalize-hook #'modi/org-capture--add-auto-org-to-hugo-export-maybe))
```


## Steps that _might_ need to be taken every time {#steps-that-might-need-to-be-taken-every-time}


### Step 3: Start the engines (Hugo Server) {#step-3-start-the-engines--hugo-server}

We start the `hugo server` so that we can see the live-preview each
time the Org file is saved.

I recommend using Hugo version 0.25 at the minimum as that added
support for the awesome `--navigateToChanged` switch!

Run below in your Hugo site root (the directory that contains the site
`config.toml`) to start the server:

```text
hugo server -D --navigateToChanged
```


### Step 4: Open your browser {#step-4-open-your-browser}

By default the site is served locally on port _1313_ on
_localhost_. So the above step would have printed something like below
at the end:

```text
Web Server is available at http://localhost:1313/ (bind address 127.0.0.1)
```

So open your favorite browser pointing to that address.


## FINAL step that needs to be taken every time {#final-step-that-needs-to-be-taken-every-time}

If you are like me, you might not need to repeat steps 3 and 4 above,
as you can leave the `hugo` server running in a separate terminal, and
have a browser tab pinned to that localhost.

So with that, have the emacs and browser frames set up side-by-side,
and edit your Org post.

Hit `C-x C-s` and be in awe as the browser auto-refreshes to the
**exact post you modified**!
