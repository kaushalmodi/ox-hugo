+++
title = "Auto-export on Saving"
draft = false
[menu.enhancements]
  weight = 3001
  identifier = "auto-export-on-saving"
+++

Wouldn't it be awesome if you can see the live-preview of your
Hugo-rendered post each time you saved your post in Org?

Well.. you can do that with these steps:


## First time setup {#first-time-setup}

The first time setup varies between the _per-subtree export flow_ and
_per-file export flow_.


### One post per Org subtree {#one-post-per-org-subtree}


#### Step 1a: Set up the `after-save-hook` {#step-1a-set-up-the-after-save-hook}

1.  Add below to the very-end of your posts Org file:

    ```org
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


#### Step 1b: Prevent auto-export during Org Capture {#step-1b-prevent-auto-export-during-org-capture}

You might find this step useful if you choose to write new posts
using `org-capture` as explained in the [_Org
Capture Setup_](/doc/org-capture-setup) section.

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


### One post per Org file {#one-post-per-org-file}


#### Step 1: Set up the `after-save-hook` {#step-1-set-up-the-after-save-hook}

If you use a seperate Org file for each blog post, you can add the
below to your config:

```emacs-lisp
(use-package ox-hugo
  :ensure t
  :after ox
  :init
  (defconst my/hugo-org-content-dir (expand-file-name "~/hugo_base_dir/content-org/")
    "Directory containing the Org mode posts.")

  (defun my/org-hugo-publish-current-buffer-as-post ()
    "Export the current Org file if a valid Hugo post.
Current file is exported using `org-hugo-export-to-md' if it
contains the #+TITLE keyword and is present in the
`my/hugo-org-content-dir'."
    (let ((fname (buffer-file-name)))
      (when (and fname
                 (string-match-p (concat "\\`" (regexp-quote my/hugo-org-content-dir) ".*\\.org\\'")
                                 fname))
        (save-excursion
          (goto-char (point-min))
          (if (< (how-many "^#\\+TITLE:") 1)
              (message "Unable to export as the Org file is missing the #+TITLE keyword.")
            (org-hugo-export-to-md))))))

  (defun my/org-mode-hook-fn ()
    "My Org mode customization."
    (add-hook 'after-save-hook #'my/org-hugo-publish-current-buffer-as-post :append :local))

  (add-hook 'org-mode-hook #'my/org-mode-hook-fn))
```


## Steps that _might_ need to be taken every time {#steps-that-might-need-to-be-taken-every-time}

Once the initial setup is done, the following steps apply to both
blogging flows.


### Step 2: Start the engines (Hugo Server) {#step-2-start-the-engines--hugo-server}

We start the `hugo server` so that we can see the live-preview each
time the Org file is saved.

I recommend using Hugo version 0.25 at the minimum as that added
support for the awesome `--navigateToChanged` switch!

Run below in your Hugo site root (the directory that contains the site
`config.toml`) to start the server:

```text
hugo server -D --navigateToChanged
```


### Step 3: Open your browser {#step-3-open-your-browser}

By default the site is served locally on port _1313_ on
_localhost_. So the above step would have printed something like below
at the end:

```text
Web Server is available at http://localhost:1313/ (bind address 127.0.0.1)
```

So open your favorite browser pointing to that address.


## FINAL step that needs to be taken every time {#final-step-that-needs-to-be-taken-every-time}


### Step 4: Save and be in awe {#step-4-save-and-be-in-awe}

If you are like me, you might not need to repeat steps 2 and 3 above,
as you can leave the `hugo` server running in a separate terminal, and
have a browser tab pinned to that localhost.

So with that, have the emacs and browser frames set up side-by-side,
and edit your Org post.

Hit `C-x C-s` and be in awe as the browser auto-refreshes to the
**exact post you modified**!
