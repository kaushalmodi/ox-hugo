;; Time-stamp: <2017-09-22 16:03:22 kmodi>

;; Helper function to export Org documentation to GitHub repo Wiki pages

(defun ox-hugo-export-gh-doc ()
  "Export `ox-hugo' Org documentation to documentation on GitHub repo."
  (interactive)
  (let* ((ox-hugo-root-dir (cdr (project-current))) ;Requires emacs 25.1
         (ox-hugo-doc-dir (concat ox-hugo-root-dir "doc/"))
         (ox-hugo-org-file (expand-file-name "ox-hugo-manual.org" ox-hugo-doc-dir))
         ;; cd doc/
         ;; git clone https://github.com/kaushalmodi/ox-hugo.wiki.git
         (ox-hugo-wiki-dir (concat ox-hugo-doc-dir "ox-hugo.wiki/"))
         (org-src-preserve-indentation t) ;Preserve the leading whitespace in src blocks
         (org-id-track-globally nil) ;Prevent "Could not read org-id-values .." error
         (org-export-with-sub-superscripts '{})
         (org-export-with-smart-quotes t)
         (org-export-headline-levels 4)
         (org-src-fontify-natively t))
    (if (file-exists-p ox-hugo-wiki-dir)
        (let ((subtree-tags-to-export '("readme" "contributing" "wiki"))
              ;; If a subtree matches a tag, do not try to export further
              ;; subtrees separately that could be under that.
              (org-use-tag-inheritance nil)
              (org-export-time-stamp-file nil) ;Do not print "Created <timestamp>" in exported files
              (org-export-with-toc nil))       ;Do not export TOC
          (dolist (tag subtree-tags-to-export)
            (let* (;;Retain tags only in the README; needed for `toc-org-insert-toc' to work
                   (org-export-with-tags (if (string= tag "readme") t nil))
                   (exported-file-list (org-map-entries '(org-org-export-to-org nil :subtreep) tag)))
              ;; Move files to their correct directories
              (cond
               ((or (string= "readme" tag)
                    (string= "contributing" tag))
                (dolist (exported-file exported-file-list)
                  (rename-file (expand-file-name exported-file ox-hugo-doc-dir)
                               (expand-file-name exported-file ox-hugo-root-dir)
                               :ok-if-already-exists)))
               ((string= "wiki" tag)
                (dolist (exported-file exported-file-list)
                  (rename-file (expand-file-name exported-file ox-hugo-doc-dir)
                               (expand-file-name exported-file ox-hugo-wiki-dir)
                               :ok-if-already-exists))))))
          ;; Generate TOC in README.org using the `toc-org' package.
          (let ((readme-buf (get-buffer "README.org"))
                (readme-file (expand-file-name "README.org" ox-hugo-root-dir)))
            (when readme-buf    ;Close README.org if it's already open
              (kill-buffer readme-buf))
            ;; Open the README.org file afresh.
            (setq readme-buf (find-file-noselect readme-file))
            (with-current-buffer readme-buf
              (require 'toc-org)
              (toc-org-insert-toc)
              ;; Now remove all Org tags from the README
              (goto-char (point-min))
              (while (replace-regexp "^\\(\\* .*?\\)[[:blank:]]*:[^[:blank:]]+:$" "\\1"))
              (save-buffer))))
      (user-error (concat "ox-hugo.wiki dir does not exist. "
                          "You need to `cd doc/' and "
                          "`git clone https://github.com/kaushalmodi/ox-hugo.wiki.git'")))))
