;; Time-stamp: <2017-10-11 04:28:38 kmodi>

;; Export Org document to GitHub documents like README.org,
;; CONTRIBUTING.org.

(defvar ox-hugo-git-root (progn
                           (require 'vc-git)
                           (file-truename (vc-git-root "."))))

(defun ox-hugo-export-gh-doc ()
  "Export `ox-hugo' Org documentation to documentation on GitHub repo."
  (interactive)
  (let* ((ox-hugo-doc-dir (concat ox-hugo-git-root "doc/"))
         (org-src-preserve-indentation t) ;Preserve the leading whitespace in src blocks
         (org-id-track-globally nil) ;Prevent "Could not read org-id-values .." error
         (org-export-with-sub-superscripts '{})
         (org-export-with-smart-quotes t)
         (org-export-headline-levels 4)
         (org-src-fontify-natively t)
         (subtree-tags-to-export '("readme" "contributing"))
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
                         (expand-file-name exported-file ox-hugo-git-root)
                         :ok-if-already-exists)))
         (t
          nil))))
    ;; Generate TOC in README.org using the `toc-org' package.
    (let ((readme-buf (get-buffer "README.org"))
          (readme-file (expand-file-name "README.org" ox-hugo-git-root)))
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
        (save-buffer)))))


(provide 'ox-hugo-export-gh-doc)
