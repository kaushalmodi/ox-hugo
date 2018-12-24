;;; ox-hugo-auto-export.el --- Auto export using ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:
;;
;; Enable auto exporting of Org files via ox-hugo
;;
;;; Usage:
;;
;; (org-hugo-auto-export-mode)
;;
;;; Or, alternatively, via .dir-locals.el:
;;
;;   (("content-org/"
;;     (org-mode
;;      (mode . org-hugo-auto-export))))

;;; Code:

(require 'ox-hugo)

(defvar org-hugo--org-capture-active-flag nil
  "Non-nil means that an Org Capture is in progress.

This variable is used to prevent auto-exports when saving Org
Captures (applicable if creating new posts using Org Capture
using the per-subtree export flow).")

(defun org-hugo-export-wim-to-md-after-save ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only if `org-hugo--org-capture-active-flag' is
nil (i.e. Org Capture is not is progress)."
  (unless org-hugo--org-capture-active-flag
    (save-excursion
      (org-hugo-export-wim-to-md))))

;;; Org Capture
;; Disallow auto-exporting when saving Org Captures.
(defun org-hugo--disallow-auto-export-during-capture ()
  "Function for `org-capture-before-finalize-hook'.

Set `org-hugo--org-capture-active-flag' to t to prevent the
`ox-hugo' auto-exports from happening when saving captures."
  (setq org-hugo--org-capture-active-flag t))

(defun org-hugo--allow-auto-export-after-capture ()
  "Function for `org-capture-after-finalize-hook'.

Set `org-hugo--org-capture-active-flag' back to nil so that the
auto-export on save can resume working"
  (setq org-hugo--org-capture-active-flag nil))

;;;###autoload
(define-minor-mode org-hugo-auto-export-mode
  "Toggle auto exporting the org file to Hugo content"
  :global nil
  :lighter ""
  :require 'ox-hugo-auto-export
  (if org-hugo-auto-export-mode
      ;; enable the mode
      (progn
        (add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local)
        (with-eval-after-load 'org-capture
          (add-hook 'org-capture-before-finalize-hook #'org-hugo--disallow-auto-export-during-capture)
          (add-hook 'org-capture-after-finalize-hook #'org-hugo--allow-auto-export-after-capture :append)))
    ;; disable the mode
    (remove-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :local)
    (remove-hook 'org-capture-before-finalize-hook #'org-hugo--disallow-auto-export-during-capture)
    (remove-hook 'org-capture-after-finalize-hook #'org-hugo--allow-auto-export-after-capture)))

(provide 'ox-hugo-auto-export-mode)

;;; ox-hugo-auto-export-mode.el ends here
