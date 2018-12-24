;;; ox-hugo-auto-export-mode.el --- Minor mode for auto-exporting using ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>, Evgeni Kolev <evgenysw@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:
;;
;; This is a minor mode for enabling auto-exporting of Org files via
;; ox-hugo.
;;
;; *It is NOT a stand-alone package.*

;;; Usage:
;;
;; To enable this minor mode for a "content-org" directory, add below
;; to the .dir-locals.el:
;;
;;   (("content-org/"
;;     . ((org-mode . ((eval . (org-hugo-auto-export-mode)))))))

;;; Code:

(defvar org-hugo--org-capture-active-flag nil
  "Non-nil means that an Org Capture is in progress.

This variable is used to prevent auto-exports when saving Org
Captures (applicable if creating new posts using Org Capture
using the per-subtree export flow).")

(defun org-hugo-export-wim-to-md-after-save ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only if `org-hugo--org-capture-active-flag'
is nil (i.e. Org Capture is not is progress)."
  (unless org-hugo--org-capture-active-flag
    (save-excursion
      (org-hugo-export-wim-to-md))))

;;; Org Capture Hook Functions
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
  "Toggle auto exporting the Org file using `ox-hugo'."
  :global nil
  :lighter ""
  (if org-hugo-auto-export-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local)
        (with-eval-after-load 'org-capture
          (add-hook 'org-capture-before-finalize-hook #'org-hugo--disallow-auto-export-during-capture)
          (add-hook 'org-capture-after-finalize-hook #'org-hugo--allow-auto-export-after-capture :append)))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :local)
    (when (featurep 'org-capture)
      (remove-hook 'org-capture-before-finalize-hook #'org-hugo--disallow-auto-export-during-capture)
      (remove-hook 'org-capture-after-finalize-hook #'org-hugo--allow-auto-export-after-capture))))


(provide 'ox-hugo-auto-export-mode)

;;; ox-hugo-auto-export-mode.el ends here
