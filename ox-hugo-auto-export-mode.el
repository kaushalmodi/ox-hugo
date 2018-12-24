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

(defun org-hugo-export-wim-to-md-after-save ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only if Org Capture is not is progress."
  (unless (eq real-this-command 'org-capture-finalize)
    (save-excursion
      (org-hugo-export-wim-to-md))))

;;;###autoload
(define-minor-mode org-hugo-auto-export-mode
  "Toggle auto exporting the Org file to Hugo content"
  :global nil
  :lighter ""
  :require 'ox-hugo-auto-export
  (if org-hugo-auto-export-mode
      (add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local)
    (remove-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :local)))

(provide 'ox-hugo-auto-export-mode)

;;; ox-hugo-auto-export-mode.el ends here
