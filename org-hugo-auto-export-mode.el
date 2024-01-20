;;; org-hugo-auto-export-mode.el --- Minor mode for auto-exporting using ox-hugo -*- lexical-binding: t -*-

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

(declare-function org-hugo-export-wim-to-md "ox-hugo")

(defcustom org-hugo-auto-export-async nil
  "When non-nil, `org-hugo-auto-export-mode' will export asynchronously.

This avoids blocking the main thread while exporting."
  :group 'org-export-hugo
  :type 'boolean)
;;;###autoload (put 'org-hugo-auto-export-async 'safe-local-variable 'booleanp)

(defun org-hugo-export-wim-to-md-after-save ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only when Org Capture is not in progress."
  (unless (eq real-this-command 'org-capture-finalize)
    (save-excursion
      (org-hugo-export-wim-to-md :async org-hugo-auto-export-async))))

;;;###autoload
(define-minor-mode org-hugo-auto-export-mode
  "Toggle auto exporting the Org file using `ox-hugo'."
  :global nil
  :lighter ""
  (if org-hugo-auto-export-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :local)))


(provide 'org-hugo-auto-export-mode)

;;; org-hugo-auto-export-mode.el ends here
