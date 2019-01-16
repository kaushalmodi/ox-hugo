;;; ox-hugo-auto-export.el --- Auto export using ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:

;; *This is NOT a stand-alone package.*
;;
;; It calls `org-hugo-export-wim-to-md' which auto-loads ox-hugo.
;;
;; What this file does when `require'd:
;; - Sets `org-hugo-auto-export-on-save' as safe if boolean.
;; - Updates `org-hugo--org-capture-active-flag'
;;   in `org-capture-before-finalize-hook' and
;;   `org-capture-after-finalize-hook' hooks.
;; - Adds `org-hugo-auto-export-hook-fn' to `org-mode-hook'.

;;; Usage:
;; With `ox-hugo' installed,
;;
;; 1. Add below to your Emacs config:
;;
;;      (require 'ox-hugo-auto-export)
;;
;;    ** Do NOT autoload any function from this package
;;       -- Just `require' it as shown above. **
;;
;; 2. Add below to your project's .dir-locals.el file (assuming that
;;    the Org files in the "content-org" directory are meant to be
;;    auto-exported using `ox-hugo':
;;
;;      (("content-org"
;;        . ((org-mode . ((org-hugo-auto-export-on-save . t))))))
;;

;;; Code:

(warn (concat "`ox-hugo-auto-export.el' has been deprecated.\n"
              "This module will be deleted on 2018/01/20.\n"
              "See https://ox-hugo.scripter.co/doc/deprecation-notices/#org-hugo-auto-export-feature-now-a-minor-mode\n"
              "for details."))

(defvar-local org-hugo-auto-export-on-save nil
  "Enable flag for `org-hugo-export-wim-to-md-after-save'.

This is a buffer local variable to enable auto-exporting using
`ox-hugo' on file saving.  It is intended to be set in
.dir-locals.el files.

For example, put the below in your project's .dir-locals.el if
the Org files in the \"content-org\" directory are meant to be
auto-exported using `ox-hugo':

\((\"content-org\"
  . ((org-mode . ((org-hugo-auto-export-on-save . t)))))) ")

(defvar org-hugo--org-capture-active-flag nil
  "Non-nil means that an Org Capture is in progress.

This variable is used to prevent auto-exports when saving Org
Captures (applicable if creating new posts using Org Capture
using the per-subtree export flow).

This is an internal flag; not to be modified by the user.")

(defun org-hugo-export-wim-to-md-after-save ()
  "Function for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The exporting happens only if `org-hugo-auto-export-on-save' is
non-nil and `org-hugo--org-capture-active-flag' is nil (i.e. Org
Capture is not is progress)."
  ;; (message "[ox-hugo export after save DBG] after-save-hook = %S"
  ;;          after-save-hook)
  ;; (message "[ox-hugo export after save DBG] org-hugo-auto-export-on-save = %S"
  ;;          org-hugo-auto-export-on-save)
  ;; (when (boundp 'org-capture-before-finalize-hook)
  ;;   (message "[ox-hugo export after save DBG] org-capture-before-finalize-hook = %S"
  ;;            org-capture-before-finalize-hook))
  ;; (when (boundp 'org-capture-after-finalize-hook)
  ;;   (message "[ox-hugo export after save DBG] org-capture-after-finalize-hook = %S"
  ;;            org-capture-after-finalize-hook))
  ;; (message "[ox-hugo export after save DBG] org-hugo--org-capture-active-flag = %S"
  ;;          org-hugo--org-capture-active-flag)
  (save-excursion
    (when (and org-hugo-auto-export-on-save
               (not org-hugo--org-capture-active-flag))
      (org-hugo-export-wim-to-md))))

(defun org-hugo-auto-export-hook-fn ()
  "Hook function to enable/disable auto-export of Org file/subtree on file save."
  (add-hook 'after-save-hook #'org-hugo-export-wim-to-md-after-save :append :local))

;;; Org Capture
;; Disallow auto-exporting when saving Org Captures.
(defun org-hugo--disallow-auto-export-during-capture ()
  "Function for `org-capture-before-finalize-hook'.

Set `org-hugo--org-capture-active-flag' to t to prevent the
`ox-hugo' auto-exports from happening when saving captures."
  ;; (message "BEFORE capture")
  (setq org-hugo--org-capture-active-flag t))

(defun org-hugo--allow-auto-export-after-capture ()
  "Function for `org-capture-after-finalize-hook'.

Set `org-hugo--org-capture-active-flag' back to nil so that the
auto-export on save can resume working (if
`org-hugo-auto-export-on-save' is non-nil)."
  ;; (message "AFTER capture")
  (setq org-hugo--org-capture-active-flag nil))

(with-eval-after-load 'org-capture
  (add-hook 'org-capture-before-finalize-hook #'org-hugo--disallow-auto-export-during-capture)
  (add-hook 'org-capture-after-finalize-hook #'org-hugo--allow-auto-export-after-capture :append))

;;; Org Mode Hook
(add-hook 'org-mode-hook #'org-hugo-auto-export-hook-fn)


(provide 'ox-hugo-auto-export)

;;; ox-hugo-auto-export.el ends here
