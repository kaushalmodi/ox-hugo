;;; ox-hugo-auto-export.el --- Auto export using ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:

;; *This is NOT a stand-alone package.*
;;
;; It calls `org-hugo-export-wim-to-md' which auto-loads ox-hugo.

;;; Code:

(defvar org-hugo-allow-export-after-save t
  "Enable flag for `org-hugo-export-wim-to-md-after-save'.
When nil, the above function will not export the Org file to
Hugo-compatible Markdown.

This variable is usually set to nil by the user in
`org-capture-before-finalize-hook' and set to t again in
`org-capture-after-finalize-hook', so that the export does not
happen as soon as a new post is created using Org capture.

Note that the export after save will not work until
`org-hugo-export-wim-to-md-after-save' is added to the
`after-save-hook' by the user.")

;;;###autoload
(defun org-hugo-export-wim-to-md-after-save ()
  "Fn for `after-save-hook' to run `org-hugo-export-wim-to-md'.

The export is also skipped if `org-hugo-allow-export-after-save'
is nil.  This variable is intended to be toggled dynamically in
`org-capture-before-finalize-hook' and
`org-capture-after-finalize-hook' hooks.  See the ‘Auto-export on
Saving’ section in this package's documentation for an example."
  (save-excursion
    (when org-hugo-allow-export-after-save
      (org-hugo-export-wim-to-md))))


(provide 'ox-hugo-auto-export)

;;; ox-hugo-auto-export.el ends here
