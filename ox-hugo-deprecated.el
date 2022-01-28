;;; ox-hugo-deprecated.el --- Deprecated stuff from ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.modi@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:

;; This file contains variables and functions deprecated from ox-hugo.
;; Do not depend on this file as it may disappear any day.


;;; Obsoletions

;; Blackfriday support is being removed from `ox-hugo' as Hugo has
;; deprecated its support for a while.
;; https://github.com/kaushalmodi/ox-hugo/discussions/485

;;; Code:

(make-obsolete-variable 'org-hugo-blackfriday-options nil "Hugo has switched to use Goldmark as the default Markdown parser since v0.60." "Jan 15, 2022")
(make-obsolete-variable 'org-hugo-blackfriday-extensions nil "Hugo has switched to use Goldmark as the default Markdown parser since v0.60." "Jan 15, 2022")


;;; Variables

(defvar org-hugo-blackfriday-options
  '("taskLists"
    "smartypants"
    "smartypantsQuotesNBSP"
    "angledQuotes"
    "fractions"
    "smartDashes"
    "latexDashes"
    "hrefTargetBlank"
    "plainIDAnchors"
    "extensions"
    "extensionsmask")
  "Deprecated Blackfriday parser option names.")

(defvar org-hugo-blackfriday-extensions
  '("noIntraEmphasis"
    "tables"
    "fencedCode"
    "autolink"
    "strikethrough"
    "laxHtmlBlocks"
    "spaceHeaders"
    "hardLineBreak"
    "tabSizeEight"
    "footnotes"
    "noEmptyLineBeforeBlock"
    "headerIds"
    "titleblock"
    "autoHeaderIds"
    "backslashLineBreak"
    "definitionLists"
    "joinLines")
  "Deprecated Blackfriday extension names.")



;;; Functions

(defun org-hugo--parse-blackfriday-prop-to-alist (str)
  "Return an alist of valid Hugo blackfriday properties converted from STR.

For example, input STR:

  \":fractions :smartdashes nil :angledquotes t\"

would convert to:

  ((fractions . \"false\") (smartDashes . \"false\") (angledQuotes . \"true\"))

The \"true\" and \"false\" strings in the return value are due to
`org-hugo--front-matter-value-booleanize'."
  (let ((blackfriday-alist (org-hugo--parse-property-arguments str))
        valid-blackfriday-alist)
    (dolist (ref-prop org-hugo-blackfriday-options)
      (dolist (user-prop blackfriday-alist)
        (when (string= (downcase (symbol-name (car user-prop)))
                       (downcase ref-prop))
          (let* ((key (intern ref-prop))
                 (value (cdr user-prop))
                 (value (if (or (equal key 'extensions)
                                (equal key 'extensionsmask))
                            (org-hugo--delim-str-to-list value)
                          (org-hugo--front-matter-value-booleanize value))))
            (push (cons key value)
                  valid-blackfriday-alist)))))
    valid-blackfriday-alist))

(defun org-hugo--return-valid-blackfriday-extension (ext)
  "Return valid case-sensitive string for Blackfriday extension EXT.

Example: If EXT is \"hardlinebreak\",
\"\"hardLineBreak\"\" (quoted string) is returned."
  (let (ret)
    (dolist (ref-ext org-hugo-blackfriday-extensions)
      ;; (message "ox-hugo bf valid ext DBG: ext=%s ref-ext=%s" ext ref-ext)
      (when (string= (downcase ext) (downcase ref-ext))
        (setq ret ref-ext)))
    (unless ret
      (user-error "Invalid Blackfriday extension name %S, see `org-hugo-blackfriday-extensions'"
                  ext))
    (org-hugo--quote-string ret)))


(provide 'ox-hugo-deprecated)

;;; ox-hugo-deprecated.el ends here
