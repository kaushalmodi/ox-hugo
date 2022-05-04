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

;; Silence byte-compiler
(defvar org-hugo--date-time-regexp)
(defvar org-hugo--subtree-coord)
(declare-function org-string-nw-p "org-macs")
(declare-function org-hugo--calc-weight "ox-hugo")
(declare-function org-hugo-slug "ox-hugo")
(declare-function org-hugo--front-matter-value-booleanize "ox-hugo")
(declare-function org-hugo--delim-str-to-list "ox-hugo")
(declare-function org-hugo--parse-property-arguments "ox-hugo")
;;

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
    (org-hugo--yaml-quote-string ret)))

;;; YAML Support
(defun org-hugo--yaml-quote-string (val &optional prefer-no-quotes)
  "Wrap VAL with quotes as appropriate.

VAL can be a string, symbol, number or nil.

VAL is returned as-it-is under the following cases:
- It is a number.
- It is a string and is already wrapped with double quotes.
- It is a string and it's value is \"true\" or \"false\".
- It is a string representing a date.
- It is a string representing an integer or float.

If VAL is nil or an empty string, a quoted empty string \"\" is
returned.

If optional argument PREFER-NO-QUOTES is non-nil, return the VAL
as-it-is if it's a string with just alphanumeric characters."
  (cond
   ((null val)                          ;nil
    val)
   ((numberp val)
    val)
   ((symbolp val)
    (format "\"%s\"" (symbol-name val)))
   ((stringp val)
    (cond
     ((org-string-nw-p val)            ;If `val' is a non-empty string
      (cond
       ((or (and (string= (substring val 0 1) "\"") ;First char is literally a "
                 (string= (substring val -1) "\"")) ;Last char is literally a "
            (and prefer-no-quotes ;If quotes are not preferred and `val' is only alpha-numeric
                 (string-match-p "\\`[a-zA-Z0-9]+\\'" val))
            ;; or if it an integer that can be stored in the system as
            ;; a fixnum.  For example, if `val' is
            ;; "10040216507682529280" that needs more than 64 bits to
            ;; be stored as a signed integer, it will be automatically
            ;; stored as a float.  So (integerp (string-to-number
            ;; val)) will return nil [or `fixnump' instead of
            ;; `integerp' in Emacs 27 or newer]
            ;; https://github.com/toml-lang/toml#integer Integer
            ;; examples: 7, +7, -7, 7_000
            (and (string-match-p "\\`[+-]?[[:digit:]_]+\\'" val)
                 (if (functionp #'fixnump) ;`fixnump' and `bignump' get introduced in Emacs 27.x
                     (fixnump (string-to-number val))
                   (integerp (string-to-number val)))) ;On older Emacsen, `integerp' behaved the same as the new `fixnump'
            (string= "true" val)
            (string= "false" val)
            ;; or if it is a date (date, publishDate, expiryDate, lastmod)
            (string-match-p org-hugo--date-time-regexp val)
            ;; or if it is a float
            ;; https://github.com/toml-lang/toml#float
            ;; Float examples (decimals): 7.8, +7.8, -7.8
            (string-match-p "\\`[+-]?[[:digit:]_]+\\.[[:digit:]_]+\\'" val)
            ;; Float examples (exponentials): 7e-8, -7E+8, 1.7e-05
            (string-match-p "\\`[+-]?[[:digit:]_]+\\(\\.[[:digit:]_]+\\)*[eE][+-]?[[:digit:]_]+\\'" val)
            ;; Special float values (infinity/NaN)
            ;; Looks like Hugo is not supporting these.. Tue Mar 20 18:05:40 EDT 2018 - kmodi
            ;; (let ((case-fold-search nil))
            ;;   (string-match-p "\\`[+-]?\\(inf\\|nan\\)\\'" val))
            )
        val)
       ((string-match-p "\n" val)       ;Multi-line string
        ;; The indentation of the multi-line string is needed only for the
        ;; YAML format.  But the same is done for TOML too just for better
        ;; presentation.
        (setq val (replace-regexp-in-string "^" "  " val))

        ;; https://yaml-multiline.info/
        ;;
        ;;     |             |foo : >
        ;;     |abc          |  abc
        ;;     |       >>>   |
        ;;     |def          |
        ;;     |             |  def
        ;;
        ;; In Org, a single blank line is used to start a new
        ;; paragraph. In the YAML multi-line string, that needs to
        ;; be 2 blank lines.
        (setq val (replace-regexp-in-string "\n[[:blank:]]*\n" "\n\n\n" val))
        (format ">\n%s" val))
       (t                                       ;Single-line string
        ;; Below 2 replacements are order-dependent.. first escape the
        ;; backslashes, then escape the quotes with backslashes.

        ;; Escape the backslashes (for both TOML and YAML).
        (setq val (replace-regexp-in-string "\\\\" "\\\\\\\\" val))
        ;; Escape the double-quotes.
        (setq val (replace-regexp-in-string "\"" "\\\\\""  val))
        (concat "\"" val "\""))))
     (t                                 ;If `val' is any empty string
      "\"\"")))
   (t                            ;Return empty string if anything else
    "\"\"")))

(defun org-hugo--get-yaml-list-string (key list)
  "Return KEY's LIST value as a YAML list, represented as a string.

KEY is a string and LIST is a list where an element can be a
symbol, number or a non-empty string.  Examples:

  \(\"abc\" \"def\")   -> \"[\\\"abc\\\", \\\"def\\\"]\"."
  (concat "["
          (mapconcat #'identity
                     (mapcar (lambda (v)
                               (org-hugo--yaml-quote-string
                                (cond
                                 ((symbolp v)
                                  (symbol-name v))
                                 ((numberp v)
                                  (number-to-string v))
                                 ((org-string-nw-p v)
                                  v)
                                 (t
                                  (user-error "Invalid element %S in `%s' value %S" v key list)))))
                             list)
                     ", ")
          "]"))

(defun org-hugo--gen-yaml-front-matter (data)
  "Generate Hugo front-matter in YAML format, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string."
  (let ((sep "---\n")
        (sign ":")
        (front-matter "")
        (indent (make-string 2 ? ))
        (nested-string "")
        (menu-string "")
        (res-string ""))
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        ;; (message "[hugo fm key value DBG] %S %S" key value)
        (unless (or (null value) ;Skip writing front-matter variables whose value is nil
                    (and (stringp value) ;or an empty string.
                         (string= "" value)))
          ;; In TOML/YAML, the value portion needs to be wrapped in
          ;; double quotes.
          ;; TOML example:
          ;;     title = "My Post"
          ;; YAML example:
          ;;     title: "My Post"
          (cond
           ((string= key "menu")
            (unless (listp value)
              (user-error (concat "The `menu' front-matter did not get the expected "
                                  "list value; probably because HUGO_MENU was not "
                                  "used to set its value.\n"
                                  "Usage examples: \":EXPORT_HUGO_MENU: :menu main\" or "
                                  "\"#+hugo_menu: :menu main\"")))
            ;; Menu name needs to be non-nil to insert menu info in front-matter.
            (when (assoc 'menu value)
              (let* ((menu-alist value)
                     ;; Menu entry string might need to be quoted if
                     ;; it contains spaces, for example.
                     (menu-entry (org-hugo--yaml-quote-string (cdr (assoc 'menu menu-alist)) :prefer-no-quotes))
                     (menu-entry-str "")
                     (menu-value-str ""))
                ;; Auto-set menu identifier if not already set by user.
                (unless (assoc 'identifier menu-alist)
                  (let ((title (cdr (assoc 'title data))))
                    (push `(identifier . ,(org-hugo-slug title)) menu-alist)))

                ;; Auto-set menu weight if not already set by user.
                (unless (assoc 'weight menu-alist)
                  (when org-hugo--subtree-coord
                    (push `(weight . ,(org-hugo--calc-weight)) menu-alist)))

                ;; (message "[menu alist DBG] = %S" menu-alist)
                (when menu-entry
                  (setq menu-entry-str (prog1
                                           (format "menu%s\n%s%s%s\n"
                                                   sign indent menu-entry sign)
                                         (setq indent (concat indent indent)))) ;Double the indent for next use
                  (dolist (menu-pair menu-alist)
                    (let ((menu-key (symbol-name (car menu-pair)))
                          (menu-value (cdr menu-pair)))
                      ;; (message "menu DBG: %S %S %S" menu-entry menu-key menu-value)
                      (unless (string= "menu" menu-key)
                        (when menu-value
                          ;; Cannot skip quote wrapping for values of keys inside menu.
                          ;; Attempting to do:
                          ;;   [menu.foo]
                          ;;     parent = main
                          ;;     # parent = "main" # But this works
                          ;; gives this error:
                          ;; ERROR 2017/07/21 10:56:07 failed to parse page metadata
                          ;; for "singles/post-draft.md": Near line 10 (last key parsed
                          ;; 'menu.foo.parent'): expected value but found "main" instead.
                          (setq menu-value (org-hugo--yaml-quote-string menu-value))
                          (setq menu-value-str
                                (concat menu-value-str
                                        (format "%s%s%s %s\n"
                                                indent menu-key sign menu-value)))))))
                  (setq menu-string (concat menu-entry-str menu-value-str))))))
           ((string= key "resources")
            (unless (listp value)
              (user-error (concat "The `resources' front-matter did not get the expected "
                                  "list value; probably because HUGO_RESOURCES was not "
                                  "used to set its value.\n"
                                  "Usage examples: \":EXPORT_HUGO_RESOURCES: :src \"my-image.png\" :title \"My Image\" "
                                  "or \"#+hugo_resources: :src \"my-image.png\" :title \"My Image\"")))
            (when value
              (dolist (res-alist value)
                (let ((res-entry-str "")
                      (res-value-str "")
                      res-src-present
                      res-param-str)
                  (setq res-entry-str
                        ;; For YAML, this string
                        ;; needs to be inserted
                        ;; only once.
                        (if (org-string-nw-p res-string)
                            ""
                          (format "resources%s\n" sign)))
                  (dolist (res-pair res-alist)
                    ;; (message "[resources DBG] res-pair: %S" res-pair)
                    (let* ((res-key (symbol-name (car res-pair)))
                           (res-value (cdr res-pair)))
                      ;; (message "[resources DBG]: %S %S" res-key res-value)
                      (cond ((string= res-key "params")
                             (setq indent (make-string 4 ? ))
                             (setq res-param-str (format "  %s%s\n" res-key sign))
                             (dolist (param-pair res-value) ;res-value would be an alist of params
                               (let ((param-key (symbol-name (car param-pair)))
                                     (param-value (cdr param-pair))
                                     param-value-str)
                                 ;; (message "[resources DBG] param-key: %S" param-key)
                                 ;; (message "[resources DBG] param-value: %S" param-value)
                                 (setq param-value-str (if (listp param-value)
                                                           (org-hugo--get-yaml-list-string param-key param-value)
                                                         (org-hugo--yaml-quote-string param-value)))
                                 (setq res-param-str
                                       (concat res-param-str
                                               (format "%s%s%s %s\n"
                                                       indent param-key sign param-value-str)))))
                             ;; (message "[resources params DBG] %s" res-param-str)
                             )
                            (t
                             (when (string= res-key "src")
                               (setq res-src-present t))
                             (if (string= res-key "src")
                                 (setq indent "- ")
                               (setq indent "  "))
                             (setq res-value (org-hugo--yaml-quote-string res-value))
                             (setq res-value-str
                                   (concat res-value-str
                                           (format "%s%s%s %s\n"
                                                   indent res-key sign res-value)))))))
                  (unless res-src-present
                    (user-error "`src' must be set for the `resources'"))
                  (setq res-string (concat res-string res-entry-str res-value-str res-param-str))))))
           (;; Front-matter with nested map values: blackfriday, custom front-matter.
            ;; Only 1 level of nesting is supported.
            (and (listp value) ;Example value: '((legs . 4) ("eyes" . 2) (friends . (poo boo)))
                 (eq 0 (cl-count-if (lambda (el) ;Check if value is a list of lists (or conses)
                                      (not (listp el)))
                                    value)))
            (let ((nested-parent-key key)
                  (nested-alist value)
                  (nested-parent-key-str "")
                  (nested-keyval-str ""))
              ;; (message "[nested entry DBG] = %s" nested-parent-key)
              ;; (message "[nested alist DBG] = %S" nested-alist)
              (setq nested-parent-key-str (format "%s%s\n" nested-parent-key sign))
              (dolist (nested-pair nested-alist)
                (unless (consp nested-pair)
                  (user-error "Ox-hugo: Custom front-matter values with nested maps need to be an alist of conses"))
                ;; (message "[nested pair DBG] = %S" nested-pair)
                (let* ((nested-key (car nested-pair))
                       (nested-key (cond
                                    ((symbolp nested-key)
                                     (symbol-name nested-key))
                                    (t
                                     nested-key)))
                       (nested-value (cdr nested-pair))
                       (nested-value (cond
                                      ((and nested-value
                                            (listp nested-value))
                                       (if (and (string= nested-parent-key "blackfriday")
                                                (or (string= nested-key "extensions")
                                                    (string= nested-key "extensionsmask")))
                                           (org-hugo--get-yaml-list-string
                                            nested-key
                                            (mapcar #'org-hugo--return-valid-blackfriday-extension
                                                    nested-value))
                                         (org-hugo--get-yaml-list-string nested-key nested-value)))
                                      ((null nested-value)
                                       "false")
                                      ((equal nested-value 't)
                                       "true")
                                      (t
                                       (org-hugo--yaml-quote-string nested-value)))))
                  ;; (message "nested DBG: %S KEY %S->%S VALUE %S->%S" nested-parent-key
                  ;;          (car nested-pair) nested-key
                  ;;          (cdr nested-pair) nested-value)
                  (when nested-value
                    (setq nested-keyval-str
                          (concat nested-keyval-str
                                  (format "%s%s%s %s\n"
                                          indent nested-key sign nested-value))))))
              (when (org-string-nw-p nested-keyval-str)
                (setq nested-string (concat nested-string nested-parent-key-str
                                            nested-keyval-str)))))
           (t
            (setq front-matter
                  (concat front-matter
                          (format "%s%s %s\n"
                                  key
                                  sign
                                  (cond (;; Tags, categories, keywords, aliases,
                                         ;; custom front-matter which are lists.
                                         (listp value)
                                         (org-hugo--get-yaml-list-string key value))
                                        (t
                                         (org-hugo--yaml-quote-string value nil)))))))))))
    (concat sep front-matter nested-string menu-string res-string sep)))


(provide 'ox-hugo-deprecated)

;;; ox-hugo-deprecated.el ends here
