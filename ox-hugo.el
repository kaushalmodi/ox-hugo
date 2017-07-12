;;; ox-hugo.el --- Hugo Markdown Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Helloyi He

;; Author: Helloyi He
;; Keywords: org, hugo, markdown

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end compatible with the
;; Hugo static site generator (https://gohugo.io/).

;;; Code:

(require 'ox-blackfriday)
(require 'ffap)                         ;For `ffap-url-regexp'

(defvar ffap-url-regexp)                ;Silence byte-compiler

(defvar org-hugo--draft-state nil
  "State variable to store the \"draft\" state of the subtree to be exported.")

(defvar org-hugo--tags-list nil
  "State variable to store the tags of the subtree to be exported.")


;;; User-Configurable Variables

(defgroup org-export-hugo nil
  "Options for exporting Org mode files to Hugo-compatible Markdown."
  :tag "Org Export Hugo"
  :group 'org-export
  :version "25.2")

(defcustom org-hugo-front-matter-format "toml"
  "Format used to front matter.
This variable can be set to either \"toml\" or \"yaml\"."
  :group 'org-export-hugo
  :type '(choice
          (const :tag "TOML" "toml")
          (const :tag "YAML" "yaml")))

(defcustom org-hugo-default-section-directory "posts"
  "Default section for Hugo posts.

This variable is the name of the directory under the \"content/\"
directory where all Hugo posts should go by default."
  :group 'org-export-hugo
  :type 'directory
  :safe 'stringp)


;;; Define Back-End

(org-export-define-derived-backend 'hugo 'blackfriday
  :menu-entry
  '(?H "Export to Hugo-compatible Markdown"
       ((?H "Subtree to file             " (lambda (a _s v b) (org-hugo-export-to-md a :subtreep v)))
        (?h "To file" (lambda (a s v b) (org-hugo-export-to-md a s v)))
        (?T "Subtree to temporary buffer "
            (lambda (a _s v b) (org-hugo-export-as-md a :subtreep v)))
        (?t "To temporary buffer"
            (lambda (a s v b) (org-hugo-export-as-md a s v)))
        (?O "Subtree to file and open    "
            (lambda (a _s v b)
              (if a (org-hugo-export-to-md t :subtreep v)
                (org-open-file (org-hugo-export-to-md nil :subtreep v)))))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-hugo-export-to-md t s v)
                (org-open-file (org-hugo-export-to-md nil s v)))))))
  :translate-alist '((headline . org-hugo-headline)
                     (src-block . org-hugo-src-block)
                     (link . org-hugo-link)
                     ;;(inner-template . org-hugo-inner-template)
                     (table . org-hugo-table))
  :filters-alist '((:filter-body . org-hugo-body-filter)
                   ;; (:filter-link . org-hugo-image-filter)
                   )

  ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
  :options-alist '((:hugo-front-matter-format "HUGO_FRONT_MATTER_FORMAT" nil     org-hugo-front-matter-format)
                   ;; Front matter variables - https://gohugo.io/content/front-matter/
                   ;; Required front matter variables
                   ;; "title" is parsed from the Org #+TITLE or the subtree heading.
                   ;; "date" is parsed from the Org #+DATE or subtree property EXPORT_HUGO_DATE
                   (:description "DESCRIPTION" nil nil)
                   (:date "DATE" nil nil)
                   (:tags "TAGS" nil nil 'space)
                   (:hugo-level-offset "HUGO_LEVEL_OFFSET" loffset 0)
                   (:hugo-tags "HUGO_TAGS" nil nil 'space) ;TODO: Also parse the Org tags as post tags
                   (:hugo-categories "HUGO_CATEGORIES" nil nil 'space)
                   ;; Optional front matter variables
                   (:hugo-aliases "HUGO_ALIASES" nil nil 'space)
                   (:hugo-draft "HUGO_DRAFT" nil nil)
                   (:hugo-publishdate "HUGO_PUBLISHDATE" nil nil)
                   (:hugo-expirydate "HUGO_EXPIRYDATE" nil nil)
                   (:hugo-type "HUGO_TYPE" nil nil)
                   (:hugo-iscjklanguage "HUGO_ISCJKLANGUAGE" nil nil)
                   (:hugo-weight "HUGO_WEIGHT" nil nil)
                   (:hugo-markup "HUGO_MARKUP" nil nil)
                   (:hugo-slug "HUGO_SLUG" nil nil)
                   (:hugo-url "HUGO_URL" nil nil)
                   ;; Non-front-matter options
                   (:with-toc nil "toc" nil) ;No TOC by default
                   (:hugo-section "HUGO_SECTION" nil org-hugo-default-section-directory)
                   (:hugo-base-dir "HUGO_BASE_DIR" nil nil)
                   (:hugo-static-images "HUGO_STATIC_IMAGES" nil "images")))


;;; Transcode Functions

;;;; Headline
(defun org-hugo-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
           (title (org-export-data (org-element-property :title headline) info))
           (todo (and (plist-get info :with-todo-keywords)
                      (let ((todo (org-element-property :todo-keyword
                                    headline)))
                        (and todo (concat (org-export-data todo info) " ")))))
           (tags (and (plist-get info :with-tags)
                      (let ((tag-list (org-export-get-tags headline info)))
                        (and tag-list
                             (format "     :%s:"
                                     (mapconcat 'identity tag-list ":"))))))
           (priority
            (and (plist-get info :with-priority)
                 (let ((char (org-element-property :priority headline)))
                   (and char (format "[#%c] " char)))))
           ;; Headline text without tags.
           (heading (concat todo priority title))
           (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
            (not (memq style '(atx setext)))
            (and (eq style 'atx) (> level 6))
            (and (eq style 'setext) (> level 2)))
        (let ((bullet
               (if (not (org-export-numbered-headline-p headline info)) "-"
                 (concat (number-to-string
                          (car (last (org-export-get-headline-number
                                      headline info))))
                         "."))))
          (concat bullet (make-string (- 4 (length bullet)) ?\s) heading tags "\n\n"
                  (and contents (replace-regexp-in-string "^" "    " contents)))))
       (t
        (let ((anchor
               (format "{#%s}" ;https://gohugo.io/extras/crossreferences/
                       (or (org-element-property :CUSTOM_ID headline)
                           (org-hugo--slug title)
                           ;; (org-export-get-reference headline info)
                           )))
              (loffset (plist-get info :hugo-level-offset)))
          (concat (org-hugo--headline-title style level loffset title anchor)
                  contents)))))))

;;;;; Headline Helpers
(defun org-hugo--slug (str)
  "Return a slug string for STR.
STR is in Markdown format, most likely a Markdown heading.  The
returned slug string has the following specification:

- Should contain only lower case alphabet, number and hyphen
  characters.
- URLs if present in STR should be removed.
- Replace \".\" in STR with \"and\", and \"&\" with \"and\".
- Parentheses should be replaced with double-hyphens ( \"foo (bar)
  baz\" becomes \"foo--bar--baz\").
- One or more consecutive spaces should be replaced with a single
  hyphen.
- Maximum number of consecutive hyphens allowed is two.
- No hyphens should be present at the leading or trailing end of the
  returned string ."
  (let* (;; All lower-case
         (str (downcase str))
         ;; Remove URLs if present in the string
         ;; The ")" in the below regexp is the closing parenthesis of a
         ;; Markdown link: [Desc](Link)
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and "
         (str (replace-regexp-in-string "&" " and " str))
         ;; Replace "." with " dot "
         (str (replace-regexp-in-string "\\." " dot " str))
         ;; Replace non-alphabets and non-numbers with spaces
         (str (replace-regexp-in-string "[^a-z0-9()]" " " str))
         ;; Remove leading and trailing whitespace
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str))

(defun org-hugo--headline-title (style level loffset title &optional anchor)
  "Generate a headline title in the preferred Markdown headline style.
STYLE is the preferred style (`atx' or `setext').  LEVEL is the
header level.  LOFFSET is the offset (a non-negative number) that
is added to the Markdown heading level for `atx' style.  TITLE is
the headline title.  ANCHOR is the Hugo anchor tag for the
section as a string."
  ;; Use "Setext" style
  (if (and (eq style 'setext) (< level 3))
      (let* ((underline-char (if (= level 1) ?= ?-))
             (underline (concat (make-string (length title) underline-char)
                                "\n")))
        (concat "\n" title " " anchor "\n" underline "\n"))
    ;; Use "Atx" style
    ;; Always translate level N Org headline to level N+1 Markdown
    ;; headline because Markdown level 1 headline and HTML title both
    ;; get the HTML <h1> tag, and we do not want the top-most heading
    ;; of a post to look the exact same as the post's title.
    (let ((level-mark (make-string (+ loffset level) ?#)))
      (concat "\n" level-mark " " title " " anchor "\n\n"))))

;;;; Source Blocks
(defun org-hugo-src-block (src-block _contents _info)
  "Convert SRC-BLOCK element to the Hugo `highlight' shortcode."
  (let* ((lang (org-element-property :language src-block))
         (code (org-element-property :value src-block))
         (shortcode (concat "{{< highlight " lang " >}}\n"))
         (close-shortcode "{{< /highlight >}}\n"))
    (concat shortcode code close-shortcode)))

;;;; Links
(defun org-hugo-link (link contents info)
  "Convert LINK to Markdown format.

CONTENTS is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will also copy local images
and rewrite link paths to make blogging more seamless."
  (let ((link-org-files-as-md
         (lambda (raw-path)
           ;; Treat links to `file.org' as links to `file.md'.
           (if (string= ".org" (downcase (file-name-extension raw-path ".")))
               (concat (file-name-sans-extension raw-path) ".md")
             raw-path)))
        (raw-path (org-element-property :path link))
        (images-dir (org-string-nw-p (plist-get info :hugo-static-images)))
        (type (org-element-property :type link)))
    ;; (message "[ox-hugo-link DBG] link filename: %s" (expand-file-name (plist-get (car (cdr link)) :path)))
    ;; (message "[ox-hugo-link DBG] link type: %s" type)
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'md))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (pcase (org-element-type destination)
          (`plain-text                  ;External file
           (let ((path (funcall link-org-files-as-md destination)))
             (if contents
                 (format "[%s](%s)" contents path)
               (format "<%s>" path))))
          (`headline
           (format
            "[%s](#%s)"
            ;; Description
            (cond ((org-string-nw-p contents))
                  ((org-export-numbered-headline-p destination info)
                   (mapconcat #'number-to-string
                              (org-export-get-headline-number destination info)
                              "."))
                  (t (org-export-data (org-element-property :title destination)
                                      info)))
            ;; Reference
            (or (org-element-property :CUSTOM_ID destination)
                (org-export-get-reference destination info))))
          (_
           (let ((description
                  (or (org-string-nw-p contents)
                      (let ((number (org-export-get-ordinal destination info)))
                        (cond
                         ((not number) nil)
                         ((atom number) (number-to-string number))
                         (t (mapconcat #'number-to-string number ".")))))))
             (when description
               (format "[%s](#%s)"
                       description
                       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      ;; (message "[org-hugo-link DBG] processing an image: %s" contents)
      (let* ((path (org-hugo--attachment-rewrite
                    (if (file-name-absolute-p raw-path)
                        (expand-file-name raw-path)
                      raw-path)
                    info))
             (caption (org-export-data
                       (org-export-get-caption
                        (org-export-get-parent-element link))
                       info))
             (parent (org-export-get-parent link))
             (attr (org-export-read-attribute :attr_html parent))
             (class (plist-get attr :class)))
        (format "{{<figure src=\"%s\"%s%s>}}"
                path
                (if (org-string-nw-p caption)
                    (format " caption=\"%s\"" caption)
                  "")
                (if (org-string-nw-p class)
                    (format " class=\"%s\"" class)
                  ""))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
        (format (org-export-get-coderef-format ref contents)
                (org-export-resolve-coderef ref info))))
     ((equal type "radio")
      contents)
     (t
      (let ((path (cond
                   ((member type '("http" "https" "ftp"))
                    (concat type ":" raw-path))
                   ((and (string= type "file")
                         (or (null images-dir)
                             ;; Do not add the "file://" prefix if the
                             ;; raw-path begins with the HUGO_STATIC_IMAGES
                             ;; dir name.
                             (not (string-match-p (concat "\\`/" images-dir "/") raw-path))))
                    (org-hugo--attachment-rewrite
                     (org-export-file-uri
                      (funcall link-org-files-as-md raw-path))
                     info))
                   (t
                    raw-path))))
        (if contents
            (format "[%s](%s)" contents path)
          (format "<%s>" path)))))))

;;;;; Helpers
(defun org-hugo--attachment-rewrite (path info)
  "Copy local images and pdfs to the \"static/\" directory.
Also rewrite image links.

PATH is the path to the image or pdf attachment.
INFO is a plist used as a communication channel."
  ;; (message "[ox-hugo attachment DBG] The Hugo images dir is: %s" (plist-get info :hugo-static-images))
  ;; (message "[ox-hugo attachment DBG] The Hugo section is: %s" (plist-get info :hugo-section))
  ;; (message "[ox-hugo attachment DBG] The Hugo base dir is: %s" (plist-get info :hugo-base-dir))

  (let* ((full-path (file-truename path))
         (exportables '("jpg" "jpeg" "tiff" "png" "pdf" "odt" ))
         (file-name (file-name-nondirectory path))
         (image-export-dir (concat
                            (file-name-as-directory (plist-get info :hugo-base-dir))
                            "static/"
                            (file-name-as-directory (plist-get info :hugo-static-images))
                            ))
         (exported-image (concat image-export-dir file-name)))
    ;; (message "[ox-hugo DBG] Image export dir is: %s" image-export-dir)
    (if (and (file-exists-p full-path)
             (member (file-name-extension path) exportables)
             (file-directory-p image-export-dir))
        (progn
          (unless (file-exists-p exported-image)
            (copy-file full-path exported-image))
          (concat "/static/" (file-name-as-directory (plist-get info :hugo-static-images)) file-name))
      path
      )))


;;;; Hugo Front Matter
(defun org-hugo--wrap-string-in-quotes (str)
  "Wrap STR with double quotes and return the string."
  (cond ((string-empty-p str)
         "")
        ((and (string= (substring str 0 1) "\"")
              (string= (substring str -1) "\""))
         str)
        (t
         (concat "\"" str "\""))))

(defun org-hugo--get-front-matter (info)
  "Return the Hugo front matter string.

INFO is a plist used as a communication channel."
  ;; (message "[hugo front matter DBG] info: %S" (pp info))
  (let* ((fm-format (org-export-data (plist-get info :hugo-front-matter-format) info))
         (hugo-date-fmt "%Y-%m-%dT%T%z")
         (date-nocolon (or ;; Get the date from the subtree property `EXPORT_DATE' if available
                        (org-string-nw-p (org-export-data (plist-get info :date) info))
                        ;; Else try to get it from the #+DATE keyword in the Org file
                        (org-string-nw-p (org-export-get-date info hugo-date-fmt))
                        ;; Else finally set the date to the current date
                        (format-time-string hugo-date-fmt (current-time))))
         ;; Hugo expects the date stamp in this format:
         ;;   2017-07-06T14:59:45-04:00
         ;; But the "%Y-%m-%dT%T%z" format produces the date in this format:
         ;;   2017-07-06T14:59:45-0400 (Note the missing colon)
         ;; Below simply adds that colon.
         (date (replace-regexp-in-string "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2" date-nocolon))
         (tags (or (org-string-nw-p (mapconcat #'identity org-hugo--tags-list " "))
                   (concat
                    (org-export-data (plist-get info :hugo-tags) info) " "
                    (org-export-data (plist-get info :tags) info))))
         (draft (or org-hugo--draft-state
                    (org-export-data (plist-get info :hugo-draft) info)))
         (data `((title . ,(org-export-data (plist-get info :title) info))
                 (date . ,date)
                 (description . ,(org-export-data (plist-get info :hugo-description) info))
                 (tags . ,tags)
                 (categories . ,(org-export-data (plist-get info :hugo-categories) info))
                 (aliases . ,(org-export-data (plist-get info :hugo-aliases) info))
                 (draft . ,draft)
                 (publishdate . ,(org-export-data (plist-get info :hugo-publishdate) info))
                 (expirydate . ,(org-export-data (plist-get info :hugo-expirydate) info))
                 (type . ,(org-export-data (plist-get info :hugo-type) info))
                 (isCJKLanguage . ,(org-export-data (plist-get info :hugo-iscjklanguage) info))
                 (weight . ,(org-export-data (plist-get info :hugo-weight) info))
                 (markup . ,(org-export-data (plist-get info :hugo-markup) info))
                 (slug . ,(org-export-data (plist-get info :hugo-slug) info))
                 (url . ,(org-export-data (plist-get info :hugo-url) info)))))
    (org-hugo--gen-front-matter data fm-format)))

(defun org-hugo--gen-front-matter (data format)
  "Generate the Hugo post front matter, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string.

Generate the front matter in the specified FORMAT.  Valid values
are \"toml\" and \"yaml\"."
  (let ((sep (cond ((string= format "toml") "+++\n")
                   ((string= format "yaml") "---\n")
                   (t "")))
        (sign (cond ((string= format "toml") "=")
                    ((string= format "yaml") ":")
                    (t "")))
        (front-matter ""))
    ;; (message "hugo fm format: %s" format)
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        (unless (or (null value)
                    (string= "" value))
          ;; In TOML/YAML, the value portion needs to be wrapped in double quotes
          ;; TOML example:
          ;;     title = "My Post"
          ;; YAML example:
          ;;     title : "My Post"
          (setq front-matter
                (concat front-matter
                        (format "%s %s %s\n"
                                key
                                sign
                                (cond ((or (string= key "tags")
                                           (string= key "categories"))
                                       ;; "abc def" -> "[\"abc\", \"def\"]"
                                       (concat "["
                                               (mapconcat #'identity
                                                          (mapcar #'org-hugo--wrap-string-in-quotes
                                                                  (split-string value)) ", ")
                                               "]"))
                                      ;; There is no need to wrap strings not expected to contain spaces in
                                      ;; double quotes, like date strings, draft state, etc.
                                      ((or (string= key "date")
                                           (string= key "publishdate")
                                           (string= key "expirydate")
                                           (string= key "draft"))
                                       value)
                                      (t
                                       (org-hugo--wrap-string-in-quotes value)))))))))
    (concat sep front-matter sep)))

;;;; Template

(defun org-hugo-body-filter (body _backend info)
  "Add front matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  (format "%s\n%s" (org-hugo--get-front-matter info) body))


;;; Interactive functions

;;;###autoload
(defun org-hugo-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo-compatible Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Hugo Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'hugo "*Org Hugo Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-hugo-convert-region-to-md ()
  "Convert text in the current region to Hugo-compatible Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'hugo))

;;;###autoload
(defun org-hugo-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo-compatible Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  ;; steals some plist reading code
  ;; from =org-export-as=
  ;; allows us to extract destination file info from
  ;; export-options-alist
  (let* ((info (org-combine-plists
                (org-export--get-export-attributes
                 'hugo subtreep visible-only)
                (org-export--get-buffer-attributes)
                (org-export-get-environment 'hugo subtreep)))
         (base-dir (if (null (plist-get info :hugo-base-dir))
                       (user-error "It is mandatory to set the HUGO_BASE_DIR property")
                     (file-name-as-directory (plist-get info :hugo-base-dir))))
         (content-dir "content/")
         (section-dir (if (null (plist-get info :hugo-section))
                          (user-error "It is mandatory to set the HUGO_SECTION property")
                        (file-name-as-directory (plist-get info :hugo-section))))
         (pub-dir (let ((dir (concat base-dir content-dir section-dir)))
                    (make-directory dir :parents) ;Create the directory if it does not exist
                    dir))
         (outfile (org-export-output-file-name ".md" subtreep pub-dir)))
    (org-export-to-file 'hugo outfile async subtreep visible-only)))

;;;###autoload
(defun org-hugo-walk-headlines ()
  "Publish each 1st level Org headline to a Hugo post."
  (interactive)
  (org-map-entries
   '(lambda ()
      (let* ((entry (org-element-at-point))
             (level (org-element-property :level entry))
             (is-commented (org-element-property :commentedp entry))
             (tags (org-element-property :tags entry)))
        (message "[ox-hugo DBG] On headline %s\n  level is %s\n  tags are %s\n  is-commented is %s\n  test value is %s"
                 (org-element-property :raw-value entry)
                 level tags is-commented
                 (and (eq 1 level)
                      (not (member "noexport" tags))
                      (not is-commented)))
        (if (and (eq 1 level)
                 (not (member "noexport" tags))
                 (not is-commented))
            (org-hugo-export-to-md nil :subtreep)))))
  ;; (org-publish-subtrees-to
  ;; (quote hugo) (buffer-file-name)
  ;; md nil
  ;; (concat (file-name-as-directory hugo-content-dir) hugo-section))
  )

;;;###autoload
(defun org-hugo-export-current-subtree ()
  "Publish the current subtree to a Hugo post.
The next parent subtree having the \"EXPORT_FILE_NAME\" property
is exported if the current subtree doesn't have that property."
  (interactive)
  ;; Reset the state variables first
  (setq org-hugo--draft-state nil)
  (setq org-hugo--tags-list nil)

  (save-excursion
    (org-back-to-heading)
    (let ((entry (catch 'break
                   (while :infinite
                     (let* ((entry (org-element-at-point))
                            (fname (org-element-property :EXPORT_FILE_NAME entry))
                            level)
                       (when fname
                         (throw 'break entry))
                       ;; Keep on jumping to the parent heading if the current
                       ;; entry does not have an EXPORT_FILE_NAME property.
                       (setq level (org-up-heading-safe))
                       ;; If no more parent heading exists, break out of the loop
                       ;; and return nil
                       (unless level
                         (throw 'break nil))))))
          fname is-commented tags is-excluded)
      (if entry
          (progn
            (setq fname (org-element-property :EXPORT_FILE_NAME entry))
            (setq is-commented (org-element-property :commentedp entry))
            (setq tags (org-get-tags))
            (dolist (exclude-tag org-export-exclude-tags)
              (when (member exclude-tag tags)
                (setq is-excluded t)))
            ;; (message "[current subtree DBG] entry: %S" entry)
            (message "[current subtree DBG] fname:%S, is-commented:%S, tags:%S, is-excluded:%S"
                     fname is-commented tags is-excluded)
            (unless (or is-commented
                        is-excluded)
              (let ((draft (if (string-match-p "\\`TODO\\|DRAFT\\'"
                                               (format "%s" (org-get-todo-state)))
                               "true"
                             "false")))
                (message "[current subtree DBG] draft:%S" draft)
                ;; Wed Jul 12 13:10:14 EDT 2017 - kmodi
                ;; FIXME: Is there a better way than passing these
                ;; values via global variables.
                (setq org-hugo--draft-state draft)
                (setq org-hugo--tags-list tags)
                (org-hugo-export-to-md nil :subtreep))))
        (user-error "It is mandatory to have a subtree with EXPORT_FILE_NAME property")))))

;;;###autoload
(defun org-hugo-publish-to-md (plist filename pub-dir)
  "Publish an Org file to Hugo-compatible Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'hugo filename ".md" plist pub-dir))


(provide 'ox-hugo)

;;; ox-hugo.el ends here
