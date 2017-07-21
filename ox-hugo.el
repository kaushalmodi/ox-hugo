;;; ox-hugo.el --- Hugo Markdown Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;;          Matt Price <moptop99@gmail.com>
;; URL: https://github.com/kaushalmodi/ox-hugo
;; Package-Requires: ((emacs "24.5"))
;; Keywords: org, hugo, markdown

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end compatible with the
;; Hugo static site generator (https://gohugo.io/).

;; Very early version of this package started off as a fork of the
;; ox-hugo package by helloyi (https://github.com/helloyi/ox-hugo).
;; But it has since been re-written and modified heavily.

;;; Code:

(require 'ox-blackfriday)
(require 'ffap)                         ;For `ffap-url-regexp'
(require 'ob-core)                      ;For `org-babel-parse-header-arguments'

(defvar ffap-url-regexp)                ;Silence byte-compiler

(defvar org-hugo--draft-state nil
  "State variable to store the \"draft\" state of the subtree to be exported.")

(defvar org-hugo--tags-list nil
  "State variable to store the tags of the subtree to be exported.")

(defvar org-hugo--subtree-coord nil
  "Variable to store the current valid Hugo subtree coordinates.")

(defvar org-hugo--subtree-count nil
  "Variable to store the count of subtrees getting exported when
exporting all subtrees in a file.")


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
       ((?H "Subtree to file"
            (lambda (a _s v _b)
              (org-hugo-export-subtree-to-md nil a v)))
        (?h "To file"
            (lambda (a s v _b)
              (org-hugo-export-to-md a s v)))
        (?O "Subtree to file and open"
            (lambda (a _s v _b)
              (if a
                  (org-hugo-export-subtree-to-md nil :async v)
                (org-open-file (org-hugo-export-subtree-to-md nil a v)))))
        (?o "To file and open"
            (lambda (a s v _b)
              (if a (org-hugo-export-to-md t s v)
                (org-open-file (org-hugo-export-to-md nil s v)))))
        (?A "All subtrees to files"
            (lambda (a _s v _b)
              (org-hugo-export-subtree-to-md :all-subtrees a v)))
        (?t "To temporary buffer"
            (lambda (a s v _b)
              (org-hugo-export-as-md a s v)))))
  :translate-alist '((headline . org-hugo-headline)
                     (keyword . org-hugo-keyword)
                     (link . org-hugo-link)
                     (src-block . org-hugo-src-block))
  :filters-alist '((:filter-body . org-hugo-body-filter))

  ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
  :options-alist '(;; Non-front-matter options
                   (:hugo-front-matter-format "HUGO_FRONT_MATTER_FORMAT" nil     org-hugo-front-matter-format)
                   (:hugo-level-offset "HUGO_LEVEL_OFFSET" nil 1)
                   (:with-toc nil "toc" nil) ;No TOC by default
                   (:hugo-section "HUGO_SECTION" nil org-hugo-default-section-directory)
                   (:hugo-base-dir "HUGO_BASE_DIR" nil nil)
                   (:hugo-static-images "HUGO_STATIC_IMAGES" nil "images")
                   (:hugo-code-fence "HUGO_CODE_FENCE" nil "t")
                   (:hugo-menu "HUGO_MENU" nil nil)
                   (:hugo-menu-override "HUGO_MENU_OVERRIDE" nil nil)

                   ;; Front matter variables
                   ;; https://gohugo.io/content-management/front-matter/#front-matter-variables
                   ;; aliases
                   (:hugo-aliases "HUGO_ALIASES" nil nil 'space)
                   ;; date
                   ;; "date" is parsed from the Org #+DATE or subtree property EXPORT_HUGO_DATE
                   (:date "DATE" nil nil)
                   ;; description
                   (:description "DESCRIPTION" nil nil)
                   ;; draft
                   ;; "draft" value is also interpreted by TODO state of a post as Org subtree.
                   (:hugo-draft "HUGO_DRAFT" nil nil)
                   ;; expiryDate
                   (:hugo-expirydate "HUGO_EXPIRYDATE" nil nil)
                   ;; isCJKLanguage
                   (:hugo-iscjklanguage "HUGO_ISCJKLANGUAGE" nil nil)
                   ;; keywords
                   ;; "keywords" is parsed from the Org #+KEYWORDS or subtree property EXPORT_KEYWORDS
                   (:keywords "KEYWORDS" nil nil 'space)
                   ;; layout
                   (:hugo-layout "HUGO_LAYOUT" nil nil)
                   ;; lastmod
                   (:hugo-lastmod "HUGO_LASTMOD" nil nil)
                   ;; linkTitle
                   (:hugo-linktitle "HUGO_LINKTITLE" nil nil)
                   ;; markup
                   (:hugo-markup "HUGO_MARKUP" nil nil) ;default is "md"
                   ;; outputs
                   (:hugo-outputs "HUGO_OUTPUTS" nil nil)
                   ;; publishDate
                   (:hugo-publishdate "HUGO_PUBLISHDATE" nil nil)
                   ;; slug
                   (:hugo-slug "HUGO_SLUG" nil nil)
                   ;; taxomonomies - tags, categories
                   ;; Org tags parsed from posts as subtrees get the highest precedence.
                   (:tags "TAGS" nil nil 'space)
                   (:hugo-tags "HUGO_TAGS" nil nil 'space)
                   (:hugo-categories "HUGO_CATEGORIES" nil nil 'space)
                   ;; title
                   ;; "title" is parsed from the Org #+TITLE or the subtree heading.
                   ;; type
                   (:hugo-type "HUGO_TYPE" nil nil)
                   ;; url
                   (:hugo-url "HUGO_URL" nil nil)
                   ;; weight
                   (:hugo-weight "HUGO_WEIGHT" nil nil)))


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

;;;; Keyword
(defun org-hugo-keyword (keyword contents info)
  "Transcode a KEYWORD element into Hugo-compatible Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((kwd (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    (cond
     ((and (equal "HUGO" kwd)           ;Hugo summary splitting
           (stringp value)
           (string-match-p "\\`\\s-*more\\s-*\\'" value))
      ;; https://gohugo.io/content-management/summaries#user-defined-manual-summary-splitting
      "<!--more-->")
     (t
      (org-md-keyword keyword contents info)))))

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
          (concat "/" (file-name-as-directory (plist-get info :hugo-static-images)) file-name))
      path)))

;;;; Source Blocks
(defun org-hugo-src-block (src-block _contents info)
  "Convert SRC-BLOCK element to Hugo-compatible element.

 If the HUGO_CODE_FENCE property is set to t (default), the
 Markdown style triple-backquoted code blocks are created.
 Otherwise, the code block is wrapped in Hugo `highlight'
 shortcode."
  (if (string= "t" (org-export-data (plist-get info :hugo-code-fence) info))
      (org-blackfriday-src-block src-block nil info)
    (let* ((lang (org-element-property :language src-block))
           (code (org-export-format-code-default src-block info)))
      (format "{{< highlight %s>}}\n%s{{< /highlight >}}\n" lang code))))


;;; Filter Functions

;;;; Body Filter
(defun org-hugo-body-filter (body _backend info)
  "Add front matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  (format "%s\n%s" (org-hugo--get-front-matter info) body))

;;;;; Hugo Front Matter
(defun org-hugo--quote-string (val &optional prefer-no-quotes)
  "Wrap VAL with double quotes if it is a string.

VAL is returned as-it-is under the following cases:
- It is not a string (or nil).
- It is a string and is already wrapped with double quotes.
- It is a string and it's value is \"true\" or \"false\".
- It is a string representing a date.

If PREFER-NO-QUOTES is non-nil, return the VAL as-it-is if it's a
string with just alphanumeric characters."
  (cond
   ((or (null val)                ;nil
        (not (stringp val))       ;could be a number, like menu weight
        (and (string= (substring val 0 1) "\"") ;First char is literally a "
             (string= (substring val -1) "\"")) ;Last char is literally a "
        (string= "true" val)
        (string= "false" val)
        ;; or if it is a date (date, publishDate, expiryDate, lastmod)
        (string-match-p "\\`[-0-9T:]+\\'" val))
    val)
   ((and prefer-no-quotes
         (string-match-p "\\`[a-zA-Z0-9]+\\'" val))
    val)
   (t
    (concat "\"" val "\""))))

(defun org-hugo--parse-menu-prop-to-alist (str)
  "Return an alist converted from a string STR of Hugo menu properties.

Example: Input MENU-PROP-STR \":name foo :weight 80\" would
convert to an alist ((:name . \"foo\") (:weight . 80))."
  (let ((menu-alist (org-babel-parse-header-arguments str))
        ret)
    ;; Hugo menu properties: https://gohugo.io/content-management/menus/
    (dolist (prop '(menu name url identifier pre post weight parent)) ;children prop is probably read-only
      (when-let* ((key (intern (concat ":" (symbol-name prop)))) ;name -> :name
                  (cell (assoc key menu-alist)))
        (push `(,prop . ,(cdr cell)) ret)))
    ret))

(defun org-hugo--get-front-matter (info)
  "Return the Hugo front matter string.

INFO is a plist used as a communication channel."
  ;; (message "[hugo front matter DBG] info: %S" (pp info))
  (let* ((fm-format (plist-get info :hugo-front-matter-format))
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
         (draft (or org-hugo--draft-state
                    (org-export-data (plist-get info :hugo-draft) info)))
         (tags (or (org-string-nw-p (mapconcat #'identity org-hugo--tags-list " "))
                   (concat
                    (org-export-data (plist-get info :hugo-tags) info) " "
                    (org-export-data (plist-get info :tags) info))))
         (title (org-export-data (plist-get info :title) info))
         (menu-alist (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu)))
         (menu-alist-override (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu-override)))
         ;; If menu-alist-override is non-nil, update menu-alist with values from that.
         (menu-alist (let ((updated-menu-alist menu-alist))
                       (dolist (override-prop menu-alist-override)
                         (let ((override-key (car override-prop))
                               (override-val (cdr override-prop)))
                           (if-let ((matching-prop (assoc override-key updated-menu-alist)))
                               (setcdr matching-prop override-val)
                             (push override-prop updated-menu-alist))))
                       updated-menu-alist))
         (data `(;; The order of the elements below will be the order in which the front matter
                 ;; variables will be ordered.
                 (title . ,title)
                 (description . ,(org-export-data (plist-get info :hugo-description) info))
                 (date . ,date)
                 (publishDate . ,(org-export-data (plist-get info :hugo-publishdate) info))
                 (expiryDate . ,(org-export-data (plist-get info :hugo-expirydate) info))
                 (aliases . ,(org-export-data (plist-get info :hugo-aliases) info))
                 (isCJKLanguage . ,(org-export-data (plist-get info :hugo-iscjklanguage) info))
                 (keywords . ,(org-export-data (plist-get info :keywords) info))
                 (layout . ,(org-export-data (plist-get info :hugo-layout) info))
                 (lastmod . ,(org-export-data (plist-get info :hugo-lastmod) info))
                 (linkTitle . ,(org-export-data (plist-get info :hugo-linktitle) info))
                 (markup . ,(org-export-data (plist-get info :hugo-markup) info))
                 (outputs . ,(org-export-data (plist-get info :hugo-outputs) info))
                 (slug . ,(org-export-data (plist-get info :hugo-slug) info))
                 (tags . ,tags)
                 (categories . ,(org-export-data (plist-get info :hugo-categories) info))
                 (type . ,(org-export-data (plist-get info :hugo-type) info))
                 (url . ,(org-export-data (plist-get info :hugo-url) info))
                 (weight . ,(org-export-data (plist-get info :hugo-weight) info))
                 (draft . ,draft)
                 (menu . ,menu-alist))))
    ;; (message "[get fm info DBG] %S" info)
    (message "[get fm menu DBG] %S" menu-alist)
    (message "[get fm menu override DBG] %S" menu-alist-override)
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
        (front-matter "")
        (indent "  ")
        (menu-string ""))
    ;; (message "hugo fm format: %s" format)
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        ;; (message "[hugo fm key value DBG] %S %S" key value)
        (unless (or (null value) ;Skip writing front matter variables whose value is nil
                    (and (stringp value) ;or an empty string.
                         (string= "" value)))
          ;; In TOML/YAML, the value portion needs to be wrapped in
          ;; double quotes.
          ;; TOML example:
          ;;     title = "My Post"
          ;; YAML example:
          ;;     title : "My Post"

          ;; In TOML, the menu information in the front matter is as a
          ;; table. So it needs to be always added to the end of the
          ;; front matter. So generate the `menu-string' separately
          ;; and then append it to `front-matter' at the end.
          (cond
           ((string= key "menu")
            ;; Menu name needs to be non-nil to insert menu info in front matter.
            (when (assoc 'menu value)
              (let* ((menu-alist value)
                     ;; Menu entry string might need to be quoted if
                     ;; it contains spaces, for example.
                     (menu-entry (org-hugo--quote-string (cdr (assoc 'menu menu-alist)) :prefer-no-quotes))
                     (menu-entry-str "")
                     (menu-value-str ""))
                ;; Auto-set menu identifier if not already set by user.
                (unless (assoc 'identifier menu-alist)
                  (let ((title (cdr (assoc 'title data))))
                    (push `(identifier . ,(org-hugo--slug title)) menu-alist)))

                ;; Auto-set menu weight if not already set by user.
                (unless (assoc 'weight menu-alist)
                  (when org-hugo--subtree-coord
                    (let* ((level (car org-hugo--subtree-coord))
                           (index (cdr org-hugo--subtree-coord))
                           (weight (+ (* 1000 level) index)))
                      (push `(weight . ,weight) menu-alist))))

                (message "[menu alist DBG] = %S" menu-alist)
                (when menu-entry
                  (setq menu-entry-str (cond ((string= format "toml")
                                              (format "[menu.%s]\n" menu-entry))
                                             ((string= format "yaml")
                                              (prog1
                                                  (format "menu %s\n%s%s%s\n" sign indent menu-entry sign)
                                                (setq indent (concat indent indent)))) ;Double the indent for next use
                                             (t
                                              "")))
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
                          (setq menu-value (org-hugo--quote-string menu-value))
                          (setq menu-value-str
                                (concat menu-value-str
                                        (format "%s%s %s %s\n"
                                                indent menu-key sign menu-value)))))))
                  (setq menu-string (concat menu-entry-str menu-value-str))))))
           (t
            (setq front-matter
                  (concat front-matter
                          (format "%s %s %s\n"
                                  key
                                  sign
                                  (cond ((or (string= key "tags")
                                             (string= key "categories")
                                             (string= key "keywords"))
                                         ;; "abc def" -> "[\"abc\", \"def\"]"
                                         (concat "["
                                                 (mapconcat #'identity
                                                            (mapcar #'org-hugo--quote-string
                                                                    (split-string value)) ", ")
                                                 "]"))
                                        (t
                                         (org-hugo--quote-string value)))))))))))
    (concat sep front-matter menu-string sep)))

(defun org-hugo--selective-property-inheritance ()
  "Returns a list of properties that should be inherited."
  (let ((prop-list '("HUGO_FRONT_MATTER_FORMAT"
                     "HUGO_TAGS"
                     "HUGO_CATEGORIES"
                     "HUGO_DRAFT"
                     "HUGO_TYPE"
                     "HUGO_WEIGHT"
                     "HUGO_MARKUP"
                     "HUGO_SECTION"
                     "HUGO_BASE_DIR"
                     "HUGO_STATIC_IMAGES"
                     "HUGO_CODE_FENCE"
                     "HUGO_MENU"
                     "TAGS"))
        prop-list-allow-inheritance)
    (dolist (prop prop-list)
      (let ((prop (concat "EXPORT_" prop)))
        (push prop prop-list-allow-inheritance)))
    prop-list-allow-inheritance))

(defun org-hugo--get-valid-subtree ()
  "Return the org element for a valid Hugo post subtree.
The condition to check validity is that the EXPORT_FILE_NAME
property is defined for the subtree element."
  (catch 'break
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

(defun org-hugo--get-post-subtree-coordinates (&optional subtree)
  "Return the coordinates for a valid Hugo post subtree.

If SUBTREE is non-nil, return the coordinates for that subtree,
else return the coordinates for the current valid Hugo subtree.

The returned value is of type (LEVEL . INDEX) where LEVEL is the
level number of the subtree and INDEX is as explained in the
below example.

If we have

  * Level 1
  ** Level A
  ** Level B
  ** Level C

this function will return 1 for Level 1 and Level A, 2 for Level
B and 3 for Level C.

So the value returned for Level C would be (2 . 3).

If the point is not in a valid Hugo post subtree, nil is returned."
  (save-restriction
    (widen)
    (save-excursion
      (org-back-to-heading)
      (let* ((entry (if subtree
                        subtree
                      (org-hugo--get-valid-subtree)))
             (level (org-element-property :level entry))
             (index 0))
        (when level
          (let ((curr-line (line-number-at-pos))
                prev-line)
            (catch 'break
              (while :infinite
                (if (and prev-line
                         (= curr-line prev-line))
                    (throw 'break nil)
                  (setq prev-line curr-line)
                  (org-backward-heading-same-level 1)
                  (setq curr-line (line-number-at-pos))
                  (setq index (1+ index))))))
          (cons level index))))))

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
  ;; Allow certain `ox-hugo' properties to be inherited.
  (let ((org-use-property-inheritance (org-hugo--selective-property-inheritance)))
    (org-export-to-buffer 'hugo "*Org Hugo Export*"
      async subtreep visible-only nil nil (lambda () (text-mode)))))

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
         (outfile (org-export-output-file-name ".md" subtreep pub-dir))
         ;; Allow certain `ox-hugo' properties to be inherited.
         (org-use-property-inheritance (org-hugo--selective-property-inheritance)))
    (org-export-to-file 'hugo outfile async subtreep visible-only)))

;;;###autoload
(defun org-hugo-publish-to-md (plist filename pub-dir)
  "Publish an Org file to Hugo-compatible Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; Allow certain `ox-hugo' properties to be inherited.
  (let ((org-use-property-inheritance (org-hugo--selective-property-inheritance)))
    (org-publish-org-to 'hugo filename ".md" plist pub-dir)))

;;;###autoload
(defun org-hugo-export-subtree-to-md (&optional all-subtrees async visible-only)
  "Publish the current subtree to a Hugo post.
The next parent subtree having the \"EXPORT_FILE_NAME\" property
is exported if the current subtree doesn't have that property.

If ALL-SUBTREES is non-nil, publish all subtrees in the current
file.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Returns output file's name.  If ALL-SUBTREES is non-nil, return
nil."
  (interactive "P")
  (save-restriction
    (widen)
    (save-excursion
      (if all-subtrees
          (progn
            (setq org-hugo--subtree-count 0)
            (org-map-entries (lambda ()
                               (let* ((entry (org-element-at-point))
                                      (fname (org-element-property :EXPORT_FILE_NAME entry)))
                                 (when fname
                                   (org-hugo-export-subtree-to-md nil async visible-only)))))
            (message "[ox-hugo] Exported %d subtrees" org-hugo--subtree-count)
            nil)
        ;; Publish only the current subtree
        (when (ignore-errors ;.. if the point is currently under an Org headline
                (org-back-to-heading))
          (let ((entry (org-hugo--get-valid-subtree))
                is-commented tags is-excluded)

            (unless entry
              (user-error "It is mandatory to have a subtree with EXPORT_FILE_NAME property"))

            ;; If entry is a valid Hugo post subtree, proceed ..
            (setq org-hugo--subtree-coord (org-hugo--get-post-subtree-coordinates entry))

            (setq is-commented (org-element-property :commentedp entry))
            ;; (setq tags (org-get-tags)) ;Return a list of tags *only* at the current heading
            (let ((org-use-tag-inheritance t))
              (setq tags (org-get-tags-at))) ;Return a list of tags at current heading
                                        ;+ inherited ones!
            (dolist (exclude-tag org-export-exclude-tags)
              (when (member exclude-tag tags)
                (setq is-excluded t)))
            ;; (message "[current subtree DBG] entry: %S" entry)
            ;; (message "[current subtree DBG] is-commented:%S, tags:%S, is-excluded:%S"
            ;;          is-commented tags is-excluded)
            (let ((title (org-element-property :title entry)))
              (cond
               (is-commented
                (message "[ox-hugo] `%s' was not exported as that subtree is commented" title))
               (is-excluded
                (message "[ox-hugo] `%s' was not exported as it is tagged with one of `org-export-exclude-tags'" title))
               (t
                (message "[ox-hugo] Exporting `%s' .." title)
                (when (numberp org-hugo--subtree-count)
                  (setq org-hugo--subtree-count (1+ org-hugo--subtree-count)))
                (let* ((todo-keyword (format "%s" (org-get-todo-state)))
                       (draft (cond
                               ((string= "TODO" todo-keyword)
                                "true")
                               ((string= "DRAFT" todo-keyword)
                                (message "[ox-hugo] `%s' post is marked as a draft" title)
                                "true")
                               (t
                                "false"))))
                  ;; (message "[current subtree DBG] draft:%S" draft)
                  ;; Wed Jul 12 13:10:14 EDT 2017 - kmodi
                  ;; FIXME: Is there a better way than passing these
                  ;; values via variables.
                  (let ((org-hugo--draft-state draft)
                        (org-hugo--tags-list tags))
                    (org-hugo-export-to-md async :subtreep visible-only))))))))))))


(provide 'ox-hugo)

;;; ox-hugo.el ends here
