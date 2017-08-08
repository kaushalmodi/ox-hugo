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
  "Cache of tags for the exported post subtree.

These are Org tags linked to a subtree directly or via
inheritance, that do not begin with the \"@\" character.
This is a list of strings.")

(defvar org-hugo--categories-list nil
  "Cache of categories for the exported post subtree.

These are Org tags linked to a subtree directly or via
inheritance, that begin with the \"@\" character.
This is a list of strings.")

(defvar org-hugo--subtree-coord nil
  "Variable to store the current valid Hugo subtree coordinates.")

(defvar org-hugo--subtree-count nil
  "Variable to count of number of subtrees getting exported.
This variable is used when exporting all subtrees in a file.")

(defvar org-hugo-allow-export-after-save t
  "Enable flag for `org-hugo-export-subtree-to-md-after-save'.
When nil, the above function will not export the Org file to
Hugo-compatible Markdown.

This variable is usually set to nil by the user in
`org-capture-before-finalize-hook' and set to t again in
`org-capture-after-finalize-hook', so that the export does not
happen as soon as a new post is created using Org capture.

Note that the export after save will not work until
`org-hugo-export-subtree-to-md-after-save' is added to the
`after-save-hook' by the user.")

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
  "Blackfriday option names as used inside Hugo.
Note that these names are case-sensitive.

This is a list of strings.

Stable Hugo version reference:
- https://gohugo.io/content-management/formats/#blackfriday-options

Development Hugo version reference:
- https://github.com/gohugoio/hugo/blob/master/docs/content/readfiles/bfconfig.md

taskLists
- default: `true'
- Purpose: `false' turns off GitHub-style automatic task/TODO list
           generation.

smartypants
- default: `true'
- Purpose: `false' disables smart punctuation substitutions, including
           smart quotes, smart dashes, smart fractions, etc.  If
           `true', it may be fine-tuned with the `angledQuotes',
           `fractions', `smartDashes', and `latexDashes' flags.

smartypantsQuotesNBSP
- default: `false'
- Purpose: `true' enables French style Guillemets with non-breaking
           space inside the quotes.

angledQuotes
- default: `false'
- Purpose: `true' enables smart, angled double quotes.
           Example: \"Hugo\" renders to «Hugo» instead of “Hugo”.

fractions
- default: `true'
- Purpose: `false' disables smart fractions.
- Example: 5/12 renders to 5⁄12(<sup>5</sup>&frasl;<sub>12</sub>).
- Caveat:  Even with \"fractions = false\", Blackfriday still converts
           1/2, 1/4, and 3/4 respectively to ½ (&frac12;), ¼
           (&frac14;) and ¾ (&frac34;), but only these three.

smartDashes
- default: `true'
- Purpose: `false' disables smart dashes; i.e., the conversion of
           multiple hyphens into an en-dash or em-dash.  If `true',
           its behavior can be modified with the `latexDashes' flag.

latexDashes
- default: `true'
- Purpose: `false' disables LaTeX-style smart dashes and selects
           conventional smart dashes.  Assuming `smartDashes': If
           `true', -- is translated into – (&ndash;), whereas ---
           is translated into — (&mdash;).  However, spaced single
           hyphen between two words is translated into an en dash
           e.g., \"12 June - 3 July\" becomes \"12 June &ndash; 3
           July\" upon rendering.

hrefTargetBlank
- default: `false'
- Purpose: `true' opens external links in a new window or tab.

plainIDAnchors
- default: `true'
- Purpose: `true' renders any heading and footnote IDs without the
           document ID.
- Example: renders \"#my-heading\" instead of
           \"#my-heading:bec3ed8ba720b970\".

extensions
- default: []
- Purpose: Enable one or more Blackfriday's Markdown extensions (if
           they aren't Hugo defaults).
- Example: Include `hardLineBreak' in the list to enable Blackfriday's
           EXTENSION_HARD_LINK_BREAK.

extensionsmask
- default: []
- Purpose: Enable one or more of Blackfriday's Markdown extensions (if
           they aren't Hugo defaults). Example: Include `autoHeaderIds'
           as `false' in the list to disable Blackfriday's
           EXTENSION_AUTO_HEADER_IDS

See `org-hugo-blackfriday-extensions' for valid Blackfriday
extensions.")

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
  "Blackfriday extension names as used inside Hugo.
Note that these names are case-sensitive.

This is a list of strings.

Stable Hugo version reference:
- https://gohugo.io/content-management/formats/#blackfriday-extensions

Development Hugo version references:
- https://github.com/gohugoio/hugo/blob/master/docs/content/readfiles/bfconfig.md
- https://github.com/russross/blackfriday#extensions
- https://github.com/russross/blackfriday/blob/master/markdown.go
- https://github.com/gohugoio/hugo/blob/master/helpers/content.go

noIntraEmphasis
- default: enabled
- Purpose: The \"_\" character is commonly used inside words when
           discussing code, so having Markdown interpret it as an
           emphasis command is usually the wrong thing.  When enabled,
           Blackfriday lets you treat all emphasis markers as normal
           characters when they occur inside a word.

tables
- default: enabled
- Purpose: When enabled, tables can be created by drawing them in the
           input using the below syntax:
- Example:
           Name    | Age
           --------|------
           Bob     | 27
           Alice   | 23

fencedCode
- default: enabled
- Purpose: When enabled, in addition to the normal 4-space indentation
           to mark code blocks, you can explicitly mark them and
           supply a language (to make syntax highlighting simple).

           You can use 3 or more backticks to mark the beginning of
           the block, and the same number to mark the end of the
           block.
- Example:
           ```emacs-lisp
           (message \"Hello\")
           ```

autolink
- default: enabled
- Purpose: When enabled, URLs that have not been explicitly marked as
           links will be converted into links.

strikethrough
- default: enabled
- Purpose: When enabled, text wrapped with two tildes will be crossed
           out.
- Example: ~~crossed-out~~

laxHtmlBlocks
- default: disabled
- Purpose: When enabled, loosen up HTML block parsing rules.
           «Needs more information»

spaceHeaders
- default: enabled
- Purpose: When enabled, be strict about prefix header rules.
           «Needs more information»

hardLineBreak
- default: disabled
- Purpose: When enabled, newlines in the input translate into line
           breaks in the output, like in Org verse blocks.

tabSizeEight
- default: disabled
- Purpose: When enabled, expand tabs to eight spaces instead of four.

footnotes
- default: enabled
- Purpose: When enabled, Pandoc-style footnotes will be supported.
           The footnote marker in the text that will become a
           superscript text; the footnote definition will be placed in
           a list of footnotes at the end of the document.
- Example:
           This is a footnote.[^1]

           [^1]: the footnote text.

noEmptyLineBeforeBlock
- default: disabled
- Purpose: When enabled, no need to insert an empty line to start a
           (code, quote, ordered list, unordered list) block.

headerIds
- default: enabled
- Purpose: When enabled, allow specifying header IDs with {#id}.

titleblock
- default: disabled
- Purpose: When enabled, support Pandoc-style title blocks.
           http://pandoc.org/MANUAL.html#extension-pandoc_title_block

autoHeaderIds
- default: enabled
- Purpose: When enabled, auto-create the header ID's from the headline
           text.

backslashLineBreak
- default: enabled
- Purpose: When enabled, translate trailing backslashes into line
           breaks.

definitionLists
- default: enabled
- Purpose: When enabled, a simple definition list is made of a
           single-line term followed by a colon and the definition for
           that term.
- Example:
           Cat
           : Fluffy animal everyone likes

           Internet
           : Vector of transmission for pictures of cats

           Terms must be separated from the previous definition by a
           blank line.

joinLines
- default: enabled
- Purpose: When enabled, delete newlines and join the lines.  This
           behavior is similar to the default behavior in Org.")


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

(defcustom org-hugo-footer ""
  "String to be appended at the end of each Hugo post.

The string needs to be in a Hugo-compatible Markdown format or HTML."
  :group 'org-export-hugo
  :type 'string
  :safe 'stringp)

(defcustom org-hugo-use-code-for-kbd t
  "When non-nil, ~text~ will translate to <kbd>text</kbd>."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-prefer-hyphen-in-tags t
  "When non-nil, replace underscores with hyphens in Org tags.
In that case, use double underscores to represent a single underscore.

This also affects the Hugo categories set via Org tags using the
\"@\" prefix."
  :group 'org-export-hugo
  :type 'boolean
  :safe #'booleanp)

(defcustom org-hugo-langs-no-descr-in-code-fences '()
  "List of languages whose descriptors should not be exported to Markdown.

This variable is effective only if the HUGO_CODE_FENCE option is set.

If `pygmentsCodeFences' is `true', and if a language is not supported
by Pygments, the HTML of that fenced code block is not rendered
correctly by Hugo.  In such cases, it is better to leave out the
language descriptor and allow the code block to render as an Org
example block.

Example value: (org)."
  :group 'org-export-hugo
  :type '(repeat symbol)
  :safe #'listp)


;;; Define Back-End

(org-export-define-derived-backend 'hugo 'blackfriday ;hugo < blackfriday < md < html
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
  :translate-alist '((code . org-hugo-kbd-tags-maybe)
                     (footnote-reference . org-hugo-footnote-reference)
                     (headline . org-hugo-headline)
                     (inner-template . org-hugo-inner-template)
                     (keyword . org-hugo-keyword)
                     (link . org-hugo-link)
                     (src-block . org-hugo-src-block))
  :filters-alist '((:filter-body . org-hugo-body-filter))

  ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
  :options-alist '(;; Non-front-matter options
                   (:with-toc nil "toc" nil) ;No TOC by default
                   (:preserve-breaks nil "\\n" t) ;Preserve breaks so that text filling in Markdown matches that of Org
                   (:with-smart-quotes nil "'" nil) ;Don't use smart quotes; that is done automatically by Blackfriday
                   (:with-special-strings nil "-" nil) ;Don't use special strings for ndash, mdash; that is done automatically by Blackfriday
                   (:hugo-front-matter-format "HUGO_FRONT_MATTER_FORMAT" nil     org-hugo-front-matter-format)
                   (:hugo-level-offset "HUGO_LEVEL_OFFSET" nil 1)
                   (:hugo-section "HUGO_SECTION" nil org-hugo-default-section-directory)
                   (:hugo-base-dir "HUGO_BASE_DIR" nil nil)
                   (:hugo-static-images "HUGO_STATIC_IMAGES" nil "images")
                   (:hugo-code-fence "HUGO_CODE_FENCE" nil t)
                   (:hugo-menu "HUGO_MENU" nil nil)
                   (:hugo-menu-override "HUGO_MENU_OVERRIDE" nil nil)
                   (:hugo-use-code-for-kbd "HUGO_USE_CODE_FOR_KBD" nil org-hugo-use-code-for-kbd)
                   (:hugo-prefer-hyphen-in-tags "HUGO_PREFER_HYPHEN_IN_TAGS" nil org-hugo-prefer-hyphen-in-tags)
                   (:hugo-custom-front-matter "HUGO_CUSTOM_FRONT_MATTER" nil nil)
                   (:hugo-blackfriday "HUGO_BLACKFRIDAY" nil nil)

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
                   ;; "draft" value is also interpreted by TODO state
                   ;; of a post as Org subtree.
                   (:hugo-draft "HUGO_DRAFT" nil nil)
                   ;; expiryDate
                   (:hugo-expirydate "HUGO_EXPIRYDATE" nil nil)
                   ;; isCJKLanguage
                   (:hugo-iscjklanguage "HUGO_ISCJKLANGUAGE" nil nil)
                   ;; keywords
                   ;; "keywords" is parsed from the Org #+KEYWORDS or
                   ;; subtree property EXPORT_KEYWORDS.
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
                   ;; Org tags parsed from posts as subtrees get the
                   ;; highest precedence as tag names.
                   (:tags "TAGS" nil nil 'space)
                   (:hugo-tags "HUGO_TAGS" nil nil 'space)
                   ;; Org tags starting with "@" parsed from posts as
                   ;; subtrees get the highest precedence as category
                   ;; names.
                   (:hugo-categories "HUGO_CATEGORIES" nil nil 'space)
                   ;; title
                   ;; "title" is parsed from the Org #+TITLE or the subtree heading.
                   ;; type
                   (:hugo-type "HUGO_TYPE" nil nil)
                   ;; url
                   (:hugo-url "HUGO_URL" nil nil)
                   ;; weight
                   (:hugo-weight "HUGO_WEIGHT" nil nil)))


;;; Miscellaneous Helper Functions

(defun org-hugo--plist-value-true-p (key info)
  "Return non-nil if KEY in INFO is non-nil.
If the value of KEY in INFO is nil, \"nil\" or \"\", nil is
returned.

INFO is a plist used as a communication channel."
  (let ((value (plist-get info key)))
    (cond
     ((or (equal t value)
          (equal nil value))
      value)
     ((stringp value)
      ;; "" -> nil
      ;; "t" -> "t"
      ;; "anything else" -> "anything else"
      (org-string-nw-p value))
     (t
      nil))))


;;; Transcode Functions

;;;; Code (<kdb> tags)
(defun org-hugo-kbd-tags-maybe (verbatim _contents info)
  "Wrap text in VERBATIM object with HTML kbd tags.
The kdb wrapping is done if `org-hugo-use-code-for-kbd' is non-nil.

INFO is a plist used as a communication channel."
  (if (org-hugo--plist-value-true-p :hugo-use-code-for-kbd info)
      (format "<kbd>%s</kbd>" (org-element-property :value verbatim))
    (org-md-verbatim verbatim nil nil)))

;;;; Footnote Reference
(defun org-hugo-footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element from Org to Hugo-compatible Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  ;; (message "footref: %s" footnote-reference)
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (and (eq (org-element-type prev) 'footnote-reference)
          (plist-get info :html-footnote-separator)))
   (format "[^fn:%d]" (org-export-get-footnote-number footnote-reference info))))

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
                                     (mapconcat #'identity tag-list ":"))))))
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
;;;###autoload
(defun org-hugo--slug (str)
  "Return a slug string for STR.
STR is in Markdown format, most likely a Markdown heading.  The
returned slug string has the following specification:

- Should contain only lower case alphabet, number and hyphen
  characters.
- Remove \"<code>..</code>\" part from STR if present.
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
         ;; Remove "<code>..</code>" stuff if present.
         (str (replace-regexp-in-string "<code>.*</code>" "" str))
         ;; Remove URLs if present in the string.  The ")" in the
         ;; below regexp is the closing parenthesis of a Markdown
         ;; link: [Desc](Link).
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and ".
         (str (replace-regexp-in-string "&" " and " str))
         ;; Replace "." with " dot ".
         (str (replace-regexp-in-string "\\." " dot " str))
         ;; Replace all characters except alphabets, numbers and
         ;; parentheses with spaces.
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens.
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character.
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens.
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

;;;; Inner Template
(defun org-hugo-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         ;; Fri Jul 21 14:33:25 EDT 2017 - kmodi
         ;; TODO: Need to learn using cl-loop
         ;; Below form from ox-md did not work.
         ;; (fn-alist-stripped
         ;;  (cl-loop for (n raw) in fn-alist collect
         ;;           (cons n (org-trim (org-export-data raw info)))))
         fn-alist-stripped)
    (let ((n 1)
          def)
      (dolist (fn fn-alist)
        ;; (message "fn: %S" fn)
        ;; (message "fn: %s" (org-export-data fn info)) ;This gives error
        ;; (message "fn nth 2 car: %s" (org-export-data (nth 2 fn) info))
        (setq def (org-trim (org-export-data (nth 2 fn) info)))
        (push (cons n def) fn-alist-stripped)
        (setq n (1+ n))))
    (when fn-alist-stripped
      (mapconcat (lambda (fn)
                   (format "[^fn:%d]: %s"
                           (car fn)     ;footnote number
                           (cdr fn)))   ;footnote definition
                 (nreverse fn-alist-stripped)
                 "\n"))))

(defun org-hugo-inner-template (contents info)
  "Return body of document after converting it to Hugo-compatible Markdown.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (org-trim (concat
             contents
             "\n"
             (org-hugo-footnote-section info))))

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

If the HUGO_CODE_FENCE property is set to t (default), the Markdown
style triple-backquoted code blocks are created.  Otherwise, the code
block is wrapped in Hugo `highlight' shortcode.

INFO is a plist used as a communication channel."
  (let ((lang (org-element-property :language src-block)))
    (if (org-hugo--plist-value-true-p :hugo-code-fence info)
        (let ((ret (org-blackfriday-src-block src-block nil info)))
          (when (member (intern lang) org-hugo-langs-no-descr-in-code-fences)
            ;; With the pygmentsCodeFences options enabled in Hugo,
            ;; `org' is not recognized as a "language".  This is
            ;; probably because Pygments does not have a lexer for Org.
            ;; Issue on Pygments repo:
            ;; https://bitbucket.org/birkenfeld/pygments-main/issues/719/wishlist-support-org
            ;; So attempt to do below:
            ;;   ```org
            ;;   # org comment
            ;;   ```
            ;; will not result in a <code> tag wrapped block in HTML.
            ;; So override the language to be an empty string in such cases.
            (setq ret (replace-regexp-in-string
                       (concat "\\`\\(```\\)" lang)
                       "\\1" ret)))
          ret)
      (let ((code (org-export-format-code-default src-block info)))
        (format "{{< highlight %s>}}\n%s{{< /highlight >}}\n" lang code)))))


;;; Filter Functions

;;;; Body Filter
(defun org-hugo-body-filter (body _backend info)
  "Add front matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  ;; `org-md-plain-text' would have escaped all underscores in plain
  ;; text i.e. "_" would have been converted to "\_".
  ;; We need to undo that underscore escaping in Emoji codes for those
  ;; to work.
  ;; Example: Convert ":raised\_hands:" back to ":raised_hands:".
  ;; More Emoji codes: https://www.emoji.codes/
  ;; (Requires setting "enableEmoji = true" in config.toml.)
  (setq body (replace-regexp-in-string
              "\\(:[a-z0-9]+\\)[\\]\\(_[a-z0-9]+:\\)"
              "\\1\\2"
              body))
  (format "%s\n%s%s"
          (org-hugo--get-front-matter info)
          body
          org-hugo-footer))

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
    (concat "\"" (replace-regexp-in-string "\"" "\\\\\""  val) "\""))))

(defun org-hugo--parse-property-arguments (str)
  "Return an alist converted from a string STR of Hugo property value.

If STR is of type \":KEY VALUE\", the returned value is ((KEY
. VALUE)).

Example: Input STR \":foo bar :baz 1 :zoo \\\"two words\\\"\" would
convert to ((foo . \"bar\") (baz . 1) (zoo . \"two words\"))."
  (let ((alist (org-babel-parse-header-arguments str)))
    (dolist (pair alist)
      ;; :KEY -> KEY
      (let ((key (intern (replace-regexp-in-string "\\`:" "" (symbol-name (car pair))))))
        (setcar pair key)))
    alist))

(defun org-hugo--front-matter-value-booleanize (str)
  "Return a \"true\" or \"false\" string for input STR."
  (let ((str-lower (and (stringp str)
                        (downcase str))))
    (cond
     ((or (null str)
          (string= "nil" str-lower)
          (string= "false" str-lower)
          (string= "no" str-lower))
      "false")
     ((or (string= "t" str)
          (string= "true" str)
          (string= "yes" str))
      "true")
     (t
      (user-error "%S needs to represent a boolean value" str)))))

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
                            value
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

(defun org-hugo--parse-menu-prop-to-alist (str)
  "Return an alist of valid Hugo menu properties converted from STR.

Example: Input STR \":name foo :weight 80\" would convert
to ((name . \"foo\") (weight . 80))."
  (let ((menu-alist (org-hugo--parse-property-arguments str))
        valid-menu-alist)
    ;; Hugo menu properties: https://gohugo.io/content-management/menus/
    (dolist (prop '(menu name url identifier pre post weight parent)) ;children prop is probably read-only
      (let ((cell (assoc prop menu-alist)))
        (when cell
          (push cell valid-menu-alist))))
    valid-menu-alist))

(defun org-hugo--sanitize-title (info)
  "Return sanitized version of the title string parsed from INFO.

- Remove bold, italics, monospace Markdown markup characters.
- Do not escape underscore characters in the title.

INFO is a plist used as a communication channel."
  (let* ((title (org-export-data (plist-get info :title) info))
         ;; Sanitize title.. cannot do bold, italics, monospace in title
         (title (replace-regexp-in-string "\\\\?`" "" title))
         (title (replace-regexp-in-string "\\`__?\\|\\`\\*\\*?\\|__?\\'\\|\\*\\*?\\'" "" title))
         (title (replace-regexp-in-string " __?\\|__? \\| \\*\\*?\\|\\*\\*? " " " title))
         ;; Do not escape underscores in title
         (title (replace-regexp-in-string "\\\\_" "_" title)))
    title))

(defun org-hugo--transform-org-tags (str)
  "Transform Org tag STR for use in Hugo tags and categories.

- Single underscores will be replaced with hyphens.
- Double underscores will be replaced with single underscores.

Below shows the example of how the Org tags would translate to
the tag strings in Hugo front matter.

Example: :some_tag:  -> \"some-tag\"
         :some__tag: -> \"some_tag\"."
  (let* ((str (replace-regexp-in-string "\\`_\\([^_]\\)" "-\\1" str))         ;"_a"   -> "-a"
         (str (replace-regexp-in-string "\\`__\\([^_]\\)" "_\\1" str))        ;"__a"  -> "_a"
         (str (replace-regexp-in-string "\\([^_]\\)_\\'" "\\1-" str))         ;"a_"   -> "a-"
         (str (replace-regexp-in-string "\\([^_]\\)__\\'" "\\1_" str))        ;"a__"  -> "a_"
         (str (replace-regexp-in-string "\\([^_]\\)_\\([^_]\\)" "\\1-\\2" str))   ;"a_b"  -> "a-b"
         (str (replace-regexp-in-string "\\([^_]\\)__\\([^_]\\)" "\\1_\\2" str))) ;"a__b" -> "a_b"
    str))

(defun org-hugo--get-front-matter (info)
  "Return the Hugo front matter string.

INFO is a plist used as a communication channel."
  ;; (message "[hugo front matter DBG] info: %S" (pp info))
  (let* ((fm-format (plist-get info :hugo-front-matter-format))
         (hugo-date-fmt "%Y-%m-%dT%T%z")
         (date-raw (or
                    ;; Get the date from the subtree property `EXPORT_DATE' if available
                    (org-string-nw-p (org-export-data (plist-get info :date) info))
                    ;; Else try to get it from the #+DATE keyword in the Org file
                    (org-string-nw-p (org-export-get-date info hugo-date-fmt))))
         (date-nocolon (and (stringp date-raw)
                            (if (string-match-p "\\`[0-9T:-]+\\'" date-raw)
                                ;; If the set DATE is already in
                                ;; Hugo-compatible date format, use it.
                                date-raw
                              (format-time-string
                               hugo-date-fmt
                               ;; Else try to parse the date.
                               (apply #'encode-time (org-parse-time-string date-raw))))))
         ;; Hugo expects the date stamp in this format:
         ;;   2017-07-06T14:59:45-04:00
         ;; But the "%Y-%m-%dT%T%z" format produces the date in this format:
         ;;   2017-07-06T14:59:45-0400 (Note the missing colon)
         ;; Below simply adds that colon.
         (date (and (stringp date-nocolon)
                    (replace-regexp-in-string "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
                                              date-nocolon)))
         (draft (or org-hugo--draft-state
                    (org-export-data (plist-get info :hugo-draft) info)))
         (tag-list (if (org-hugo--plist-value-true-p :hugo-prefer-hyphen-in-tags info)
                       (mapcar #'org-hugo--transform-org-tags
                               org-hugo--tags-list)
                     org-hugo--tags-list))
         (tags (org-string-nw-p ;Don't allow tags to be just whitespace
                (or (org-string-nw-p (mapconcat #'identity tag-list " "))
                    (concat
                     (org-export-data (plist-get info :hugo-tags) info) " "
                     (org-export-data (plist-get info :tags) info)))))
         (categories-list (if (org-hugo--plist-value-true-p :hugo-prefer-hyphen-in-tags info)
                              (mapcar #'org-hugo--transform-org-tags
                                      org-hugo--categories-list)
                            org-hugo--categories-list))
         (categories (or (org-string-nw-p
                          (mapconcat (lambda (str)
                                       ;; Remove "@" from beg of categories.
                                       (replace-regexp-in-string "\\`@" "" str))
                                     categories-list
                                     " "))
                         (org-export-data (plist-get info :hugo-categories) info)))
         (menu-alist (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu)))
         (menu-alist-override (org-hugo--parse-menu-prop-to-alist (plist-get info :hugo-menu-override)))
         ;; If menu-alist-override is non-nil, update menu-alist with values from that.
         (menu-alist (let ((updated-menu-alist menu-alist))
                       (dolist (override-prop menu-alist-override)
                         (let* ((override-key (car override-prop))
                                (override-val (cdr override-prop))
                                (matching-prop (assoc override-key updated-menu-alist)))
                           (if matching-prop
                               (setcdr matching-prop override-val)
                             (push override-prop updated-menu-alist))))
                       updated-menu-alist))
         (custom-fm-data (org-hugo--parse-property-arguments (plist-get info :hugo-custom-front-matter)))
         (blackfriday (org-hugo--parse-blackfriday-prop-to-alist (plist-get info :hugo-blackfriday)))
         (data `(;; The order of the elements below will be the order in which the front matter
                 ;; variables will be ordered.
                 (title . ,(org-hugo--sanitize-title info))
                 (description . ,(org-export-data (plist-get info :description) info))
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
                 (categories . ,categories)
                 (type . ,(org-export-data (plist-get info :hugo-type) info))
                 (url . ,(org-export-data (plist-get info :hugo-url) info))
                 (weight . ,(org-export-data (plist-get info :hugo-weight) info))
                 (draft . ,draft)
                 (blackfriday . ,blackfriday)
                 (menu . ,menu-alist)))
         (data `,(append data custom-fm-data)))
    ;; (message "[get fm info DBG] %S" info)
    ;; (message "[get fm blackfriday DBG] %S" blackfriday)
    ;; (message "[get fm menu DBG] %S" menu-alist)
    ;; (message "[get fm menu override DBG] %S" menu-alist-override)
    ;; (message "[custom fm data DBG] %S" custom-fm-data)
    ;; (message "[fm data DBG] %S" data)
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
        (bf-string "")
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
          ;; and then append it to `front-matter' at the end.  Do the
          ;; same for blackfriday param values.
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

                ;; (message "[menu alist DBG] = %S" menu-alist)
                (when menu-entry
                  (setq menu-entry-str (cond ((string= format "toml")
                                              (format "[menu.%s]\n" menu-entry))
                                             ((string= format "yaml")
                                              (prog1
                                                  (format "menu %s\n%s%s%s\n"
                                                          sign indent menu-entry sign)
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
           ((string= key "blackfriday")
            (when value
              (let* ((bf-alist value)
                     (bf-entry-str "")
                     (bf-value-str ""))
                (setq bf-entry-str (cond ((string= format "toml")
                                          "[blackfriday]\n")
                                         ((string= format "yaml")
                                          (format "blackfriday %s\n" sign))
                                         (t
                                          "")))
                (dolist (bf-pair bf-alist)
                  (let* ((bf-key (symbol-name (car bf-pair)))
                         (bf-value (cdr bf-pair))
                         (bf-value (cond ((or (string= bf-key "extensions")
                                              (string= bf-key "extensionsmask"))
                                          ;; "abc def" -> "[\"abc\", \"def\"]"
                                          (concat "["
                                                  (mapconcat #'identity
                                                             (mapcar #'org-hugo--return-valid-blackfriday-extension
                                                                     (split-string bf-value)) ", ")
                                                  "]"))
                                         (t
                                          (org-hugo--quote-string bf-value)))))
                    ;; (message "blackfriday DBG: %S %S" bf-key bf-value)
                    (setq bf-value-str
                          (concat bf-value-str
                                  (format "%s%s %s %s\n" indent bf-key sign bf-value)))))
                (setq bf-string (concat bf-entry-str bf-value-str)))))
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
    (concat sep front-matter bf-string menu-string sep)))

(defun org-hugo--selective-property-inheritance ()
  "Return a list of properties that should be inherited."
  (let ((prop-list '("HUGO_FRONT_MATTER_FORMAT"
                     "HUGO_PREFER_HYPHEN_IN_TAGS"
                     "HUGO_BLACKFRIDAY"
                     "HUGO_SECTION"
                     "HUGO_BASE_DIR"
                     "HUGO_STATIC_IMAGES"
                     "HUGO_CODE_FENCE"
                     "HUGO_MENU"
                     "HUGO_CUSTOM_FRONT_MATTER"
                     "HUGO_DRAFT"
                     "HUGO_ISCJKLANGUAGE"
                     "KEYWORDS"
                     "HUGO_MARKUP"
                     "HUGO_OUTPUTS"
                     "TAGS"
                     "HUGO_TAGS"
                     "HUGO_CATEGORIES"
                     "HUGO_TYPE"
                     "HUGO_WEIGHT")))
    (mapcar (lambda (str)
              (concat "EXPORT_" str))
            prop-list)))

(defun org-hugo--get-valid-subtree ()
  "Return the org element for a valid Hugo post subtree.
The condition to check validity is that the EXPORT_FILE_NAME
property is defined for the subtree element.

Returns nil if a valid Hugo post subtree is not found."
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

(defun org-hugo--get-post-subtree-coordinates (subtree)
  "Return the coordinates for the current valid Hugo post SUBTREE.

The Org element returned by `org-hugo--get-valid-subtree' is a
valid Hugo post subtree.

The returned value is of type (LEVEL . INDEX) where LEVEL is the
level number of the subtree and INDEX is as explained in the
below example.

If we have

  * Level 1
  ** Level A
  ** Level B
  ** Level C
  * Level 2

the INDEX will be 1 for Level 1 and Level A, 2 for Level
B and Level 2, and 3 for Level C.

So the value returned for Level C will be (2 . 3)."
  (save-excursion
    (let* ((level (org-element-property :level subtree))
           (index 1)
           (current-pos (point))
           (scope (if (org-up-heading-safe)
                      'tree ;Map entries only in parent subtree scope if parent exists
                    nil))) ;Else map in the whole buffer (provided the MATCH conditions below)
      (when level
        (org-map-entries (lambda ()
                           (when (< (point) current-pos)
                             (setq index (1+ index))))
                         ;; Loop through only headings that are at the
                         ;; same level as SUBTREE, and those which have
                         ;; the EXPORT_FILE_NAME property defined.
                         (concat "+LEVEL="
                                 (number-to-string level)
                                 "+EXPORT_FILE_NAME<>\"\"")
                         scope)
        (cons level index)))))

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
  (save-window-excursion
    (save-restriction
      (widen)
      (save-excursion
        (if all-subtrees
            (progn
              (setq org-hugo--subtree-count 0)
              (org-map-entries (lambda ()
                                 (org-hugo-export-subtree-to-md nil async visible-only))
                               ;; Export only the subtrees where
                               ;; EXPORT_FILE_NAME property is not
                               ;; empty.
                               "EXPORT_FILE_NAME<>\"\"")
              (message "[ox-hugo] Exported %d subtrees" org-hugo--subtree-count)
              nil)
          ;; Publish only the current subtree
          (org-back-to-heading :invisible-ok)
          (let ((subtree (org-hugo--get-valid-subtree))
                is-commented tags categories is-excluded)

            (unless subtree
              (user-error "It is mandatory to have a subtree with EXPORT_FILE_NAME property"))

            ;; If subtree is a valid Hugo post subtree, proceed ..
            (setq is-commented (org-element-property :commentedp subtree))

            (cl-labels ((categoryp (tag) ;Returns non-nil if TAG begins with "@"
                                   (string-match-p "\\`@" tag)))
              (let ((org-use-tag-inheritance t)
                    ;; `org-get-tags' returns a list of tags *only*
                    ;; at the current heading; `org-get-tags-at'
                    ;; returns inherited tags too.
                    (all-tags (org-get-tags-at)))
                (setq tags (cl-remove-if #'categoryp all-tags))
                (setq categories (cl-remove-if-not #'categoryp all-tags))))
            (dolist (exclude-tag org-export-exclude-tags)
              (when (member exclude-tag tags)
                (setq is-excluded t)))
            ;; (message "[current subtree DBG] subtree: %S" subtree)
            ;; (message "[current subtree DBG] is-commented:%S, tags:%S, is-excluded:%S"
            ;;          is-commented tags is-excluded)
            (let ((title (org-element-property :title subtree)))
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
                        (org-hugo--tags-list tags)
                        (org-hugo--categories-list categories))
                    ;; Get the current subtree coordinates if it or
                    ;; one of its parents has the HUGO_MENU property defined.
                    (when (org-entry-get nil "EXPORT_HUGO_MENU" :inherit)
                      (setq org-hugo--subtree-coord
                            (org-hugo--get-post-subtree-coordinates subtree)))
                    (org-hugo-export-to-md async :subtreep visible-only))))))))))))

;;;###autoload
(defun org-hugo-export-subtree-to-md-after-save ()
  "Fn for `after-save-hook' to run `org-hugo-export-subtree-to-md'.
Executes `org-hugo-export-subtree-to-md', but only when in a
valid Hugo post subtree.

The export is also skipped if `org-hugo-allow-export-after-save'
is nil."
  (save-excursion
    (when (and
           ;; Condition 1: `org-hugo-allow-export-after-save' must be
           ;; non-nil.
           org-hugo-allow-export-after-save
           ;; Condition 2: Point must be under an Org headline
           (ignore-errors
             (org-back-to-heading :invisible-ok))
           ;; Condition 3: Point must be in a valid Hugo post subtree
           (org-hugo--get-valid-subtree))
      (org-hugo-export-subtree-to-md))))

;;;###autoload
(defun org-hugo-debug-info ()
  "Get Emacs, Org and Hugo version and ox-hugo customization info.
The information is converted to Markdown format and copied to the
kill-ring.  The same information is displayed in the Messages
buffer and returned as a string in Org format."
  (interactive)
  (let* ((emacs-version (emacs-version))
         (org-version (org-version nil :full))
         (hugo-binary (executable-find "hugo"))
         (hugo-version (when hugo-binary
                         (org-trim
                          (shell-command-to-string
                           (concat hugo-binary " version")))))
         (info-org (mapconcat #'identity
                              `("* Debug information for =ox-hugo="
                                "** Emacs Version"
                                ,(format "%s%s"
                                         emacs-version
                                         (if emacs-repository-version
                                             (format " (commit %s)" emacs-repository-version)
                                           ""))
                                "** Org Version"
                                ,(format "%s" org-version)
                                "*** Org =load-path= shadows"
                                ,(let* ((str (list-load-path-shadows :stringp))
                                        (str-list (split-string str "\n" :omit-nulls))
                                        (org-shadow-str ""))
                                   (dolist (shadow str-list)
                                     (when (string-match-p ".*org.+hides.+org.*" shadow)
                                       (setq org-shadow-str (concat org-shadow-str shadow "\n\n"))))
                                   (if (org-string-nw-p org-shadow-str)
                                       (mapconcat #'identity
                                                  `("*Warning*: Possible mixed installation of Org"
                                                    "#+BEGIN_QUOTE"
                                                    ,(org-trim org-shadow-str)
                                                    "#+END_QUOTE"
                                                    "Study the output of =M-x list-load-path-shadows=.")
                                                  "\n")
                                     "No Org mode shadows found in =load-path="))
                                "** Hugo Version"
                                ,(if hugo-binary
                                     (format "%s" hugo-version)
                                   "=hugo= binary not found in PATH")
                                "** =ox-hugo= defcustoms"
                                ,(format "|org-hugo-front-matter-format          |%S|" org-hugo-front-matter-format)
                                ,(format "|org-hugo-default-section-directory    |%S|" org-hugo-default-section-directory)
                                ,(format "|org-hugo-use-code-for-kbd             |%S|" org-hugo-use-code-for-kbd)
                                ,(format "|org-hugo-prefer-hyphen-in-tags        |%S|" org-hugo-prefer-hyphen-in-tags)
                                ,(format "|org-hugo-langs-no-descr-in-code-fences|%S|" org-hugo-langs-no-descr-in-code-fences))
                              "\n"))
         (org-export-with-toc nil)
         (info-md (progn
                    (require 'ox-md)
                    (org-export-string-as info-org 'md :body-only))))
    (kill-new info-md)
    (message "%s" info-org)
    info-org))


(provide 'ox-hugo)

;;; ox-hugo.el ends here
