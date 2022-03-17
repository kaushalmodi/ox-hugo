;;; org-test-lib.el --- Library of functions and macros needed for running the tests          -*- lexical-binding: t; -*-

;; Authors: Kaushal Modi <kaushal.modi@gmail.com>
;; URL: https://ox-hugo.scripter.co

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org)

;; Source: https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/testing/org-test.el
(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
         (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
         (if point
             (progn
               (insert (replace-match "" nil nil inside-text))
               (goto-char (1+ (match-beginning 0))))
           (insert inside-text)
           (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))
(def-edebug-spec org-test-with-temp-text (form body))

;; Source: https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/testing/lisp/test-ox.el
(defmacro org-test-with-parsed-data (data &rest body)
  "Execute body with parsed data available.
DATA is a string containing the data to be parsed.  BODY is the
body to execute.  Parse tree is available under the `tree'
variable, and communication channel under `info'."
  (declare (debug (form body)) (indent 1))
  `(org-test-with-temp-text ,data
     (org-export--delete-comment-trees)
     (let* ((tree (org-element-parse-buffer))
            (info (org-combine-plists
                   (org-export--get-export-attributes 'hugo)
                   (org-export-get-environment 'hugo))))
       (org-export--prune-tree tree info)
       (org-export--remove-uninterpreted-data tree info)
       (let ((info (org-combine-plists
                    info (org-export--collect-tree-properties tree info))))
         ,@body))))


(provide 'org-test-lib)
