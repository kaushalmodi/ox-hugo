;;; telement.el --- Tests related to Ox-hugo and Org element     -*- lexical-binding: t; -*-

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

(require 'org-test-lib)
(require 'ox-hugo)

(ert-deftest test-elem/no-heading ()
  "Test nil return conditions."

  ;; No heading in the tree above has the needed property.
  (should
   (equal nil
          (org-test-with-parsed-data
              "* Heading<point>"
            (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME))))

  ;; Point is at the beginning of the buffer, outside the heading with
  ;; the needed property.
  (should
   (equal nil
          (org-test-with-parsed-data
              "<point>
* Heading
:PROPERTIES:
:EXPORT_FILE_NAME: file
:END:"
            (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME))))

  ;; Needed property is in a different heading tree.
  (should
   (equal nil
          (org-test-with-parsed-data
              "* Heading 1
:PROPERTIES:
:EXPORT_FILE_NAME: file
:END:
* Heading 2
<point>"
            (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME)))))

(ert-deftest test-elem/export-file-name ()
  "Test finding of Hugo post subtree element."

  ;; Point at the very beginning of a heading with the needed
  ;; property.
  (should
   (string= "file2"
            (org-test-with-parsed-data
                "* Heading 1
:PROPERTIES:
:EXPORT_FILE_NAME: file1
:END:
<point>* Heading 2
:PROPERTIES:
:EXPORT_FILE_NAME: file2
:END:
"
              (cdr (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME)))))

  ;; Point in a nested heading under a heading with the needed
  ;; property.
  (should
   (string= "file1"
            (org-test-with-parsed-data
                "* Heading 1
:PROPERTIES:
:EXPORT_FILE_NAME: file1
:END:
** Heading 1.1
*** Heading 1.1.1
<point>"
              (cdr (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME))))))


(ert-deftest test-elem/export-hugo-section ()
  "Test finding the Hugo section."

  ;; The first value seen while traversing up should be used. The way
  ;; the EXPORT_HUGO_SECTION property is set in the below test doesn't
  ;; make sense; but it's just a test ..
  (should
   (string= "sec2"
            (org-test-with-parsed-data
                "* Heading 1
:PROPERTIES:
:EXPORT_HUGO_SECTION: sec1
:END:
** Heading 1.1
:PROPERTIES:
:EXPORT_HUGO_SECTION: sec2
:END:
<point>"
              (cdr (org-hugo--get-elem-with-prop :EXPORT_HUGO_SECTION))))))

(ert-deftest test-elem/starting-position ()
  "Test property lookup by specifying the starting position."

  ;; Here, the needed property is not found because the starting
  ;; position for search is set to the beginning of the buffer.
  (should
   (equal nil
          (org-test-with-parsed-data
              "
* Heading 1<point>
:PROPERTIES:
:EXPORT_OPTIONS: toc:t
:END:"
            (cdr (org-hugo--get-elem-with-prop :EXPORT_OPTIONS 1)))))

  ;; Here, the needed property *is* found because the starting
  ;; position for search is set correctly, and that overrides the init
  ;; position set by <point> in this test.
  (should
   (string= "toc:t"
            (org-test-with-parsed-data
                "<point>
* Heading 2
:PROPERTIES:
:EXPORT_OPTIONS: toc:t
:END:"
              (cdr (org-hugo--get-elem-with-prop :EXPORT_OPTIONS 2))))))


(provide 'telement)
