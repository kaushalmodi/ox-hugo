;;; tslug.el --- Tests related to slug string derivation           -*- lexical-binding: t; -*-

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

(ert-deftest test-slug/return-nil ()
  "Test nil return conditions."

  ;; Empty title
  (should
   (equal nil
          (org-test-with-parsed-data
           "* <point>"
           (let ((el (org-element-at-point)))
             (org-hugo--heading-get-slug el info)))))

  ;; Heading with EXPORT_FILE_NAME.
  (should
   (equal nil
          (org-test-with-parsed-data
           "* Some Heading<point>"
           (let ((el (org-element-at-point)))
             (org-hugo--heading-get-slug el info))))))

(ert-deftest test-slug/title ()
  "Test slug when EXPORT_HUGO_SLUG is set."
  ;; EXPORT_FILE_NAME + EXPORT_HUGO_SLUG
  (should
   (string= "slug"
            (org-test-with-parsed-data
             "* Some Heading<point>
:PROPERTIES:
:EXPORT_FILE_NAME: file
:EXPORT_HUGO_SLUG: slug
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; EXPORT_FILE_NAME + EXPORT_HUGO_SLUG + EXPORT_HUGO_SECTION
  (should
   (string= "section/slug"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Some Heading<point>
:PROPERTIES:
:EXPORT_FILE_NAME: file
:EXPORT_HUGO_SLUG: slug
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info))))))

(ert-deftest test-slug/export-file-name ()
  "Test derivation of the slug from EXPORT_FILE_NAME."

  ;; Only EXPORT_FILE_NAME
  (should
   (string= "file"
            (org-test-with-parsed-data
             "* Some Heading<point>
:PROPERTIES:
:EXPORT_FILE_NAME: file
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; EXPORT_FILE_NAME + EXPORT_HUGO_SECTION
  (should
   (string= "section/file"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Some Heading<point>
:PROPERTIES:
:EXPORT_FILE_NAME: file
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info))))))

;; Leaf bundles
(ert-deftest test-slug/leaf-bundles ()
  "Test derivation of the slug leaf bundles."
  (should
   (string= "leaf"
            (org-test-with-parsed-data
             "* Some Heading<point>
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: leaf
:EXPORT_FILE_NAME: index
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; Leaf bundle in a section
  (should
   (string= "section/leaf"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Some Heading<point>
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: leaf
:EXPORT_FILE_NAME: index
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info))))))

;; Branch bundles
(ert-deftest test-slug/branch-bundles ()
  (should
   (string= "branch"
            (org-test-with-parsed-data
             "* Some Heading<point>
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: branch
:EXPORT_FILE_NAME: _index
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; Inherit :EXPORT_HUGO_BUNDLE
  (should
   (string= "branch"
            (org-test-with-parsed-data
             "* Branch bundle
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: branch
:END:
** Landing page<point>
:PROPERTIES:
:EXPORT_FILE_NAME: _index
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; Page in branch bundle, inheritance
  (should
   (string= "branch/branch-page"
            (org-test-with-parsed-data
             "* Branch bundle
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: branch
:END:
** Branch page<point>
:PROPERTIES:
:EXPORT_FILE_NAME: branch-page
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; Branch bundle in a section
  (should
   (string= "section/branch"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Branch bundle
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: branch
:END:
*** Some Heading<point>
:PROPERTIES:
:EXPORT_FILE_NAME: _index
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; Branch page in a branch bundle in a section
  (should
   (string= "section/branch/branch-page"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Branch bundle
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: branch
:END:
*** Some Heading<point>
:PROPERTIES:
:EXPORT_FILE_NAME: branch-page
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info))))))

;; Section fragments
(ert-deftest test-slug/section-fragments ()

  (should
   (string= "section/sec2/sec3/file"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Section 2
:PROPERTIES:
:EXPORT_HUGO_SECTION*: sec2
:END:
*** Section 3
:PROPERTIES:
:EXPORT_HUGO_SECTION*: sec3
:END:
**** Some page<point>
:PROPERTIES:
:EXPORT_FILE_NAME: file
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info)))))

  ;; Branch page in a branch bundle in section fragments.
  (should
   (string= "section/sec2/sec3/branch/branch-page"
            (org-test-with-parsed-data
             "* Section
:PROPERTIES:
:EXPORT_HUGO_SECTION: section
:END:
** Section 2
:PROPERTIES:
:EXPORT_HUGO_SECTION*: sec2
:END:
*** Section 3
:PROPERTIES:
:EXPORT_HUGO_SECTION*: sec3
:EXPORT_HUGO_BUNDLE: branch
:END:
**** Branch bundle
:PROPERTIES:
:EXPORT_HUGO_BUNDLE: branch
:END:
***** Some page<point>
:PROPERTIES:
:EXPORT_FILE_NAME: branch-page
:END:"
             (let ((el (org-element-at-point)))
               (org-hugo--heading-get-slug el info))))))


(provide 'tslug)
