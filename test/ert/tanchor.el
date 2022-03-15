;;; tanchor.el --- Tests related to anchor string derivation           -*- lexical-binding: t; -*-

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

(ert-deftest test-anchor/org-hugo-get-md5 ()
  "Test md5 based anchor string generation."
  ;; md5 of "abc" heading
  (should
   (string= "900150"
            (org-test-with-parsed-data "* abc<point>"
              (let ((el (org-element-at-point)))
                (org-hugo-get-md5 el info)))))

  ;; Blank heading
  (should
   (string= "d41d8c"
            (org-test-with-parsed-data "* <point>"
              (let ((el (org-element-at-point)))
                (org-hugo-get-md5 el info)))))

  ;; No heading
  (should
   (string= "d41d8c"
            (org-test-with-parsed-data "<point>"
              (let ((el (org-element-at-point)))
                (org-hugo-get-md5 el info)))))
  )


(provide 'tanchor)
