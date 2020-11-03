(require 'ert)
(ert-deftest org-hugo-link/url-underscore-asis ()
  "Expect org-hugo-link can export sanitized/asis
by toggling 'org-hugo-link-url-asis'"
  (let ((url (url-encode-url "https://a_b.com")))
    (progn
      (let ((org-hugo-link-url-asis t))
        (should (equal (org-blackfriday--url-sanitize url t)
                       url)))
      (let ((org-hugo-link-url-asis nil))
        (should (equal (org-blackfriday--url-sanitize url nil)
                       "https://a%5Fb.com"))))))


