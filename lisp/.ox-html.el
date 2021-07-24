;; export html to mke vertical line at both side of table
;; |/ | < | < | is need in table see the "Column Groups" in org.pdf
;; Though org.pdf describes specified the following attribute in org file, doesn't effect.
;; #+ATTR_HTML: border="2" cellspacing="0" cellpadding="6" rules="groups" frame="border"
;; should be plist style
;; #+ATTR_HTML: :border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "border"
;; but only
;; #+attr_html: :frame box
;; is EFFECTIVE.
;(custom-set-variables
; '(org-html-table-default-attributes
;   '(:border "2"
;	     :cellspacing "0"
;	     :cellpadding "6"
;	     :rules "groups"
;	     :frame "border")))

(use-package ox-html
  :custom
  ;; the same works
  ;; #+attr_html: :frame box in org file
  (org-html-table-default-attributes
   '(:rules "groups" ; groups is needed to have vertical line inside table
	    :frame "border"))

  ;; not to export postamble "created" ... in html
  ;; see the org manual HTML preamble and postamble
  (org-html-postamble nil)

  ;; to remove "Validate" link at the bottom of exported html
  ;; https://stackoverflow.com/questions/15134911/in-org-mode-how-do-i-remove-the-validate-xhtml-1-0-message-from-html-export
  (org-html-validation-link nil))

(provide '.ox-html)

