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

;; the same works
;; #+attr_html: :frame box in org file
(custom-set-variables
 '(org-html-table-default-attributes
   '(:rules "groups" ; groups is needed to have vertical line inside table
     :frame "border")))

;; not to export postamble "created" ... in html
;; see the org manual HTML preamble and postamble
(custom-set-variables
 '(org-html-postamble nil))
