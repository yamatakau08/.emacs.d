(with-eval-after-load "ox-confluence"
  (defun org-confluence-link (link desc info)
    "Redfined the original function to export image link !url! not [url]
https://support.atlassian.com/confluence-cloud/docs/insert-confluence-wiki-markup/
See Link,Images"
    (let* ((raw-link  (org-element-property :raw-link link ))
	   (fname-extension (file-name-extension raw-link))
	   (parent    (org-element-property :parent link))
	   (attr_html (org-element-property :attr_html parent))
	   (plist_oattr_html (read (format "(%s)" (car attr_html))))
	   ;; Available HTML image tags: align border bordercolor hspace vspace width height title alt
	   ;; remove :align because if :align left is specified, next text block is aligned at a side of image
	   (plist_attr_html (map-delete plist_oattr_html :align))
	   (width  (plist-get plist_attr_html :width))
	   (height (plist-get plist_attr_html :height))
	   twidth theight tags)

      (if (and fname-extension
	       (string-match "png\\|PNG\\|jpg\\|JPG" fname-extension))
	  ;; image
	  (progn
	    ;; confluence support only px width,height
	    ;; without px is available. eg. width=300
	    (if width
		(setq twidth (format "width=%s" width)))
	    (if height
		(setq theight (format "height=%s" height)))

	    (setq tags (mapconcat #'identity (delq nil `(,twidth ,theight)) ","))

	    (if (string-equal tags "")
		(format "!%s!" raw-link)
	      (format "!%s|%s!" raw-link tags)))
	;; link
	(concat "["
		(when (org-string-nw-p desc) (format "%s|" desc))
		(cond
		 ((string-match "^confluence:" raw-link)
		  (replace-regexp-in-string "^confluence:" "" raw-link))
		 (t
		  raw-link))
		"]")))))

(defun xorg-confluence-link (link desc info)
  "Redfined the original function to export image link !url! not [url]
https://support.atlassian.com/confluence-cloud/docs/insert-confluence-wiki-markup/
See Link,Images"
  (let* ((raw-link  (org-element-property :raw-link link ))
	 (fname-extension (file-name-extension raw-link))
	 (parent    (org-element-property :parent link))
	 (attr_html (org-element-property :attr_html parent))
	 plist_attr_html
	 width
	 height)

    ;; Available HTML image tags: align border bordercolor hspace vspace width height title alt

    ;; attr_html (":align left :width \"100px\" :height \"100px\" :title \"sqa_verification_sample\"")
    (setq plist_attr_html (read (format "(%s)" (car attr_html))))
    (setq width  (plist-get plist_attr_html :width))
    (setq height (plist-get plist_attr_html :height))

    (if (and fname-extension
	     (string-match "png\\|jpg" fname-extension))
	;; for image tags
	(if attr_html
	    ;; if align=left is specified, next text is align at the right side of image
	    (format "!%s|%s!" raw-link (replace-regexp-in-string " " "=" (replace-regexp-in-string " :" "," (substring (car attr_html) 1))))
	  (format "!%s!" raw-link))
      ;; non image link
      (concat "["
	      (when (org-string-nw-p desc) (format "%s|" desc))
	      (cond
	       ((string-match "^confluence:" raw-link)
		(replace-regexp-in-string "^confluence:" "" raw-link))
	       (t
		raw-link))))))
