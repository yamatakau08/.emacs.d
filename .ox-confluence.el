(defun org-confluence-link (link desc info)
  "Redfined the original function to export image link !url! not [url]
https://support.atlassian.com/confluence-cloud/docs/insert-confluence-wiki-markup/
See Link,Images"
  (let* ((raw-link  (org-element-property :raw-link link))
	 (fname-extension (file-name-extension raw-link))
	 (parent    (org-element-property :parent link))
	 (attr_html (org-element-property :attr_html parent))
	 plist_attr_html
	 width
	 height
	 tags)

    ;; attr_html (":align left :width \"100px\" :height \"100px\" :title \"sqa_verification_sample\"")
    (setq plist_attr_html (read (format "(%s)" (car attr_html))))
    (setq width  (plist-get plist_attr_html :width))
    (setq height (plist-get plist_attr_html :height))

    (if (and fname-extension
	     (string-match "png\\|jpg" fname-extension))
	;; image
	(progn
	  (if width
	      (if height
		  (setq tags (format "width=%s,height=%s" width height))
		(setq tags (format "width=%s" width)))
	    (if height
		(setq tags (format "height=%s" height))))
	  (if tags
	      (format "!%s|%s!" raw-link tags)
	    (format "!%s!" raw-link)))
	  ;; link
      (concat "["
	      (when (org-string-nw-p desc) (format "%s|" desc))
	      (cond
	       ((string-match "^confluence:" raw-link)
		(replace-regexp-in-string "^confluence:" "" raw-link))
	       (t
		raw-link))
	      "]"))))
