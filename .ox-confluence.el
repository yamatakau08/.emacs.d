(with-eval-after-load
    (defun org-confluence-link (link desc info)
      " redfined the original function to export image link !url! not [url]!"
      (let ((raw-link (org-element-property :raw-link link)))
	(if (and (string-match "^http:" raw-link)
		 (string-match "png" (file-name-extension raw-link)))
	    (if (org-string-nw-p desc)
		(format "!%s! %s" raw-link desc)
	      (format "!%s!" raw-link))
	  (concat "["
		  (when (org-string-nw-p desc) (format "%s|" desc))
		  (cond
		   ((string-match "^confluence:" raw-link)
		    (replace-regexp-in-string "^confluence:" "" raw-link))
		   (t
		    raw-link))
		  "]"))))
  )
