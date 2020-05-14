(require 'ox-confluence)

;; without (require 'ox-confluence) , only with-eval-after-load
;; redfined org-confluence-link doesn't effect.
;; need (require 'ox-confluence)
(with-eval-after-load
    (defun org-confluence-link (link desc info)
      "Redfined the original function to export image link !url! not [url]
Note that !url! is only availble cofluence confvert wiki format to html.
Thouhg the all followings Confluence Wiki format are displayed image, the explanation http://external/link.html is not displayed.
!http://orgmode.org/img/org-mode-unicorn-logo.png!
[!http://orgmode.org/img/org-mode-unicorn-logo.png!|]
[!http://orgmode.org/img/org-mode-unicorn-logo.png!|http://external/link.html]
"
      (let* ((raw-link (org-element-property :raw-link link))
	     (fname-extension (file-name-extension raw-link)))
	(if (and fname-extension
		 (string-match "png\\|jpg" fname-extension))
	    (format "!%s!" raw-link)
	  (concat "["
		  (when (org-string-nw-p desc) (format "%s|" desc))
		  (cond
		   ((string-match "^confluence:" raw-link)
		    (replace-regexp-in-string "^confluence:" "" raw-link))
		   (t
		    raw-link))
		  "]"))))
  )

