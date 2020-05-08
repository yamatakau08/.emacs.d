(require 'ox-confluence)

;; without (require 'ox-confluence) , only with-eval-after-load
;; redfined org-confluence-link doesn't effect.
;; need (require 'ox-confluence)
(with-eval-after-load
    (defun org-confluence-link (link desc info)
      "Redfined the original function to export image link !url! not [url]!"
      (let* ((raw-link (org-element-property :raw-link link))
	     (fname-extension (file-name-extension raw-link)))
	(if fname-extension
	    ;;(setq case-fold-search nil) ;; to png/PNG match
	    (if (and (string-match "^http" raw-link)
		     (string-match "png\\|jpg" fname-extension))
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
		      "]")))))
  )
