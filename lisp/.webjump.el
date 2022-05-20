(use-package webjump
  :custom
  ;; refer webjump-sample-sites variable in webjump.el standrd library
  (webjump-sites
   '(("Google" .
      [simple-query "www.google.com"
		    "www.google.com/search?q=" ""])
     ("Slack emacs-jp" . "https://app.slack.com/client/T1B60DPHV/CMJ3J83QE")
     ("Slack ruby-jp"  . "https://app.slack.com/client/T5LLAJ415/C5LKS0BC6")
     ("Slack seleniumjp" . "https://app.slack.com/client/T04DC36C9/C04GR4YRA")
     ("Keep" . "https://keep.google.com/u/0/#home") ; with http,https are available
     ("Chrome Bookmarks" . "chrome://bookmarks/") ; can't open chrome: style
     ("アプリ" . "chrome://apps/") ; can't open chrome: style
     ))

  :config
  ;; redefine original function to utilize thing-at-point 'word for google
  (defun webjump-read-string (prompt)
    (let ((input (read-string (concat prompt ": ") (thing-at-point 'word))))
      (if (webjump-null-or-blank-string-p input) nil input)))

  (defun google ()
    (interactive)
    (let* ((site '("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""]))
	   (name (car site))
	   (expr (cdr site)))
      (browse-url-default-browser (webjump-url-fix (webjump-builtin expr name)))))
  )

(provide '.webjump)

