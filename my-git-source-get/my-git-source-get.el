(defun my-git-source-get (repository &optional destdir)
  "get source from arg specified git repsoitry url into ~/.emacs.d/repository-name/"
  (interactive)
  (let* ((repository-name (file-name-base repository))
	 (ib-directory (expand-file-name "~/.emacs.d")) ; ib-directory install base directory
	 ;; https://qiita.com/tadsan/items/17d32514b81f1e8f208a#%E3%81%AE%E5%B1%95%E9%96%8B
	 (destdir (or destdir (expand-file-name (concat ib-directory "/" repository-name)))))
    (if (file-directory-p destdir)
	;; https://qiita.com/tadsan/items/17d32514b81f1e8f208a#default-directory%E3%81%AB%E6%B0%97%E3%82%92%E9%85%8D%E3%82%8B
	(progn
	  ;; (message "default-dirrectory:%s, directory:%s exists" default-directory destdir)
	  (message "pulling %s into %s" repository-name destdir)
	  (shell-command-to-string
	   (mapconcat #'shell-quote-argument
		      (list "git" "pull")
		      " "))
	  )
      ;; else
      (message "cloning %s into %s" repository-name destdir)
      (shell-command-to-string
       (mapconcat #'shell-quote-argument
		  (list "git" "clone" repository destdir)
		  " "))
      (message "done cloning %s into %s" repository-name destdir)
      )))

(provide 'my-git-source-get)
