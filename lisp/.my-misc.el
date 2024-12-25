(defun my-get-file-list-under-current-directory ()
  ;;
  ;; refer https://stackoverflow.com/questions/17164767/emacs-lisp-concise-way-to-get-directory-files-without-and
  (interactive)
  ;;(directory-files (file-name-parent-directory (buffer-file-name)) nil "[^.][^. ]*") ; supported by ellama
  (directory-files (file-name-parent-directory (buffer-file-name)) nil "[^.][^.]*") ; supported by ellama and modified
  )

(defun my-insert-file-list ()
  (interactive)
  (insert
   (mapconcat #'identity (my-get-file-list-under-current-directory) "\n")))

(provide '.my-misc)
