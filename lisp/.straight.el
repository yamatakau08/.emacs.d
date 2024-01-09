;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;(straight-use-package 'use-package)

;; to create straight-profiles under ~/.emacs.d/lisp
;; refer https://github.com/radian-software/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases
;; after modified custom-set-variables setting, rm ~/.emacs.d/custom.el then launch emacs.
(custom-set-variables
 '(straight-profiles
   `((nil . ,(concat user-emacs-directory
		     (if (eq system-type 'windows-nt)
			 "lisp/straight-default-windows-nt.el"
		       "lisp/straight-default.el"))))))

(provide '.straight)
