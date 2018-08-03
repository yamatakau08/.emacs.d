;;; https://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit#6023
;;; Before v2.4 it was also possible to set the remote branch that you are pushing to as the upstream branch using p-ueREMOTE/BRANCHRET. But now the --set-upstream switch is no longer available in the push popup by default. You can get it back by setting
(setq magit-push-current-set-remote-if-missing nil)

;;; https://magit.vc/manual/magit/Repository-Setup.html
(setq magit-clone-set-remote.pushDefault t)

;;; Windows時の条件を入れる
(eval-after-load "magit"
  '(progn
     (message "[INFO] magit loaded!")
     ))

;;;; following code doesn't work
;(defadvice magit-clone (around magit-clone-repository-template-windows)
;  "set the \"git@github:Username/Repository_name.git\" in prompt"
;  (let ((ad-set-arg 0 "git@github:Username/Repository_name.git"))
;    ad-do-it))
;(ad-activate 'magit-clone)

(when (eq window-system 'w32)
  (autoload 'magit-read-string-ns "magit-utils.el") ; for magit-clone
  (defun magit-clone (repository directory)         ; overwrite magit-clone
    "Clone the REPOSITORY to DIRECTORY.
Then show the status buffer for the new repository."
    (interactive
     (let  ((url (magit-read-string-ns "Clone repository" "git@github.com:Username/Repository.git")))
       (list url (read-directory-name
                  "Clone to: " nil nil nil
                  (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
		       (match-string 1 url))))))
    (setq directory (file-name-as-directory (expand-file-name directory)))
    (magit-run-git-async "clone" repository
			 (magit-convert-filename-for-git directory))
    ;; Don't refresh the buffer we're calling from.
    (process-put magit-this-process 'inhibit-refresh t)
    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
	 (let ((magit-process-raise-error t))
           (magit-process-sentinel process event)))
       (when (and (eq (process-status process) 'exit)
                  (= (process-exit-status process) 0))
	 (let ((default-directory directory))
           (when (or (eq  magit-clone-set-remote.pushDefault t)
		     (and magit-clone-set-remote.pushDefault
                          (y-or-n-p "Set `remote.pushDefault' to \"origin\"? ")))
	     (setf (magit-get "remote.pushDefault") "origin"))
           (unless magit-clone-set-remote-head
	     (magit-remote-unset-head "origin")))
	 (with-current-buffer (process-get process 'command-buf)
           (magit-status-internal directory))))))
)

