;;; https://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit#6023
;;; Before v2.4 it was also possible to set the remote branch that you are pushing to as the upstream branch using p-ueREMOTE/BRANCHRET. But now the --set-upstream switch is no longer available in the push popup by default. You can get it back by setting
(setq magit-push-current-set-remote-if-missing nil)

;;; https://magit.vc/manual/magit/Repository-Setup.html
(setq magit-clone-set-remote.pushDefault t)

(defun magit-clone (repository directory)
  (interactive
   (let ((url (magit-read-string-ns "Clone repository" "git@github.com:Username/Repository.git")))
     (list url (read-directory-name
                "Clone to: " nil nil nil
                (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
                     (match-string 1 url)))))
   (funcall 'magit-clone repository directory)))

;;;; following code doesn't work
;(defadvice magit-clone (around magit-clone-repository-template-windows)
;  "set the \"git@github:Username/Repository_name.git\" in prompt"
;  (let ((ad-set-arg 0 "git@github:Username/Repository_name.git"))
;    ad-do-it))
;(ad-activate 'magit-clone)

