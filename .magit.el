;;; https://emacs.stackexchange.com/questions/6021/change-a-branchs-upstream-with-magit#6023
;;; Before v2.4 it was also possible to set the remote branch that you are pushing to as the upstream branch using p-ueREMOTE/BRANCHRET. But now the --set-upstream switch is no longer available in the push popup by default. You can get it back by setting
(setq magit-push-current-set-remote-if-missing nil)

;;; https://magit.vc/manual/magit/Repository-Setup.html
;(setq magit-clone-set-remote.pushDefault t)

(autoload 'magit-read-string-ns "magit-utils.el") ; for the following redefine magit-clone
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

;;; https://qiita.com/onjiro/items/15eda8d539937a9991f5
;;; this setting makes
;;; *ERROR*: Symbol’s function definition is void: nilerror: There was a problem with the editor '"c:/msys64/mingw64/bin/emacsclient.exe"'.
;;; Please supply the message using either -m or -F option.
;(add-hook 'git-commit-mode-hook
;	  (setq auto-fill-mode nil))
; define-minor-mode git-commit-mode により git-commit-mode-hook が自動で作られる効果が発動．

(eval-after-load "magit"
  '(delq 'git-commit-turn-on-auto-fill git-commit-setup-hook))
