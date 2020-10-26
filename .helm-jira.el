;; Note
;; Since helm-jira.elc is byte-compiled when install helm-jira with package-install doesn't require cl,request packages,
;; need to delete helm-jira.elc
;; Even if require cl and request package from out side of helm-jira package, it doesn't effect and helm-jira.elc is prior to load.

(use-package helm-jira

  ;; since original helm-jira function* is defined in cl.el which is in obsolete directory on Emacs 28.0.50
  ;;:load-path "c:/winbin/emacs-28.0.50/x86_64/share/emacs/28.0.50/lisp/obsolete/"

  :load-path "helm-jira" ; load forked my helm-jira under ~/.emacs.d

  :custom
  (helm-jira-url       company-jira-url) ; URL of your JIRA instance (should not end in a slash)
  (helm-jira-username  "0000910700") ; The username to use to log in to JIRA
  (helm-jira-project   "BISYAMON3G") ; The JIRA-project you want to interact with

  ;; the following are not used for my environment
  (helm-jira-board-id  153) ; The ID of the board you want to interact with
  (helm-jira-stash-url "https://src.yourcompany.com") ; URL of the stash/bitbucket API (should not end in a slash)
  (helm-jira-repo      "myRepo") ; The stash/bitbucket repo you want to interact with

  :config
  ;; see the top Note
  ;; (require 'cl)
  ;; (require 'request) ; to suppress "Symbol’s function definition is void: request"
  (define-key dired-mode-map "b" #'helm-jira-add-atachment)

  ;; from https://github.com/jwiegley/use-package#notes-about-lazy-loading
  ;; Since :bind block doesn't require package, it's not possible to call other interactive functions which have (interactive)
  ;; :bind (:map dired-mode-map
  ;;            ("b" . helm-jira-add-atachment))

)
