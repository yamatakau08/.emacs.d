;; Note
;; Since byte-compiled helm-jira.elc installed by package-install doesn't require cl,request packages,
;; it doesn't work.
;; Even if require cl and request package from out side of helm-jira package,
;; it doesn't effect and helm-jira.elc is prior to load.
;; So need to delete helm-jira.elc and require cl,request explicitly.

(use-package helm-jira

  ;; use my modified helm-jira
  :straight (helm-jira :type git :flavor melpa :host github :repo "yamatakau08/helm-jira")

  :custom
  (helm-jira-url       company-jira-url) ; URL of your JIRA instance (should not end in a slash)
  (helm-jira-username  "0000910700")     ; The username to use to log in to JIRA
  (helm-jira-project   "bisyamon3g")     ; The JIRA-project you want to interact with

  ;; the following are not used for my environment
  (helm-jira-board-id  153) ; The ID of the board you want to interact with
  (helm-jira-stash-url "https://src.yourcompany.com") ; URL of the stash/bitbucket API (should not end in a slash)
  (helm-jira-repo      "myRepo") ; The stash/bitbucket repo you want to interact with

  :config
  ;; see the top Note
  ;; (require 'cl)
  ;; (require 'request) ; to suppress "Symbol’s function definition is void: request"
  ;; finally add the above in helm-jira.el

  ;;(define-key dired-mode-map "b" #'helm-jira-add-atachment) ; movie :bind part

  :demand t
  ;; from https://github.com/jwiegley/use-package#notes-about-lazy-loading
  ;; Since :bind block doesn't require the package and it's impossible to call other interactive functions,
  ;; Need to specify :demand t

  :bind (:map dired-mode-map
              ("b" . helm-jira-add-attachment))
  )

(provide '.helm-jira)

