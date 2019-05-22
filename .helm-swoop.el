;;; package helm-swoop
;;; edit mode have an error
;;; don't use package version
;;(use-package helm-swoop
;;  :ensure t)

;;; use helm-swoop patch appiled
;;; https://github.com/ashiklom/helm-swoop.git
;;; https://github.com/pkryger/helm-swoop/tree/patches
(add-to-list 'load-path "~/.emacs.d/helm-swoop")

;;; When use C-c C-e (helm-swoop-edit), unfortunately faced the following error
;;; Debugger entered--Lisp error: (cl-assertion-failed ((or (eq action actions) (byte-code-function-p action)...
;;; I give up to use helm-swoop

;;; https://github.com/ShingoFukuyama/helm-swoop/issues/133
;;; doesn't resolve helm-swoop-edit error
(setq actions 'helm-swoop--edit)
