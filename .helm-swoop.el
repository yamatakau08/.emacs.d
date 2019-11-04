;; package helm-swoop
;; edit mode have an error
;; don't use package version
(use-package helm-swoop
  :ensure t)

;;;
(when (featurep 'helm-swoop)
  ;; replace C-s/C-r to helm-occur
  ;; firstly I replace C-s/C-r with helm-ag-this-file.
  ;; but it can't be used in buffers e.g. *GNU Emacs* is not file buffers
  ;; helm-ag-this-file: Wrong type argument: stringp, nil
  ;; it's better to use helm-occur in this case.
  (define-key global-map (kbd "C-s") 'helm-occur)
  (define-key global-map (kbd "C-r") 'helm-occur)
  )
