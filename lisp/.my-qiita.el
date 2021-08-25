(use-package my-qiita
  :load-path "my-qiita"
  )

(use-package helm-qiita
  :after
  (my-qiita helm)
  )

(use-package consult-qiita
  :init
  (defun tako ()
    (message "tako"))

  :after
  (my-qiita consult)

  :bind
  (:map minibuffer-local-map
        ("C-c c" . 'my-qiita--action-open-page)  ; fail, argument
        ("C-c d" . 'xmy-qiita--action-open-page) ; pass, no argument
	)
  )

(provide '.my-qiita)
