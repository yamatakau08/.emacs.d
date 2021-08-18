(use-package my-qiita
  :load-path "my-qiita"
  )

(use-package helm-qiita
  :after
  (my-qiita helm)
  )

(use-package consult-qiita
  :after
  (my-qiita consult)
  )

(provide '.my-qiita)

