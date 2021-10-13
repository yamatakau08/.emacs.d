(use-package my-qiita
  :load-path "my-qiita"
  )

(use-package helm-qiita
  :after
  (my-qiita)
  )

(use-package consult-qiita
  :after
  ;; fail to load consult-qiita-get-pages function just after Emacas launched and consult also not loaded
  ;;(my-qiita consult)
  (my-qiita)
  )

(provide '.my-qiita)
