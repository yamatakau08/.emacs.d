(use-package my-zenn
  :load-path "my-zenn"
  )

(use-package consult-zenn
  :after
  ;; fail to load consult-zenn-get-pages function just after Emacas launched and consult also not loaded
  ;;(my-zenn consult)
  (my-zenn)
  )

(provide '.my-zenn)
