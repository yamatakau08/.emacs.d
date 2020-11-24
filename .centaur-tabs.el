(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-," . centaur-tabs-backward)
  ("C-."  . centaur-tabs-forward)
  )

;; (defvar tabbar-prefix-map
;; (let ((km (make-sparse-keymap)))
;;   (define-key km [(control home)]  'tabbar-press-home)
;;   (define-key km [(control left)]  'tabbar-backward)
;;   (define-key km [(control right)] 'tabbar-forward)
;;   (define-key km [(control up)]    'tabbar-backward-group)
;;   (define-key km [(control down)]  'tabbar-forward-group)
;;   (define-key km [(control prior)] 'tabbar-press-scroll-left)
;;   (define-key km [(control next)]  'tabbar-press-scroll-right)
;;   (define-key km [(control f10)]   'tabbar-local-mode)
;;   km)
;; "The key bindings provided in Tabbar mode.")
