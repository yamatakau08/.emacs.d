;;; for w3m
(cond
 ((eq window-system 'w32)
  ;; since the package come with Meadow doesn't work on 23,24, use the latest cvs head
  (add-to-list 'load-path "c:/winbin/meadow/packages/lisp/w3m")
  (add-to-list 'load-path "~/mylisp/emacs-w3m"))
 ((eq window-system 'x)
  (add-to-list 'load-path "~/mylisp/emacs-w3m-1.3.3")))

;(require 'w3m-load)
;(require 'mime-w3m)

;; proxy 
;(setq w3m-command-arguments
;      (nconc w3m-command-arguments
;             '("-o" "http_proxy=http://proxy.hogege.com:8000/")))
;; above setting have error on loading emacs-w3m, use below.
;(setq w3m-command-arguments
;      '("-o" "http_proxy=http://proxy.hogege.com:10080/")))

(setq w3m-command-arguments '("-o" (getenv "http_proxy")))

;; 
(setq w3m-home-page "http://hereri.blog105.fc2.com/")

(setq w3m-default-display-inline-images t)

(setq w3m-search-default-engine "google-ja")
;(setq w3m-weather-default-area "Kanagawa")
