;;; git https://github.com/blue0513/instant-maximized-window.git
(add-to-list 'load-path "~/.emacs.d/my-git-source-get")
(require 'my-git-source-get)
(my-git-source-get "https://github.com/blue0513/instant-maximized-window.git")

;;; refer https://github.com/blue0513/instant-maximized-window
(add-to-list 'load-path "~/.emacs.d/instant-maximized-window")
(require 'instant-maximized-window)

;; M-x describe-bindings shows the global key mapping
;;(define-key global-map (kbd "C-i") 'window-temp-maximize)
