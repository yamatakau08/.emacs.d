;;; refer https://github.com/blue0513/instant-maximized-window
;;; repository https://github.com/blue0513/instant-maximized-window.git
(add-to-list 'load-path "~/.emacs.d/instant-maximized-window")
(require 'instant-maximized-window)

;; M-x describe-bindings shows the global key mapping
(define-key global-map (kbd "C-i") 'window-temp-maximize)
