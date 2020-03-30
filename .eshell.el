;; https://myhobby20xx.hatenadiary.org/entry/20110309/1299641425

;; to avoid garbled in eshell mode
(add-hook 'shell-mode-hook
          (lambda ()
            (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
            ))
