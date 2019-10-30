;;; customize thingatpt.el

;; https://emacs.stackexchange.com/a/33441
(with-eval-after-load "thingatpt"
  ;; add ' ' space, '　' zenkaku space, '\\' for windows file path
  (setq thing-at-point-file-name-chars (concat thing-at-point-file-name-chars " 　\\"))
  )
