;;; customize thingatpt.el

;; https://emacs.stackexchange.com/a/33441
(eval-after-load "thingatpt"
  (setq thing-at-point-file-name-chars (concat thing-at-point-file-name-chars "　\\")) ; add '\\' zenkaku space for windows file
  )
