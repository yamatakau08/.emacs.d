;; refer https://emacs-jp.slack.com/messages/C6T2T9H4G/
;;(add-hook 'nxml-mode-hook (lambda() (hs-minor-mode 1))) ; move to .nxml-mode.el

(add-to-list 'hs-special-modes-alist
             '(nxml-mode
               "<!--\\|<[^/>]*[^/]>" ;; regexp for start block
               "-->\\|</[^/>]*[^/]>" ;; regexp for end block
               "<!--"
               nxml-forward-element
               nil))
