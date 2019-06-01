;; refer https://emacs-jp.slack.com/messages/C6T2T9H4G/
(defun my-nxml-mode-hook ()
  (hs-minor-mode 1)
  (my-sgml-pretty-print))

(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
