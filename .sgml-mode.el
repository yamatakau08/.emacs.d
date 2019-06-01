;; refer https://emacs.stackexchange.com/a/31990
(defun my-sgml-pretty-print ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    ;; (indent-region (point-min) (point-max)
    ))

