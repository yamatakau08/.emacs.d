;;; refer https://ja.stackoverflow.com/questions/31708/%E4%BB%BB%E6%84%8F%E3%81%AE%E3%83%90%E3%83%83%E3%83%95%E3%82%A1%E3%81%AB%E7%A7%BB%E5%8B%95%E3%81%99%E3%82%8B%E6%96%B9%E6%B3%95
;;; モードラインの色を変更する
;(set-face-background 'mode-line "black")
;(set-face-foreground 'mode-line "gray95")

;;; Shift+←→↑↓ でウィンドウ移動
(windmove-default-keybindings) ; shift++←→↑↓ org-mode uses that, use modifier M
;;; refer http://d.hatena.ne.jp/tama_sh/20110206/1296976730
;; (windmove-default-keybindings 'meta)
;;; following error
;;; refer https://orgmode.org/manual/Conflicts.html
;;; windmove
;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook    'windmove-up   )
(add-hook 'org-shiftleft-final-hook  'windmove-left )
(add-hook 'org-shiftdown-final-hook  'windmove-down )
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq windmove-wrap-around t)
