;;; font

;; ricty (diminished)
;; refer the folloing page install
;; on mac
;; https://hajipro.com/local-development-environment-mac/ricty-diminished
;; on windows
;; http://vdeep.net/ricty-font#WindowsRicty
(cond
 ((eq system-type 'windows-nt)
  ; http://www.techscore.com/blog/2016/12/25/emacs-%E3%81%93%E3%81%A8%E3%81%AF%E3%81%98%E3%82%81-%E7%A7%81%E3%81%AE%E4%BA%8B%E4%BE%8B/
  ; "フォントを統一する" の項参照
  (add-to-list 'default-frame-alist '(font . "Ricty Diminished-12")))
  ((eq system-type 'darwin)
   ; (add-to-list 'default-frame-alist '(font . "ricty-15"))) ; フォント名 ricty でも問題ないようだが、windows版と併せておく
   (when (member "Ricty Diminished" (font-family-list))
     (add-to-list 'default-frame-alist '(font . "Ricty Diminished-15"))))
   ;(add-to-list 'default-frame-alist '(font . "Menlo-16"))
  (t (message "defautl font"))
)
