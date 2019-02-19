;;; org-version changed to Ver. 9.2.1,、"<e TAB" template doesn't work
;;; supported by emacs-jp slack
(when (version< "9.1.4" (org-version))
      (add-to-list 'org-modules 'org-tempo))

(setq org-edit-src-content-indentation 0) ; ソースブロックの中身が右にずらさないように左端にする

;;; to remove "Validate" link at the bottom of exported html
;;; https://stackoverflow.com/questions/15134911/in-org-mode-how-do-i-remove-the-validate-xhtml-1-0-message-from-html-export
(setq org-html-validation-link nil)

;;; http://nobunaga.hatenablog.jp/entry/2015/10/25/161305
;;; より howmモード時に、org-modeを有効にする
;;; auto-mode-alist の設定だけで一旦対応
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$"  . org-mode))

;;; DONEの時刻を記録 
;;; TODOをC-c C-tでDONEした際に、CLOSED: [YYYY-MM-DD...] が追加される
;(setq org-log-done 'time) ; C-c C-x C-i(IN)後、C-c C-tで、DONEした際に、:LOGBOOK:.. END内に終了時間が自動でつくので不要 

;;; 2019/02/19
;;; To suppress Emacs is too slow when opening org-mode file or enable org-mode
;;; on org-plus-contrib-20190218
;;; because org-mode loads org-gnus even though I don't use gnus.... makes movemail
;;; emacs-jp slack teach me how to not to load org-gnus
(with-eval-after-load "org"
  (delq 'org-gnus org-modules))
