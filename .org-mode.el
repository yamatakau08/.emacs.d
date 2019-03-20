;;; org-version changed to Ver. 9.2.1,、"<e TAB" template doesn't work
;;; supported by emacs-jp slack
;;; need to (require 'org-temp)
;;; on Ver. 9.1.9 this cause "Problems while trying to load feature org-tempo"
(if (version< "9.1.9" (org-version))
      (require 'org-tempo)
  (message "update org to 9.2.X"))

(setq org-edit-src-content-indentation 0) ; ソースブロックの中身が右にずらさないように左端にする

;;; to remove "Validate" link at the bottom of exported html
;;; https://stackoverflow.com/questions/15134911/in-org-mode-how-do-i-remove-the-validate-xhtml-1-0-message-from-html-export
(setq org-html-validation-link nil)

;;; http://nobunaga.hatenablog.jp/entry/2015/10/25/161305
;;; より howmモード時に、org-modeを有効にする
;;; auto-mode-alist の設定だけで一旦対応
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$"  . org-mode))

;;; C-c C-x C-d でDONEの時刻を記録
;;; TODOをC-c C-tでDONEした際に、CLOSED: [YYYY-MM-DD...] でも追加される
(setq org-log-done 'time)

;;; 2019/02/19
;;; To suppress Emacs is too slow when opening org-mode file or enable org-mode
;;; on org-plus-contrib-20190218
;;; because org-mode loads org-gnus even though I don't use gnus.... makes movemail
;;; emacs-jp slack teach me how to not to load org-gnus
(with-eval-after-load "org"
  (delq 'org-gnus org-modules))

;;;
;;; to open the org file for skips is man-hour manage
;;; http://www.mhatta.org/wp/category/org-mode/#%E3%83%A1%E3%83%A2%E3%82%92%E5%8F%96%E3%82%8B
;(global-set-key (kbd "C-c c") 'my-skips-org-file-open)
;(defun my-skips-org-file-open ()
;  (interactive)
;  (let ((skips-org-file (concat "/plink:yama@" elmo-imap4-default-server ":~/org/skips.org")))
;    (find-file skips-org-file)))

;; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)
;; Org-captureのテンプレート（メニュー）の設定
(setq my-skips-org-file (concat "/plink:yama@" elmo-imap4-default-server ":~/org/skips.org"))
(setq my-skips-headline-etc "その他")
;; https://orgmode.org/manual/Template-elements.html
;; https://www-he.scphys.kyoto-u.ac.jp/member/shotakaha/dokuwiki/doku.php?id=toolbox:emacs:org:capture:start#%E3%83%86%E3%83%B3%E3%83%97%E3%83%AC%E3%83%BC%E3%83%88%E3%81%AE%E8%A8%AD%E5%AE%9A
;; "e"      :keys
;; "その他" :description
;; entry    :type
;; (file+headline my-skips-org-file "その他") :target
;; "* %?\nEntered on %U\n" :template
;; :clock-in 1 :properties 1などを付ける必要有り
;; org-capture-templates
;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
;; %? org-captureのバッファを開いたときのカーソルの位置
;; \n 改行
;; %U タイムスタンプ
;; %i C-c c を叩いたときに選択されていたリージョンの内容
;; %a C-c cを叩いたときに開いていたファイルへのリンク（本当はorg-store-linkで保存されたリンクだが）
(setq org-capture-templates
      `(("e"
	 "その他"
	 entry
	 (file+headline my-skips-org-file ,my-skips-headline-etc)
	 nil
	 :clock-in 1)
        ))

