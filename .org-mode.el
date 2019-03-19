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
(global-set-key (kbd "C-c c") 'my-skips-org-file-open)
(defun my-skips-org-file-open ()
  (interactive)
  (let ((skips-org-file (concat "/plink:yama@" elmo-imap4-default-server ":~/org/skips.org")))
    (find-file skips-org-file)))
