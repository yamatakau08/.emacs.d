;;; Since pre-installed org version is 9.1.9,
;;; install org version 9.2.X by package to use some functions provided by its version.

;; to Daily report descending when :step day is set on org-clock-reprot
(require 'ad_org-clock-report)

(use-package org
  ;; :ensure t
  ;; https://kumaroot.readthedocs.io/ja/latest/emacs-use-package.html#id2
  ;; Error (use-package): Cannnot load
  ;; https://github.com/jwiegley/use-package/issues/597#issuecomment-352898477
  ;;:ensure org-plus-contrib
  ;; After Emacs laaunch on clean, the org mode built in shows "IMPORTANT: please install Org from GNU ELPA as Org ELPA will close before Org 9.6"
  ;; to suppress it, use straight and download org from GNU ELPA.
  ;; unfortunately straight can't download org-plus-contrib,
  ;; refer the workaround https://github.com/radian-software/straight.el/issues/753#issuecomment-832553456
  :straight org-contrib

  :hook
  ;; https://github.com/jwiegley/use-package#hooks
  ;; When using :hook omit the "-hook" suffix if you specify the hook explicitly, as this is appended by default.

  ;; refer https://orgmode.org/manual/Conflicts.html
  ;; ((org-shiftup-final    . windmove-up   )
  ;;  (org-shiftleft-final  . windmove-left )
  ;;  (org-shiftdown-final  . windmove-down )
  ;;  (org-shiftright-final . windmove-right))

  ;; the following also works
  ((org-shiftup    . windmove-up   )
   (org-shiftleft  . windmove-left )
   (org-shiftdown  . windmove-down )
   (org-shiftright . windmove-right))

  :init
  ;; org-version changed to Ver. 9.2.1,、"<e TAB" template doesn't work
  ;; supported by emacs-jp slack
  ;; need to (require 'org-temp)
  ;; on Ver. 9.1.9 this cause "Problems while trying to load feature org-tempo"
  (if (version< "9.1.9" (org-version))
      (require 'org-tempo)
    (message "update org to 9.2.X"))

  ;; org day of the week format Japanese to English
  ;; https://w.atwiki.jp/opentfc/pages/116.html "日付を挿入"
  ;;(add-hook 'org-mode-hook
  ;;          (lambda ()
  ;;            (set (make-local-variable 'system-time-locale) "C")))
  ;; https://qiita.com/tadsan/items/9d287a57c26711387043#make-local-variable
  ;; recomment to use setq-local instead of make-local-variable is compatibility under Emacs 24.3
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local system-time-locale "C")))

  :custom
  ;; add "INPROGRESS" in to-do-keywordsin
  ;; refer http://aaronbedra.com/emacs.d/#org29f8f0d Org Settings
  (org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "DONE(d)")))

  ;; align source block at the left, original 2 space added as indent
  (org-edit-src-content-indentation 0) ; is defined org-src.el

  ;; C-c C-x C-d で DONE の時刻 CLOSED: [YYYY-MM-DD...] を記録
  ;; TODO を C-c C-t で DONEに変更した際にも追加される
  (org-log-done 'time)

  :config
  ;; 2019/02/19
  ;; To suppress Emacs is too slow when opening org-mode file or enable org-mode
  ;; on org-plus-contrib-20190218
  ;; because org-mode loads org-gnus even though I don't use gnus.... makes movemail
  ;; emacs-jp slack teach me how to not to load org-gnus
  (delq 'org-gnus org-modules)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (ruby . t)))

  :mode
  ;; http://nobunaga.hatenablog.jp/entry/2015/10/25/161305
  ;; "設定" より file suffix が howm の時、org-mode 有効
  ;; auto-mode-alist 設定だけで一旦対応
  ;;(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.txt$"  . org-mode))
  ("\\.howm\\'" . org-mode)
  ("\\.txt\\'"  . org-mode)

  ;; :bind*
  ;; (("C-c c" . 'org-capture))
  )

;;; to suppress not to put the annotation whichi is link text in org capture buffer
;;; without this settings, org-capture-templates template propertie "** %?" behave the same.
;(setq org-capture-link-is-already-stored t)

;;; C-c C-x C-j (org-clock-goto) 8.4.1 Clocking commands in https://orgmode.org/org.pdf
;;; Jump to the headline of the currently clocked in task. With a C-u prefix
;;; argument, select the target task from a list of recently clocked task
;;; Since I didn't know the func org-clock-goto, I made the following function.
(defun my-org-clock-in-last ()
  "open org file and go to the headline is in clock-in status"
  (interactive)
  (if org-clock-history
      (progn
	(find-file my-skips-org-file)
	(goto-char (car org-clock-history)))))

;;; redefine tempo-insert to suppress autoindent the following
;;; * test
;;; <eTAB
;;;   #+begin_example
;;;   #+end_example
;;;
;;; modify point of function tempo-insert
;;; original: ((eq element '>) (indent-according-to-mode))
;;; modified: ((eq element '>) (ignore))
(with-eval-after-load "org-tempo"
  (defun tempo-insert (element on-region)
    "Insert a template element.
Insert one element from a template. If ON-REGION is non-nil the `r'
elements are replaced with the current region.

See documentation for `tempo-define-template' for the kind of elements
possible."
    (cond ((stringp element) (tempo-process-and-insert-string element))
	  ((and (consp element)
		(eq (car element) 'p)) (tempo-insert-prompt-compat
					(cdr element)))
	  ((and (consp element)
		(eq (car element) 'P)) (let ((tempo-interactive t))
					 (tempo-insert-prompt-compat
					  (cdr element))))
;;;	((and (consp element)
;;;	      (eq (car element) 'v)) (tempo-save-named
;;;				      (nth 1 element)
;;;				      nil
;;;				      (nth 2 element)))
	  ((and (consp element)
		(eq (car element) 'r)) (if on-region
					 (goto-char tempo-region-stop)
					 (tempo-insert-prompt-compat
					  (cdr element))))
          ((and (consp element)
		(eq (car element) 'r>)) (if on-region
                                            (progn
                                              (goto-char tempo-region-stop)
                                              (indent-region (mark) (point) nil))
                                          (tempo-insert-prompt-compat
                                           (cdr element))))
	  ((and (consp element)
		(eq (car element) 's)) (tempo-insert-named (car (cdr element))))
	  ((and (consp element)
		(eq (car element) 'l)) (mapcar (function
						(lambda (elt)
						  (tempo-insert elt on-region)))
					       (cdr element)))
	  ((eq element 'p) (tempo-insert-mark (point-marker)))
	  ((eq element 'r) (if on-region
			       (goto-char tempo-region-stop)
			     (tempo-insert-mark (point-marker))))
	  ((eq element 'r>) (if on-region
				(progn
				  (goto-char tempo-region-stop)
				  (indent-region (mark) (point) nil))
			      (tempo-insert-mark (point-marker))))
	  ((eq element '>) (ignore))
	  ((eq element '&) (if (not (or (= (current-column) 0)
					(save-excursion
					  (re-search-backward
					   "^\\s-*\\=" nil t))))
			       (insert "\n")))
	  ((eq element '%) (if (not (or (eolp)
					(save-excursion
					  (re-search-forward
					   "\\=\\s-*$" nil t))))
			       (insert "\n")))
	  ((eq element 'n) (insert "\n"))
	  ((eq element 'n>) (insert "\n") (indent-according-to-mode))
	  ;; Bug: If the 'o is the first element in a template, strange
	  ;; things can happen when the template is inserted at the
	  ;; beginning of a line.
	  ((eq element 'o) (if (not (or on-region
					(eolp)
					(save-excursion
					  (re-search-forward
					   "\\=\\s-*$" nil t))))
			       (open-line 1)))
	  ((null element))
	  (t (tempo-insert (or (tempo-is-user-element element)
			       (eval element))
			   on-region)))))

;;; smart expression on the above by @takaxp #org-mode emacs-jp slack
;(with-eval-after-load "org-tempo"
;  ;; (defun ad:org-tempo-before-complete-tag (&rest _)
;  ;; (advice-add 'indent-according-to-mode :override #'ignore))
;  (defun ad:org-tempo-before-complete-tag (&rest _)
;    (save-excursion
;      (beginning-of-line)
;      (when (looking-at "<")
;        (advice-add 'indent-according-to-mode :override #'ignore))))
;  (defun ad:org-tempo-after-complete-tag (&rest _)
;    (advice-remove 'indent-according-to-mode #'ignore))
;  (advice-add 'org-tempo-complete-tag
;              :before #'ad:org-tempo-before-complete-tag)
;  (advice-add 'org-tempo-complete-tag
;              :after #'ad:org-tempo-after-complete-tag))

;;; smart expression on the above by @conao3 #org-mode emacs-jp slack
;(with-eval-after-load "org-tempo"
;    (defun ad:org-tempo-around (fn &rest args)
;      (if (save-excursion (beginning-of-line) (looking-at "<"))
;          (flet ((indent-according-to-mode () #'ignore))
;            (funcall fn args))
;        (funcall fn args))))
;
;(advice-add 'org-tempo-complete-tag :around #'ad:org-tempo-around)
;;;(advice-remove 'org-tempo-complete-tag #'ad:org-tempo-around)

(provide '.org)
