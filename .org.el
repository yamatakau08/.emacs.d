;;; Since pre-installed org version is 9.1.9,
;;; install org version 9.2.X by package to use some functions provied by its version.
;;; :ensure t
;;; https://kumaroot.readthedocs.io/ja/latest/emacs-use-package.html#id2
;;; Error (use-package): Cannnot load
;;; https://github.com/jwiegley/use-package/issues/597#issuecomment-352898477
(use-package org
  :ensure org-plus-contrib)

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

;; Org-captureを呼び出すキーシーケンス
(define-key global-map "\C-cc" 'org-capture)

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

;;; to Daily report descending when :step day is set on org-clock-reprot
(my-load "~/.emacs.d/ad_org-clock-report.el")

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

;; refer https://orgmode.org/manual/Conflicts.html
;; Make windmove work in Org mode:
;;(add-hook 'org-shiftup-final-hook    'windmove-up   )
;;(add-hook 'org-shiftleft-final-hook  'windmove-left )
;;(add-hook 'org-shiftdown-final-hook  'windmove-down )
;;(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'org-shiftup-hook    'windmove-up   )
(add-hook 'org-shiftleft-hook  'windmove-left )
(add-hook 'org-shiftdown-hook  'windmove-down )
(add-hook 'org-shiftright-hook 'windmove-right)

;;; org day of the week format Japanese to English
;;; https://w.atwiki.jp/opentfc/pages/116.html "日付を挿入"
;(add-hook 'org-mode-hook
;          (lambda ()
;            (set (make-local-variable 'system-time-locale) "C")))
;; https://qiita.com/tadsan/items/9d287a57c26711387043#make-local-variable
;; recomment to use setq-local instead of make-local-variable is compatibility under Emacs 24.3
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local system-time-locale "C")))

;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
;; function jk-org-kwd gets the propertie specifed by args.
;; your original propertiy is also available.
;; To utilize my original #+PAGEID: 123456 property in org file for Confluence page update
;; property should be BIG CHARATER
;; the following function is available in org BUFFER, means it's not available in with-temp-buffer with org-mode.

;; http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun jk-org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key   keyword)
                                           (org-element-property :value keyword)))))

(defun jk-org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (jk-org-kwds))))

;; to open html file in share folder which is exported by org with browser on windows environment
(defun advice:w32-shell-execute-filter-args (args)
  ;; (message "filter-args before: %s" args) ; for debug
  ;;(message "%s" (cadr args))
  (setcar (cdr args) (replace-regexp-in-string "/" "\\\\" (cadr args))) ; pass ("opne" "path is replaced with '/'")
  ;;(setf (cdr args) (replace-regexp-in-string "/" "\\\\" (cadr args))) ; fail ("opne" . "path is replaced with '/'")
  ;; (message "filter-args after: %s" args) ; for debug
  args ; return args processed for w32-shell-execute function to execute
)

(advice-add 'w32-shell-execute
	    :filter-args
            'advice:w32-shell-execute-filter-args)
