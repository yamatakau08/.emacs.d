(use-package tab-bar
  :bind (("<C-tab>" . tab-bar-switch-to-next-tab)
	 ("<C-S-tab>" . tab-bar-switch-to-prev-tab) ; hit shift,then Ctrl-tab
	 ;; it's better than < ,same position without shift
	 ("C-," . tab-bar-switch-to-next-tab) ; conflict org-mode buffer
	 ("C-." . tab-bar-switch-to-prev-tab))
  )

(defun xswitch-to-buffer (buffer-or-name &optional norecord force-same-window)
  "Display buffer BUFFER-OR-NAME in the selected window.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids
messing with the window-buffer correspondences.

If the selected window cannot display the specified buffer
because it is a minibuffer window or strongly dedicated to
another buffer, call `pop-to-buffer' to select the buffer in
another window.  In interactive use, if the selected window is
strongly dedicated to its buffer, the value of the option
`switch-to-buffer-in-dedicated-window' specifies how to proceed.

If called interactively, read the buffer name using `read-buffer'.
The variable `confirm-nonexistent-file-or-buffer' determines
whether to request confirmation before creating a new buffer.
See `read-buffer' for features related to input and completion
of buffer names.

BUFFER-OR-NAME may be a buffer, a string (a buffer name), or nil.
If BUFFER-OR-NAME is a string that does not identify an existing
buffer, create a buffer with that name.  If BUFFER-OR-NAME is
nil, switch to the buffer returned by `other-buffer'.

If optional argument NORECORD is non-nil, do not put the buffer
at the front of the buffer list, and do not make the window
displaying it the most recently selected one.

If optional argument FORCE-SAME-WINDOW is non-nil, the buffer
must be displayed in the selected window when called
non-interactively; if that is impossible, signal an error rather
than calling `pop-to-buffer'.  It has no effect when the option
`switch-to-buffer-obey-display-actions' is non-nil.

The option `switch-to-buffer-preserve-window-point' can be used
to make the buffer appear at its last position in the selected
window.

If the option `switch-to-buffer-obey-display-actions' is non-nil,
run the function `pop-to-buffer-same-window' instead.
This may display the buffer in another window as specified by
`display-buffer-overriding-action', `display-buffer-alist' and
other display related variables.  If this results in displaying
the buffer in the selected window, window start and point are adjusted
as prescribed by the option `switch-to-buffer-preserve-window-point'.
Otherwise, these are left alone.

Return the buffer switched to."
  (interactive
   (let ((force-same-window
          (unless switch-to-buffer-obey-display-actions
            (cond
             ((window-minibuffer-p) nil)
             ((not (eq (window-dedicated-p) t)) 'force-same-window)
             ((pcase switch-to-buffer-in-dedicated-window
                ('nil (user-error
                       "Cannot switch buffers in a dedicated window"))
                ('prompt
                 (if (y-or-n-p
                      (format "Window is dedicated to %s; undedicate it"
                              (window-buffer)))
                     (progn
                       (set-window-dedicated-p nil nil)
                       'force-same-window)
                   (user-error
                    "Cannot switch buffers in a dedicated window")))
                ('pop nil)
                (_ (set-window-dedicated-p nil nil) 'force-same-window)))))))
     (list (read-buffer-to-switch "Switch to buffer: ") nil force-same-window)))
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name))
        (set-window-start-and-point (not switch-to-buffer-obey-display-actions)))
    (cond
     ;; Don't call set-window-buffer if it's not needed since it
     ;; might signal an error (e.g. if the window is dedicated).
     ((and (eq buffer (window-buffer))
	   (message "[debug] switch-to-buff 1 %s" buffer-or-name)
           ;; pop-to-buffer-same-window might decide to display
           ;; the same buffer in another window
	   (unless (tab-bar-get-buffer-tab buffer-or-name)
             (not switch-to-buffer-obey-display-actions))
	   ))
     ((and (window-minibuffer-p)
           (not switch-to-buffer-obey-display-actions))
      (message "[debug] switch-to-buff 2 %s" buffer-or-name)
      (if force-same-window
          (user-error "Cannot switch buffers in minibuffer window")
        (pop-to-buffer buffer norecord)))
     ((and (eq (window-dedicated-p) t)
           (not switch-to-buffer-obey-display-actions))
      (message "[debug] switch-to-buff 3 %s" buffer-or-name)
      (if force-same-window
          (user-error "Cannot switch buffers in a dedicated window")
        (pop-to-buffer buffer norecord)))
     (t
      (message "[debug] switch-to-buff 4 %s" buffer-or-name)
      (when switch-to-buffer-obey-display-actions
	(message "[debug] switch-to-buff 5 %s" buffer-or-name)
        (let ((selected-window (selected-window)))
          (pop-to-buffer-same-window buffer norecord)
          (when (eq (selected-window) selected-window)
            (setq set-window-start-and-point t))))

      (when set-window-start-and-point
	(message "[debug] switch-to-buff 6 %s" buffer-or-name)
        (let* ((entry (assq buffer (window-prev-buffers)))
	       (displayed (and (eq switch-to-buffer-preserve-window-point
				   'already-displayed)
			       (get-buffer-window buffer 0))))
	  (set-window-buffer nil buffer)
	  (when (and entry
		     (or (eq switch-to-buffer-preserve-window-point t)
		         displayed))
	    (message "[debug] switch-to-buff 7 %s" buffer-or-name)
	    ;; Try to restore start and point of buffer in the selected
	    ;; window (Bug#4041).
	    (set-window-start (selected-window) (nth 1 entry) t)
	    (set-window-point nil (nth 2 entry)))))))

    (unless norecord
      (message "[debug] switch-to-buff 8 %s" buffer-or-name)
      (if (tab-bar-get-buffer-tab buffer-or-name)
	  (progn
	    (message "[debug] switch-to-buff 9 %s" buffer-or-name)
	    (tab-bar-switch-to-tab buffer-or-name))
      (select-window (selected-window))))

    (message "[debug] switch-to-buff 10 %s" buffer)
    (set-buffer buffer)
    ))

;      (if (tab-bar-get-buffer-tab buffer-or-name)
;	  (progn
;	    (message "[debug] switch-to-buff 9 %s" buffer-or-name)
;	    (tab-bar-switch-to-tab buffer-or-name))
;	(select-window (selected-window))))
;
;    (unless (tab-bar-get-buffer-tab buffer-or-name)
;      (message "[debug] switch-to-buff 10 %s" buffer-or-name)
;	(set-buffer buffer))
