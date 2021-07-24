(use-package selectrum
  :ensure t

  :custom
  (selectrum-num-candidates-displayed 20)

  :config ; https://libraries.io/emacs/selectrum#getting-started
  (selectrum-mode +1)
  ;;(selectrum-prescient-mode +1)
  ;;(prescient-persist-mode +1)

  ;; for selectrm-swiper
  ;; https://github.com/raxod502/selectrum/wiki/Useful-Commands#swiper-like-jumping-to-matching-lines
  (defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")

  (defun selectrum-swiper ()
    "Search for a matching line and jump to the beginning of its text.
The default candidate is a non-empty line closest to point.
This command obeys narrowing."
    (interactive)
    (let ((selectrum-should-sort-p nil)
          ;; Get the current line number for determining the travel distance.
          (current-line-number (line-number-at-pos (point) t)))
      (cl-destructuring-bind (default-candidate formatted-candidates)
          (cl-loop
           with buffer-lines = (split-string (buffer-string) "\n")
           with number-format = (concat "L%0"
                                        (number-to-string
                                         (length (number-to-string
                                                  (length buffer-lines))))
                                        "d: ")

           with formatted-candidates = nil
           for line-text in buffer-lines
           for line-num = (line-number-at-pos (point-min) t) then (1+ line-num)

           with default-candidate = nil
           with prev-distance-to-default-cand = 1.0e+INF ; This updated later.
           for distance-to-default-cand = (abs (- current-line-number line-num))

           unless (string-empty-p line-text) ; Just skip empty lines.
           do
           ;; Find if we’ve started to move away from the current line.
           (when (null default-candidate)
             (when (> distance-to-default-cand
                      prev-distance-to-default-cand)
               (setq default-candidate (cl-first formatted-candidates)))
             (setq prev-distance-to-default-cand distance-to-default-cand))

           ;; Format current line and collect candidate.
           (push (propertize line-text
                             'selectrum-candidate-display-prefix
                             (propertize (format number-format line-num)
                                         'face 'completions-annotations)
                             'line-num line-num)
                 formatted-candidates)

           finally return (list default-candidate
                                (nreverse formatted-candidates)))
	(let ((chosen-line-number
               (get-text-property
		0 'line-num
		(selectrum-read "Jump to matching line: "
				formatted-candidates
				:default-candidate default-candidate
				:history 'selectrum-swiper-history
				:require-match t
				:no-move-default-candidate t))))
          (push-mark (point) t)
          (forward-line (- chosen-line-number current-line-number))
          (beginning-of-line-text 1)))))
  )
