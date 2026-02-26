;;; consult-gtasks.el --- Consult integration for Google Tasks -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (consult "0.35") (gtasks "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourname/consult-gtasks

;;; Commentary:
;;
;; consult-gtasks provides a `consult' interface for Google Tasks via gtasks.el.
;;
;; Features:
;;   - `consult-gtasks-tasks'     : Browse/search tasks in a selected task list
;;   - `consult-gtasks-add-task'  : Quickly add a new task to a selected list
;;   - `consult-gtasks-complete'  : Mark a task as completed
;;   - `consult-gtasks-delete'    : Delete a task
;;
;; Setup:
;;   (require 'consult-gtasks)
;;   (global-set-key (kbd "C-c g t") #'consult-gtasks-tasks)
;;   (global-set-key (kbd "C-c g a") #'consult-gtasks-add-task)

;;; Code:

(require 'consult)
(require 'gtasks)

;;; --------------------------------------------------------------------------
;;; Internal helpers
;;; --------------------------------------------------------------------------

(defun consult-gtasks--get-lists ()
  "Return a list of (TITLE . ID) pairs for all Google Task lists."
  (mapcar (lambda (lst)
            (cons (plist-get lst :title)
                  (plist-get lst :id)))
          (plist-get (gtasks-list-list) :items)))

(defun consult-gtasks--select-list ()
  "Prompt the user to select a Google Task list.
Returns a cons cell (TITLE . ID)."
  (let* ((lists (consult-gtasks--get-lists))
         (titles (mapcar #'car lists))
         (chosen (completing-read "Task list: " titles nil t)))
    (assoc chosen lists)))

(defun consult-gtasks--sort-tasks (tasks)
  "Re-order TASKS so each subtask immediately follows its parent.
Within the same parent, tasks are sorted by :position (lexicographic).
TASKS is a flat list of plists as returned by the Google Tasks API."
  (let* (;; id -> plist lookup
         (by-id (let ((tbl (make-hash-table :test #'equal)))
                  (dolist (task tasks)
                    (puthash (plist-get task :id) task tbl))
                  tbl))
         ;; parent-id -> sorted list of child plists
         (children (let ((tbl (make-hash-table :test #'equal)))
                     (dolist (task tasks)
                       (let ((parent (plist-get task :parent)))
                         (when parent
                           (puthash parent
                                    (cons task (gethash parent tbl))
                                    tbl))))
                     ;; sort each child list by :position
                     (maphash (lambda (k v)
                                (puthash k
                                         (sort v (lambda (a b)
                                                   (string< (or (plist-get a :position) "")
                                                            (or (plist-get b :position) ""))))
                                         tbl))
                              tbl)
                     tbl))
         ;; top-level tasks sorted by :position
         (roots (sort (seq-filter (lambda (t) (not (plist-get t :parent))) tasks)
                      (lambda (a b)
                        (string< (or (plist-get a :position) "")
                                 (or (plist-get b :position) "")))))
         (result '()))
    ;; depth-first walk: root -> its children -> their children ...
    (cl-labels ((walk (task)
                  (push task result)
                  (dolist (child (gethash (plist-get task :id) children))
                    (walk child))))
      (dolist (root roots)
        (walk root)))
    (nreverse result)))

(defun consult-gtasks--get-tasks (list-id)
  "Return a list of task plists for LIST-ID, including completed tasks,
sorted so subtasks follow their parent."
  (let* ((items (plist-get (gtasks-task-list list-id
                                        t   ; show-completed
                                        t   ; show-deleted
                                        t)  ; show-hidden
                              :items))
         (valid (seq-filter
                 (lambda (task)
                   (let ((title (plist-get task :title)))
                     (and (stringp title) (not (string-empty-p title)))))
                 (or items '()))))
    (consult-gtasks--sort-tasks valid)))

(defun consult-gtasks--task-annotation (task)
  "Return an annotation string for TASK plist."
  (let ((notes (plist-get task :notes))
        (due   (plist-get task :due)))
    (concat
     (when due   (propertize (format " %s" (substring due 0 10))
                             'face 'consult-key))
     (when notes (propertize (format " %s" (truncate-string-to-width notes 40 nil nil "..."))
                             'face 'shadow)))))

(defun consult-gtasks--tasks-to-candidates (tasks)
  "Convert TASKS plists into consult candidate strings with text properties.
Each candidate is prefixed with a status marker:
  ' ' = needsAction (incomplete)
  'C' = completed
  'D' = deleted
Subtasks are additionally prefixed with \"↳ \"."
  (mapcar (lambda (task)
            (let* ((status (plist-get task :status))
                   (deleted (plist-get task :deleted))
                   (marker (cond (deleted                          "D ")
                                 ((equal status "completed")       "C ")
                                 (t                                "  ")))
                   (title  (or (plist-get task :title) "(no title)"))
                   (indent (if (plist-get task :parent) "↳ " ""))
                   (cand   (concat marker indent title)))
              (propertize cand 'consult-gtasks-task task)))
          tasks))

;;; --------------------------------------------------------------------------
;;; Interactive commands
;;; --------------------------------------------------------------------------

;;;###autoload
(defun consult-gtasks-all-tasks ()
  "Browse and act on tasks across ALL Google Task lists.

Each candidate is prefixed with the task list name so you can tell
which list a task belongs to.  Selecting a task offers the same
v/c/d/q actions as `consult-gtasks-tasks'."
  (interactive)
  (let* ((lists (consult-gtasks--get-lists))
         (cands
          (mapcan (lambda (list-pair)
                    (let* ((list-title (car list-pair))
                           (list-id    (cdr list-pair))
                           (tasks      (consult-gtasks--get-tasks list-id)))
                      (mapcar (lambda (task)
                                (let* ((status  (plist-get task :status))
                                       (deleted (plist-get task :deleted))
                                       (marker  (cond (deleted                    "D ")
                                                      ((equal status "completed") "C ")
                                                      (t                          "  ")))
                                       (title  (or (plist-get task :title) "(no title)"))
                                       (indent (if (plist-get task :parent) "↳ " ""))
                                       (cand   (format "[%s] %s%s%s" list-title marker indent title)))
                                  (propertize cand
                                              'consult-gtasks-task    task
                                              'consult-gtasks-list-id list-id)))
                              tasks)))
                  lists)))
    (if (null cands)
        (message "No pending tasks in any list.")
      (let ((chosen
             (consult--read
              cands
              :prompt "All tasks: "
              :category 'consult-gtasks-task
              :sort nil
              :require-match t
              :lookup (lambda (selected cands &rest _)
                        (when selected
                          (car (member selected cands)))))))
        (when chosen
          (let ((task    (get-text-property 0 'consult-gtasks-task    chosen))
                (list-id (get-text-property 0 'consult-gtasks-list-id chosen)))
            (consult-gtasks--show-task-details task list-id)))))))

;;;###autoload
(defun consult-gtasks-tasks (&optional list-id)
  "Browse and act on Google Tasks in a task list.

If LIST-ID is nil, prompt for a task list first.

Keybindings in the minibuffer:
  RET   - Show task details in a temporary buffer
  M-d   - Delete the selected task
  M-c   - Mark the selected task as completed"
  (interactive)
  (let* ((list-pair (if list-id
                        (cons nil list-id)
                      (consult-gtasks--select-list)))
         (list-name (or (car list-pair) ""))
         (list-id   (cdr list-pair))
         (tasks   (consult-gtasks--get-tasks list-id))
         (cands   (consult-gtasks--tasks-to-candidates tasks)))
    (if (null cands)
        (message "No pending tasks in this list.")
      (let ((chosen
             (consult--read
              cands
              :prompt (format "%s Task: " list-name)
              :category 'consult-gtasks-task
              :sort nil
              :require-match t
              :lookup (lambda (selected cands &rest _)
                        (when selected
                          (get-text-property 0 'consult-gtasks-task
                                             (car (member selected cands))))))))
        (when chosen
          (consult-gtasks--show-task-details chosen list-id))))))

(defun consult-gtasks--show-task-details (task list-id)
  "Display TASK details and offer actions."
  (let* ((title  (or (plist-get task :title) "(no title)"))
         (action  (read-char-choice
                   (format "[%s] — (v)iew  (q)uit: "
                           title)
                   '(?v ?q))))
    (pcase action
      (?v (consult-gtasks--view-task task))
      (?q (message "Cancelled.")))))

(defun consult-gtasks--view-task (task)
  "Pop up a buffer showing details of TASK plist.
Displays :kind, :id, :title, :notes, and any :link inside a
:type \"generic\" entry of the :links list."
  (let* ((kind   (plist-get task :kind))
         (id     (plist-get task :id))
         (title  (or (plist-get task :title) "(no title)"))
         ;; API may return [] for absent string fields; treat non-strings as nil
         (notes  (let ((v (plist-get task :notes)))
                   (and (stringp v) v)))
         ;; :links may be [] when absent; find first :type "generic" entry
         (links  (plist-get task :links))
         (generic-link
          (catch 'found
            (seq-doseq (lnk (if (sequencep links) links []))
              (when (equal (plist-get lnk :type) "generic")
                (throw 'found (plist-get lnk :link)))))))
    (with-current-buffer (get-buffer-create "*gtask-detail*")
      (read-only-mode -1)
      (erase-buffer)
      (cl-flet ((row (label value)
                  ;; guard against non-string values returned by the API
                  (when (and (stringp value) (not (string-empty-p value)))
                    (insert (propertize (format "%-8s" label) 'face 'bold)
                            value "\n"))))
        (row "kind:"  kind)
        (row "id:"    id)
        (row "title:" title)
        (when (and notes (not (string-empty-p notes)))
          (insert (propertize "notes:\n" 'face 'bold)
                  notes "\n"))
        (row "link:"  generic-link))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun consult-gtasks-add-task (title &optional list-id notes due)
  "Add a new task with TITLE to a Google Task list.

Interactively prompts for the task title and optionally notes / due date.
If LIST-ID is non-nil, skip the list selection prompt."
  (interactive
   (list (read-string "Task title: ")
         nil
         (let ((n (read-string "Notes (optional): ")))
           (unless (string-empty-p n) n))
         (let ((d (read-string "Due date YYYY-MM-DD (optional): ")))
           (unless (string-empty-p d)
             (concat d "T00:00:00.000Z")))))
  (let* ((list-id (or list-id (cdr (consult-gtasks--select-list))))
         (payload (list :title title)))
    (when notes (setq payload (plist-put payload :notes notes)))
    (when due   (setq payload (plist-put payload :due due)))
    (let ((new-task (gtasks-task-insert list-id payload)))
      (message "Created task: %s (id: %s)"
               (plist-get new-task :title)
               (plist-get new-task :id)))))

;;;###autoload
(defun consult-gtasks-complete (list-id)
  "Select and mark a task as completed in LIST-ID (prompts if nil)."
  (interactive (list nil))
  (let* ((list-id (or list-id (cdr (consult-gtasks--select-list))))
         (tasks   (consult-gtasks--get-tasks list-id))
         (cands   (consult-gtasks--tasks-to-candidates tasks))
         (chosen  (consult--read cands
                                 :prompt "Complete task: "
                                 :sort nil
                                 :require-match t
                                 :lookup (lambda (sel cs &rest _)
                                           (when sel
                                             (get-text-property
                                              0 'consult-gtasks-task
                                              (car (member sel cs))))))))
    (when chosen
      (consult-gtasks-complete-task list-id (plist-get chosen :id)
                                    (plist-get chosen :title)))))

(defun consult-gtasks-complete-task (list-id task-id title)
  "Mark TASK-ID in LIST-ID as completed."
  (gtasks-task-patch list-id task-id (list :status "completed"))
  (message "Completed: %s" title))

;;;###autoload
(defun consult-gtasks-delete (list-id)
  "Select and delete a task in LIST-ID (prompts if nil)."
  (interactive (list nil))
  (let* ((list-id (or list-id (cdr (consult-gtasks--select-list))))
         (tasks   (consult-gtasks--get-tasks list-id))
         (cands   (consult-gtasks--tasks-to-candidates tasks))
         (chosen  (consult--read cands
                                 :prompt "Delete task: "
                                 :sort nil
                                 :require-match t
                                 :lookup (lambda (sel cs &rest _)
                                           (when sel
                                             (get-text-property
                                              0 'consult-gtasks-task
                                              (car (member sel cs))))))))
    (when chosen
      (consult-gtasks-delete-task list-id (plist-get chosen :id)
                                  (plist-get chosen :title)))))

(defun consult-gtasks-delete-task (list-id task-id title)
  "Delete TASK-ID from LIST-ID after confirmation."
  (when (yes-or-no-p (format "Delete task \"%s\"? " title))
    (gtasks-task-delete list-id task-id)
    (message "Deleted: %s" title)))

;;; --------------------------------------------------------------------------
;;; Embark integration (optional)
;;; --------------------------------------------------------------------------

(with-eval-after-load 'embark
  (defvar-keymap consult-gtasks-embark-map
    :doc "Embark keymap for consult-gtasks tasks."
    "c" #'consult-gtasks-embark-complete
    "d" #'consult-gtasks-embark-delete)

  (add-to-list 'embark-keymap-alist
               '(consult-gtasks-task . consult-gtasks-embark-map))

  (defun consult-gtasks-embark-complete (cand)
    "Complete the task represented by CAND via Embark."
    (when-let ((task (get-text-property 0 'consult-gtasks-task cand)))
      (message "Embark complete: provide list-id manually or store it on the candidate.")))

  (defun consult-gtasks-embark-delete (cand)
    "Delete the task represented by CAND via Embark."
    (when-let ((task (get-text-property 0 'consult-gtasks-task cand)))
      (message "Embark delete: provide list-id manually or store it on the candidate."))))

(provide 'consult-gtasks)
;;; consult-gtasks.el ends here
