;;;; linked tasks in org-mode
;;; Time-stamp: <2016-03-12 19:27:07 jcgs>

;; Copyright (C) 2015, 2016 John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Propagates state changes up trees of tasks (by marking parents as
;; DONE when the last child task is DONE, all the way to the top
;; level), and along lists of tasks (by moving a :next: marker, and
;; propagating markers such as :urgent: as it does so).

;;; Code:

(defun jcgs/org-clock-in-prepare-function ()
  "My customization of task clock-in.
When opening a sub-task, it opens it ancestral tasks too."
  (save-excursion
    (while (> (funcall outline-level) 1)
      (outline-up-heading 1)
      (when (looking-at org-complex-heading-regexp)
	(let ((state (match-string-no-properties 2)))
	  (when (equal state "TODO")
	    (org-todo "OPEN")))))))

(add-hook 'org-clock-in-prepare-hook 'jcgs/org-clock-in-prepare-function)

(defun jcgs/org-after-todo-state-change-propagate-upwards ()
  "When the last of a set of sibling tasks is marked as DONE,
mark its ancestral tasks as DONE."
  (save-excursion
    (while (> (funcall outline-level) 1)
      (outline-up-heading 1)
      (let ((not-all-done nil)
	    (on-first t))
	(org-map-entries
	 (lambda ()
	   (when (and (not on-first)
		      (org-entry-is-todo-p))
	     (setq not-all-done t))
	   (setq on-first nil))
	 nil
	 'tree)
	(unless not-all-done
	  (org-todo "DONE"))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-propagate-upwards t)

(defvar jcgs/org-after-todo-state-follower-tags
  '("urgent" "soon")
  "Tags which `jcgs/org-after-todo-state-change-move-next-marker'
should move as the \"next\" tag moves.")

(defun jcgs/org-after-todo-state-change-move-next-marker ()
  "If this task is being marked as done, and has a :next: tag, move the tag.
Propagate :urgent: and :soon: tags as needed."
  (save-excursion
    (let ((original-tags (org-get-tags)))
      (when (and (org-entry-is-done-p)
		 (member "next" original-tags))
	(let ((tags-to-move nil))
	  (dolist (maybe jcgs/org-after-todo-state-follower-tags)
	    (if (member maybe original-tags)
		(push maybe tags-to-move)))
	  (org-toggle-tag "next" 'off)
	  (beginning-of-line 1)
	  (let ((started-at (point)))
	    (org-forward-heading-same-level 1)
	    (if (/= (point) started-at)
		(progn
		  (org-toggle-tag "next" 'on))
	      (when (y-or-n-p "Move :next: marker to next subtree? ")
		(outline-next-heading)
		(org-toggle-tag "next" 'on))))
	  (dolist (moving-tag tags-to-move)
	    (org-toggle-tag moving-tag 'on)))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-after-todo-state-change-move-next-marker)

;;;;;;;;;;;;;;;;;;;;;;
;; chaining entries ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-add-chained-task (uuid tag state)
  "Add blocked UUID with TAG and STATE to the current task."
  (jcgs/org-set-chained-tasks nil
			      (cons (list uuid tag state)
				    (jcgs/org-get-chained-tasks nil))))

(defun jcgs/org-count-chained-tasks ()
  "Return the number of tasks dependent on the current task."
  (let* ((chained-tasks-raw (org-entry-get nil "CHAINED_TASKS"))
	 (directly-chained-tasks (if chained-tasks-raw
				     (read chained-tasks-raw)
				   nil))
	 (chained-task-count (length directly-chained-tasks)))
    (save-excursion
      (dolist (dct directly-chained-tasks)
	(org-id-goto (first dct))
	(setq chained-task-count (+ chained-task-count 
				    (jcgs/org-count-chained-tasks)))))
    chained-task-count))

(defun jcgs/org-setup-chain-task (uuid tag)
  "Set up a chained task.
When the current task is done, onto the task with UUID add the TAG."
  (interactive
   (let ((pair (save-window-excursion
		 (save-excursion
		   (message 
		    (substitute-command-keys
		     "Move to task to chain, press \\[exit-recursive-edit]"))
		   (recursive-edit)
		   (cons (org-id-get nil t)
			 (org-get-buffer-tags))))))
     (list (car pair)
	   (completing-read "Tag: " (cdr pair)))))
  (jcgs/org-add-chained-task uuid tag nil))

(defun jcgs/org-get-todo-state-no-properties ()
  "Like org-get-todo-state, but returning an undecorated string."
  (let ((state-string (org-get-todo-state)))
    (set-text-properties 0 (length state-string) nil state-string)
    state-string))

(defun jcgs/org-get-blocking-tasks (pom)
  "Return ids of tasks blocking the task at point-or-marker POM."
  (let* ((raw-old-blockers (org-entry-get pom "BLOCKED_BY")))
    (if raw-old-blockers
	(read raw-old-blockers)
      nil)))

(defun jcgs/org-set-blocking-tasks (pom tasks)
  "Set the blocking tasks at POM to TASKS."
  (org-entry-put pom "BLOCKED_BY"
		 (let ((print-length nil))
		   (prin1-to-string tasks))))

(defun jcgs/org-get-chained-tasks (pom)
  "Return ids of tasks blocked by the task at point-or-marker POM."
  (let* ((raw-old-chained (org-entry-get pom "CHAINED_TASKS")))
    (if raw-old-chained
	(read raw-old-chained)
      nil)))

(defun jcgs/org-set-chained-tasks (pom tasks)
  "Set the chained tasks at POM to TASKS."
  (org-entry-put pom "CHAINED_TASKS"
		 (let ((print-length nil))
		   (prin1-to-string tasks))))

(defun jcgs/org-get-pre-blocking-state ()
  "Get the state this task was in before it was blocked."
  (let ((old-state (jcgs/org-get-todo-state-no-properties))
	(id (org-id-get nil t)))
    (if (string= old-state "BLOCKED")
	(save-excursion
	  (catch 'found-one
	    (dolist (blocking-task (jcgs/org-get-blocking-tasks nil))
	      (org-id-goto blocking-task)
	      (let ((relevant-chained-task (assoc id (jcgs/org-get-chained-tasks nil))))
		(when (consp relevant-chained-task)
		  (let ((state (third relevant-chained-task)))
		    (unless (equal state "BLOCKED")
		      (throw 'found-one state))))))
	    "OPEN"))
      old-state)))

(defun jcgs/org-block-task ()
  "Mark the current task as blocked, and link the blocking task to unblock it.
Also add a link to the blocking task from the current one."
  (interactive)
  (let* ((old-tag-state (jcgs/org-get-pre-blocking-state))
	 (blocked-uuid (org-id-get nil t))
	 (blocked-by (save-window-excursion
		       (save-excursion
			 (message
			  (substitute-command-keys
			   "Move to task blocking this one, press \\[exit-recursive-edit]"))
			 (recursive-edit)
			 (jcgs/org-add-chained-task blocked-uuid nil old-tag-state)
			 (org-id-get nil t)))))
    (org-todo "BLOCKED")
    (jcgs/org-set-blocking-tasks nil
				 (cons blocked-by
				       (jcgs/org-get-blocking-tasks nil)))))

(defun jcgs/org-maybe-chain-task ()
  "Activate the next stage of a chain."
  (when (org-entry-is-done-p)
    (let ((id (org-id-get nil)))
      (dolist (chained-task (jcgs/org-get-chained-tasks nil))
	(let ((chained-task-id (first chained-task))
	      (chained-task-tag (second chained-task))
	      (chained-task-state (third chained-task)))
	  (save-window-excursion
	    (save-excursion
	      (org-id-goto chained-task-id)
	      (let ((remaining-blockers (delete id (jcgs/org-get-blocking-tasks nil))))
		(jcgs/org-set-blocking-tasks nil remaining-blockers)
		(when (null remaining-blockers)
		  (when chained-task-tag (org-toggle-tag chained-task-tag 'on))
		  (when chained-task-state (org-todo chained-task-state)))))))))))

(add-hook 'org-after-todo-state-change-hook 'jcgs/org-maybe-chain-task)

(provide 'org-linked-tasks)
