;;;; linked tasks in org-mode
;;; Time-stamp: <2016-03-12 21:15:11 jcgs>

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

(defun jcgs/org-entry-get-lisp-val (pom property)
  "Like org-entry-get but interprets the property as Lisp."
  (let* ((property-string (org-entry-get pom property)))
    (if property-string
	(read property-string)
      nil)))

(defun jcgs/org-entry-put-lisp-val (pom property value)
  "Like org-entry-put, but handles non-strings."
  (org-entry-put pom property
		 (let ((print-length nil))
		   (prin1-to-string value))))

(defun jcgs/org-get-blocking-tasks (pom)
  "Return ids of tasks blocking the task at point-or-marker POM."
  (jcgs/org-entry-get-lisp-val pom "BLOCKED_BY"))

(defun jcgs/org-set-blocking-tasks (pom tasks)
  "Set the blocking tasks at POM to TASKS."
  (jcgs/org-entry-put-lisp-val pom "BLOCKED_BY" tasks))

(defun jcgs/org-get-chained-tasks (pom)
  "Return ids of tasks blocked by the task at point-or-marker POM."
  (jcgs/org-entry-get-lisp-val pom "CHAINED_TASKS"))

(defun jcgs/org-set-chained-tasks (pom tasks)
  "Set the chained tasks at POM to TASKS."
  (jcgs/org-entry-put-lisp-val pom "CHAINED_TASKS" tasks))

(defun jcgs/org-get-pre-blocking-state (pom)
  "Get the state this task was in before it was blocked."
  (org-entry-get pom "PRE_BLOCKING_STATE"))

(defun jcgs/org-set-pre-blocking-state (pom state)
  "Set a reminder that the task at POM was in STATE before it was blocked."
  (org-entry-put pom "PRE_BLOCKING_STATE" state))

(defun jcgs/org-block-task ()
  "Mark the current task as blocked, and link the blocking task to unblock it.
Also add a link to the blocking task from the current one."
  (interactive)
  (let* ((old-state (jcgs/org-get-todo-state-no-properties))
	 (blocked-uuid (org-id-get nil t))
	 (blocked-by (save-window-excursion
		       (save-excursion
			 (message
			  (substitute-command-keys
			   "Move to task blocking this one, press \\[exit-recursive-edit]"))
			 (recursive-edit)
			 (jcgs/org-add-chained-task blocked-uuid nil old-state)
			 (org-id-get nil t)))))
    (unless (equal old-state "BLOCKED")
	(org-entry-put nil "PRE_BLOCKING_STATE" old-state))
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
		  (when chained-task-state (org-todo (jcgs/org-get-pre-blocking-state nil))))))))))))

(provide 'org-linked-tasks)
