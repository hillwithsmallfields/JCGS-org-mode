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

(defun jcgs/org-propagate-openness-upward ()
  "When opening a sub-task, open its ancestral tasks too."
  (save-excursion
    (while (> (funcall outline-level) 1)
      (outline-up-heading 1)
      (when (looking-at org-complex-heading-regexp)
	(let ((state (match-string-no-properties 2)))
	  (when (equal state "TODO")
	    (org-todo "OPEN")))))))

(defun jcgs/org-propagate-doneness-upwards ()
  "When the last of a set of sibling tasks is marked as a \"done\" state,
mark its ancestral tasks as in that state too."
  (save-excursion
    (let ((done-state (jcgs/org-get-todo-state-no-properties)))
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
	    (org-todo done-state)))))))

(provide 'org-upwards-propagation)

;;; org-upwards-propagation ends here
