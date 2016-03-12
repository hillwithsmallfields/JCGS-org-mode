;;;; linked tasks in org-mode
;;; Time-stamp: <2016-03-12 21:18:58 jcgs>

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

(provide 'org-moving-tags)

;;; org-moving-tags.el ends here
