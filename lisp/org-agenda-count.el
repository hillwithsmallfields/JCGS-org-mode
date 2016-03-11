;;; org-agenda-count.el --- count matching entries   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: 

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

;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;
;; counting entries ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-count-entry ()
  "Count the current entry, in the appropriate counter."
  (let* ((state (and (looking-at org-todo-line-regexp)
		     (match-end 2)
		     (match-string-no-properties 2)))
	 (pair (assoc state jcgs/org-state-counters)))
    (if pair
	(rplacd pair (1+ (cdr pair)))
      (setq jcgs/org-state-counters
	    (cons (cons state 1)
		  jcgs/org-state-counters)))))

(defun jcgs/org-count-entries (scope)
  "Count the entries in each state for SCOPE."
  (let ((jcgs/org-state-counters nil))
    (org-map-entries 'jcgs/org-count-entry nil scope)
    (with-output-to-temp-buffer "*Entry state counts*"
      (let ((fmt (format "%% %ds: %%d\n" (apply 'max (mapcar 'length (mapcar 'car jcgs/org-state-counters))))))
	(dolist (state (sort jcgs/org-state-counters (lambda (a b) (> (cdr a) (cdr b )))))
	  (when (car state)
	    (princ (format fmt (car state) (cdr state)))))))))

(defun jcgs/org-count-all-entries ()
  "Count all entries."
  (interactive)
  (jcgs/org-count-entries 'agenda))

(provide 'org-agenda-count)
;;; org-agenda-count.el ends here
