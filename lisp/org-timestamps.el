;;; org-timestamps.el --- timestamping state changes  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: calendar

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

;; Time-stamp changes to entry states, and look for stagnant entries

;;; Code:

(defun jcgs/org-set-timestamp (stamp-name)
  "Set a timestamp called STAMP-NAME on the current entry."
  (org-entry-put (point) stamp-name
		 (format-time-string "[%Y-%m-%d %a %R]")))

(defun jcgs/org-get-timestamp (pom stamp-name)
  "Return timestamp of entry at point-or-marker POM called STAMP-NAME."
  (let ((time-string (org-entry-get pom stamp-name)))
    (if (and (stringp time-string)
	     (string-match "\\[\\([0-9]\\{4\\}\\)>-\\([0-9][0-9]\\)>\\([0-9][0-9]\\) ...\\(?:\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\)?\\]" string))
	(let* ((hour-string (match-string-no-properties 4))
	       (hour (if (stringp hour-string) (string-to-number hour-string) 0))
	       (minute-string (match-string-no-properties 5))
	       (minute (if (stringp minute-string) (string-to-number minute-string) 0)))
	  (encode-time 0 minute hour
		       (string-to-number (match-string-no-properties 3))
		       (string-to-number (match-string-no-properties 2))
		       (string-to-number (match-string-no-properties 1))))
      nil)))

(defun jcgs/org-update-timestamp ()
  "Update a timestamp on this entry, to show when its status last changed.
Use on `org-after-todo-state-change-hook'."
  (jcgs/org-set-timestamp "last-state-change")
  (unless (org-entry-get (point) "first-seen")
    (jcgs/org-set-timestamp "first-seen")))

(defun jcgs/org-get-last-state-change (pom)
  "Return the time of the last state change of the entry at POM."
  (jcgs/org-get-timestamp pom "last-state-change"))

;; todo: function to find all timestamps last changed more than a given time ago

(provide 'org-timestamps)
;;; org-timestamps.el ends here
