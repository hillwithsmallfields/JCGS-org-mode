;;;; Pomodoros
;;; Time-stamp: <2017-11-23 17:30:49 jcgs>

;; Copyright (C) 2015, 2016, 2017  John Sturdy

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

;;

;;; Code:

(defun jcgs/org-clock-in-start-timer ()
  "Start the timer when I clock in to a task."
  (unless org-timer-current-timer
    (org-timer-set-timer
     ;; 16 (two C-u arguments) means use `org-timer-default-timer'
     ;; without prompting the user for a duration and automatically
     ;; replace any running timer.
     '(16)))
  (org-todo "CURRENT"))

(defun jcgs/org-clock-out-stop-timer ()
  "Stop the timer when I clock out from a task."
  (when org-timer-current-timer
    (org-timer-cancel-timer))
  (org-timer-stop)
  (org-todo "OPEN"))

(defun jcgs/org-timer-setup ()
  "Customizer the org timer to suit me, for pomodoro use."
  (add-hook 'org-clock-in-hook
	    jcgs/org-clock-in-start-timer)
  (add-hook 'org-clock-out-hook
	    jcgs/org-clock-out-stop-timer))

(defvar jcgs/org-timer-pomodoros-done-count 0
  "Count of the pomodoros I have done.
Reset to zero whenever `jcgs/org-timer-log-pomodoro-done' decides to log
the end of a day.")

(defvar jcgs/pomodoro-log-file
  (let ((work-org "~/work-org"))
    (cond
     ((file-directory-p work-org)
      (expand-file-name "pomodoro-log.org" work-org))
     ((getenv "ORG")
      (substitute-in-file-name "$ORG/pomodoro-log.org"))
     ((file-directory-p "~/Dropbox/QS-inbox")
      (expand-file-name "~/Dropbox/QS-inbox/pomodoro-log.org"))
     ((file-directory-p "~/Dropbox")
      (expand-file-name "~/Dropbox/pomodoro-log.org"))
     (t (expand-file-name "~/pomodoro-log.org"))))
  "Where I log my pomodoro completion.")

(defun jcgs/pomodoro-log-show ()
  "Show my pomodoro log."
  (interactive)
  (find-file-other-window jcgs/pomodoro-log-file)
  (goto-char (point-max )))

(defvar jcgs/org-timer-pomodoros-done-log nil
  "Log of the pomodoros I have done.")

(defvar jcgs/org-strip-timer-stuff-regexp
  "\\(.+\\)\\(: time out\\)"
  "Regexp to identify what part of a string from the timer system to keep.")

(defun jcgs/org-strip-timer-stuff (string)
  "Remove timing system related text from STRING."
  (if (string-match jcgs/org-strip-timer-stuff-regexp
		    string)
      (match-string 1 string)
    string))

(defun jcgs/org-timer-log-pomodoro-done (string)
  "Log that I have completed a timed activity slot.
Argument STRING is the log entry."
  (let* ((now (current-time))
	 (day (jcgs/date-string now))
	 (pomodoro-string (jcgs/org-strip-timer-stuff string)))
    (save-window-excursion
      (save-excursion
	(find-file jcgs/pomodoro-log-file)
	(let* ((new-day (jcgs/org-journal-open-date))
	       (already (org-entry-get nil "pomodoros-done" nil)))
	  (insert "**** " pomodoro-string "\n")
	  (org-set-property "pomodoros-done"
			    (number-to-string
			     (1+ (if (stringp already)
				     (string-to-number already)
				   0))))
	  (basic-save-buffer))))
    (setq jcgs/org-timer-pomodoros-done-log (cons (cons (current-time-string)
							pomodoro-string)
						  jcgs/org-timer-pomodoros-done-log)
	  jcgs/org-timer-pomodoros-done-count (1+ jcgs/org-timer-pomodoros-done-count))))

(provide 'org-pomodoros)
