;;;; Pomodoros
;;; Time-stamp: <2019-05-01 08:47:37 jcgs>

;; Copyright (C) 2015, 2016, 2017, 2019  John Sturdy

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

(require 'org-jcgs-journal)

(defun jcgs/org-timer-value-string-wrapper (orig-result)
  "Modify the timer display string.
Argument ORIG-RESULT is the result of the original function."
  (add-face-text-property 0 (length orig-result) '(:foreground "green") nil orig-result)
  orig-result)

(advice-add 'org-timer-value-string
	    :filter-return
	    #'jcgs/org-timer-value-string-wrapper)

(defun jcgs/org-clock-in-start-timer ()
  "Start the timer when I clock in to a task.
Assumes that point is on the task."
  (unless (and org-timer-start-time
	     (not org-timer-countdown-timer))
    ;; I could do it unconditionally, but perhaps this lets me
    ;; transfer the rest of a pomodoro to another task?
    (org-timer-set-timer
     ;; 16 (two C-u arguments) means use `org-timer-default-timer'
     ;; without prompting the user for a duration and automatically
     ;; replace any running timer.
     '(16)))
  (org-todo "CURRENT"))

(defun jcgs/org-clock-out-stop-timer ()
  "Stop the timer when I clock out from a task."
  (org-timer-stop)
  (org-todo "OPEN"))

(defun jcgs/org-timer-setup ()
  "Customizer the org timer to suit me, for pomodoro use."
  (add-hook 'org-clock-in-hook
	    'jcgs/org-clock-in-start-timer)
  (add-hook 'org-clock-out-hook
	    'jcgs/org-clock-out-stop-timer))

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

(message "Set jcgs/pomodoro-log-file to %S" jcgs/pomodoro-log-file)

(jcgs/org-journals-add-journal
 "pomodoro"
 jcgs/pomodoro-log-file)

(defun jcgs/pomodoro-log-show ()
  "Show my pomodoro log."
  (interactive)
  (find-file-other-window jcgs/pomodoro-log-file)
  (goto-char (point-max )))

(defvar jcgs/org-timer-pomodoros-done-log nil
  "Log of the pomodoros I have done in this session.")

(defvar jcgs/org-strip-timer-stuff-regexp
  "\\(.+\\)\\(: time out\\)"
  "Regexp to identify what part of a string from the timer system to keep.")

(defun jcgs/org-strip-timer-stuff (string)
  "Remove timing system related text from STRING."
  ;; todo: also remove the state keyword
  (if (string-match jcgs/org-strip-timer-stuff-regexp
		    string)
      (match-string 1 string)
    string))

(defun jcgs/org-increment-count (property)
  "Increment a counter called PROPERTY in the current entry."
  (org-set-property property
		    (number-to-string
		     (1+ (string-to-number
			  (or (org-entry-get nil "pomodoros-done" nil) "0"))))))

(defun jcgs/org-timer-log-pomodoro-done (string)
  "Log that I have completed a timed activity slot.
Argument STRING is the log entry."
  (let ((pomodoro-string (if nil
			     (jcgs/org-strip-timer-stuff string)
			   org-clock-current-task)))
    (save-window-excursion
      (save-excursion
	(jcgs/org-journal-open-journal-at-date "pomodoro")
	(let ((entry-end (or (save-excursion
			       (outline-get-next-sibling))
			     (point-max)))
	      (full-string (concat "**** " pomodoro-string "\n")))
	  (unless (search-forward full-string entry-end t)
	    (insert full-string)))
	(jcgs/org-increment-count "pomodoros-done")
	(outline-up-heading 1)
	(jcgs/org-increment-count "pomodoros-done")
	(basic-save-buffer)))
    (setq jcgs/org-timer-pomodoros-done-log (cons (cons (current-time-string)
							pomodoro-string)
						  jcgs/org-timer-pomodoros-done-log)
	  jcgs/org-timer-pomodoros-done-count (1+ jcgs/org-timer-pomodoros-done-count))))

(provide 'org-pomodoros)
