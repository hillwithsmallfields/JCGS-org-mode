;;; org-agenda-notify.el --- notify when agenda items happen  -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2017  John Sturdy

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

;; Use the browser to tell the user that something has happened,
;; possibly interrupting whatever they were doing on it while waiting
;; for whatever it was to finish.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;
;; Timer notification ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jcgs/background-images-directory (catch 'found
					   (dolist (dir '("$HOME/Pictures/backgrounds"
							  "$HOME/Pictures"
							  "$HOME/backgrounds"
							  "$HOME"))
					     (let ((s (substitute-in-file-name dir)))
					       (when (file-directory-p s)
						 (throw 'found s))))
					   nil)
  "My directory of background images.")

(defun jcgs/random-background-image ()
  "Pick a background image at random."
  (let ((images (directory-files jcgs/background-images-directory t "\\.jpg" t)))
    (nth (random (length images)) images)))

(defun jcgs/org-timer-notifier (notification)
  "Display NOTIFICATION in an arresting manner.
Interactive mostly for testing."
  (interactive "sNotification: ")
  (let ((overrun nil))
    (jcgs/org-timer-log-pomodoro-done notification)
    (require 'notify-via-browse-url)
    (notify-via-browse-url
     (format "<STYLE type=\"text/css\"> BODY { background: url(\"file:%s\") } </STYLE>"
	     (jcgs/random-background-image))
     (format "Pomodoro completed at %s" (current-time-string))
     notification)
    (save-window-excursion
      (switch-to-buffer (get-buffer-create "*Org timer notification*"))
      (erase-buffer)
      (insert notification (substitute-command-keys "\n\n\\[exit-recursive-edit] to continue\n\n"))
      (let ((start-of-overrun-keystrokes (point)))
	(recursive-edit)
	;; avoid wasting any keystrokes the user was typing at the time
	(when (> (point-max)
		 start-of-overrun-keystrokes)
	  (setq overrun  (buffer-substring start-of-overrun-keystrokes
					   (point-max)))
	  (kill-new overrun))))
    (message "%s saved in kill ring" overrun)))

(setq org-show-notification-handler 'jcgs/org-timer-notifier)

(eval-after-load "org"
  '(jcgs/org-timer-setup))

(defun jcgs/org-clock-out-on-typing-break-function ()
  "Clock out of the current task, as a typing break is starting."
  (message "jcgs/org-clock-out-on-typing-break-function: %S %S %S"
	   org-clock-goto-may-find-recent-task
	   (car org-clock-history)
	   (if (car org-clock-history)
	       (marker-buffer (car org-clock-history))
	     "<none>"))
  (when (and org-clock-goto-may-find-recent-task
	     (car org-clock-history)
	     (marker-buffer (car org-clock-history)))
    (let ((jcgs/org-clocking-out-for-type-break t))
      (org-clock-goto)
      (message "clocking out at %d in %S" (point) (current-buffer))
      (org-clock-out))
    ;; we did not actually do the typing break:
    nil))

(defvar jcgs/type-break-start-break-hook nil
  "Hooks for starting a typing break.
You may want to turn voice input off at this point; and suspend task timers.")

(eval-after-load "type-break"
  '(progn
     (defadvice type-break (before jcgs/type-break-hook-runner activate)
       (run-hooks 'jcgs/type-break-start-break-hook))
     (add-hook 'jcgs/type-break-start-break-hook 'jcgs/org-clock-out-on-typing-break-function)))

(provide 'org-agenda-notify)
;;; org-agenda-notify.el ends here
