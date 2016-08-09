;;; org-agenda-server.el --- Run an agenda server    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: hypermedia, extensions

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

;; This runs an "agenda server / kiosk".

;; The "server" side of it means that it updates agenda files in
;; various formats (text, html, ps), whenever it detects the
;; underlying agenda .org files changing, allowing you to read your
;; various lists of things to do when you're on a machine that doesn't
;; have Emacs.  (I wrote this because I wasn't satisfied with
;; mobile-org on Android.)

;; The idea is to run it on a home / cloud server, and rsync your
;; changes in (or you could use Dropbox or similar).  You can read the
;; changes via an HTTP server.

;; The "kiosk" side of it means that you can access the files directly
;; on the above server, using a very reduced interface, such as a USB
;; keypad, so that the server can be built into a noticeboard
;; (pinboard) so it can be next to the rest of your central household
;; information, and you can mark tasks as "DONE" without having to go
;; to "a computer".

;;; Code:

;;;;;;;;;;;;;;;;;
;; Agenda loop ;;
;;;;;;;;;;;;;;;;;

(defun jcgs/org-revert-agenda-files ()
  "Re-read any agenda files that have changed."
  (interactive)
  (message "Looking for changed files")
  (mapcar (lambda (file)
	    (let ((filebuf (find-buffer-visiting file)))
	      (when (bufferp filebuf)
		(with-current-buffer filebuf
		  (unless (verify-visited-file-modtime)
		    (message "Re-reading agenda buffer %S" file)
		    (revert-buffer t t t))))))
	  org-agenda-files))

(defun jcgs/org-agenda-write-agenda-to-file (agenda-letter org-file json-file)
  "Generate the agenda for AGENDA-LETTER and write it to ORG-FILE and JSON-FILE.
Either of these may be null."
  (when (bufferp (get-buffer "*Org Agenda*"))
    (kill-buffer "*Org Agenda*"))
  (org-agenda nil agenda-letter)
  (let ((agenda-string (buffer-string)))
    (when (stringp org-file)
      (find-file org-file)
      (read-only-mode -1)
      (erase-buffer)
      (insert agenda-string)
      (goto-char (point-min))
      (delete-matching-lines "Press `C-u r' to search again with new search string")
      (delete-matching-lines "^\s-*|")
      (goto-char (point-min))
      (while (re-search-forward "^Headlines with" (point-max) t)
	(replace-match "* \\&"))
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+" (point-max) t)
	;; todo: make this re-arrange the items on the line, so the keyword comes first
	(replace-match (concat (make-string (- (match-end 0) (match-beginning 0)) ?*) " ")))
      (goto-char (point-min))
      (basic-save-buffer))
    (when (stringp json-file)
      (find-file json-file)
      (read-only-mode -1)
      (erase-buffer)
      (insert agenda-string)
      (goto-char (point-min))
      (delete-matching-lines "Press `C-u r' to search again with new search string")
      (delete-matching-lines "^\s-*|")
      (goto-char (point-min))
      (while (search-forward "\n" (point-max) t)
	(replace-match "\\n" t t))
      (goto-char (point-min))
      (while (search-forward "\"" (point-max) t)
	(replace-match "\\\"" t t))
      (goto-char (point-min))
      (insert "{content: \"")
      (goto-char (point-max))
      (insert "\"}\n")
      (basic-save-buffer))))

(defun jcgs/org-agenda-monitor-really-stop ()
  "Stop the monitor system.
This is done in such a way that the calling script will not restart it."
  (interactive)
  (find-file "/tmp/stop-agenda-kiosk")
  (insert "Flag file\n")
  (basic-save-buffer))

(defvar agenda-card-filename-format (or (getenv "CARDFILENAMEFORMAT")
					"~/public_html/agenda/agenda-%s.json")
  "The format for card file names.")

(defun jcgs/org-make-stored-agenda-index ()
  "Index my stored agenda files."
  (interactive)
  (save-window-excursion
    (find-file (expand-file-name "index.html" jcgs/org-agenda-store-directory))
    (erase-buffer)
    (insert "<html><head><title>My agenda files</title></head>\n")
    (insert "<body>\n<h1>My agenda files</h1>\n<ul>\n")
    (mapcar (lambda (file)
	      (let ((base-name (file-name-sans-extension file)))
		(insert (format "  <li> <a href=\"%s\">%s</a> (<a href=\"%s\">txt</a>, <a href=\"%s.org\">org</a>, <a href=\"%s.ps\">ps</a>)\n"
				file
				(capitalize
				 (subst-char-in-string ?_ ?  base-name))
				base-name base-name base-name))))
	    (delete-if
	     (lambda (file)
	       (string-match "index.html" file))
	     (directory-files jcgs/org-agenda-store-directory
			      nil ".html$")))
    (insert "</ul>\n</body></html>\n")
    (basic-save-buffer)))

(defun jcgs/org-agenda-monitor-update (&optional with-mobile)
  "Update my outgoing agenda files from incoming org file alterations.
With optional WITH-MOBILE, pull and push the mobile data."
  (interactive)				; for debugging, mostly
  (when (or (file-exists-p "/tmp/restart-agenda-kiosk")
	    (file-exists-p "/tmp/stop-agenda-kiosk"))
    ;; Exit this emacs session; the shell script that it is meant to
    ;; be started (agenda-kiosk-emacs) from will start a new emacs
    ;; session unless the file /tmp/stop-agenda-kiosk exists.
    (save-buffers-kill-emacs))
  (save-window-excursion
    (message "Starting agenda update at %s" (current-time-string))
    (save-excursion
      (let ((x (find-buffer-visiting org-mobile-capture-file)))
	(when x
	  ;; todo: could I just "revert" it?
	  (kill-buffer x)))
      (when with-mobile
	(find-file org-mobile-capture-file))
      (message "Reloading agenda files")
      (jcgs/org-revert-agenda-files))
    (when with-mobile
      (message "Pulling input from org-mobile")
      (org-mobile-pull))
    (message "Saving agenda views")
    (org-store-agenda-views)
    (dolist (file org-agenda-files)
      (htmlize-file file jcgs/org-agenda-store-directory))
    (message "Indexing agenda views")
    (jcgs/org-make-stored-agenda-index)
    (when with-mobile
      (message "Pushing to mobile")
      (org-mobile-push))
    (message "Done agenda update")))

(defvar org-agenda-monitor-last-updated "not yet"
  "String indicating when the last update happened.")

(defun jcgs/org-agenda-monitor-update-step ()
  "Update my outgoing agenda files from incoming org file alterations.
Then arrange for it to happen again when the files change again."
  (interactive)
  (when (fboundp 'org-agenda-kiosk-log)
    (org-agenda-kiosk-log 2 "Updated"))
  (setq org-agenda-monitor-last-updated (current-time-string))
  (jcgs/org-agenda-monitor-update t)
  ;; set the next one going
  (message "Restarting notifier for incoming agenda changes, at %s"
	   (current-time-string))
  (jcgs/org-agenda-monitor-start))

(setq remote-update 'remote-update)

(global-set-key [ remote-update ] 'jcgs/org-agenda-monitor-update-step)

(defun jcgs/org-agenda-trigger-monitor-update ()
  "Trigger an agenda update.
Doing it this way means we're not running anything large in the sentinel."
  (interactive)
  (message "Triggering agenda update at %s" (current-time-string))
  (setq unread-command-events (nreverse (cons remote-update (nreverse unread-command-events)))))

(defvar jcgs/org-agenda-monitor-timer nil
  "The timer to batch updates rather than doing them on every change.")

(defvar jcgs/org-agenda-monitor-delay 15
  "How long to delay to allow other changes to come in.")

(defun jcgs/org-agenda-monitor-sentinel (process change-descr)
  "Run on each state change of the agenda monitor.
Argument PROCESS is the monitor process.
CHANGE-DESCR is the change"
  (message "agenda monitor sentinel \"%s\"" change-descr)
  (when (string= change-descr "finished\n")
    (message "Agenda directory has changed, waiting %d seconds in case of further changes"
	     jcgs/org-agenda-monitor-delay)
    (when (timerp jcgs/org-agenda-monitor-timer)
      (cancel-timer jcgs/org-agenda-monitor-timer)
      (setq jcgs/org-agenda-monitor-timer nil))
    (message "Detected a change to an agenda file; allowing %d seconds for more changes to appear" 
	     jcgs/org-agenda-monitor-delay)
    (setq jcgs/org-agenda-monitor-timer
	  (run-with-idle-timer jcgs/org-agenda-monitor-delay nil 'jcgs/org-agenda-trigger-monitor-update))))

(defun jcgs/org-agenda-monitor-start ()
  "Arrange to monitor incoming alterations to my agenda files."
  (interactive)				; mostly for testing
  (message "Starting agenda monitor")
  (let* ((agenda-monitor-buffer (get-buffer-create " *agenda monitor*"))
	 (agenda-monitor-process (apply 'start-process "agenda-monitor"
					agenda-monitor-buffer
					"/usr/bin/inotifywait"
					"-e" "modify"
					"-e" "create"
					"-e" "attrib"
					org-agenda-files)))
    (set-process-sentinel agenda-monitor-process
			  'jcgs/org-agenda-monitor-sentinel)))

(defun jcgs/org-agenda-monitor-stop ()
  "Arrange to stop monitoring incoming alterations to my agenda files."
  (interactive)				; mostly for testing
  (setq jcgs/org-agenda-monitor-timer nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda from home server ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-agenda-from-server ()
  "Fetch my agenda files from my home server, and update buffers."
  (interactive)
  (message "Fetching agenda files from home server")
  (shell-command (substitute-in-file-name "$EHOME/JCGS-scripts/pullorg"))
  (jcgs/org-revert-agenda-files))

(provide 'org-agenda-server)
;;; org-agenda-server.el ends here
