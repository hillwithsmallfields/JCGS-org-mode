;;; org-jcgs-journal.el --- keep track of things I've done
;; Based on my earlier tracked-compile.el

;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2017  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience

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

;;; Code:

(defvar jcgs/org-journal-files (list (expand-file-name "~/common/notes/hackery.org-log"))
  "The file into which you log your work.
You could set this per-buffer for local logs.")

;;;;;;;;;;;;;;;;;;;;;;;
;; Date-based filing ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun read-ymd-string (&optional prompt)
  "Read a year-month-day string from the user, using PROMPT."
  (let ((matched nil))
     (while (not matched)
       (setq ymd-string (read-from-minibuffer (or prompt "Date (YYYY-MM-DD): ")
					      (format-time-string "%Y-%m-%d"))
	     matched (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)"
				   ymd-string)))
   (list (string-to-number (match-string 1 ymd-string))
	 (string-to-number (match-string 2 ymd-string))
	 (string-to-number (match-string 3 ymd-string)))))

(defmacro with-surrounding-blank-lines (&rest body)
  "Execute BODY forms.
Put a single blank line before and after whatever it inserts."
  `(let ((before-marker (point-marker))
	 (after-marker (point-marker)))
     (set-marker-insertion-type after-marker t)
     (insert "\n")
     (progn
       ,@body)
     (insert "\n")
     (goto-char before-marker)
     (delete-blank-lines)
     (delete-blank-lines)
     (open-line 1)
     (goto-char after-marker)
     (delete-blank-lines)
     (delete-blank-lines)
     (open-line 1)))

(defun jcgs/org-journal-open-date (year month day &optional no-blank-lines)
  "Ensure there is an open work-log record for YEAR MONTH DAY.
With optional NO-BLANK-LINES, don't surround it with blank lines."
  (interactive (read-ymd-string))
  ;; we must be in something based on org-mode for some org-mode
  ;; functions we use to work; we mustn't call the mode setup
  ;; function each time, because it kills all local variables
  (unless (eq major-mode 'jcgs/org-journal-mode)
    (jcgs/org-journal-mode))
  (if no-blank-lines
      (org-datetree-find-date-create (list month day year))
    (with-surrounding-blank-lines
     (org-datetree-find-date-create (list month day year)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; logged shell commands ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jcgs/org-journal-recent-shell-commands nil
  "Recent shell commands.
Used while selecting a shell command to log.")

(defun jcgs/org-journal-read-recent-shell-command (prompt)
  "Select a recent shell command from the history.
Argument PROMPT is passed to `read-from-minibuffer'."
  (setq jcgs/org-journal-recent-shell-commands (ring-elements comint-input-ring))
  (when nil
    (mapcar (function
	     (lambda (str)
	       (set-text-properties 0 (length str) nil str)
	       str))
	    jcgs/org-journal-recent-shell-commands)
    (message "recent-commands are now %S" jcgs/org-journal-recent-shell-commands))
  (read-from-minibuffer prompt
			(car jcgs/org-journal-recent-shell-commands)
			nil		; keymap
			nil		; read
			'jcgs/org-journal-recent-shell-commands
			))

(defun jcgs/org-journal-recent-shell-command (command)
  "Record a recent shell COMMAND in your work log.
For use from the comint (shell) buffer."
  (interactive
   (list
    (jcgs/org-journal-read-recent-shell-command "Record shell command: ")))
  (save-window-excursion
    (save-excursion
      (tracking-open-date (tracking-format-current-date))
      (goto-char (point-max))
      (insert "\n        $ " command "\n\n"))))

(require 'shell)			; for shell-mode-map
(define-key shell-mode-map (kbd "C-<return>") 'jcgs/org-journal-recent-shell-command)

;;;;;;;;;;;;;;;;
;; Major mode ;;
;;;;;;;;;;;;;;;;

(define-derived-mode jcgs/org-journal-mode org-mode
  "Org-Journal"
  "Major mode for making notes on what I've done while developing software.
Organizes the log hierarchically by date (day, month, year)."
  (make-local-variable 'org-archive-location)
  (setq org-archive-location "~/work-org/archive/%s::"))

(defun jcgs/org-journal-mode-return ()
  "If on a shell command, re-run it."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "^\\s-+\\([^:]+:\\)?[^|]+\\$ \\(.+\\)$"))
      (let* ((shell-buffer-name-from-line (match-string-no-properties 1))
	     (shell-buffer-name (if shell-buffer-name-from-line
				    (substring shell-buffer-name-from-line 0 -1)
				  (read-buffer "Execute command in buffer: ")))
	     (command (match-string-no-properties 2)))
	(switch-to-buffer shell-buffer-name)
	(goto-char (point-max))
	(insert command)
	(comint-send-string (get-buffer-process (current-buffer))
			    (concat command "\n")))
    (call-interactively 'org-ctrl-c-ret)))

(define-key jcgs/org-journal-mode-map "\C-c\C-d" 'jcgs/org-journal-open-date)
(define-key jcgs/org-journal-mode-map "\C-c<return>" 'jcgs/org-journal-mode-return)

(add-to-list 'auto-mode-alist (cons "work.org-log" 'jcgs/org-journal-mode))
(add-to-list 'auto-mode-alist (cons "hackery.org-log" 'jcgs/org-journal-mode))
(dolist (org-journal-file jcgs/org-journal-files)
  (add-to-list 'auto-mode-alist
	       (cons (file-name-nondirectory org-journal-file) 'jcgs/org-journal-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion from old format ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-dates ()
  "Convert the dates in my journal file.
From my old format to org-datetree's format."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\*\\*\\* Date \\(....\\)_\\(..\\)_\\(..\\)" (point-max) t)
    (let* ((year (string-to-int (match-string 1)))
	   (month (string-to-int (match-string 2)))
	   (day (string-to-int (match-string 3)))
	   (date (encode-time 0 0 0 day month year))
	   (date-string (current-time-string date))
	   (decoded (decode-time date))
	   (dow (format-time-string "%A" date)))
      (replace-match (format "*** %04d-%02d-%02d %s" year month day dow))))
  (goto-char (point-min))
  (while (re-search-forward "^\\*\\* Month \\(....\\)_\\(..\\)" (point-max) t)
    (let* ((year (string-to-int (match-string 1)))
	   (month (string-to-int (match-string 2)))
	   (date (encode-time 0 0 0 1 month year))
	   (date-string (current-time-string date))
	   (decoded (decode-time date))
	   (mon (format-time-string "%B" date)))
      (replace-match (format "** %04d-%02d %s" year month mon))))
  (goto-char (point-min))
  (while (re-search-forward "^\\* Year \\(....\\)" (point-max) t)
    (replace-match "* \\1")))

(provide 'org-jcgs-journal)

;;; org-jcgs-journal.el ends here
