;;; org-jcgs-journal.el --- keep track of things I've done
;; Based on my earlier tracked-compile.el

;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2024, 2025  John Sturdy

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

(require 'cl)

(defvar jcgs/org-journals nil
  "Alist of journal names to places where they are kept.
If the place is a directory, the files in it are named by year.")

(defun jcgs/org-journals-add-journal (name place)
  "Add journal NAME at PLACE if it exists."
  (when (file-exists-p place)
    (cl-pushnew (cons name place)
                jcgs/org-journals)))

(jcgs/org-journals-add-journal
 "incoming" (substitute-in-file-name "$SYNCED/journal/incoming.journal"))

(jcgs/org-journals-add-journal
 "hackery" (substitute-in-file-name "$SYNCED/journal/hacking.journal"))

(jcgs/org-journals-add-journal
 "work" (expand-file-name "~/work-org/work.journal"))

(jcgs/org-journals-add-journal
 "shell-commands" (expand-file-name "~/work-org/shell-command-history.org"))

;;;;;;;;;;;;;;;;;;;;;;;
;; Date-based filing ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-journal-find-ymd (year month day)
  "Find YEAR MONTH DAY in the current buffer.
Return the search result."
  (goto-char (point-min))
  (search-forward (format "*** %04d-%02d-%02d"
			  year month day)
		  (point-max) t))

(defun read-ymd-as-list (&optional prompt)
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
  `(let* ((change-start nil)
	  (change-end nil)
	  (after-change-functions (cons (lambda (begin end len)
					  (when (or (not change-start)
						    (< begin change-start))
					    (setq change-start begin))
					  (when (or (not change-end)
						    (> end change-end))
					    (setq change-end end)))
					after-change-functions))
	  (old-tick (buffer-modified-tick))
	  (result (progn
		    ,@body)))
     (when (not (= (buffer-modified-tick) old-tick))
       (save-excursion
	 (goto-char change-end)
	 (delete-blank-lines)
	 (delete-blank-lines)
	 (open-line 1)
	 (goto-char change-start)
	 (delete-blank-lines)
	 (delete-blank-lines)
	 (open-line 1)))
     result))

(defun jcgs/org-journal-open-date (&optional year month day no-blank-lines recording-buffer-mode)
  "Ensure there is an open work-log record for YEAR MONTH DAY.
If they are not given, use the current time.
With optional NO-BLANK-LINES, don't surround it with blank lines.
With optional RECORDING-BUFFER-MODE, use that mode for the recording buffer;
otherwise, `jcgs/org-journal-mode' is used.
Returns whether we created a new entry."
  (interactive (read-ymd-as-list))
  ;; we must be in something based on org-mode for some org-mode
  ;; functions we use to work; we mustn't call the mode setup
  ;; function each time, because it kills all local variables
  (when (and (null year)
	     (null month)
	     (null day))
    (let ((now (decode-time)))
      (setq year (nth 5 now)
	    month (nth 4 now)
	    day (nth 3 now))))
  (unless recording-buffer-mode
    (setq recording-buffer-mode 'jcgs/org-journal-mode))
  (unless (eq major-mode recording-buffer-mode)
    (funcall recording-buffer-mode))
  (let ((already-open (save-excursion
			(jcgs/org-journal-find-ymd year month day))))
    (if no-blank-lines
	(org-datetree-find-date-create (list month day year))
      (with-surrounding-blank-lines
       (org-datetree-find-date-create (list month day year))))
    (goto-char (point-min))
    (jcgs/org-journal-find-ymd year month day)
    (unless already-open
      ;; fills in jcgs/org-journal-day-boilerplate:
      (load-file (substitute-in-file-name "$SYNCED/templates/journal-daily-boilerplate.el"))
      jcgs/org-journal-day-boilerplate
      (let ((boilerplate (assoc (file-name-nondirectory (buffer-file-name))
                                jcgs/org-journal-day-boilerplate)))
        (when boilerplate
          (save-excursion
            (goto-char (point-max))
            (re-search-backward ".+$" (point-min) t)
            (end-of-line)
            (insert (cdr boilerplate))))))
    (beginning-of-line (if no-blank-lines 2 3))
    (not already-open)))

(defun jcgs/org-journal-read-journal-name (&optional prompt)
  "Read a journal name, using completion, with PROMPT."
  (completing-read (or prompt "Journal: ")
                   jcgs/org-journals
                   nil t))

(defun jcgs/org-journal-open-journal (journal-name &optional year)
  "Open the journal called JOURNAL-NAME, in YEAR."
  (let ((journal-file (cdr (assoc journal-name jcgs/org-journals))))
    (when (file-directory-p journal-file)
      (setq journal-file (expand-file-name
                          (concat (number-to-string
                                   (or year (nth 5 (decode-time))))
                                  ".org")
                          journal-file)))
    (find-file journal-file)))

(defun jcgs/org-journal-open-journal-at-date (journal &optional year month day no-blank-lines recording-buffer-mode)
  "Ensure there is an open work-log record in JOURNAL for YEAR MONTH DAY.
If they are not given, use the current time.
With optional NO-BLANK-LINES, don't surround it with blank lines.
With optional RECORDING-BUFFER-MODE, use that mode for the recording buffer;
otherwise, `jcgs/org-journal-mode' is used.
Returns whether we created a new entry."
  (interactive
   (let* ((journal (jcgs/org-journal-read-journal-name "Journal: "))
          (ymd (read-ymd-as-list)))
     (cons journal ymd)))
  (jcgs/org-journal-open-journal journal year)
      (jcgs/org-journal-open-date year month day no-blank-lines recording-buffer-mode))

(defun jcgs/org/journal-set-journal-date-property (journal property value
                                                           &optional year month day)
  "In JOURNAL set PROPERTY to VALUE for a day.
Use YEAR MONTH DAY if given, otherwise the current date.
Intended for importing Quantified Self data such as weight, calories,
and financial transactions."
  (save-window-excursion
    (save-excursion
      (jcgs/org-journal-open-journal-at-date journal year month day)
      (org-set-property property value))))

(defun jcgs/org/journal-set-journal-isodate-property (journal isodate property value)
  "In JOURNAL on ISODATE set PROPERTY to VALUE for a day.
Intended for importing Quantified Self data such as weight, calories,
and financial transactions."
  (let ((matched (string-match
                  "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" isodate)))
    (jcgs/org/journal-set-journal-date-property
     journal property value
     (string-to-number (match-string 1 isodate))
     (string-to-number (match-string 2 isodate))
     (string-to-number (match-string 3 isodate)))))

(defun days-back (dayname)
  "Show how many days back the latest DAYNAME was."
  (mod (- (nth 6 (decode-time))
          (cdr (assoc (downcase dayname) parse-time-weekdays)))
       7))

(defun jcgs/org/journal-recent-day (dayname)
  "Return the year-month-day for the most recent DAYNAME."
  (let ((that-day (decode-time
                   (days-to-time
                    (- (time-to-days (current-time))
                       (time-to-days (encode-time 0 0 0 0 1 1970))
                       (days-back dayname))))))
    (list (nth 5 that-day) (nth 4 that-day) (nth 3 that-day))))

(defun jcgs/org/journal-open-recent-day (journal dayname)
  "Open the most recent day called DAYNAME.
Argument JOURNAL is the journal to use."
  (interactive "fJournal: \nsDay: ")
  (let ((day (jcgs/org/journal-recent-day dayname)))
    (jcgs/org-journal-open-journal-at-date journal
                                           (nth 0 day)
                                           (nth 1 day)
                                           (nth 2 day))))

;;;;;;;;;;;;;;;;;;;;;
;; re-use headings ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar jcgs/org-reuse-headings-list nil
  "The headings we have seen in this buffer.")

(defun jcgs/org-reuse-headings ()
  "Read a heading for the current level, with completion and history.
Offers all headings that start the same as the current line up to point."
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (unless (looking-at "\\*+")
      (error "Not on a heading line")))
  (setq jcgs/org-reuse-headings-list nil)
  (let ((like-this (buffer-substring-no-properties
                    (line-beginning-position)
                    (point))))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward like-this (point-max) t)
        (let ((line-body (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
          (unless (member line-body jcgs/org-reuse-headings-list)
            (push line-body jcgs/org-reuse-headings-list)))))
    (let ((chosen (completing-read "Heading: "
                                   jcgs/org-reuse-headings-list
                                   nil  ; predicate
                                   nil  ; require-match
                                   like-this ; initial-input
                                   'jcgs/org-reuse-headings-list ; history
                                   )))
      (delete-region (line-beginning-position)
                     (line-end-position))
      (insert chosen))))

(defun jcgs/org-previous-identical-heading ()
  "Jump back to the previous heading with the same text as the current one."
  (interactive)
  (unless (org-at-heading-p)
    (outline-previous-heading))
  (unless (search-backward
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position))
           (point-min) t)
    (error "No more previous occurrences")))

(defun jcgs/org-next-identical-heading ()
  "Jump back to the next heading with the same text as the current one."
  (interactive)
  (unless (search-forward
           (save-excursion
             (unless (org-at-heading-p)
               (outline-previous-heading))
             (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position)))
           (point-max) t)
    (error "No more next occurrences")))

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
  (setq org-archive-location "~/work-org/archive/%s::")
  ;; (jcgs/org-journal-last-day)
  )

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
(define-key jcgs/org-journal-mode-map "\C-c\C-l" 'jcgs/org-journal-last-day)
(define-key jcgs/org-journal-mode-map "\C-c\M-p" 'jcgs/org-reuse-headings)
(define-key jcgs/org-journal-mode-map "\C-c\C-\M-p" 'jcgs/org-previous-identical-heading)
(define-key jcgs/org-journal-mode-map "\C-c\C-\M-n" 'jcgs/org-next-identical-heading)
(when (fboundp 'standup-report-add-to-yesterday)
  (define-key jcgs/org-journal-mode-map "\C-c\C-y" 'standup-report-add-to-yesterday))

(add-to-list 'auto-mode-alist (cons "\\.journal" 'jcgs/org-journal-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion from old format ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-dates ()
  "Convert the dates in my journal file.
From my old format to org-datetree's format."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\*\\*\\* Date \\(....\\)_\\(..\\)_\\(..\\)" (point-max) t)
    (let* ((year (string-to-number (match-string 1)))
	   (month (string-to-number (match-string 2)))
	   (day (string-to-number (match-string 3)))
	   (date (encode-time 0 0 0 day month year))
	   (date-string (current-time-string date))
	   (decoded (decode-time date))
	   (dow (format-time-string "%A" date)))
      (replace-match (format "*** %04d-%02d-%02d %s" year month day dow))))
  (goto-char (point-min))
  (while (re-search-forward "^\\*\\* Month \\(....\\)_\\(..\\)" (point-max) t)
    (let* ((year (string-to-number (match-string 1)))
	   (month (string-to-number (match-string 2)))
	   (date (encode-time 0 0 0 1 month year))
	   (date-string (current-time-string date))
	   (decoded (decode-time date))
	   (mon (format-time-string "%B" date)))
      (replace-match (format "** %04d-%02d %s" year month mon))))
  (goto-char (point-min))
  (while (re-search-forward "^\\* Year \\(....\\)" (point-max) t)
    (replace-match "* \\1")))

(defun jcgs/org-journal-last-day (&optional month)
  "Show just the last day of the journal in this buffer.
With optional argument MONTH, show the last month."
  (interactive "P")
  (outline-hide-sublevels 1)
  (dotimes (i (if month 1 2))
    (goto-char (point-max))
    (outline-back-to-heading)
    (outline-show-children))
  (goto-char (point-max))
  (outline-back-to-heading)
  (outline-show-subtree)
  (goto-char (point-max)))

(provide 'org-jcgs-journal)

;;; org-jcgs-journal.el ends here
 
