;;; org-shell-command-records.el --- Keep a journal of shell commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  John Sturdy

;; Author: John Sturdy <john.sturdy@grapeshot.com>
;; Keywords: convenience, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on my hierarchical journalling, but for commands rather than
;; natural language text.  This has often rescued me when I've lost
;; shell command histories (for example by other shells overwriting
;; them).

;;; Code:

(defvar jcgs/shell-mode-recorded-commands nil
  "For deduplication.
Not persistent between sessions, and reset each day.")

(make-variable-buffer-local 'jcgs/shell-mode-recorded-commands)

(defconst jcgs/shell-command-record-regexp
  "^ +\\([^:]+\\):\\([^$]+\\)\\$ \\(.+\\)$")

(defun jcgs/shell-command-records-get-buffer ()
  "Get the buffer of the current line, and put it on the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at jcgs/shell-command-record-regexp)
        (kill-new (match-string-no-properties 1))
      (error "Not on a shell history line"))))

(defun jcgs/shell-command-records-get-command ()
  "Get the command of the current line, and put it on the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at jcgs/shell-command-record-regexp)
        (kill-new (match-string-no-properties 3))
      (error "Not on a shell history line"))))

(defun jcgs/shell-command-move-to-command ()
  "Move to the command on the current line."
  (interactive)
  (let ((position (save-excursion
                    (beginning-of-line 1)
                    (if (looking-at jcgs/shell-command-record-regexp)
                        (match-beginning 3)
                      nil))))
    (if position
        (goto-char position)
      (error "Not on a history line"))))

(defun jcgs/shell-command-records-get-directory ()
  "Get the directory of the current line, and put it on the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (if (looking-at jcgs/shell-command-record-regexp)
        (kill-new (match-string-no-properties 2))
      (error "Not on a shell history line"))))

(defun jcgs/shell-command-records-set-buffer-other-window ()
  "Select the buffer of the current line, in another window."
  (interactive)
  (switch-to-buffer-other-window
   (save-excursion
     (beginning-of-line 1)
     (if (looking-at jcgs/shell-command-record-regexp)
         (match-string-no-properties 1)
       (error "Not on a shell history line")))))

(defun jcgs/shell-command-records-insert-command-other-window ()
  "Insert the command on this line into the other window."
  (interactive)
  (let ((command (save-excursion
                   (beginning-of-line 1)
                   (if (looking-at jcgs/shell-command-record-regexp)
                       (match-string-no-properties 3)
                     (error "Not on a shell history line")))))
    (other-window 1)
    (insert command)))

(defun jcgs/shell-command-records-run-in-nearest-shell (&optional no-confirm)
  "Run the command on this line, in the most recent shell buffer.
Ask for confirmation of the buffer, except with prefix arg NO-CONFIRM."
  (interactive "P")
  (let* ((command (save-excursion
                    (beginning-of-line 1)
                    (if (looking-at jcgs/shell-command-record-regexp)
                        (match-string-no-properties 3)
                      (error "Not on a shell history line"))))
         (shell-buffer (catch 'got-one
                         (dolist (buffer (buffer-list))
                           (set-buffer buffer)
                           (when (eq major-mode 'shell-mode)
                             (throw 'got-one buffer)))
                         nil)))
    (when (and shell-buffer (or no-confirm (y-or-n-p (format "Run %s in %s? " command shell-buffer))))
      (switch-to-buffer shell-buffer)
      (comint-send-string (get-buffer-process shell-buffer) (concat command "\n")))))

(defun jcgs/shell-command-records-run-in-same-shell (&optional no-confirm)
  "Run the command on this line, in the same shell it was originally run in.
Ask for confirmation of the buffer, except with prefix arg NO-CONFIRM."
  (interactive "P")
  (let* ((buffer-directory-command (save-excursion
                                     (beginning-of-line 1)
                                     (if (looking-at jcgs/shell-command-record-regexp)
                                         (list
                                          (match-string-no-properties 1)
                                          (match-string-no-properties 2)
                                          (match-string-no-properties 3))
                                       (error "Not on a shell history line"))))
         (shell-buffer (nth 0 buffer-directory-command))
         (directory (nth 1 buffer-directory-command))
         (command (nth 2 buffer-directory-command)))
    (when (and shell-buffer (or no-confirm (y-or-n-p (format "Run %s in %s? (dir %s)" command shell-buffer directory))))
      (switch-to-buffer shell-buffer)
      (unless (equal directory default-directory)
        (comint-send-string (get-buffer-process shell-buffer)
                            (concat "cd " directory "\n")))
      (comint-send-string (get-buffer-process shell-buffer) (concat command "\n")))))

(defun jcgs/shell-command-records-run-in-same-shell-with-gdb (&optional no-confirm)
  "Run the command on this line with gdb, in the same shell it was originally run in.
Ask for confirmation of the buffer, except with prefix arg NO-CONFIRM."
  (interactive "P")
  (let* ((buffer-directory-command (save-excursion
                                     (beginning-of-line 1)
                                     (if (looking-at
                                          jcgs/shell-command-record-regexp)
                                         (list
                                          (match-string-no-properties 1)
                                          (match-string-no-properties 2)
                                          (match-string-no-properties 3))
                                       (error "Not on a shell history line"))))
         (shell-buffer (nth 0 buffer-directory-command))
         (directory (nth 1 buffer-directory-command))
         (command-line-parts (split-string (nth 2 buffer-directory-command)))
         (command (car command-line-parts))
         (command-args (mapconcat 'identity
                                  (cons "run" (cdr command-line-parts))
                                  " ")))
    (when (and shell-buffer (or no-confirm
                                (y-or-n-p (format "Run %s in %s? (dir %s)"
                                                  command
                                                  shell-buffer
                                                  directory))))
      (switch-to-buffer shell-buffer)
      (unless (equal directory default-directory)
        (comint-send-string (get-buffer-process shell-buffer)
                            (concat "cd " directory "\n")))
      (kill-new command-args)
      (message "run command %S is on the kill ring" command-args)
      (comint-send-string (get-buffer-process shell-buffer)
                          (concat "gdb " command "\n")))))

(defvar jcgs/shell-command-records-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\M-m" 'jcgs/shell-command-move-to-command)
    (define-key map "b" 'jcgs/shell-command-records-get-buffer)
    (define-key map "c" 'jcgs/shell-command-records-get-command)
    (define-key map "d" 'jcgs/shell-command-records-get-directory)
    (define-key map "g" 'jcgs/shell-command-records-run-in-same-shell-with-gdb)
    (define-key map "B" 'jcgs/shell-command-records-set-buffer-other-window)
    (define-key map "C" 'jcgs/shell-command-records-insert-command-other-window)
    (define-key map "r" 'jcgs/shell-command-records-run-in-nearest-shell)
    (define-key map (kbd "RET") 'jcgs/shell-command-records-run-in-same-shell)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'outline-up-heading)
    map))

(define-derived-mode jcgs/shell-command-records-mode jcgs/org-journal-mode "Shell command records"
  "Major mode for an accumulated list of shell commands.")

(defun jcgs/shell-mode-record-command-in-journal (shell-command)
  "Record SHELL-COMMAND in my journal file."
  (when (and (boundp 'jcgs/shell-mode-accumulated-command-history-file)
	     (stringp jcgs/shell-mode-accumulated-command-history-file)
	     (file-writable-p jcgs/shell-mode-accumulated-command-history-file)
	     (not (string-match "^!\\(!\\|[-0-9]+\\)$" shell-command)))
    (set-text-properties 0 (length shell-command)
			 nil shell-command)
    (let ((cwd default-directory)
	  (buffer (buffer-name)))
      (save-window-excursion
	(find-file jcgs/shell-mode-accumulated-command-history-file)
	(goto-char (point-max))
	(when (let ((date (decode-time)))
		(jcgs/org-journal-open-date (nth 5 date) (nth 4 date) (nth 3 date) t
                                            'jcgs/shell-command-records-mode))
          ;; reset the de-duplication for the new day
	  (setq jcgs/shell-mode-recorded-commands nil))
	(goto-char (point-max))
	(dolist (command-line (split-string shell-command "\n" t))
	  ;; todo: fix this filtering, it doesn't seem to be working
	  ;; todo: filter out history commands that don't alter anything
	  ;; todo: filter out "history" commands
	  (unless (member command-line jcgs/shell-mode-recorded-commands)
	    (push command-line jcgs/shell-mode-recorded-commands)
	    ;; (message "Got shell command %S to record as happening in buffer %S with default directory %s." command-line buffer cwd)
	    (let ((was-cd (string-match "^cd\\(?:\\s-+\\(.+\\)\\)?" command-line)))
	      (when was-cd
		(let ((cd-arg (match-string 1 command-line)))
		  ;; (message "old cwd %S" cwd)
		  (setq cwd (if cd-arg
				(expand-file-name cd-arg cwd) ; todo: this area is not behaving as I expected, cwd is already one too high
			      (getenv "HOME")))
		  ;; (message "Command was cd, with arg %S; cwd now %S" cd-arg cwd)
		  ))))
	  (delete-blank-lines)
	  (insert "    " buffer ":" cwd "$ " command-line)
	  (basic-save-buffer)
	  (bury-buffer))))))

(provide 'org-shell-command-records)

;;; org-shell-command-records.el ends here
