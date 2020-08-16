;;; org-rearrange-mode.el --- rearrange org text with less use of modifiers  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, outlines

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

;; Convenience functions and bindings for rearranging org-mode text

;;; Code:

;;;; Handle tags when moving trees around

;;; The tags are stored into a text property, and can be restored from that.

(defun org-tags-to-text-property (beg end)
  "Copy the org tags to a text property between BEG and END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when (org-at-heading-p)
        (put-text-property (line-beginning-position) (line-end-position)
                           'org-copied-tags
                           (org-get-tags-at)))
      (beginning-of-line 2))))

(defun org-tags-to-text-property-buffer ()
  "Copy the org tags to a text property throughout the buffer."
  (interactive)
  (org-tags-to-text-property (point-min) (point-max)))

(defun text-property-to-org-tags (beg end)
  "Set the org tags from a text property between BEG and END."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (when (org-at-heading-p)
        (let ((line-beginning (line-beginning-position))
              (line-end (line-end-position))
              (tags-already-in-org (org-get-tags)))
          (dolist (incoming-tag (get-text-property (point) 'org-copied-tags))
            (message "on line %S; incoming tag %S; already got %S" (buffer-substring-no-properties (line-beginning-position) (line-end-position)) incoming-tag tags-already-in-org)
            (if (member incoming-tag tags-already-in-org)
                (message "already got %S" incoming-tag)
              (message "Adding tag %S" incoming-tag)
              (org-change-tag-in-region line-beginning (save-excursion (beginning-of-line 2) (point)) incoming-tag nil)))))
      (beginning-of-line 2))))

(defun text-property-to-org-tags-buffer ()
  "Set the org tags from a text property throughout the buffer."
  (interactive)
  (text-property-to-org-tags (point-min) (point-max))
  (org-set-tags t t))

(let ((keymap (make-keymap)))
  ;; movements
  (define-key keymap "n" 'next-line)
  (define-key keymap "p" 'previous-line)
  (define-key keymap "u" 'outline-up-heading)
  (define-key keymap "f" 'org-forward-heading-same-level)
  (define-key keymap "b" 'org-backward-heading-same-level)
  (define-key keymap "<" 'beginning-of-buffer)
  (define-key keymap ">" 'end-of-buffer)
  ;; position
  (define-key keymap "N" 'org-move-subtree-down)
  (define-key keymap "P" 'org-move-subtree-up)
  ;; depth
  (define-key keymap "U" 'org-promote-subtree)
  (define-key keymap "D" 'org-demote-subtree)
  ;; kill, copy, yank
  (define-key keymap "K" 'org-cut-subtree)
  (define-key keymap "W" 'org-copy-subtree)
  (define-key keymap "Y" 'org-paste-subtree)
  ;;
  (define-key keymap "s" 'org-tags-to-text-property-buffer)
  (define-key keymap "R" 'text-property-to-org-tags-buffer)
  ;; back to normal editing
  (define-key keymap "q" 'org-mode)
  (setq org-rearrange-mode-map keymap))

(define-derived-mode org-rearrange-mode org-mode "Org rearrangement"
  "Convenience bindings and functions for rearranging org-mode text."
  (interactive))

(provide 'org-rearrange-mode)
;;; org-rearrange-mode.el ends here
