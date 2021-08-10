;;; org-parcels.el --- track expected parcel arrivals  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, calendar, outlines

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

;; Handle :expected: and :arrived: properties in the property drawers
;; of my shopping list.

;;; Code:

(defun jcgs/org-get-expected-dates (&optional earliest latest display)
  "Get the expected arrival dates in my shopping list.
Optional argument EARLIEST gives the earliest date to include.
Optional argument LATEST gives the latest date to include.
Optional argument DISPLAY says to display result, as if interactive."
  (interactive "sEarliest date: \nsLatest date: ")
  (when (string= earliest "") (setq earliest nil))
  (when (string= latest "") (setq latest nil))
  (let ((dates
         (sort
         (delete nil
          (save-excursion
            (find-file (substitute-in-file-name "$ORG/shopping.org"))
            (org-map-entries
             (lambda ()
               (let ((parts (org-heading-components))
                     (date (org-entry-get (point) "expected")))
                 (if (and date
                          (or (null earliest)
                              (string> date earliest)
                              (string= date earliest))
                          (or (null latest)
                              (string< date latest)
                              (string= date latest))
                          (org-entry-is-todo-p))
                     (list date (nth 4 parts) (point))
                   nil))))))
          (lambda (a b) (string< (car a) (car b ))))))
    (when (or display (interactive-p))
      (with-output-to-temp-buffer "*Expected dates*"
        (dolist (d dates)
          (princ (format "%s: %s\n" (car d) (cadr d))))
        ))
    dates))

(defun jcgs/org-shopping-list-state-change-function ()
  "Mark this item as having arrived, if its state becomes done."
  (when (and (org-entry-is-done-p)
             (null (org-entry-get (point) "arrived" nil)))
    (org-entry-put (point) "arrived" (format-time-string "%F"))))

(setq jcgs/org-shopping-list-mode-map
      (if (boundp 'jcgs/org-shopping-list-mode-map)
          jcgs/org-shopping-list-mode-map
        (make-sparse-keymap)))

(define-key jcgs/org-shopping-list-mode-map "\C-c\C-e" 'jcgs/org-expect-parcel)

(define-derived-mode jcgs/org-shopping-list-mode org-mode
  "Shopping list" "Major mode for shopping lists."
  (add-hook 'org-after-todo-state-change-hook
            'jcgs/org-shopping-list-state-change-function))

(defun jcgs/org-expect-parcel (date)
  "Mark the expected arrival DATE of a parcel."
  (interactive "sDate expected: ")
  (org-entry-put (point) "expected" date))

(provide 'org-parcels)
;;; org-parcels.el ends here
