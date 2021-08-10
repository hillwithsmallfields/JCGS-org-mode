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

;; TODO: use org-after-todo-state-change-hook or org-trigger-hook to set the :arrived: property when the state changes to BOUGHT

(provide 'org-parcels)
;;; org-parcels.el ends here
