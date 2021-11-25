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

(require 'org-linked-tasks)

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

(defun jcgs/org-expected-dates-to-json-file (filename)
  "Write the parcels expected list to FILENAME."
  (save-excursion
    (find-file filename)
    (erase-buffer)
    (insert (jcgs/org-expected-dates-to-json-string))
    (basic-save-buffer)))

(defun jcgs/org-expected-dates-to-json-string ()
  "Create a parcels expected list string."
  (concat "{\n  \"expected\": [\n    "
          (mapconcat (lambda (parcel)
                       (format "[\"%s\", \"%s\"]" (car parcel) (cadr parcel)))
                     (jcgs/org-get-expected-dates)
                     ",\n    ")
          "\n  ]\n}\n"))

(defun jcgs/org-shopping-list-state-change-function ()
  "Mark this item as having arrived, if its state becomes done.
Pick up the price and category when it is ordered."
  (jcgs/org-maybe-chain-task)
  (cond ((and (org-entry-is-done-p)
              (null (org-entry-get (point) "arrived" nil)))
         (org-entry-put (point) "arrived" (format-time-string "%F"))
         (cond ((y-or-n-p "Add to item inventory? ")
                (storage-add-item (org-get-heading t t t t)
                                  (org-entry-get (point) "category")
                                  (org-entry-get (point) "price")
                                  (org-entry-get (point) "supplier")))
               ((y-or-n-p "Add to parts inventory? ")
                (storage-add-part (org-get-heading t t t t)
                                  (org-entry-get (point) "category")
                                  (org-entry-get (point) "price")))))
        ((equal org-state "ORDERED")
         (when (y-or-n-p "Add to expenditure file? ")
           (finances-read-completions)
           (let* ((category (completing-read "Category: " category-completions))
                  (supplier-and-price (finances-enter-from-shopping-list nil
                                                                         (org-get-heading t t t t)
                                                                         category))
                  (supplier (car supplier-and-price))
                  (price (cdr supplier-and-price)))
             (org-entry-put (point) "category" category)
             (org-entry-put (point) "supplier" supplier)
             (org-entry-put (point) "price" (format "%.2f" price))
             (when (y-or-n-p "Add postage? ")
               (finances-enter-from-shopping-list supplier "" "Postage"))))
         (call-interactively 'jcgs/org-expect-parcel))))

(setq jcgs/org-shopping-list-mode-map
      (if (boundp 'jcgs/org-shopping-list-mode-map)
          jcgs/org-shopping-list-mode-map
        (make-sparse-keymap)))

(define-key jcgs/org-shopping-list-mode-map "\C-c\C-e" 'jcgs/org-expect-parcel)

(define-derived-mode jcgs/org-shopping-list-mode org-mode
  "Shopping list" "Major mode for shopping lists."
  (make-local-variable 'org-after-todo-state-change-hook)
  (add-hook 'org-after-todo-state-change-hook
            'jcgs/org-shopping-list-state-change-function))

(defun jcgs/org-expect-parcel (date)
  "Mark the expected arrival DATE of a parcel."
  (interactive "sDate expected: ")
  (unless (string= date "")
    (org-entry-put (point) "expected" date)))

(defun jcgs/org-buy-for-project-non-blocking (item)
  "Add ITEM to the shopping list."
  (interactive "sItem: ")
  (save-excursion
    (find-file (substitute-in-file-name "$ORG/shopping.org"))
    (save-excursion
      (goto-char (point-min))
      (unless (org-find-exact-headline-in-buffer "Project parts")
        (let ((imminent (org-find-exact-headline-in-buffer "Imminent")))
          (when imminent
            (goto-char imminent)
            (org-forward-heading-same-level 1)))
        (org-insert-heading nil nil t)
        (insert "Project parts"))
      (goto-char (org-find-exact-headline-in-buffer "Project parts"))
      (if (org-goto-first-child)
          (org-insert-heading 16)
        (beginning-of-line 2)
        (org-insert-subheading 16))
      (insert item)
      (org-todo "BUY")
      (org-id-get nil t))))

(add-to-list 'auto-mode-alist (cons "shopping.org" 'jcgs/org-shopping-list-mode))

(provide 'org-parcels)
;;; org-parcels.el ends here
