;;; org-mi3.el --- Work with the Most Important Three tasks  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Sturdy

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

;; 

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Most Important Three ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jcgs/org-most-important-3 ()
  "Get the items tagged as the three most important."
  (jcgs/org-tag-list "mi3"))

(defun jcgs/org-mark-as-most-important ()
  "Mark the current item as one of three most important things to do today."
  (let ((already (jcgs/org-most-important-3)))
    (when (>= (length already) 3)
      (error "Already got 3 most important tasks"))
    (org-toggle-tag "mi3" 'on)))

(provide 'org-mi3)
;;; org-mi3.el ends here
