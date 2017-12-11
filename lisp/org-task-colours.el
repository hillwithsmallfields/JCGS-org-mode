;;;; Switch colour themes
;;; Time-stamp: <2017-12-11 15:47:46 jcgs>

;; Copyright (C) 2015, 2016, 2017  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience, tools

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

;;; Use nicer colour themes when I'm clocked in to some task, to
;;; encourage me to be clocked in more of the time.

;; (add-lispdir "$GATHERED/emacs/color-theme")

;; (require 'color-theme)

;;; Code:

(require 'custom)			; for custom-theme-enabled-p

(defvar jcgs/org-dull-theme 'light-blue
  "The theme to use for making things look dull.
This is for use when not clocked into any task, to remind me to
clock in as much as possible.")

(defvar jcgs/org-nice-theme 'leuven	; or whiteboard
  "The theme to use for making things look nicer.
This is for use when clocked into a task, to encourage me to
clock in as much as possible.
If nil, it just reverts to the default appearance.")

(defun jcgs/org-dull-appearance ()
  "Make Emacs look dull.
This is for use when not clocked into any task, to remind me to
clock in as much as possible."
  (interactive)
  (when (custom-theme-enabled-p jcgs/org-nice-theme)
    (disable-theme jcgs/org-nice-theme))
  (when jcgs/org-dull-theme
    (if (custom-theme-enabled-p jcgs/org-dull-theme)
	(enable-theme jcgs/org-dull-theme)
      (load-theme jcgs/org-dull-theme)))
  (when (fboundp 'jcgs/set-normal-font-size)
    (jcgs/set-normal-font-size)))

(defun jcgs/org-nice-appearance ()
  "Make Emacs look nice.
This is for use when clocked into a task, to encourage me to
clock in as much as possible."
  (interactive)
  (when (custom-theme-enabled-p jcgs/org-dull-theme)
    (disable-theme jcgs/org-dull-theme))
  (when jcgs/org-nice-theme
    (if (custom-theme-enabled-p jcgs/org-nice-theme)
	(enable-theme jcgs/org-nice-theme)
      (load-theme jcgs/org-nice-theme)))
  (when (fboundp 'jcgs/set-normal-font-size)
    (jcgs/set-normal-font-size)))

(provide 'org-task-colours)
