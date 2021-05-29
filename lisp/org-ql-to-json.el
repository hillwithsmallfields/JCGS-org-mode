;;; org-ql-to-json.el --- run an org-ql query and output the results as JSON  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: outlines, data, matching

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

;; Runs an org-ql query and outputs the result as JSON for ingestion
;; into my personal dashboard system.

;;; Code:

(defun jcgs/org-ql-file (descr)
  "Return the name of the file the entry DESCR is from."
  (buffer-file-name
   (marker-buffer
    (plist-get descr :org-marker))))

(defun jcgs/org-ql-path (descr)
  "Return the outline path of DESCR."
  (save-excursion
    (let ((m (plist-get descr :org-marker)))
      (set-buffer (marker-buffer m))
      (goto-char (marker-position m))
      (org-get-outline-path))))

(defun lisp-list-to-json-list (lisp-list)
  "Convert a LISP-LIST to json text."
  (concat "[" (mapconcat (lambda (elt)
                           (set-text-properties 0 (length elt) nil elt)
                           (message "elt is now %s" elt)
                           (prin1-to-string  elt))
                         lisp-list
                         ", ") "]"))


(defconst jcgs/org-ql-json-format
  (concat "{"
          "\"title\": %s,\n"
          "         \"todo-keyword\": \"%s\",\n"
          "%s         \"tags\": %s,\n"
          "         \"file\": \"%s\",\n"
          "         \"position-in-file\": %d,\n"
          "         \"path\": %s}")
  "Format for org ql results.")

(defun jcgs/org-ql-views-to-json (json-file views)
  "Write into JSON-FILE the result of querying VIEWS."
  (let ((query-results (mapcar (lambda (view)
                                 (cons view
                                       (let ((descr (cdr (assoc view org-ql-views))))
                                         (org-ql-select
                                           (plist-get descr :buffers-files)
                                           (plist-get descr :query)
                                           :sort (plist-get view :sort)
                                           :action 'element-with-markers))))
                               views)))
    (save-excursion
      (find-file json-file)
      (erase-buffer)
      (insert "{\n")
      (insert (mapconcat (lambda (view)
                           (concat (format "    \"%s\": [\n        " (car view))
                                   (mapconcat (lambda (result)
                                                (let* ((data (plist-get result 'headline))
                                                       (file (jcgs/org-ql-file data)))
                                                  (format jcgs/org-ql-json-format
                                                          (prin1-to-string (plist-get data :raw-value))
                                                          (plist-get data :todo-keyword)
                                                          (let ((priority  (plist-get data :priority)))
                                                            (if priority
                                                                (format "         \"priority\": \"%d\",\n" priority)
                                                              ""))
                                                          (lisp-list-to-json-list (plist-get data :tags))
                                                          file
                                                          (marker-position (plist-get data :org-marker))
                                                          (lisp-list-to-json-list (cons (capitalize
                                                                                         (file-name-sans-extension
                                                                                          (file-name-nondirectory file)))
                                                                                        (jcgs/org-ql-path data))))))
                                              (cdr view)
                                              ",\n        ")
                                   "]"))
                         query-results
                         ",\n"))
      (insert "}\n")
      (basic-save-buffer))))

(provide 'org-ql-to-json)
;;; org-ql-to-json.el ends here
