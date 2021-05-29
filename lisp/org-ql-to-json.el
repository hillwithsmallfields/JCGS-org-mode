;;; org-ql-to-json.el --- run an org-ql query and output the results as JSON  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  John Sturdy

;; Author: John Sturdy <jsturdy@ccsl.com>
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

(defun jcgs/org-ql-views-to-json (json-file views)
  "Write into JSON-FILE the result of querying VIEWS."
  (let ((query-results (mapcar (lambda (view)
                                 (cons view
                                       (let ((descr (cdr (assoc view org-ql-views))))
                                         (org-ql-select
                                           (plist-get descr :buffers-files)
                                           (plist-get descr :query)
                                           :sort (plist-get view :sort)))))
                               views)))
    (save-excursion
      (find-file json-file)
      (erase-buffer)
      (insert "{\n")
      (dolist (view query-results)
        (insert (format "    \"%s\": [\n        " (car view))
                (mapconcat (lambda (result)
                             (let ((data (plist-get result 'headline)))
                               (format "{\"title\": \"%s\",\n         \"todo-keyword\": \"%s\",\n%s         \"tags\": [%s]}"
                                       (plist-get data :raw-value)
                                       (plist-get data :todo-keyword)
                                       (let ((priority  (plist-get data :priority)))
                                         (if priority
                                             (format "         \"priority\": \"%d\",\n" priority)
                                           ""))
                                       (mapconcat (lambda (tag) (format "\"%s\"" tag))
                                                  (plist-get data :tags)
                                                  ", "))

                               ))
                           (cdr view)
                           ",\n        ")
                "]\n"))
      (insert "}\n")
      (basic-save-buffer))))

(provide 'org-ql-to-json)
;;; org-ql-to-json.el ends here
