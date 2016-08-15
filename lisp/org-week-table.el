;;;; make a table with main points for the week
;;; Time-stamp: <2016-08-15 06:39:03 jcgs>

(defun week-table-line (titles char divider)
  "Make a table line for TITLES with CHAR and DIVIDER."
  (concat divider
	  (mapconcat 'identity
		     (mapcar
		      (lambda (title)
			(make-string (+ 2 (length title))
				     char))
		      columns)
		     divider)
	  divider
	  "\n"))

(defun week-table-insert-new ()
  "Insert a week table."
  (interactive)
  (let* ((days '("Sunday" "Monday" "Tuesday" "Wednesday"
		 "Thursday" "Friday" "Saturday"))
	 (columns '("Day" "Morning" "Transport" "Evening"))
	 (row-separator (week-table-line days ?- "+")))
    (insert "*This week\n\n"
	    "   " row-separator)
    (insert "   " (concat "|" (mapconcat 'identity
				   columns
				   "|") "|"))
    (insert row-separator)
    (dolist (day days)
      (insert "   | " day "|")
      (insert row-separator))
    ))
