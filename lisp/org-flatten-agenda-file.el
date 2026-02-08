;; Flatten an agenda file

(defun jcgs/org-propagate-tags()
  "Propagate tags downwards in this buffer."
  (interactive)
  (goto-char (point-min))
  (while (and (forward-line) (not (eobp)))
    (when (org-at-heading-p)
      (let ((old (org-get-heading)))
        (org-set-tags (org-get-tags))
        (message "%s --> %s" old (org-get-heading))))))
