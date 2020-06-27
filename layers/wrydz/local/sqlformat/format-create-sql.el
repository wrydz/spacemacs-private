(defun format-single-create-sql (create-sql-content)
  "format one create sql"
  (progn
    (with-temp-buffer
      (insert (replace-regexp-in-string "\n" "" create-sql-content))
      (goto-char (point-min))
      (search-forward-regexp
       ))))
