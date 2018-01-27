(defun create-work-temp-file (name-or-suffix)
  "create new file in work dir"
  (interactive "sPlease input file name or suffix : ")
  (progn
    (setq work-dir-name "/home/wrydz/Work/sswk/worklog")
    (cond ((string= "" (string-trim name-or-suffix))
           (setq temp-file-name (get-work-file-name-not-exists work-dir-name "log")))
          ((string= "." (substring name-or-suffix 0 1))
           (setq temp-file-name (get-work-file-name-not-exists work-dir-name (substring name-or-suffix 1))))
          (t
           (setq temp-file-name (concat work-dir-name "/" name-or-suffix))))
    (find-file temp-file-name)
    ))


(defun get-work-file-name-not-exists(work-dir-name suffix)
  "get a file name not exists"
  (do ((index 1 (+ 1 index)))
      ((not (file-exists-p (concat work-dir-name
                                   "/"
                                   (format-time-string "%Y%m%d")
                                   "_temp_"
                                   (int-to-string index)
                                   "."
                                   suffix))) (concat work-dir-name
                                                     "/"
                                                     (format-time-string "%Y%m%d")
                                                     "_temp_"
                                                     (int-to-string index)
                                                     "."
                                                     suffix))))
