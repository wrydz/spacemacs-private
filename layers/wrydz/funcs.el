(defun create-work-temp-file (name-or-suffix)
  "create new file in work dir"
  (interactive "sPlease input file name or suffix : ")
  (progn
    (setq work-dir-name "/Users/wrydz/Work/sswk/worklog")
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

(defun screenshot(shot-type)
  ""
  (interactive "sPlease input type of shot a/s/f : ")
  (lower-frame)
  (let ((filename (concat (format-time-string "%Y%m%d%H%M%S")  ".png"))
        (filepath (concat (getenv "HOME")
                          "/.org-images/"
                          (file-name-base (buffer-file-name)))))
    (unless (file-exists-p filepath)
      (mkdir filepath "p"))
    ;; (shell-command (concat "scrot " filepath "/" filename))
    (call-process-shell-command "shutter" nil nil nil nil
                                (concat " -"
                                        shot-type
                                        " -o "
                                        filepath
                                        "/"
                                        filename
                                        " --min_at_startup -e"))
    (insert (concat "[[file:"filepath "/" filename "][capture]]"))
    (org-display-inline-images)
    ))

(defvar wrydz/pandoc-input-list '(commonmark
                                  docbook
                                  docx
                                  epub
                                  haddock
                                  html
                                  json
                                  latex
                                  markdown
                                  markdown_github
                                  markdown_mmd
                                  markdown_phpextra
                                  markdown_strict
                                  mediawiki
                                  native
                                  odt
                                  opml
                                  org
                                  rst
                                  t2t
                                  textile
                                  twiki))

(defvar wrydz/pandoc-output-list '(asciidoc
                                   beamer
                                   commonmark
                                   context
                                   docbook
                                   docbook5
                                   docx
                                   dokuwiki
                                   dzslides
                                   epub
                                   epub3
                                   fb2
                                   haddock
                                   html
                                   html5
                                   icml
                                   json
                                   latex
                                   man
                                   markdown
                                   markdown_github
                                   markdown_mmd
                                   markdown_phpextra
                                   markdown_strict
                                   mediawiki
                                   native
                                   odt
                                   opendocument
                                   opml
                                   org
                                   plain
                                   revealjs
                                   rst
                                   rtf
                                   s5
                                   slideous
                                   slidy
                                   tei
                                   texinfo
                                   textile
                                   zimwiki))

(defun wrydz/pandoc-convert(from-file-name from-type to-file-name to-type)
  (if (and (wrydz/doc-type-exists-p from-type wrydz/pandoc-input-list)
           (wrydz/doc-type-exists-p to-type wrydz/pandoc-output-list))
      (call-process-shell-command "pandoc" nil nil nil nil
                                  (concat " -f "
                                          from-type
                                          " -t "
                                          to-type
                                          " -o "
                                          to-file-name
                                          " "
                                          from-file-name))
    (error "error, not support file type"))
  )


(defun wrydz/doc-type-exists-p(doc-type doc-type-list)
  (setq exists nil)
  (loop for one-type in doc-type-list do
        (if (eql one-type doc-type)
            (setq exists t)))
  exists)


(defun wrydz/pandoc-convert-org-to-docx()
  (interactive)
  (let ((file-name (buffer-name))
        (convert-file-name (concat (file-name-base file-name) ".docx")))
    (save-buffer file-name)
    (wrydz/pandoc-convert file-name 'org convert-file-name 'docx)))


(defun wrydz/netease-music-api-docker-start()
  (interactive)
  (call-process-shell-command "docker" nil nil nil nil
                              "run -d -p 3000:3000 pengxiao/netease-music-api"))


(defun wrydz/replace_macro_to_value(date)
  (interactive "sEnter date: ")
  (let ((start-point-proces (if (use-region-p) (region-beginning) (point-min)))
        (end-poin-procss (if (use-region-p) (region-end) (point-max)))
        (save-point (point)))
    (goto-char start-point-proces)
    (while (and (search-forward-regexp "\\$[a-z0-9_.]+code\\$" nil t)
                (<= (point) end-poin-procss))
      (replace-match "510000"))
    (goto-char start-point-proces)
    (while (and (search-forward-regexp "\\$[a-z0-9_.]+date\\$" nil t)
                (<= (point) end-poin-procss))
      (replace-match (message "%s" date)))
    (goto-char start-point-proces)
    (while (and (search-forward-regexp "\\$" nil t)
                (<= (point) end-poin-procss))
      (backward-char 1)
      (delete-char 1))
  (goto-char save-point)))


