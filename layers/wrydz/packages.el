;;; packages.el --- wrydz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: wrdz <wrydz@debian.debian.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `wrydz-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `wrydz/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `wrydz/pre-init-PACKAGE' and/or
;;   `wrydz/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst wrydz-packages
  '(
    youdao-dictionary
    ;; blog-admin
    org
    org-pomodoro
    (sqlformat :location local)
    ;; (company-english-helper :location local)
    ;; (insert-translated-name :location local)
    ;; (netease-music :location local)
    ;; (sqlformat :fetcher github :repo "steckerhalter/sqlformat.el")
    scala-mode
    sbt-mode
    lsp-scala
    ))

(defun wrydz/init-sqlformat()
  (use-package sqlformat
    :init
    (spacemacs/set-leader-keys "aff" 'sqlformat)
    ))


;; (defun wrydz/init-company-english-helper()
;;   (use-package company-english-helper
;;     :init
;;     ;; (spacemacs/set-leader-keys "aec" 'toggle-company-english-helper)
;;     (global-set-key (kbd "C-c e h") 'toggle-company-english-helper)
;;     ))

;; (defun wrydz/init-insert-translated-name()
;;   (use-package insert-translated-name
;;     :init
;;     ;; (spacemacs/set-leader-keys "aet" 'insert-translated-name-insert)
;;     (global-set-key (kbd "C-c e t") 'insert-translated-name-insert)
;;     ))

;; (defun wrydz/init-netease-music()
;;   (use-package netease-music
;;     :init
;;     (progn
;;       (setq netease-music-username "1_15828651672")
;;       (setq netease-music-password "123456wr")
;;       (setq netease-music-user-id "1442643196")
;;       (setq netease-music-api "http://localhost:3000")
;;       )))

;; (defun wrydz/init-blog-admin()
;;   (use-package blog-admin
;;     :init
;;     (progn
;;       (setq blog-admin-backend-path "~/static-blog")
;;       (setq blog-admin-backend-type 'hexo)
;;       (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
;;       (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
;;       (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
;;       )))

(defun wrydz/init-youdao-dictionary()
  (use-package youdao-dictionary
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "ayy" 'youdao-dictionary-search-at-point)
      (spacemacs/set-leader-keys "ayv" 'youdao-dictionary-play-voice-at-point)
      (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
      (push "*Youdao Dictionary*" popwin:special-display-config))
    :config (setq url-automatic-caching t)))


(defun wrydz/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (zilongshanren/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (zilongshanren/growl-notification "Short Break" "🐝 Ready to Go?" t)))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (zilongshanren/growl-notification "Long Break" " 💪 Ready to Go?" t)))
    ))

(defun wrydz/post-init-org()
  (with-eval-after-load 'org
    (progn
      (org-indent-mode)
      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))
      ;; (setq org-image-actual-width '(600))

      (setq-default org-agenda-dir "~/Documents/org-notes")
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        )


      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "SOMEDAY(s)" "|" "DONE(d)" )
                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

      (setq org-todo-keyword-faces
            (quote (("TODO" :foreground "red" :weight bold)
                    ("NEXT" :foreground "blue" :weight bold)
                    ("DONE" :foreground "forest green" :weight bold)
                    ("WAITING" :foreground "orange" :weight bold)
                    ("HOLD" :foreground "magenta" :weight bold)
                    ("CANCELLED" :foreground "forest green" :weight bold)
                    ("MEETING" :foreground "forest green" :weight bold)
                    ("PHONE" :foreground "forest green" :weight bold))))
      (setq org-use-fast-todo-selection t)
      (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("HOLD" ("WAITING") ("HOLD" . t))
                    (done ("WAITING") ("HOLD"))
                    ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

      (setq org-tag-alist
            (quote ((:startgroup)
                    ("@home" . ?h)
                    ("@office" . ?o))))

      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("i" "Ideas" entry (file+headline org-agenda-file-note "Ideas")
               "* SOMEDAY [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry (file org-agenda-file-code-snippet)
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("w" "work" entry (file+headline org-agenda-file-gtd "wxwl")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry" entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                )))))))


;; Add melpa-stable to your packages repositories
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Enable defer and ensure by default for use-package
;;(setq use-package-always-defer t
;;      use-package-always-ensure t)

;; Enable scala-mode and sbt-mode
;; (defun wrydz/init-scala-mode()
;;   (use-package scala-mode
;;     :mode "\\.s\\(cala\\|bt\\)$"))

;; (defun wrydz/init-sbt-mode()
;;   (use-package sbt-mode
;;     :commands sbt-start sbt-command
;;     :config
;;     ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;     ;; allows using SPACE when in the minibuffer
;;     (substitute-key-definition
;;      'minibuffer-complete-word
;;      'self-insert-command
;;      minibuffer-local-completion-map)))

;; ;; (defun wrydz/init-eglot()
;; ;;   (use-package eglot
;; ;;     :pin melpa-stable
;; ;;     :config
;; ;;     (add-to-list 'eglot-server-programs '(scala-mode . ("metals-emacs")))
;; ;;     ;; (optional) Automatically start metals for Scala files.
;; ;;     :hook (scala-mode . eglot-ensure)))

;; ;; Enable nice rendering of diagnostics like compile errors.
;; (defun wrydz/init-lsp-scala()
;;   (use-package lsp-scala
;;     :after scala-mode
;;     :demand t
;;     ;; Optional - enable lsp-scala automatically in scala files
;;     :hook (scala-mode . lsp)
;;     :init (setq lsp-scala-server-command "metals-emacs")))


;;; packages.el ends here
