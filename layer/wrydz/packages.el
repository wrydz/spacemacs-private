;;; packages.el --- wrydz layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: wrydz <wrydz@debian.debian.org>
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
  '(youdao-dictionary
    blog-admin
    org
    ))

(defun wrydz/init-blog-admin()
  (use-package blog-admin
    :init
    (progn
      (setq blog-admin-backend-path "~/static-blog")
      (setq blog-admin-backend-type 'hexo)
      (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
      (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
      (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
      )))

(defun wrydz/init-youdao-dictionary()
  (use-package youdao-dictionary
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point)
      (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao"
            push "*Youdao Dictionary*" popwin:special-display-config))
    :config (setq url-automatic-caching t)))

(defun wrydz/post-init-org()
  )

;;; packages.el ends here
