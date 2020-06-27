(setq org-startup-indented t)

;; use apsell as ispell backend
(setq-default ispell-program-name "aspell")
;; use American English as ispell default dictionary
(ispell-change-dictionary "american" t)

(add-hook 'org-mode-hook 'iimage-mode)

;; (add-hook 'sh-mode-hook #'lsp-sh-enable)




(unless window-system
  (when (getenv "DISPLAY")
    ;; Callback for when user cuts
    ;; Attach callbacks to hooks
    (setq interprogram-cut-function 'wrydz/copy-content-win-clip)
    ))

