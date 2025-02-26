(use-package hide-mode-line
  :config (global-hide-mode-line-mode t))

(use-package nano-modeline
  :hook (after-init . nano-modeline-text-mode)
  :hook
  (prog-mode . nano-modeline-prog-mode)
  (text-mode . nano-modeline-text-mode)
  (org-mode . nano-modeline-org-mode))
