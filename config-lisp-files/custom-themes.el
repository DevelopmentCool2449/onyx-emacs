(use-package solaire-mode
  :config
  (solaire-global-mode t))

(use-package doom-themes
  :config
  (doom-themes-org-config)
  (load-theme 'doom-moonlight t))
