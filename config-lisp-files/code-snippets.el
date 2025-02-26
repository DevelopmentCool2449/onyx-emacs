(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of snippets
(use-package yasnippet-snippets
  :after yasnippet)
