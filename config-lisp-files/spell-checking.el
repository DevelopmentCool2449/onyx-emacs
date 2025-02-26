(setopt ispell-program-name "hunspell")

(use-package flyspell
  :ensure nil
  :if (executable-find ispell-program-name)
  :custom
  (ispell-dictionary "en_US")
  (org-fold-core-style 'overlays)
  :hook
  ((text-mode markdown-mode org-mode) . flyspell-mode)
  ((html-mode yaml-mode) . flyspell--mode-off)
  :config
  (dolist (my-list '((org-property-drawer-re)
                     ("=" "=") ("~" "~")
                     ("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
    (add-to-list 'ispell-skip-region-alist my-list)))
