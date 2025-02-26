(use-package package
  :ensure nil
  :custom
  (package-vc-register-as-project nil)
  (use-package-always-ensure t) ; Auto-download package if not exists
  (use-package-enable-imenu-support t) ; Let imenu finds use-package definitions
  :config
  (setopt package-check-signature nil)

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)
  :preface
  ;; HACK: DO NOT save package-selected-packages to `custom-file'.
  ;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
  (defun my-package--save-selected-packages (&optional value)
    "Set `package-selected-packages' to VALUE but don't save to option `custom-file'."
    (if value
        (setopt package-selected-packages value))
    (unless after-init-time
      (add-hook 'after-init-hook #'my-package--save-selected-packages))))
