;;; init.el -- OnyX Emacs -*- lexical-binding: t; -*-
;;; Code:
(add-hook 'emacs-startup-hook
          (lambda ()
            (setopt gc-cons-threshold 800000
                    gc-cons-percentage 0.1
                    file-name-handler-alist startup/file-name-handler-alist)))

(let ((configuration-directory (concat user-emacs-directory "config-lisp-files/")))
  (load (concat configuration-directory "packages"))

  (load (concat configuration-directory "internal-configurations"))

  (load (concat configuration-directory "key-mappings"))

  (load (concat configuration-directory "syntax-highlighting"))

  (load (concat configuration-directory "tool-bar"))
  (load (concat configuration-directory "menu-bar"))

  (load (concat configuration-directory "minibuffer"))
  (load (concat configuration-directory "ui-enhancements"))
  (load (concat configuration-directory "misc"))

  (load (concat configuration-directory "spell-checking"))

  (load (concat configuration-directory "window-management"))

  (load (concat configuration-directory "file-management"))

  (load (concat configuration-directory "mode-line"))

  (load (concat configuration-directory "custom-themes"))

  (load (concat configuration-directory "dashboard"))

  (load (concat configuration-directory "org-mode"))
  (load (concat configuration-directory "denote"))

  (load (concat configuration-directory "window-tabs"))

  (load (concat configuration-directory "code-snippets"))

  (load (concat configuration-directory "auto-insert-templates")))

(set-face-attribute 'custom-group-tag nil :height 1.2)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'region nil :extend nil)

;; Private User Configurations:
(load (concat user-emacs-directory "user-configurations"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-items-face ((t (:inherit widget-button :weight normal))))
 '(form-feed-st-line ((t (:strike-through t :extend t :inherit font-lock-comment-face))))
 '(isearch ((t (:weight bold :inherit custom-saved))))
 '(show-paren-match ((t (:box (-1 . -1) :foreground unspecified))))
 '(tool-bar ((t (:box nil :inherit tab-bar-tab-inactive)))))

;;; init.el ends here
;;--------------------------------------------------------------------------------
