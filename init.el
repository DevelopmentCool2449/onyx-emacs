;;; init.el -- OnyX Emacs -*- lexical-binding: t; -*-
;;; Code:
(let ((config-dir (expand-file-name "config-lisp-files" user-emacs-directory))
      (modules
       '(;; Core Setup
         "packages"                 ; Package management setup
         "key-mappings"             ; Basic Keybindings
         "internal-configurations"  ; Basic Emacs settings

         ;; UI and Aesthetics
         "custom-themes"            ; Theme configuration
         "tool-bar"                 ; Toolbar settings
         "menu-bar"                 ; Menu bar settings
         "mode-line"                ; Modeline customization
         "window-tabs"              ; Tab bar configuration
         "dashboard"                ; Startup dashboard
         "ui-enhancements"          ; Additional UI improvements

         ;; Editor Enhancements
         "syntax-highlighting"      ; Syntax coloring
         "spell-checking"           ; Spell checking configuration

         ;; Development Tools
         "window-management"        ; Window layout management
         "file-management"          ; File handling and navigation
         "code-snippets"            ; Code snippet

         ;; Special Modes
         "org-mode"                 ; Org mode configuration
         "denote"                   ; Denote configuration (note taking system)

         ;; Miscellaneous
         "minibuffer"               ; Minibuffer enhancements
         "auto-insert-templates"    ; File templates
         "misc"                     ; Other settings
         )))
  (dolist (module modules)
    (let ((module-file (expand-file-name module config-dir)))
      (condition-case-unless-debug err
          (progn (load module-file nil t)
                 (message "✔ Module `%s' loaded" module))
        (error (user-error "Error loading module `%s': %s" module err))))))

;; Private User Configurations:
(load (concat user-emacs-directory "user-configurations"))

;; Faces
(set-face-attribute 'custom-group-tag nil :height 1.2)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'region nil :extend nil)

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
