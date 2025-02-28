(use-package centaur-tabs
  :hook
  (emacs-startup . centaur-tabs-mode)
  :custom
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-style "slant")
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-enable-key-bindings t) ; Enable Centaur Tabs Key bindings
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-show-navigation-buttons t) ; Navigations Buttons
  (centaur-tabs-backward-tab-text
   (concat " " (nerd-icons-faicon "nf-fa-chevron_circle_left") " "))
  (centaur-tabs-forward-tab-text
   (concat " " (nerd-icons-faicon "nf-fa-chevron_circle_right") "  "))
  (centaur-tabs-down-tab-text
   (concat " " (nerd-icons-faicon "nf-fa-chevron_circle_down") " "))
  (centaur-tabs-set-icons t) ; Icons
  (centaur-tabs-gray-out-icons 'buffer)
  :config
  (dolist (centaur-face '(centaur-tabs-selected
                          centaur-tabs-selected-modified
                          centaur-tabs-unselected
                          centaur-tabs-unselected-modified))
    (set-face-attribute centaur-face nil :height 160))

  (dolist (names '("*Backtrace*" "*Native-compile-Log" "*cpp"
                   "*Completions" "*Ilist" "*dap" "*copilot"
                   "*EGLOT" "*Debug" "*gud-" "*locals of" "*stack frames"
                   "*input/output of" "*breakpoints of " "*threads of "
                   "*local values of " "*css-ls" "*html-ls" "*json-ls" "*ts-ls"
                   "*dashboard" "*format-all-" "*marksman" "Treemacs"
                   "*Dirvish-preview-" "*dirvish batch" "*yasnippet" "*clang"
                   "*mybuf" "*Messages" "*py" "*rg" "*lua-" "*comment-tags"
                   "*Flymake log" "dir-data-" "*Async-native" "*zone"
                   "widget-choose"))
    (add-to-list 'centaur-tabs-excluded-prefixes names))

  (defun centaur-tabs--create-new-empty-buffer ()
    "Open an Untitled buffer."
    (interactive)
    (let ((buffer (generate-new-buffer "Org-file")))
      (switch-to-buffer buffer)
      (funcall #'org-mode)
      (setq buffer-offer-save t)
      (insert "#+TITLE: ")
      (save-excursion (insert "\n#+AUTHOR: \n#+DATE: \n"))
      buffer))

  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((memq major-mode '(magit-process-mode
                          magit-status-mode
                          magit-diff-mode
                          magit-log-mode
                          magit-file-mode
                          magit-blob-mode
                          magit-blame-mode))
       "Magit")

      ((string-prefix-p "*vc-" (buffer-name))
       "VC")

      ((derived-mode-p 'Custom-mode)
       "Custom")
      ((derived-mode-p 'dired-mode)
       "Dired")

      ((memq major-mode '(helpful-mode help-mode Info-mode))
       "Help")

      ((memq major-mode '(flycheck-error-list-mode
                          flymake-diagnostics-buffer-mode
                          flymake-project-diagnostics-mode
                          compilation-mode comint-mode eshell-mode shell-mode eat-mode
                          term-mode quickrun--mode dap-ui-breakpoints-ui-list-mode
                          inferior-python-mode calendar-mode
                          inferior-emacs-lisp-mode grep-mode occur-mode))
       "Side Bar")

      ((cl-dolist (prefix centaur-tabs-excluded-prefixes)
         (when (string-prefix-p prefix (buffer-name))
           (cl-return ""))))

      (t (if-let* ((project (project-current)))
             (project-name project)
           "No project")))))

  (defun centaur-tabs-hide-tab (buffer)
    "Do no to show buffer BUFFER in tabs."
    (let ((name (buffer-name buffer)))
      (or
       (if-let* ((w (window-dedicated-p)))
           (not (eq w 'side)))

       ;; Buffer name not match below blacklist.
       (cl-dolist (prefix centaur-tabs-excluded-prefixes)
         (when (string-prefix-p prefix name)
           (cl-return t)))

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))

;;;###autoload
  (defun run-after-load-theme-hook (&rest _)
    (centaur-tabs-buffer-init)
    (centaur-tabs-display-update)
    (centaur-tabs-headline-match))
  (advice-add #'load-theme :after #'run-after-load-theme-hook))
