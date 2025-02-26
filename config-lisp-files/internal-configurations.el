(use-package emacs
  :ensure nil
  :hook
  ((prog-mode text-mode conf-mode help-mode)
   . visual-wrap-prefix-mode)
  ((prog-mode text-mode conf-mode) . display-line-numbers-mode)
  :custom
  (undo-limit 80000000)
  (recentf-auto-cleanup 300)
  (make-backup-files nil)

  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)

  (truncate-lines t)
  (truncate-partial-width-windows nil)

  (save-interprogram-paste-before-kill t)

  (x-gtk-show-hidden-files t)

  (global-goto-address-mode t)
  (global-visual-line-mode t)

  (delete-selection-mode t)
  (cursor-type 'bar)
  (context-menu-mode t)

  (tab-width 4)

  (indent-tabs-mode nil)

  (confirm-kill-emacs nil)

  (undo-no-redo t)

  (image-animate-loop t)

  (backward-delete-char-untabify-method nil)

  (inhibit-startup-screen t)

  (cursor-in-non-selected-windows nil)

  (scroll-conservatively 101)
  (scroll-step 1)

  (use-short-answers t)
  :config
  (advice-add 'y-or-n-p :around
              (lambda (orig-func &rest args)
                (let ((query-replace-map (copy-keymap query-replace-map)))
                  (keymap-set query-replace-map "<return>" 'act)
                  (apply orig-func args))))

  ;; Set Coding System
  (if (fboundp 'set-charset-priority)
      (set-charset-priority 'unicode))
  (prefer-coding-system 'utf-8)
  (setopt locale-coding-system 'utf-8)

  :preface
  (advice-add #'fundamental-mode :after
              (lambda (&rest _)
                (unless buffer-read-only
                  (display-line-numbers-mode)
                  (electric-pair-mode))))

  ;; Kill Scratch Buffer
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*"))

  ;; Fix Cases region commands
  (put 'upcase-region     'disabled nil)
  (put 'downcase-region   'disabled nil)
  (put 'capitalize-region 'disabled nil))

(use-package savehist
  :init
  (savehist-mode))
