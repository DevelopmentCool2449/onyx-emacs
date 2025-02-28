(use-package form-feed-st
  :diminish
  :config (global-form-feed-st-mode 1)
  (dolist (modes '(browse-kill-ring-mode
                   emacs-lisp-compilation-mode
                   outline-mode
                   emacs-news-view-mode))
    (add-to-list 'form-feed-st-include-modes modes)))

(use-package fill-column
  :ensure nil
  :hook
  ((prog-mode text-mode) . display-fill-column-indicator-mode)
  (display-fill-column-indicator-mode
   . (lambda ()
       (add-hook
        'post-command-hook
        (lambda ()
          (if (> (save-excursion (end-of-line) (current-column))
                 fill-column)
              (progn
                (setq-local
                 display-fill-column-indicator-character 9475)
                (face-remap-set-base 'fill-column-indicator
                                     (list :inherit 'error :stipple nil
                                           :box nil :strike-through nil
                                           :overline nil :underline nil)))
            (setq-local
             display-fill-column-indicator-character 9474)
            (face-remap-reset-base 'fill-column-indicator)))
        nil t))))

(setopt window-divider-default-places t
        window-divider-default-bottom-width 4
        window-divider-default-right-width  4)

(setopt ansi-color-for-comint-mode 'filter)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package shell :ensure nil
  :commands shell
  :hook ((term-mode
          vterm-mode
          shell-mode
          eshell-mode)
         . compilation-shell-minor-mode))

(setopt show-paren-style 'parenthesis
        show-paren-when-point-inside-paren t
        show-paren-predicate
        '(not
          (or (derived-mode . special-mode) (major-mode . text-mode)
              (derived-mode . hexl-mode))))

(define-advice show-paren-function (:around (fn) fix)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

(setopt scroll-bar-adjust-thumb-portion nil)

(setopt display-line-numbers-width 3
        display-line-numbers-widen t)

;; (setopt help-at-pt-display-when-idle t) ;; Show Any Tooltip In Echo Buffer

(use-package olivetti
  :commands olivetti-mode
  :hook ((Custom-mode
          Info-mode
          image-mode
          org-mode
          markdown-mode)
         . olivetti-mode)
  :custom
  (olivetti-style 'fancy)
  (olivetti-body-width 100))

(use-package nerd-icons :demand t
  :config
  ;; Create symbolic link for fonts directory from emacs dotfiles
  ;; directory.  If ~/fonts exists and fonts from user emacs directory
  ;; doesn't exist then do nothing.
  (when-let* ((target (expand-file-name "~/fonts"))
              (link (expand-file-name "fonts" user-emacs-directory))
              ((not (file-exists-p target)))
              ((file-exists-p link)))
    (make-symbolic-link link target)
    (message "Symbolic link created: %s -> %s" link target))

  ;; And set font.
  (if-let* ((font "RobotoMono Nerd Font")
            ((member font (font-family-list))))
      (set-frame-font (concat font " 15") nil t)))

(use-package nerd-icons-completion :demand t
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  (after-init . nerd-icons-completion-mode))
