(use-package form-feed-st
  :diminish
  :hook
  ((prog-mode
    conf-mode
    outline-mode
    emacs-news-view-mode
    emacs-lisp-compilation-mode)
   . form-feed-st-mode))

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
          image-mode)
         . olivetti-mode)
  :custom
  (olivetti-style 'fancy)
  (olivetti-body-width 100))

(use-package nerd-icons :demand t)

(use-package nerd-icons-completion :demand t
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  (after-init . nerd-icons-completion-mode))
