(use-package hl-line
  :ensure nil
  :config (global-hl-line-mode t)
  :hook ((eshell-mode
          shell-mode
          term-mode
          comint-mode
          cfrs-input-mode
          image-mode
          vterm-mode)
         . (lambda () (setq-local global-hl-line-mode nil))))

(use-package markdown-mode
  :custom (markdown-max-image-size '(500 . 500)))

(use-package rainbow-delimiters
  :demand t
  :custom (rainbow-delimiters-max-face-count 4)
  :hook ((prog-mode yaml-mode xml-mode mhtml-mode)
         . rainbow-delimiters-mode))

(use-package colorful-mode :diminish
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;; (use-package hl-todo
;;   :custom-face
;;   (hl-todo ((t (:inherit variable-pitch :height 0.9
;;                          :width condensed :weight bold
;;                          :underline nil :inverse-video t))))
;;   :hook
;;   ((prog-mode text-mode) . hl-todo-mode)
;;   :custom
;;   (hl-todo-require-punctuation t)
;;   (hl-todo-highlight-punctuation ":")
;;   :config
;;   (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake)

;;   (let ((_error   (face-attribute 'error :foreground))
;;         (_warning (face-attribute 'warning :foreground))
;;         (_info    (face-attribute 'success :foreground))
;;         (_misc    (face-attribute 'nerd-icons-blue :foreground)))

;;     (dolist (keyword '("BUG" "DEFECT" "ISSUE" "FIX" "FAIL" "FIXME" "FAIL"))
;;       (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_error)))
;;     (dolist (keyword '("WARNING"))
;;       (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_warning)))
;;     (dolist (keyword '("WORKAROUND" "NOTE" "TRICK" "HACK"))
;;       (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_info)))
;;     (dolist (keyword '("DEBUG" "STUB" "TODO"))
;;       (add-to-list 'hl-todo-keyword-faces `(,keyword . ,_misc))))

;;   (put 'hl-todo-flymake 'flymake-type-name " TODO")
;;   (advice-add 'hl-todo-make-flymake-diagnostic :override #'my/hl-todo-types-icons)
;;   :preface
;;   (defun my/hl-todo-types-icons (locus beg end text _keyword)
;;     (let ((keyword (string-remove-suffix
;;                     ":" (substring-no-properties _keyword)))
;;           type)
;;       (pcase keyword
;;         ("TODO" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
;;         ("BUG" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
;;         ("WARNING" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
;;         ("FIXME" (setq type (intern-soft (concat "hl-todo-flymake-" keyword))))
;;         (_ (setq type 'hl-todo-flymake)))
;;       (flymake-make-diagnostic locus beg end type text))))
