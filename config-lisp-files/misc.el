(global-completion-preview-mode t)

(advice-add 'consult-buffer :before
            (lambda (&rest _)
              (recentf-mode +1)))

(advice-add 'consult-recent-file :before
            (lambda (&rest _)
              (recentf-mode +1)))

;; (use-package magit
;;   :commands magit-status
;;   :custom
;;   (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;;; WHITESPACES
(use-package whitespace
  :diminish
  :ensure nil
  :hook (prog-mode . whitespace-mode)
  (picture-mode . (lambda () (whitespace-mode -1)))
  (diff-mode . (lambda () (whitespace-mode -1)))
  (whitespace-mode . word-wrap-whitespace-mode)
  :custom
  (whitespace-action '(auto-cleanup warn-if-read-only))
  (whitespace-line-column nil)
  (whitespace-display-mappings
   '((space-mark 32 [183] [46]) (space-mark 160 [164] [95])
     (newline-mark 10 [11154 10]) (tab-mark 9 [8250 9] [92 9])))
  (whitespace-style '(face spaces space-mark trailing
                           tabs tab-mark
                           newline newline-mark))
  :config
  (set-face-attribute 'whitespace-tab nil :background nil))

;; auto format code at saving
(use-package apheleia
  :custom
  (apheleia-hide-log-buffers t)
  (apheleia-global-mode t))

;; auto close block comments in c derived modes
(use-package elec-pair
  :ensure nil
  :custom
  (electric-pair-open-newline-between-pairs t)
  :hook
  ((prog-mode text-mode conf-mode) . electric-pair-mode)
  (message-mode
   . (lambda ()
       (setq-local electric-pair-pairs
                   (append electric-pair-pairs
                           '((?` . ?'))))))
  ((c-mode-common
    c-ts-base-mode
    js-ts-mode css-ts-mode json-ts-mode typescript-ts-base-mode
    go-ts-mode go-mode-ts-mode rust-ts-mode
    java-ts-mode csharp-ts-mode)
   . (lambda ()
       (add-hook 'post-self-insert-hook
                 (lambda ()
                   (when (and (looking-back "/[*]" 2)
                              (null (re-search-forward "[^ \t]"
                                                       (line-end-position) t)))
                     (insert " ")
                     (save-excursion
                       (insert " */"))))
                 nil t))))

;;; Helpful (provides much more contextual information)
(use-package helpful
  :config
  ;; Shut down echo elisp messages in helpful
  (setopt elisp-refs-verbose nil)
  :hook (helpful-mode . (lambda ()
                          (interactive)
                          (setq-local tool-bar-map help-mode-tool-bar-map))))

(setopt comint-move-point-for-output t)
(advice-add #'compile :around
            (lambda (orig-fn command &rest _)
              (apply orig-fn command '(t))))

;; Shrink Compilation Buffer once it's finished
(setopt compilation-scroll-output t)
(add-hook 'compilation-finish-functions
          (lambda (buf _)
            (when-let* ((win (get-buffer-window buf 'visible)))
              (with-selected-window (get-buffer-window buf 'visible)
                (let ((ignore-window-parameters t))
                  (shrink-window-if-larger-than-buffer))))))

(advice-add #'customize-dirlocals :around
            (lambda (orig-fn &rest args)
              (if-let* ((project (project-current))
                        (default-directory (project-root project)))
                  (progn
                    (apply orig-fn args)
                    (olivetti-mode))
                (progn
                  (apply orig-fn args)
                  (olivetti-mode)))))

;;; project management
(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '(".project" ".dir-locals.el" "*.gemspec" "autogen.sh" "GTAGS" "TAGS"
     "configure.ac" "configure.in" "cscope.out" "rebar.config" "project.clj"
     "build.boot" "deps.edn" "SConstruct" "default.nix" "flake.nix" "pom.xml"
     "build.sbt" "build.sc" "gradlew" "build.gradle" ".ensime" "Gemfile"
     "requirements.txt" "setup.py" "tox.ini" "composer.json" "Cargo.toml"
     "mix.exs" "stack.yaml" "dune-project" "info.rkt" "DESCRIPTION" "TAGS"
     "GTAGS" "configure.in" "autoconf old styl" "configure.ac" "cscope.out"
     "CMakeLists.txt" "WORKSPACE" "debian/control"))
  (project-vc-ignores '(".elc" ".pyc" ".o" ".github")))

(use-package projection
  :ensure projection-multi
  :hook
  (after-init . global-projection-hook-mode))

(use-package woman
  :ensure nil
  :hook (woman-mode . olivetti-mode))
