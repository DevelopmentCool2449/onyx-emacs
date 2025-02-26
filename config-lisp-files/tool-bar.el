;;; FUNCTIONS

;;;###autoload
(defun my/insert-tool-bar-icon (map icon function)
  "Insert Icon in Tool Bar."
  (keymap-set-after map (concat "<" icon ">")
    `(menu-item ,(capitalize icon) ,function
                :image
                `(image :type svg :file ,(concat user-emacs-directory
                                                 "assets/org-tool-bar/"
                                                 ,icon ".svg")
                        :scale 4.0))))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local tool-bar-map
                        (let ((map (make-sparse-keymap)))
                          (tool-bar-local-item-from-menu 'find-file "open" map)
                          (tool-bar-local-item-from-menu 'save-buffer "save" map)
                          (tool-bar-local-item-from-menu 'undo "undo" map)
                          (tool-bar-local-item-from-menu 'undo-redo "redo" map)
                          (keymap-set-after map "<separator-1>" menu-bar-separator)
                          (my/insert-tool-bar-icon map "h1" (lambda() (interactive) (insert "\n* ")))
                          (my/insert-tool-bar-icon map "h2" (lambda() (interactive) (insert "\n** ")))
                          (my/insert-tool-bar-icon map "h3" (lambda() (interactive) (insert "\n*** ")))
                          (my/insert-tool-bar-icon map "h4" (lambda() (interactive) (insert "\n**** ")))
                          (my/insert-tool-bar-icon map "bold" (lambda() (interactive) (org-emphasize ?*)))
                          (my/insert-tool-bar-icon map "italic" (lambda() (interactive) (org-emphasize ?/)))
                          (my/insert-tool-bar-icon map "verbatium" (lambda() (interactive) (org-emphasize ?=)))
                          (my/insert-tool-bar-icon map "code" (lambda() (interactive) (org-emphasize ?~)))
                          (my/insert-tool-bar-icon map "strike" (lambda() (interactive) (org-emphasize ?+)))
                          (my/insert-tool-bar-icon map "list" (lambda() (interactive) (insert "\n- ")))
                          (my/insert-tool-bar-icon map "link" (lambda() (interactive) (org-insert-link)))
                          (my/insert-tool-bar-icon map "quote" (lambda() (interactive) (org-insert-structure-template "quote")))
                          (my/insert-tool-bar-icon map "src_block" (lambda() (interactive) (org-insert-structure-template "src")))
                          map))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local tool-bar-map
                        (let ((map (make-sparse-keymap)))
                          (tool-bar-local-item-from-menu 'find-file "open" map)
                          (tool-bar-local-item-from-menu 'save-buffer "save" map)
                          (tool-bar-local-item-from-menu 'undo "undo" map)
                          (tool-bar-local-item-from-menu 'undo-redo "redo" map)
                          (keymap-set-after map "<separator-1>" menu-bar-separator)
                          (my/insert-tool-bar-icon map "h1" (lambda() (interactive) (insert "\n# ")))
                          (my/insert-tool-bar-icon map "h2" (lambda() (interactive) (insert "\n## ")))
                          (my/insert-tool-bar-icon map "h3" (lambda() (interactive) (insert "\n### ")))
                          (my/insert-tool-bar-icon map "h4" (lambda() (interactive) (insert "\n#### ")))
                          (my/insert-tool-bar-icon map "bold" (lambda() (interactive) (markdown-insert-bold)))
                          (my/insert-tool-bar-icon map "italic" (lambda() (interactive) (markdown-insert-italic)))
                          (my/insert-tool-bar-icon map "code" (lambda() (interactive) (markdown-insert-code)))
                          (my/insert-tool-bar-icon map "strike" (lambda() (interactive) (markdown-insert-strike-through)))
                          (my/insert-tool-bar-icon map "list" (lambda() (interactive) (markdown-insert-list-item 0)))
                          (my/insert-tool-bar-icon map "link" (lambda() (interactive) (markdown-insert-link)))
                          (my/insert-tool-bar-icon map "quote" (lambda() (interactive) (markdown-insert-blockquote)))
                          (my/insert-tool-bar-icon map "src_block" (lambda() (interactive) (markdown-insert-gfm-code-block "lang")))
                          map))))

;;; ADD TOOL BAR BUTTONS
;; It's possible add submenus in tool bar such as: <tool-bar> <copy> <COMMAND>
(when (display-graphic-p)
  (setopt tool-bar-style 'image
          tool-bar-position 'bottom)
  (modifier-bar-mode t)

  (tool-bar-add-item-from-menu 'undo-redo "redo" nil) ; Add Redo

  (keymap-set-after (default-value 'tool-bar-map) "<undo-redo>"
    (cdr (assq 'undo-redo tool-bar-map))
    'undo)

  (keymap-set-after (default-value 'tool-bar-map) "<explorer>"
    '(menu-item "Explorer" my/explorer-open
                :help "Hide/Show Side Explorer"
                :visible (or (derived-mode-p 'prog-mode)
                             (derived-mode-p 'text-mode))
                :image
                `(image :type svg :file ,(concat user-emacs-directory "assets/tree_explorer.svg") :scale default))
    'isearch-forward)

  (keymap-set-after (default-value 'tool-bar-map) "<separator-4>"
    menu-bar-separator 'my/explorer-open) ; Add Separator

  (keymap-set-after (default-value 'tool-bar-map) "<packages>"
    '(menu-item "packages" list-packages
                :help   "Show List Packages"
                :image
                `(image :type svg :file ,(concat user-emacs-directory "assets/elpa.svg") :scale default))
    'my/explorer-open)

  (keymap-set-after (default-value 'tool-bar-map) "<dashboard>"
    '(menu-item "Dashboard" dashboard-open
                :help "Back to Startpage"
                :image (find-image '((:type xpm :file "home.xpm"))))
    'list-packages)

  (keymap-set-after (default-value 'tool-bar-map) "<customize>"
    '(menu-item "Settings" customize
                :help "Show Settings Buffer"
                :image (find-image '((:type xpm :file "preferences.xpm"))))
    'dashboard-open))
