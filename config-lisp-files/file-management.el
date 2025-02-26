;; Define which file manager use
;; (e.g 'treemacs' or 'dirvish-side').
(defalias 'my/explorer-open 'dirvish-side)

;;; DIRED CONFIGURATIONS
(use-package dired-x
  :ensure nil
  :custom
  (dired-mouse-drag-files t)
  (delete-by-moving-to-trash t)
  (dired-omit-files
   (rx (or (seq bol (one-or-more "flycheck_"))
           (seq bol (? ".") "#")
           (seq bol "." eol)
           (seq bol ".." eol)))))

;;; DIRVISH

(use-package dirvish
  :hook
  (dired-mode . auto-revert-mode)
  (dirvish-find-entry
   . (lambda (&rest _)
       (interactive)
       (dired-omit-mode)
       (setq-local truncate-lines t
                   mouse-1-click-follows-link 'double)))
  :custom
  (dirvish-side-follow-mode t)
  (dirvish-side-width 31)
  (dirvish-subtree-state-style 'nerd)
  (dirvish-subtree-always-show-state t)
  (dirvish-attributes
   '(nerd-icons subtree-state git-msg file-time vc-stat))
  (dirvish-path-separators
   (list (format "  %s " (nerd-icons-codicon "nf-cod-home"))
         (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
         (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dirvish-override-dired-mode t)
  (dirvish-reuse-session nil)
  (dirvish-use-mode-line nil)
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind
  (:map dirvish-mode-map
        ("<remap> <kill-this-buffer>" . dirvish-quit)
        ("TAB"       . dirvish-subtree-toggle)
        ("<delete>"  . dired-do-delete)
        ("<double-mouse-1>" . dirvish-subtree-toggle-or-open))
  (:map dirvish-directory-view-mode-map
        ("<remap> <kill-this-buffer>" . dirvish-quit)
        ("<double-mouse-1>" . dired-find-file)
        ("RET"              . dired-find-file)))
