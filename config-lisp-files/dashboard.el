(use-package dashboard
  :hook
  (window-configuration-change
   . (lambda ()
       (if (and (dirvish-side--session-visible-p) (derived-mode-p 'dashboard-mode))
           (delete-window (dirvish-side--session-visible-p)))))
  (after-init . dashboard-setup-startup-hook)
  (dashboard-before-initialize . turn-on-solaire-mode)
  (dashboard-mode . (lambda ()
                      (setq-local left-fringe-width 0
                                  right-fringe-width 0
                                  left-margin-width 0
                                  right-margin-width 0)))
  :custom-face
  (dashboard-banner-logo-title ((t (:height 2.0 :weight ultra-heavy :inherit (variable-pitch)))))
  :custom
  (dashboard-banner-logo-title "Emacs")
  (dashboard-startup-banner
   `(,(concat user-emacs-directory "assets/splash.svg") .
     ,(concat dashboard-banners-directory "4.txt")))
  (dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (dashboard-vertically-center-content t)
  (dashboard-path-style 'truncate-middle)
  (dashboard-path-max-length 50)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 9)
                     (projects . 5)))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-footer-icon (nerd-icons-faicon "nf-fa-pencil" :height 1.2 :face 'default))
  (dashboard-heading-shorcut-format (propertize " [%s]" 'face 'shadow))
  (dashboard-startupify-list `(dashboard-insert-banner
                               (lambda ()
                                 (dashboard-insert-center
                                  (propertize
                                   "OnyX " 'face
                                   '(:weight ultra-bold
                                             :height 1.5
                                             :inherit (font-lock-keyword-face
                                                       dashboard-banner-logo-title)))
                                  (propertize
                                   dashboard-banner-logo-title 'face
                                   '(:weight light
                                             :height 1.2
                                             :inherit dashboard-banner-logo-title))))
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               (lambda () (delete-char -1))
                               dashboard-insert-items
                               dashboard-insert-init-info))

  (dashboard-navigator-buttons
   (let ((l (nerd-icons-powerline "nf-ple-left_half_circle_thick" :face 'dashboard-navigator :height 1.5))
         (r (nerd-icons-powerline "nf-ple-right_half_circle_thick" :face 'dashboard-navigator :height 1.5)))
     `(((,(nerd-icons-faicon "nf-fa-file_text_o")
         "Open File" ; Title
         "Open External File" ; Description
         (lambda (&rest _)
           (menu-find-file-existing))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-mdicon "nf-md-timelapse")
         " Recent files" ; Title
         "Open Recently files" ; Description
         (lambda (&rest _)
           (consult-recent-file))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r))
       (("" nil nil nil nil "" ""))
       ((,(nerd-icons-sucicon "nf-custom-orgmode")
         " New Org Note" ; Title
         "Create Denote Note for org-mode" ; Description
         (lambda (&rest _)
           (let ((denote-file-type 'org))
             (call-interactively #'denote)))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-octicon "nf-oct-markdown")
         " New Markdown Note" ; Title
         "Create Denote Note for org-mode" ; Description
         (lambda (&rest _)
           (let ((denote-file-type 'markdown-yaml))
             (call-interactively #'denote)))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r))
       (("" nil nil nil nil "" ""))
       ((,(nerd-icons-mdicon "nf-md-book_open_page_variant")
         " Open Note" ; Title
         "Open Denote Note" ; Description
         (lambda (&rest _)
           (call-interactively #'denote-open-or-create))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-mdicon "nf-md-notebook_edit")
         " Rename Note" ; Title
         "Rename Denote Note" ; Description
         (lambda (&rest _)
           (call-interactively #'denote-rename-file))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r))
       (("" nil nil nil nil "" ""))
       ((,(nerd-icons-mdicon "nf-md-calendar")
         " Org Agenda" ; Title
         "Open Org Agenda" ; Description
         (lambda (&rest _)
           (org-agenda))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-faicon "nf-fa-file_text_o")
         "New Org file" ; Title
         "Create new org file" ; Description
         (lambda (&rest _)
           (let ((buf (generate-new-buffer "Org File")))
             (switch-to-buffer buf)
             (funcall #'org-mode)
             (insert "#+TITLE: ")
             (save-excursion
               (insert "\n#+AUTHOR:\n#+DATE:")
               (setq buffer-offer-save t))))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r))
       (("" nil nil nil nil "" ""))
       ((,(nerd-icons-mdicon "nf-md-package_variant")
         "Search Packages" ; Title
         "Search and install Emacs Packages" ; Description
         (lambda (&rest _) (list-packages))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r)
        (,(nerd-icons-octicon "nf-oct-gear")
         " " ; Title
         "Open settings" ; Description
         (lambda (&rest _) (customize))
         (:inverse-video t :inherit dashboard-navigator) ,l ,r))))))
