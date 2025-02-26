;;; ORG MODERN

(use-package org
  :ensure nil
  :hook
  (org-agenda-finalize . org-modern-agenda)
  :custom-face
  (org-link ((t (:slant italic))))
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-pretty-entities t)
  (org-edit-src-content-indentation 0)
  (org-insert-heading-respect-content t)
  (org-ellipsis "…")
  (org-special-ctrl-a/e t)
  (org-support-shift-select 'always)

  ;; Agenda styling
  (org-agenda-tags-column     0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid '((daily today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  :init
  ;; Enable other Org Features
  (require 'org-element)
  (require 'org-mouse)
  :config
  ;;; Faces
  (set-face-attribute 'org-document-info nil :height 1.2)
  (set-face-attribute 'org-document-title nil :height 1.4)
  (set-face-attribute 'org-level-1 nil :extend nil :weight 'bold :height 1.3)
  (set-face-attribute 'org-level-2 nil :extend nil :weight 'normal :height 1.2)
  (set-face-attribute 'org-level-3 nil :extend nil :height 1.15))

(use-package org-appear
  :custom
  (org-appear-autolinks t)
  :hook
  (org-mode . org-appear-mode))

(use-package org-modern
  :custom
  (org-modern-fold-stars
   `(("▶" . ,(nerd-icons-faicon "nf-fa-gem"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_2_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_3_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_4_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_5_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_6_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_7_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_8_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_9_box_multiple"))
     ("▶" . ,(nerd-icons-mdicon "nf-md-numeric_9_plus_box_multiple"))))
  :config
  (global-org-modern-mode t))
