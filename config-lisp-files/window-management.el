(use-package eldoc-box
  :diminish
  :custom-face
  (eldoc-box-body ((t (:inherit solaire-default-face))))
  (eldoc-highlight-function-argument ((t (:inherit success :bold t))))
  :custom
  (eldoc-idle-delay 0.7)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode)
  (eldoc-box-frame . (lambda (&rest _)
                       (set-window-margins (selected-window) 0 0)))
  :config
  ;; Prettify `eldoc-box' frame
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 0
        (alist-get 'right-fringe eldoc-box-frame-parameters) 0
        (alist-get 'internal-border-width eldoc-box-frame-parameters) 1)
  (add-to-list 'eldoc-box-frame-parameters '(alpha-background . 90)))

(use-package window
  :ensure nil
  :config
  (set-face-attribute 'window-divider nil :background (face-attribute 'solaire-default-face :background))
  (set-face-attribute 'window-divider nil :foreground (face-attribute 'solaire-default-face :background))
  (set-face-attribute 'window-divider nil :inherit nil)
  :custom
  (window-divider-default-places t)
  (window-divider-mode t)
  (quit-window-kill-buffer
   '(help-mode
     helpful-mode
     magit-status-mode
     magit-process-mode
     magit-status-mode
     magit-diff-mode
     magit-log-mode
     magit-file-mode
     magit-blob-mode
     magit-blame-mode))
  (display-buffer-alist ; NOTE: add major modes is possible.
   `((,(rx (seq "*"
                (one-or-more (or "quickrun" "compilation"
                                 "deadgrep" "rg" "grep"))))
      display-buffer-in-side-window
      (reusable-frames . visible)
      (window-height . 0.40)
      (slot . 0)
      (side . bottom))
     (,(rx
        (seq "*"
             (one-or-more
              (or "Python" "lua" "Compile-Log" (seq (any "Hh") "elp")
                  "ielm" "Occur" "Flycheck errors" "Calendar"
                  "comment-tags" "Breakpoints" "vc-git"
                  (seq (opt "ansi-") "term") "eat" (seq (opt "e") "shell")
                  "Flymake diagnostics for"))))
      display-buffer-in-side-window
      (reusable-frames . visible)
      (window-height . 0.25)
      (slot . 0)
      (side . bottom))))
  :preface
  ;; Put Package Description Buffer in Right Side
  (advice-add #'describe-package :around
              (lambda (orig &rest r)
                (let ((display-buffer-alist
                       '(("*Help*"
                          display-buffer-in-side-window
                          (window-width . 0.35)
                          (side . right)))))
                  (apply orig r)))))
