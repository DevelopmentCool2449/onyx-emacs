;; Enable Auto-insert mode for File Templates
(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-mode t)
  (auto-insert 'other)
  (auto-insert-query nil)
  (auto-insert-directory (concat user-emacs-directory "templates/"))
  :custom
  ;; Add org file template
  (add-to-list 'auto-insert-alist
               '(("\\.org$" . "Org mode file")
                 nil
                 "#+TITLE: " (file-name-base (buffer-file-name)) \n))
  :preface
;;;###autoload
  (defun my/autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max))))
