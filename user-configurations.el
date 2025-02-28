;;; This file is intended to store private or personal configurations,
;;; such as emails, passwords, packages, hooks, etc.

;; (setopt
;; user-full-name "John Doe" ; Your full name
;; user-mail-address "example@host.domain" ; Your email address
;; )

;; (use-package wakib-keys :diminish
;;   :custom (wakib-keys t nil (wakib-keys))
;;   :bind
;;   (:map wakib-keys-overriding-map
;;         ("M-u" . nil)
;;         ("M-l" . nil)
;;         ("M-j" . nil)
;;         ("M-:" . nil)
;;         ("C-<next>" . nil)
;;         ("C-<prior>" . nil)
;;         ("C-n" . nil)

;;         ("C-b" . consult-buffer) ; Change to Consult-buffer
;;         ("C-t" . centaur-tabs--create-new-tab) ; Create New tab (requires Centar Tabs)

;;         ("C-f" . consult-line)
;;         ("C-S-f" . consult-line)

;;         ;; Copied from CUA
;;         ("<C-return>" . rectangle-mark-mode)

;;         ("C-y"   . undo-redo)
;;         ("C-S-z" . undo-redo)

;;         ;; Keybinding for Autocomplete
;;         ("C-SPC" . completion-at-point)))
