;; ╭─────────────────────────────────────────────────────────────────────────────────╮
;; │             C    means (press and hold) the 'Control' key                       │
;; │             M    means the Meta key (the 'Alt' key, on most keyboards)          │
;; │             S    means the 'Shift' key (e.g. S─TAB means Shift Tab)             │
;; │             DEL  means the 'Backspace' key (not the Delete key)                 │
;; │             RET  means the 'Return' or 'Enter' key                              │
;; │             SPC  means the 'Space' bar                                          │
;; │             ESC  means the 'Escape'key                                          │
;; │             TAB  means the 'Tab' key                                            │
;; └─────────────────────────────────────────────────────────────────────────────────╯

;;; OPTIONS
(setopt tab-always-indent nil)

;;; WHICH KEY
(use-package which-key :ensure t :diminish
  :config
  (which-key-mode t)
  :custom
  (which-key-max-description-length 40)
  (which-key-sort-order 'which-key-description-order))

;;; OVERRIDE FUNCTIONS

(defun my/call-interactively-inhibit-kill-ring (fun &rest args)
  (if (called-interactively-p 'any)
      (let ((kill-ring '(""))
            (select-enable-clipboard nil))
        (call-interactively fun))
    (apply fun args)))

(advice-add 'backward-kill-word :around #'my/call-interactively-inhibit-kill-ring)
(advice-add 'kill-word          :around #'my/call-interactively-inhibit-kill-ring)
(advice-add 'kill-whole-line    :around #'my/call-interactively-inhibit-kill-ring)

;;; KEY DEFINITIONS

;;; `Re-Binding' keys
(bind-key "F" 'describe-face help-map) ; Bind describe-face

(bind-keys :map isearch-mode-map
           ("<up>"   . isearch-repeat-backward)
           ("<down>" . isearch-repeat-forward))

(bind-keys
 ("TAB" . indent-for-tab-command)

 ("<remap> <describe-key>"      . helpful-key) ; Helpful keybinding
 ("<remap> <describe-command>"  . helpful-command)
 ("<remap> <describe-variable>" . helpful-variable)
 ("<remap> <describe-function>" . helpful-callable))
