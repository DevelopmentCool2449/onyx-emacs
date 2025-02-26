(use-package marginalia
  :custom
  (marginalia-mode t)
  :preface
  (advice-add #'marginalia-annotate-command
              :around (lambda (orig cand)
                        "Annotate minor-mode command CAND with mode state."
                        (concat
                         (when-let* ((sym (intern-soft cand))
                                     (mode (if (and sym (boundp sym))
                                               sym
                                             (lookup-minor-mode-from-indicator cand))))
                           (if (and (boundp mode) (symbol-value mode))
                               #(" [On]" 1 5 (face marginalia-on))
                             #(" [Off]" 1 6 (face marginalia-off))))
                         (funcall orig cand))))

  (advice-add #'marginalia--documentation :override
              (lambda (str)
                "Show current mode state"
                (if str
                    (marginalia--fields
                     (str :truncate 1.2 :face 'marginalia-documentation))))))

(use-package consult
  :demand t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (xref-show-xrefs-function #'consult-xref) ; Use Consult to select xref locations with preview
  (xref-show-definitions-function #'consult-xref)
  (register-preview-function #'consult-register-format)
  (consult-find-command "fd --color=always --full-path ARG OPTS")
  :bind ("<remap> <imenu>" . consult-imenu)
  :config
  ;; Preview on any key press, but delay 2s
  (consult-customize
   consult-recent-file consult-theme consult-buffer consult-bookmark
   :preview-key '(:debounce 2 any))
  (advice-add #'project--read-file-cpd-relative :around
              (lambda (_ prompt all-files &optional pred hist __)
                "Use consult for previewing files"
                (consult--read (mapcar
                                (lambda (f)
                                  (file-relative-name f))
                                all-files)
                               :state (consult--file-preview)
                               :prompt (format "%s: " prompt)
                               :require-match t
                               :history hist
                               :category 'file
                               :preview-key '(:debounce 2 any)
                               :predicate pred))))

(use-package vertico
  :ensure vertico-prescient
  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t
               face (:inherit minibuffer-prompt
                              :weight bold :height 1.3)))
  (vertico-count 14)
  (vertico-count-format
   `("%-6s " . ,(concat (nerd-icons-octicon "nf-oct-search")
                        " ( %s/%s )")))
  (vertico-mode t)
  (vertico-multiform-mode t)
  (vertico-mouse-mode t)
  :config
  (advice-add
   #'vertico--format-candidate :around
   (lambda (orig-fun cand prefix suffix index start)
     (apply orig-fun (list cand
                           (if (= vertico--index index)
                               (concat (nerd-icons-faicon
                                        "nf-fa-hand_o_right"
                                        :face 'nerd-icons-red)
                                       "  " prefix)
                             (concat "   " prefix))
                           suffix
                           index start)))))

(defun message-filter-center (args)
  "ARGS Center message string.
This is a :filter-args advice for `message`."
  (if (car args)
      (with-current-buffer (window-buffer (minibuffer-window))
        (let ((str (apply #'format-message args)))
          (list "%s" (propertize str 'line-prefix (list 'space :align-to (max 0 (/ (- (window-width (minibuffer-window)) (string-width str)) 2)))))))
    args))
(advice-add #'message :filter-args #'message-filter-center)
