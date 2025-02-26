(setopt menu-bar-close-window t)

(setq-local my/menu-menu--get-major-modes nil)
(setq-local my/menu-major-mode-menu-map-extra-modes
            '(lisp-interaction-mode enriched-mode))

(setq-local my/menu-excluded-major-modes
            '(conf-colon-mode
              conf-xdefaults-mode conf-space-mode conf-javaprop-mode
              conf-ppd-mode mail-mode compilation-mode
              ebrowse-tree-mode diff-mode fundamental-mode
              emacs-lisp-byte-code-mode elisp-byte-code-mode
              erts-mode R-transcript-mode S-transcript-mode XLS-mode tar-mode
              git-commit-mode git-rebase-mode image-mode perl-mode
              octave-maybe-mode makefile-gmake-mode makefile-imake-mode
              makefile-makepp-mode makefile-bsdmake-mode makefile-automake-mode
              archive-mode))

(setq-local my/menu-mode-names
            '((conf-mode "Config File")
              (enriched-mode "Enriched Text")
              (conf-toml-mode "TOML")
              (ses-mode "Emacs Spreadsheet")
              (m2-mode "Modula-2")
              (cperl-mode "Perl (CPerl)")
              (hexl-mode "Hex Edit")
              (f90-mode "Fortran 90/95")
              (objc-mode "Objetive C")
              (snmpv2-mode "SNMPv2 MIBs")
              (mhtml-mode "Html (Mhtml)")
              (snmp-mode "SKMP MIBs")))

(defun my/menu-menu--get-major-mode-name (mode)
  "Gets the MODE language name.
Tries to get the value from `my/menu-mode-names'.  If not guess the language name."
  (let ((ret (assoc mode my/menu-mode-names)))
    (if (not ret)
        (setq ret (replace-regexp-in-string
                   "-" " "
                   (replace-regexp-in-string
                    "-mode" ""
                    (symbol-name mode))))
      (setq ret (car (cdr ret))))
    (setq ret (concat (upcase (substring ret 0 1))
                      (substring ret 1)))
    ret))

(defun my/menu-menu--get-major-modes ()
  "Gets a list of language modes known to `my/menu-mode'.
This gets all major modes known from the variables:
-  `interpreter-mode-alist';
-  `magic-mode-alist'
-  `magic-fallback-mode-alist'
-  `auto-mode-alist'
- `my/menu-major-mode-menu-map-extra-modes'

All other modes are assumed to be minor modes or unimportant.
"
  ;; Get known major modes
  (let ((ret '())
        all dups cur-lst current-letter
        added-modes
        (modes '()))
    (dolist (elt my/menu-major-mode-menu-map-extra-modes)
      (unless (memq elt modes)
        (when (and (functionp elt)
                   (ignore-errors (string-match "-mode$" (symbol-name elt))))
          (unless (or (memq elt my/menu-excluded-major-modes)
                      (member (downcase (symbol-name elt)) added-modes))
            (let* ((name (my/menu-menu--get-major-mode-name elt))
                   (first (upcase (substring name 0 1))))
              (if (member first all)
                  (unless (member first dups)
                    (push first dups))
                (push first all))
              (push (list elt 'menu-item
                          name
                          elt)
                    ret))
            (push (downcase (symbol-name elt)) added-modes)
            (push elt modes)))))
    (dolist (elt (append
                  interpreter-mode-alist
                  magic-mode-alist
                  magic-fallback-mode-alist
                  auto-mode-alist))
      (unless (memq (cdr elt) modes)
        (when (and (functionp (cdr elt))
                   (ignore-errors (string-match "-mode$" (symbol-name (cdr elt)))))
          (unless (or (memq (cdr elt) my/menu-excluded-major-modes)
                      (member (downcase (symbol-name (cdr elt))) added-modes))
            (let* ((name (my/menu-menu--get-major-mode-name (cdr elt)))
                   (first (upcase (substring name 0 1))))
              (if (member first all)
                  (unless (member first dups)
                    (push first dups))
                (push first all))
              (push (list (cdr elt) 'menu-item
                          name
                          (cdr elt))
                    ret))
            (push (downcase (symbol-name (cdr elt))) added-modes)
            (push (cdr elt) modes)))))
    (setq modes (sort ret (lambda(x1 x2) (string< (downcase (nth 2 x2))
                                                  (downcase (nth 2 x1)))))
          my/menu-menu--get-major-modes (mapcar (lambda(x) (intern x)) added-modes))
    (setq ret '())
    (dolist (elt modes)
      (let ((this-letter (upcase (substring (nth 2 elt) 0 1))))
        (cond
         ((not (member this-letter dups))
          ;; not duplicated -- add prior list and push current element.
          (when cur-lst
            (push `(,(intern current-letter) menu-item ,current-letter
                    (keymap ,@cur-lst)) ret))
          (push elt ret)
          (setq current-letter this-letter)
          (setq cur-lst nil))
         ((not (equal this-letter current-letter))
          ;; duplicated, but not last letter.
          (when cur-lst
            (push `(,(intern current-letter) menu-item ,current-letter
                    (keymap ,@cur-lst)) ret))
          (setq cur-lst nil)
          (setq current-letter this-letter)
          (push elt cur-lst))
         (t
          ;; duplicated and last letter
          (push elt cur-lst)))))
    (when cur-lst
      (push `(,(intern current-letter) menu-item ,current-letter
              (keymap ,@cur-lst)) ret))
    ;; Now create nested menu.
    `(keymap ,@ret
             (separator1 menu-item "--")
             (package menu-item  "Find more languages" list-packages))))

;;; Major Modes Menu
(keymap-set-after (current-global-map) "<menu-bar> <major-modes-menu>"
  (cons "Lang-Modes"  (my/menu-menu--get-major-modes))
  'view)

;;; Edit menu
(keymap-set-after (current-global-map) "<menu-bar> <edit> <blank-operations>"
  (cons "Blank/Whitespace Operations"
        '(keymap
          (trim-trailing-space menu-item
                               "Trim Trailing Space"
                               delete-trailing-whitespace
                               :help "Trim Trailing spaces on each line")
          (separator-tabify menu-item "--")
          (tabify-region menu-item
                         "Change multiple spaces to tabs (Tabify)"
                         (lambda() (interactive)
                           (if mark-active
                               (tabify (region-beginning)
                                       (region-end))
                             (tabify (point-min) (point-max))))
                         :help "Convert multiple spaces in the nonempty region to tabs when possible"
                         :enable  (not buffer-read-only))
          (untabify menu-item
                    "Change Tabs To Spaces (Untabify)"
                    (lambda() (interactive)
                      (if mark-active
                          (untabify (region-beginning)
                                    (region-end))
                        (untabify (point-min) (point-max))))
                    :help "Convert all tabs in the nonempty region or buffer to multiple spaces"
                    :enable (not buffer-read-only))))
  'separator-search)

(keymap-set-after (current-global-map) "<menu-bar> <edit> <change-case>"
  (cons "Convert Case To"
        '(keymap
          (capitalize-region menu-item
                             "Capitalize" capitalize-region
                             :help "Capitalize (initial caps) words in the nonempty region"
                             :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (downcase-region menu-item
                           "downcase" downcase-region
                           :help "Make words in the nonempty region lower-case"
                           :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (upcase-region menu-item "UPCASE" upcase-region
                         :help "Make words in the nonempty region upper-case"
                         :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning)))))
        )
  'blank-operations)

(keymap-set-after (current-global-map) "<menu-bar> <edit> <sort>"
  (cons "Sort"
        '(keymap
          (regexp-fields menu-item
                         "Regexp Fields" sort-regexp-fields
                         :help "Sort the nonempty region lexicographically"
                         :enable (and last-kbd-macro
                                      (not buffer-read-only)
                                      mark-active
                                      (> (region-end) (region-beginning))))
          (pages menu-item
                 "Pages" sort-pages
                 :help "Sort pages in the nonempty region alphabetically"
                 :enable (and last-kbd-macro
                              (not buffer-read-only)
                              mark-active
                              (> (region-end) (region-beginning))))
          (sort-paragraphs menu-item
                           "Alphabetically" sort-paragraphs
                           :help "Sort paragraphs in the nonempty region alphabetically"
                           :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (sort-numeric-fields menu-item
                               "Numeric Field" sort-numeric-fields
                               :help "Sort lines in the nonempty region numerically by the Nth field"
                               :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (sort-fields menu-item
                       "Field" sort-fields
                       :help "Sort lines in the nonempty region lexicographically by the Nth field"
                       :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (sort-columns menu-item
                        "Columns" sort-columns
                        :help "Sort lines in the nonempty region alphabetically, by a certain range of columns"
                        :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (sort-lines menu-item
                      "Lines" sort-lines
                      :help "Sort lines in the nonempty region alphabetically"
                      :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning))))
          (reverse-region menu-item "Reverse" reverse-region
                          :help "Reverse the order of the selected lines"
                          :enable (and (not buffer-read-only)  mark-active  (> (region-end) (region-beginning)))))
        )
  'change-case)

(easy-menu-add-item (lookup-key global-map [menu-bar file])
                    nil
                    ["Restart Emacs" restart-emacs
                     :help "Kill the current Emacs process and start a new one"]
                    "Quit")
