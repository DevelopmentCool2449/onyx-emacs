;; -*- lexical-binding: t; -*-
;; This file is loaded before the package system and GUI is initialized.
;; Intended for:
;; - Setting GC thresholds for faster startup
;; - Disabling unnecessary UI elements early
;; - Setting up basic frame parameters
;; - Optimizing startup performance

;; Variables
(setopt debug-on-error (and (not noninteractive)
                            init-file-debug)
        inhibit-startup-screen t
        native-comp-async-report-warnings-errors 'silent
        ;; smooth frame rezising..
        frame-resize-pixelwise t)

;; Debug config files
(if (getenv "DEBUG")
    (setq init-file-debug t))

;; Kill scratch Buffer
(if (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))

;; Disable scroll-bars and fringes in minibuffer.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)
            (set-window-fringes
             (minibuffer-window frame) 0 0 nil t)))

;; Performance optimization during startup
(let ((normal-gc-cons-threshold gc-cons-threshold)
      (normal-gc-cons-percentage gc-cons-percentage)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold most-positive-fixnum)
      (init-gc-cons-percentage 0.6))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage init-gc-cons-percentage
        file-name-handler-alist nil)
  (add-hook 'after-init-hook
            `(lambda ()
               (setq gc-cons-threshold ,normal-gc-cons-threshold
                     gc-cons-percentage ,normal-gc-cons-percentage
                     file-name-handler-alist ',normal-file-name-handler-alist))))

(when (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
                         (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path)
  (setopt image-scaling-factor 3.0))
