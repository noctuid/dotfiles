;; -*- lexical-binding: t -*-
;; * Improve Startup Speed
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold most-positive-fixnum)

;; reset gc-cons-threshold
;; idle timer suggested by vermiculus
(run-with-idle-timer
 10 nil
 (lambda ()
   ;; recommended amount by flx
   ;; (setq gc-cons-threshold (* 20 100 100))
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

;; NOTE: Altering `file-name-handler-alist' can potentially cause problems:
;; https://www.reddit.com/r/emacs/comments/8cpkc3/emacs_lite_just_the_essentials_config_in_200_lines/dxhhnfc/
(defvar noct:file-name-handler-alist-backup file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun noct:restore-file-name-handler-alist ()
  (when noct:file-name-handler-alist-backup
    (setq file-name-handler-alist (cl-union noct:file-name-handler-alist-backup
                                            file-name-handler-alist))
    (setq noct:file-name-handler-alist-backup nil)))

(add-hook 'after-init-hook #'noct:restore-file-name-handler-alist)
(add-hook 'desktop-save-mode-hook #'noct:restore-file-name-handler-alist)

;; * Immediate Setup/Helpers
;; ** Settings
(push "~/.emacs.d/lisp/" load-path)

(setq load-prefer-newer t
      ;; I don't use vc
      ;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
      ;; https://magit.vc/manual/magit/Performance.html
      vc-handled-backends nil
      ;; don't want emacs touching this file
      custom-file "~/.emacs.d/custom.el"
      ;; no GUI prompts
      use-dialog-box nil)

(defalias 'yes-or-no-p #'y-or-n-p)

;; ** Faster Untangling
(require 'noct-util)

(defun noct:async-init-tangle ()
  "Tangle org init file asynchronously. "
  (interactive)
  (let ((inhibit-message t))
    (message "Asynchronously tangling org init file...")
    (async-start (lambda ()
                   (push "~/.emacs.d/lisp/" load-path)
                   (require 'noct-util)
                   (noct:tangle-init))
                 (lambda (_)
                   (let ((inhibit-message t))
                     (message "Finished tangling org init file."))))))

(run-with-timer 300 300 #'noct:async-init-tangle)

;; * Load Org Config
(setq debug-on-error t
      debug-on-quit t)

;; init not currently compatible with compilation
(noct:tangle-init t)

(setq debug-on-error nil
      debug-on-quit nil)
