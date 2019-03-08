;; -*- lexical-binding: t -*-
;; * Improve Startup Speed
;; also see ./early-init.el for setting changes that must be made at an earlier
;; stage in the init process; this section could be moved there, but setting
;; `gc-cons-threshold' earlier has no noticeable additional impact, so I'm only
;; putting things that /need/ to be done earlier in the early init file

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; huge impact to profile-dotemacs results; GC takes up a lot of init time
(setq gc-cons-threshold most-positive-fixnum)

;; reset gc-cons-threshold
;; idle timer suggested by vermiculus
(run-with-idle-timer
 10 nil
 (lambda ()
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (message "gc-cons-threshold restored to %S" gc-cons-threshold)))

;; NOTE: Altering `file-name-handler-alist' can potentially cause problems:
;; https://www.reddit.com/r/emacs/comments/8cpkc3/emacs_lite_just_the_essentials_config_in_200_lines/dxhhnfc/
;; https://www.reddit.com/r/emacs/comments/83l1g1/creating_a_quicker_startup_in_a_fashion_like/dwe41y7/
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
(require 'cl-lib)

;; ** Settings
(cl-pushnew "~/.emacs.d/lisp/" load-path :test #'string=)

(setq load-prefer-newer t
      ;; TODO check if `vc-follow-symlinks' is needed and works without this
      ;; I don't use vc
      ;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
      ;; https://magit.vc/manual/magit/Performance.html
      vc-handled-backends nil
      ;; don't want emacs touching this file
      custom-file "~/.emacs.d/custom.el"
      ;; no GUI prompts
      use-dialog-box nil)

(defalias 'yes-or-no-p #'y-or-n-p)

(defun noct:command-line-flag-specified-p (flag)
  "Return whether FLAG was specified as an argument to emacs."
  (cl-loop for arg in command-line-args
           if (string= arg flag)
           return t))

;; ** Faster Untangling
(require 'noct-util)

(defun noct:async-init-tangle ()
  "Tangle org init file asynchronously. "
  (interactive)
  (when (require 'async nil t)
    (let ((inhibit-message t))
      (message "Asynchronously tangling org init file...")
      (async-start (lambda ()
                     (push "~/.emacs.d/lisp" load-path)
                     (require 'noct-util)
                     ;; byte-compilation makes no noticeable difference for
                     ;; current config; not byte compiling since it potentially
                     ;; causes other annoying problems
                     (noct:tangle-awaken t))
                   (lambda (_)
                     (message "Finished tangling org init file."))))))

(run-with-timer 120 120 #'noct:async-init-tangle)

;; * Start Benchmarking
(cl-pushnew (cons "--benchmark-init" #'ignore) command-switch-alist
            :test #'equal)

(defvar noct:benchmark-init-files
  '("~/.emacs.d/straight/build/benchmark-init/benchmark-init.elc"
    "~/.emacs.d/straight/build/benchmark-init/benchmark-init-modes.elc"))

(defvar noct:benchmark-init
  (and (noct:command-line-flag-specified-p "--benchmark-init")
       (cl-every (lambda (x) (file-exists-p x))
                 noct:benchmark-init-files) )
  "Whether to benchmark initialization.")

(when noct:benchmark-init
  (dolist (file noct:benchmark-init-files)
    (load-file file))
  (benchmark-init/activate))

;; * Load Org Config
(setq debug-on-error t
      debug-on-quit t)

(cond ((noct:command-line-flag-specified-p "--with-demoted-errors")
       ;; prevent message about the option being unknown
       (cl-pushnew (cons "--with-demoted-errors" #'ignore) command-switch-alist
                   :test #'equal)
       ;; this prevents errors in one source block from preventing other source
       ;; blocks from running
       (noct:tangle-awaken t nil t))
      ((noct:command-line-flag-specified-p "--stable")
       (cl-pushnew (cons "--stable" #'ignore) command-switch-alist
                   :test #'equal)
       (load-file "~/.emacs.d/awaken-stable.el")
       ;; TODO remove
       (when (file-exists-p "~/.emacs.d/unclean-stable.el")
         (load-file "~/.emacs.d/unclean-stable.el")))
      ((noct:command-line-flag-specified-p "--profile-dotemacs")
       (cl-pushnew (cons "--profile-dotemacs" #'ignore) command-switch-alist
                   :test #'equal)
       (load-file "~/.emacs.d/straight/build/profile-dotemacs/profile-dotemacs.elc")
       (setq profile-dotemacs-file (expand-file-name "~/.emacs.d/awaken.el")
             profile-dotemacs-low-percentage 1)
       (profile-dotemacs))
      (t
       (noct:tangle-awaken t)))

(setq debug-on-error nil
      debug-on-quit nil)

;; * End Benchmarking
;; Ending benchmarking here doesn't capture things that load just after init,
;; but manually ending it can capture unwanted loads (e.g. if you don't require
;; helm during init but use it to deactivate benchmarking).
(when noct:benchmark-init
  (benchmark-init/deactivate)
  (benchmark-init/show-durations-tree))
