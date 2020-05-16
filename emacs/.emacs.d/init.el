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
   ;; (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   ;; https://github.com/emacs-lsp/lsp-mode#performance
   ;; TODO try out different values
   (setq gc-cons-threshold 100000000)
   (message "gc-cons-threshold restored to %S" gc-cons-threshold)))

;; NOTE: Altering `file-name-handler-alist' can potentially cause problems:
;; https://www.reddit.com/r/emacs/comments/8cpkc3/emacs_lite_just_the_essentials_config_in_200_lines/dxhhnfc/
;; https://www.reddit.com/r/emacs/comments/83l1g1/creating_a_quicker_startup_in_a_fashion_like/dwe41y7/
(defvar noct-file-name-handler-alist-backup file-name-handler-alist)

(setq file-name-handler-alist nil)

(defun noct-restore-file-name-handler-alist ()
  (when noct-file-name-handler-alist-backup
    (setq file-name-handler-alist (cl-union noct-file-name-handler-alist-backup
                                            file-name-handler-alist))
    (setq noct-file-name-handler-alist-backup nil)))

(add-hook 'after-init-hook #'noct-restore-file-name-handler-alist)
(add-hook 'desktop-save-mode-hook #'noct-restore-file-name-handler-alist)

;; * Immediate Setup/Helpers
(require 'cl-lib)

;; ** Settings
(cl-pushnew (expand-file-name "lisp" user-emacs-directory)
            load-path :test #'string=)

(setq load-prefer-newer t
      ;; TODO check if `vc-follow-symlinks' is needed and works without this
      ;; I don't use vc
      ;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
      ;; https://magit.vc/manual/magit/Performance.html
      vc-handled-backends nil
      ;; don't want emacs touching this file
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      ;; no GUI prompts
      use-dialog-box nil)

(defalias 'yes-or-no-p #'y-or-n-p)

;; ** Command Line Flag Checking
(defun noct-command-line-flag-specified-p (flag)
  "Return whether FLAG was specified as an argument to Emacs.
Also tell Emacs to ignore FLAG when processing command line arguments."
  ;; prevent message about the option being unknown
  (cl-pushnew (cons flag #'ignore) command-switch-alist
              :test #'equal)
  (cl-loop for arg in command-line-args
           if (string= arg flag)
           return t))

;; ** Faster Untangling
(require 'noct-util)

(defun noct-emacs-id-file ()
  "Return the file containing the unique id for the newest Emacs instance."
  ;; `expand-file-name' handles existent/non-existent trailing slash
  (expand-file-name ".emacs-pid" user-emacs-directory))

(defun noct-current-emacs-id ()
  "Return the unique id for the current Emacs instance."
  (format "%s" before-init-time))

(defun noct-newest-emacs-id ()
  "Return the unique id for the newest Emacs instance."
  (with-temp-buffer
    (insert-file-contents (noct-emacs-id-file))
    (buffer-string)))

(defun noct-newest-emacs-instance-p ()
  "Return whether the current Emacs instance is the newest one."
  (string= (noct-current-emacs-id) (noct-newest-emacs-id)))

(write-region (noct-current-emacs-id) nil
              (noct-emacs-id-file))

;; TODO should use a lock file instead of restricting which Emacs does it
;; is elc written before complete? also need to check when determining whether
;; to load compiled init if so
(defvar noct-async-tangle-in-progress nil
  "Whether currently asynchronously tangling init.")

(defun noct-async-init-tangle ()
  "Tangle org init file asynchronously.
Only tangle when the current Emacs instance is the newest one."
  (interactive)
  (when (and (noct-newest-emacs-instance-p)
             ;; don't try to tangle if already tangling; possible for overlap to
             ;; happen if something goes wrong and the process hangs; can end up
             ;; killing cpu
             (not noct-async-tangle-in-progress)
             (require 'async nil t))
    (let ((inhibit-message t))
      (setq noct-async-tangle-in-progress t)
      (message "Asynchronously tangling org init file...")
      (async-start (lambda ()
                     (push (expand-file-name "lisp" user-emacs-directory)
                           load-path)
                     (require 'noct-util)
                     ;; with current config compiling does save some time: ~0.1s
                     (noct-tangle-awaken nil t))
                   (lambda (_)
                     (setq noct-async-tangle-in-progress nil)
                     (message "Finished tangling org init file."))))))

(defvar noct-async-tangle-timer (run-with-timer 120 120 #'noct-async-init-tangle)
  "Holds the timer to asynchronously tangle so it can be easily canceled.")

;; * Start Benchmarking
(defconst noct-benchmark-init-files
  (list (expand-file-name "straight/build/benchmark-init/benchmark-init.elc"
                          user-emacs-directory)
        (expand-file-name "straight/build/benchmark-init/benchmark-init-modes.elc"
                          user-emacs-directory)))

(defconst noct-benchmark-init
  (and (noct-command-line-flag-specified-p "--benchmark-init")
       (cl-every (lambda (x) (file-exists-p x))
                 noct-benchmark-init-files) )
  "Whether to benchmark initialization.")

(when noct-benchmark-init
  (dolist (file noct-benchmark-init-files)
    (load-file file))
  (benchmark-init/activate))

;; * Start Recording Requires
(defvar noct-requires nil
  "Alist of feature to `load-file-name' when it was first required.
Start Emacs with --record-requires to populate. This still needs work.")

(defconst noct-record-requires
  (noct-command-line-flag-specified-p "--record-requires")
  "Whether to record the current `load-file-name' when requiring a package.")

(when noct-record-requires
  (defun noct-require-advice (feature &optional filename &rest _)
    "For every require, record the current `load-file-name'."
    (unless (cond (feature
                   (featurep feature))
                  (filename
                   (load-history-filename-element
                    (purecopy (load-history-regexp filename))))
                  (t t))
      (setf (alist-get (or feature filename) noct-requires) load-file-name)))
  (advice-add 'require :before #'noct-require-advice))

;; * Load Org Config
;; NOTE can't use `let' for these
(setq debug-on-error t
      debug-on-quit t
      ;; prevent messages/flashing during initialization
      inhibit-message t)

(defconst noct-load-compiled
  (not (noct-command-line-flag-specified-p "--no-compile"))
  "Whether to load compiled init files if they exist and are newer.")

(defconst noct-retangle (noct-command-line-flag-specified-p "--retangle")
  "Whether to retangle init even if tangled init is newer than init.")

(cond ((noct-command-line-flag-specified-p "--with-demoted-errors")
       ;; this prevents errors in one source block from preventing other source
       ;; blocks from running
       ;; don't use compiled file for this
       (noct-tangle-awaken t nil noct-retangle t))
      ((noct-command-line-flag-specified-p "--stable")
       ;; NOTE for now ignoring stable elc files
       (load-file (expand-file-name "awaken-stable.el" user-emacs-directory))
       ;; TODO remove
       (let ((unclean-stable-file (expand-file "unclean-stable.el"
                                               user-emacs-directory)))
         (when (file-exists-p unclean-stable-file)
           (load-file unclean-stable-file))))
      ((noct-command-line-flag-specified-p "--profile-dotemacs")
       (load-file (expand-file-name
                   "straight/build/profile-dotemacs/profile-dotemacs.elc"
                   user-emacs-directory))
       (call-process-shell-command
        (format "cat %s %s > %s"
                (expand-file-name "awaken.el" user-emacs-directory)
                (expand-file-name "unclean.el" user-emacs-directory)
                (expand-file-name "full.el" user-emacs-directory)))
       (setq profile-dotemacs-file (expand-file-name "full.el"
                                                     user-emacs-directory)
             profile-dotemacs-low-percentage 1)
       (profile-dotemacs))
      (t
       (noct-tangle-awaken t noct-load-compiled noct-retangle)))

(setq debug-on-error nil
      debug-on-quit nil)

;; get doom mode line flicker and "nil" message otherwise
(add-hook 'after-init-hook
          (lambda ()
            (run-with-timer 1 nil (lambda ()
                                    (setq inhibit-message nil)))))


;; * End Require Recording
(defconst noct-keep-benchmarking
  (noct-command-line-flag-specified-p "--keep-benchmarking")
  "Whether to keep benchmarking and/or recording require calls after init.")

(when (and noct-record-requires
           (not noct-keep-benchmarking))
  (advice-remove 'require #'noct-require-advice))

;; * End Benchmarking
;; Ending benchmarking here doesn't capture things that load just after init,
;; but manually ending it can capture unwanted loads (e.g. if you don't require
;; helm during init but use it to deactivate benchmarking).
(when (and noct-benchmark-init
           (not noct-keep-benchmarking))
  (benchmark-init/deactivate)
  (benchmark-init/show-durations-tree))
