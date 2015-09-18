;; https://github.com/seth/my-emacs-dot-d/blob/master/init.el
;; NOTE: The name of the Org files is important!  When a file gets tangled,
;; it gets the same base name as the Org file.  Thus, tangling Emacs Lisp from
;; a file `init.org` would generate `init.el`, obliterating this file in the
;; process. So your config org file should not be named "init.org".

(let ((file-name-handler-alist nil))
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  ;; don't want emacs messing with init
  (setq custom-file "~/.emacs.d/custom.el")

  ;; prevent prompt when loading org file
  (setq vc-follow-symlinks "t")

  ;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
  ;; don't garbage collect during initialization
  (setq gc-cons-threshold (* 100 1024 1024))
  (org-babel-load-file "~/.emacs.d/awaken.org")
  ;; reset to slightly higher than default; idle timer suggested by vermiculus
  (run-with-idle-timer
   10 nil
   (lambda ()
     (setq gc-cons-threshold (* 100 1024))
     (message "gc-cons-threshold restored to %S"
              gc-cons-threshold))))
