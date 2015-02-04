;; https://github.com/seth/my-emacs-dot-d/blob/master/init.el
;; NOTE: The name of the Org files is important!  When a file gets tangled,
;; it gets the same base name as the Org file.  Thus, tangling Emacs Lisp from
;; a file `init.org` would generate `init.el`, obliterating this file in the
;; process. So your config org file should not be named "init.org".

(org-babel-load-file "~/.emacs.d/awaken.org")
