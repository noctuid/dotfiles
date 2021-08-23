;; -*- lexical-binding: t -*-
;; * Improve Startup Speed
;; ** Kill package.el
;; Must use early init file as of Emacs 27
;; improves startup speed when using an alternative package manager
(setq package-enable-at-startup nil
      ;; this is no longer necessary
      ;; package--init-file-ensured t
      )
;; can check later that `package--initialized' is nil

;; ** Disable Tool Bar, Menu Bar, and Scroll Bar
;; doing this here reduces init time by ~0.2 seconds
;; disabling `tool-bar-mode' in normal init file takes ~0.1s
;; https://github.com/raxod502/radian/issues/180
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(let ((gap (or (getenv "WM_GAP") 15)))
  (push (cons 'internal-border-width (string-to-number gap)) default-frame-alist))

;; ** Set Font
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; "Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default."
;; I haven't actually noticed any difference for fonts I've tried
(setq frame-inhibit-implied-resize t)

(push '(font . "Office Code Pro-10") default-frame-alist)

;; ** Don't Load Site Startup File or Default Init File
;; not loading default.el actually does have an impact (sometimes? it seems like
;; it's loaded after init and not always?)
(setq site-run-file nil
      inhibit-default-init t)

;; * Prevent Default Mode Line from Showing
;; https://github.com/hlissner/doom-emacs/blob/7460e9e7989c9b219879073690e6f43ac535d274/modules/ui/modeline/config.el#L16
;; doesn't actually need to be set this early but it still makes sense to put it
;; here
(unless after-init-time
  ;; prevent flash of unstyled modeline at startup
  (setq-default mode-line-format nil))
