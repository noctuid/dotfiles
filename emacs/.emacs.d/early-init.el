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

;; ** Set Font
(push '(font . "Office Code Pro-10") default-frame-alist)

;; ** Don't Load Site Startup File or Default Init File
;; I don't use these, but not trying to load them has no noticeable impact on
;; startup time
;; (setq site-run-file nil
;;       inhibit-default-init t)
