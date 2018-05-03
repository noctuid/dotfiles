;; -*- lexical-binding: t -*-
;; * Kill package.el
;; Must use early init file as of Emacs 27
;; improves startup speed when using an alternative package manager
(setq package-enable-at-startup nil
      ;; this is no longer necessary
      ;; package--init-file-ensured t
      )
;; can check later that `package--initialized' is nil
