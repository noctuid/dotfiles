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
;; doing this here reduces init time by ~0.2 seconds for me
;; disabling `tool-bar-mode' in normal init file takes ~0.1s
;; https://github.com/raxod502/radian/issues/180
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(let ((gap (or (getenv "WM_GAP") "15")))
  (push (cons 'internal-border-width (string-to-number gap)) default-frame-alist))

;; unfortunately Emacs has no way to exclude text from being transparent
;; active and inactive alpha
(push '(alpha . (85 . 85)) default-frame-alist)

;; no titlebar
;; added in a patch; see my emacs.nix overlay
(when (eq system-type 'darwin)
  (push '(undecorated-round . t) default-frame-alist))

;; ** Set Font
;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
;; "Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default."
;; I haven't actually noticed any difference for fonts I've tried
(setq frame-inhibit-implied-resize t)

(defconst noct-preferred-fonts (list
                                ;; Delugia is basically a better CaskaydiaCove
                                ;; (mainly it has italics and CaskaydiaCove Nerd
                                ;; Font still doesn't)
                                "Delugia-10"
                                "CaskaydiaCove Nerd Font-10"
                                "Cascadia Code-10"
                                "Office Code Pro D-10")
  "Fonts to try to use as the default if they exist(in order of priority).")

(require 'cl-lib)

(defun noct-maybe-set-default-font (&optional frame)
  "Before creating the first frame, set the default font.
The point of this is to set the font using `default-frame-alist' rather than
`set-face-attribute' or `set-frame-font' as the former is faster.  Adding this
to `after-make-frame-functions' is necessary because `find-font' does not work
immediately in early-init.el.  However, this will not work for the first frame
with a daemon Emacs, so this function will store found fonts, so that we can
check for them wit h`noct-known-font' in the early-init.el.  Ideally there is
some better way to do this.

If the font is already set in `default-frame-alist', this will still try to look
for fonts that appear before it in `noct-preferred-fonts' (if there are any) so
that preferred fonts are still discovered and marked as known/existing."
  (let ((current-font (assq 'font default-frame-alist)))
    (cl-dolist (font noct-preferred-fonts)
      (when (equal font current-font)
        (cl-return))
      (when (find-font (font-spec :name font) frame)
        (push (cons 'font font) default-frame-alist)
        (with-temp-buffer
          (write-file (expand-file-name font user-emacs-directory)))
        (cl-return)))))

(defun noct-known-font ()
  "Return a font from `noct-preferred-fonts' that has been previously found.
If no fonts have been found, return nil."
  (cl-dolist (font noct-preferred-fonts)
    (when (file-exists-p (expand-file-name font user-emacs-directory))
      (cl-return font))))

(let ((known-font (noct-known-font)))
  (when known-font
    (push (cons 'font known-font) default-frame-alist))
  (unless (and known-font
               ;; still check if #1 preferred font exists after init
               (equal known-font (car noct-preferred-fonts)))
    ;; this is too late when using the server
    (add-hook 'after-make-frame-functions #'noct-maybe-set-default-font)))

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

;; * Early LSP Configuration
(setenv "LSP_USE_PLISTS" "true")
