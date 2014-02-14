;; attempt at bindings

(global-set-key(kbd "§") 'save-buffer)

;; http://blog.zhengdong.me/2012/03/14/how-i-manage-emacs-packages
;; http://ergoemacs.org/emacs/emacs_package_system.html
;; package management; melpa > marmalade?

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(rainbow-mode
    evil
    undo-tree
    projectile
    flx-ido
    helm
    helm-projectile
    surround
    elscreen
    linum-relative
    ace-jump-mode
    evil-nerd-commenter
    browse-kill-ring
    popup-kill-ring
    bookmark+)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

(package-initialize)
(evil-mode 1)        ;; enable evil-mode
;; enable projectile
(projectile-global-mode)
;; elscreen
(elscreen-start)
(global-linum-mode 1)

;; relative line numbers; http://stackoverflow.com/questions/6874516/relative-line-numbers-in-emacs ;;{{{rar
(defvar my-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
                             (count-lines (point-min) (point-max))))))
         (format (concat "%" (number-to-string width) "d")))
    (setq my-linum-format-string format)))

; (defvar my-linum-current-line-number (line-number-at-pos))

(setq linum-format 'my-linum-relative-line-numbers)

(defun my-linum-relative-line-numbers (line-number)
  (let ((offset (- line-number my-linum-current-line-number)))
    (propertize (format my-linum-format-string offset) 'face 'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)
 ;;}}}


;; closest thing to sneak, easymotion, etc.
(define-key evil-normal-state-map "s" 'ace-jump-word-mode)

;; use projectile even without project
(setq projectile-require-project-root nil)

;;commenting

;; don't use emacs state when I do stuff like :list-packages
;; changed from motion to normal
(setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))
(setq evil-emacs-state-modes nil)

;; change mode-line color by evil state; http://www.emacswiki.org/emacs/Evil
;; compensates for lack of cursor change in temrinal as well
   (lexical-let ((default-color (cons (face-background 'mode-line)
                                      (face-foreground 'mode-line))))
     (add-hook 'post-command-hook
       (lambda ()
         (let ((color (cond ((minibufferp) default-color)
                            ((evil-insert-state-p) '("#55818A" . "#ffffff"))
                            ((evil-visual-state-p) '("#8A5E55" . "#ffffff"))
                            ; ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                            ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                            (t default-color))))
           (set-face-background 'mode-line (car color))
           (set-face-foreground 'mode-line (cdr color))))))

;; more vim like tabs; http://www.emacswiki.org/emacs/Evil;;{{{
(define-key evil-normal-state-map (kbd "\\ t") 'elscreen-create) ;create tab

;; kill buffer; keep open window/tab
(define-key evil-normal-state-map (kbd "\\ d") (kbd "M-x kill-buffer RET"))

(define-key evil-normal-state-map (kbd "\\ D") 'elscreen-kill) ;kill tab
;; rename
(define-key evil-normal-state-map (kbd "\\ r") 'elscreen-screen-nickname)

(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab

;; kill ring
(define-key evil-normal-state-map (kbd "SPC y") 'browse-kill-ring)

;; bookmark based on tabs

;; buffer search
(define-key evil-normal-state-map (kbd "\\ p") 'helm-mini)

;; file search (ctrlp like)
(define-key evil-normal-state-map (kbd "\\ P") 'helm-find-files)

;;helm mode.. show as type completion so now.. if do :help f will show functions in helm window and change as type

;; persistent mini buffer history and kill ring and such
;; http://stackoverflow.com/questions/1229142/how-can-i-save-my-mini-buffer-history-in-emacs
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/tmp/savehist")




;; vim like folding ({{{}}}); folding.el; not in melpa
;; total pain, so going to do later
; (add-to-list 'load-path "~/.emacs.d/")
; (load "folding" 'nomessage 'noerror)
; (folding-mode-add-find-file-hook)
; (folding-add-to-marks-list 'emacs-lisp-mode "{{{" "}}}" nil t)
; fuck that ^ shit



;; vim folding
(defun set-vim-foldmarker (fmr)
  "Set Vim-type foldmarkers for the current buffer"
  (interactive "sSet local Vim foldmarker: ")
  (if (equal fmr "")
      (message "Abort")
    (setq fmr (regexp-quote fmr))
    (set (make-local-variable 'outline-regexp)
         (concat ".*" fmr "\\([0-9]+\\)"))
    (set (make-local-variable 'outline-level)
         `(lambda ()
            (save-excursion
              (re-search-forward
               ,(concat fmr "\\([0-9]+\\)") nil t)
              (string-to-number (match-string 1)))))))
;; and turn it on
; (defun turn-on-vim-folding () (outline-minor 1))
; (add-hook 'find-file-hooks 'turn-on-flyspell)
; (outline-minor-mode 1)
; (set-vim-foldmarker "{{{")

;;number of lines a column is shifted with >
(setq evil-shift-width 4)
;; evil regexp search
;; wrap search (default)
(setq evil-search-wrap t)
;; for gui
(setq evil-insert-state-cursor '("green" bar))

;; keybindings (see evil-maps.el)
;; reload emacs config; tested and works.. I wonder if change M-x to : would work..
(define-key evil-normal-state-map (kbd "\\ s") (kbd "M-x load-file RET ~/.emacs RET"))

;; swap ; and :
(define-key evil-normal-state-map ";" 'evil-ex)
(define-key evil-normal-state-map ":" 'evil-repeat-find-char)
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;; colemak
(define-key evil-normal-state-map "n" 'evil-next-line)
(define-key evil-normal-state-map "e" 'evil-previous-line)
(define-key evil-visual-state-map "n" 'evil-next-line)
(define-key evil-visual-state-map "e" 'evil-previous-line)
(define-key evil-motion-state-map "n" 'evil-next-line)
(define-key evil-motion-state-map "e" 'evil-previous-line)

(define-key evil-normal-state-map "k" 'evil-search-next)
(define-key evil-normal-state-map "K" 'evil-search-previous)
; (define-key evil-normal-state-map "n" (kbd "nzozz"))


(define-key evil-normal-state-map "l" 'evil-jump-backward)
(define-key evil-normal-state-map "L" 'evil-jump-forward)



(define-key evil-normal-state-map "U" 'undo-tree-redo)
;; look into these packages: fasd, bitlebee, weechat, launch, magit, muttrc mode, org
