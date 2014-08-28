;; attempt at bindings
;; TODO:
;; .experiment more on replicating unite and sneak functionality
;; .implement all nav bindings
;; .try out org mode and magit
;; .actually get marker folding working
;; .cry

(global-set-key(kbd "§") 'save-buffer)

;; http://blog.zhengdong.me/2012/03/14/how-i-manage-emacs-packages
;; http://ergoemacs.org/emacs/emacs_package_system.html
;; package management; melpa > marmalade

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
    surround
    ; evil-numbers
    ; evil-leader
    ; evil-visualstar
    evil-nerd-commenter
    ; evil-indent-textobject
    ; evil-matchit
    projectile
    flx-ido
    helm
    helm-projectile
    elscreen
    linum-relative
    ace-jump-mode
    browse-kill-ring
    popup-kill-ring
    fiplr
    bookmark+
    aurel)
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

;; terminal emacs can't access x libraries and clipboard.. wtf?
;; http://comments.gmane.org/gmane.emacs.vim-emulation/2019
;; copy and paste to from clipboard in terminal emacs {{{
;; works with evil
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; I prefer using the "clipboard" selection (the one the
;; typically is used by c-c/c-v) before the primary selection
;; (that uses mouse-select/middle-button-click)
(setq x-select-enable-clipboard t)

;; If emacs is run in a terminal, the clipboard- functions have no
;; effect. Instead, we use of xsel, see
;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
;; program for getting and setting the contents of the X selection"
(unless window-system
 (when (getenv "DISPLAY")
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      ;; I prefer using the "clipboard" selection (the one the
      ;; typically is used by c-c/c-v) before the primary selection
      ;; (that uses mouse-select/middle-button-click)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
	xsel-output )))
  ;; Attach callbacks to hooks
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function)
  ;; Idea from
  ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
  ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
 ))
;}}}


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
; (define-key evil-normal-state-map "s" 'ace-jump-word-mode)

;; TODO: make silent
;; gui emacs likes to get screwed up and doesn't work with certain commands
;; unmap "r" and "s"
(define-key evil-normal-state-map "s" nil)
(define-key evil-normal-state-map "r" nil)
(define-key evil-normal-state-map "R" nil)
;; equiv of if has gui running
(if (display-graphic-p)
    (progn
    ;; if gui
    ;; wm experimentation;{{{
    ;; "r" is redraw;{{{
    ;; worskpace/Destkop switch"{{{
    (define-key evil-normal-state-map (kbd "ra") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^1"))))
    (define-key evil-normal-state-map (kbd "rr") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^2"))))
    (define-key evil-normal-state-map (kbd "rs") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^3"))))
    (define-key evil-normal-state-map (kbd "rs") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^4"))))
    (define-key evil-normal-state-map (kbd "rd") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^5"))))
    (define-key evil-normal-state-map (kbd "rh") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^6"))))
    (define-key evil-normal-state-map (kbd "rn") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^7"))))
    (define-key evil-normal-state-map (kbd "re") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^8"))))
    (define-key evil-normal-state-map (kbd "ri") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^9"))))
    (define-key evil-normal-state-map (kbd "ro") (lambda () (interactive) (shell-command (concat "bspc desktop -f ^10"))))
    ;;}}}
    ;; move to desktop;{{{
    (define-key evil-normal-state-map (kbd "Ra") (lambda () (interactive) (shell-command (concat "bspc window -d ^1"))))
    (define-key evil-normal-state-map (kbd "Rr") (lambda () (interactive) (shell-command (concat "bspc window -d ^2"))))
    (define-key evil-normal-state-map (kbd "Rs") (lambda () (interactive) (shell-command (concat "bspc window -d ^3"))))
    (define-key evil-normal-state-map (kbd "Rt") (lambda () (interactive) (shell-command (concat "bspc window -d ^4"))))
    (define-key evil-normal-state-map (kbd "Rd") (lambda () (interactive) (shell-command (concat "bspc window -d ^5"))))
    (define-key evil-normal-state-map (kbd "Rh") (lambda () (interactive) (shell-command (concat "bspc window -d ^6"))))
    (define-key evil-normal-state-map (kbd "Rn") (lambda () (interactive) (shell-command (concat "bspc window -d ^7"))))
    (define-key evil-normal-state-map (kbd "Re") (lambda () (interactive) (shell-command (concat "bspc window -d ^8"))))
    (define-key evil-normal-state-map (kbd "Ri") (lambda () (interactive) (shell-command (concat "bspc window -d ^9"))))
    (define-key evil-normal-state-map (kbd "Ro") (lambda () (interactive) (shell-command (concat "bspc window -d ^10"))))
    ;;}}}

    ;; moving windows within desktop {{{
    ;; move to biggest
    (define-key evil-normal-state-map (kbd "rcm") (lambda () (interactive) (shell-command (concat "bspc window -s biggest"))))
    ;; directional
    (define-key evil-normal-state-map (kbd "rch") (lambda () (interactive) (shell-command (concat "bspc window -s left"))))
    (define-key evil-normal-state-map (kbd "rcn") (lambda () (interactive) (shell-command (concat "bspc window -s down"))))
    (define-key evil-normal-state-map (kbd "rce") (lambda () (interactive) (shell-command (concat "bspc window -s up"))))
    (define-key evil-normal-state-map (kbd "rci") (lambda () (interactive) (shell-command (concat "bspc window -s right"))))
    ;; circulate
    (define-key evil-normal-state-map (kbd "r\.") (lambda () (interactive) (shell-command (concat "bspc desktop -C forward"))))
    (define-key evil-normal-state-map (kbd "r\,") (lambda () (interactive) (shell-command (concat "bspc desktop -C backward"))))
    ;; }}} 
    ;; resize
    (define-key evil-normal-state-map (kbd "rmh") (lambda () (interactive) (shell-command (concat "~/bin/resize.sh left"))))
    (define-key evil-normal-state-map (kbd "rmn") (lambda () (interactive) (shell-command (concat "~/bin/resize.sh down"))))
    (define-key evil-normal-state-map (kbd "rme") (lambda () (interactive) (shell-command (concat "~/bin/resize.sh up"))))
    (define-key evil-normal-state-map (kbd "rmi") (lambda () (interactive) (shell-command (concat "~/bin/resize.sh right"))))
    ;; other
    (define-key evil-normal-state-map (kbd "ru") (lambda () (interactive) (shell-command (concat "urxvt &"))))
    ;;}}}
    ;; "s" is select/show/settings
    ;; select;{{{
    (define-key evil-normal-state-map (kbd "sh") (lambda () (interactive) (shell-command (concat "bspc window -f left"))))
    (define-key evil-normal-state-map (kbd "sn") (lambda () (interactive) (shell-command (concat "bspc window -f down"))))
    (define-key evil-normal-state-map (kbd "se") (lambda () (interactive) (shell-command (concat "bspc window -f up"))))
    (define-key evil-normal-state-map (kbd "si") (lambda () (interactive) (shell-command (concat "bspc window -f right"))))
    (define-key evil-normal-state-map (kbd "sl") (lambda () (interactive) (shell-command (concat "bspc window -f last"))))
    ;;}}}
    ;; monocle toggle 
    (define-key evil-normal-state-map (kbd "st") (lambda () (interactive) (shell-command (concat "bspc desktop -l next"))))
    (define-key evil-normal-state-map (kbd "ss") (lambda () (interactive) (shell-command (concat "bspc window -t sticky"))))
    (define-key evil-normal-state-map (kbd "sf") (lambda () (interactive) (shell-command (concat "bspc window -t fullscreen"))))
    ;; gap up and down
    (define-key evil-normal-state-map (kbd "su") (lambda () (interactive) (shell-command (concat "bspc config -d focused window_gap $((`bspc config -d focused window_gap` + 4 ))"))))
    (define-key evil-normal-state-map (kbd "sd") (lambda () (interactive) (shell-command (concat "bspc config -d focused window_gap $((`bspc config -d focused window_gap` - 4 ))"))))
    ;; preselect;{{{
    (define-key evil-normal-state-map (kbd "sph") (lambda () (interactive) (shell-command (concat "bspc window -p left"))))
    (define-key evil-normal-state-map (kbd "spn") (lambda () (interactive) (shell-command (concat "bspc window -p down"))))
    (define-key evil-normal-state-map (kbd "spe") (lambda () (interactive) (shell-command (concat "bspc window -p up"))))
    (define-key evil-normal-state-map (kbd "spi") (lambda () (interactive) (shell-command (concat "bspc window -p right"))))
    (define-key evil-normal-state-map (kbd "spx") (lambda () (interactive) (shell-command (concat "bspc window -p cancel"))))
    (define-key evil-normal-state-map (kbd "spd") (lambda () (interactive) (shell-command (concat "bspc desktop -c"))))
    ;;}}}
    ;;}}}
      )
    ;; else tmux;{{{
    ;; "r" is redraw;{{{
    ;; window switching;{{{
    (define-key evil-normal-state-map (kbd "ra") (lambda () (interactive) (shell-command (concat "tmux select-window -t 1"))))
    (define-key evil-normal-state-map (kbd "rr") (lambda () (interactive) (shell-command (concat "tmux select-window -t 2"))))
    (define-key evil-normal-state-map (kbd "rs") (lambda () (interactive) (shell-command (concat "tmux select-window -t 3"))))
    (define-key evil-normal-state-map (kbd "rt") (lambda () (interactive) (shell-command (concat "tmux select-window -t 4"))))
    (define-key evil-normal-state-map (kbd "rd") (lambda () (interactive) (shell-command (concat "tmux select-window -t 5"))))
    (define-key evil-normal-state-map (kbd "rh") (lambda () (interactive) (shell-command (concat "tmux select-window -t 6"))))
    (define-key evil-normal-state-map (kbd "rn") (lambda () (interactive) (shell-command (concat "tmux select-window -t 7"))))
    (define-key evil-normal-state-map (kbd "re") (lambda () (interactive) (shell-command (concat "tmux select-window -t 8"))))
    (define-key evil-normal-state-map (kbd "ri") (lambda () (interactive) (shell-command (concat "tmux select-window -t 9"))))
    (define-key evil-normal-state-map (kbd "ro") (lambda () (interactive) (shell-command (concat "tmux select-window -t 10"))))
    ;;}}}

    ;; resize panes"{{{
    (define-key evil-normal-state-map (kbd "rmh") (lambda () (interactive) (shell-command (concat "tmux resize-pane -L 10"))))
    (define-key evil-normal-state-map (kbd "rmn") (lambda () (interactive) (shell-command (concat "tmux resize-pane -D 10"))))
    (define-key evil-normal-state-map (kbd "rme") (lambda () (interactive) (shell-command (concat "tmux resize-pane -U 10"))))
    (define-key evil-normal-state-map (kbd "rmi") (lambda () (interactive) (shell-command (concat "tmux resize-pane -R 10"))))
    ;;}}}

    ;; circulate
    ;; previous
    (define-key evil-normal-state-map (kbd "r\,") (lambda () (interactive) (shell-command (concat "tmux swap-pane -U"))))
    ;; next
    (define-key evil-normal-state-map (kbd "r\.") (lambda () (interactive) (shell-command (concat "tmux swap-pane -D"))))
    ;; new session
    (define-key evil-normal-state-map (kbd "r\_") (lambda () (interactive) (shell-command (concat "tmux new-session"))))
    ;; new window
    (define-key evil-normal-state-map (kbd "rc") (lambda () (interactive) (shell-command (concat "tmux new-window"))))
    ;; kill pane
    (define-key evil-normal-state-map (kbd "rx") (lambda () (interactive) (shell-command (concat "tmux kill-pane"))))
    ;; last window
    (define-key evil-normal-state-map (kbd "rl") (lambda () (interactive) (shell-command (concat "tmux last-window"))))
    ;; split windows
    (define-key evil-normal-state-map (kbd "r\/") (lambda () (interactive) (shell-command (concat "tmux split-window -h"))))
    (define-key evil-normal-state-map (kbd "r\-") (lambda () (interactive) (shell-command (concat "tmux split-window"))))
    ;; break pane
    (define-key evil-normal-state-map (kbd "r\!") (lambda () (interactive) (shell-command (concat "tmux break-pane"))))
    ;;}}}
    ;; "s" is select;{{{
    ;; directional
    (define-key evil-normal-state-map (kbd "sh")
                (lambda ()
                  (interactive)
                  (shell-command (concat "tmux select-pane -L"))))
    
      )
    (define-key evil-normal-state-map (kbd "sn") (lambda () (interactive) (shell-command (concat "tmux select-pane -D"))))
    (define-key evil-normal-state-map (kbd "se") (lambda () (interactive) (shell-command (concat "tmux select-pane -U"))))
    (define-key evil-normal-state-map (kbd "si") (lambda () (interactive) (shell-command (concat "tmux select-pane -R"))))
    ;; last
    (define-key evil-normal-state-map (kbd "sl") (lambda () (interactive) (shell-command (concat "tmux select-pane -l"))))

    ;; select layout
    (define-key evil-normal-state-map (kbd "sv") (lambda () (interactive) (shell-command (concat "tmux select-layout main-vertical"))))
    ;; toggle "monocle" (zoom)
    (define-key evil-normal-state-map (kbd "st") (lambda () (interactive) (shell-command (concat "tmux resize-pane -Z"))))

    ;; select session
    (define-key evil-normal-state-map (kbd "ss") (lambda () (interactive) (shell-command (concat "tmux choose-client"))))
    ;;}}}
    ;;}}}

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
(helm-mode 1)

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
