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
    flycheck
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
;; start flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

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
(define-key evil-normal-state-map (kbd "t t") 'elscreen-create) ;create tab

;; kill buffer; keep open window/tab
(define-key evil-normal-state-map (kbd "t d") (kbd "M-x kill-buffer RET"))

(define-key evil-normal-state-map (kbd "t D") 'elscreen-kill) ;kill tab
;; rename
(define-key evil-normal-state-map (kbd "t r") 'elscreen-screen-nickname)

(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab
(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab

;; after using elscreen-goto, resource the file
(defun load-emacs-navigation () (load-file "~/.emacs.d/navigation.el"))
;; (add-hook 'elscreen-goto-hook 'load-emacs-navigation)
(eval-after-load "elscreen" (add-hook 'elscreen-goto-hook 'load-emacs-navigation))

(define-key evil-normal-state-map (kbd "SPC a") (lambda () (interactive) (elscreen-goto 0)))
(define-key evil-normal-state-map (kbd "SPC r") (lambda () (interactive) (elscreen-goto 1)))
(define-key evil-normal-state-map (kbd "SPC s") (lambda () (interactive) (elscreen-goto 2)))
(define-key evil-normal-state-map (kbd "SPC t") (lambda () (interactive) (elscreen-goto 3)))
(define-key evil-normal-state-map (kbd "SPC d") (lambda () (interactive) (elscreen-goto 4)))
(define-key evil-normal-state-map (kbd "SPC h") (lambda () (interactive) (elscreen-goto 5)))
(define-key evil-normal-state-map (kbd "SPC n") (lambda () (interactive) (elscreen-goto 6)))
(define-key evil-normal-state-map (kbd "SPC e") (lambda () (interactive) (elscreen-goto 7)))
(define-key evil-normal-state-map (kbd "SPC i") (lambda () (interactive) (elscreen-goto 8)))
(define-key evil-normal-state-map (kbd "SPC o") (lambda () (interactive) (elscreen-goto 9)))
;; kill ring
(define-key evil-normal-state-map (kbd "SPC y") 'browse-kill-ring)

;; bookmark based on tabs

;; buffer search
(define-key evil-normal-state-map (kbd "t p") 'helm-mini)

;; file search (ctrlp like)
(define-key evil-normal-state-map (kbd "t P") 'helm-find-files)

;;helm mode.. show as type completion so now.. if do :help f will show functions in helm window and change as type
(helm-mode 1)

;; persistent mini buffer history and kill ring and such
;; http://stackoverflow.com/questions/1229142/how-can-i-save-my-mini-buffer-history-in-emacs
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file "~/.emacs.d/tmp/savehist")

;; general settings;{{{
;; don't ask; follow symlinks to file under version control
(setq vc-follow-symlinks "t")
;;}}}

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
(define-key evil-normal-state-map (kbd "t s") (kbd "M-x load-file RET ~/.emacs RET"))

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



;; _org mode {{{
;; general settings {{{
;; make headings levels same size and smaller
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; don't indent headings or text below them
(setq org-adapt-indentation nil)
;; add time when closing a todo
(setq org-log-done 'time)
;; tabs have width 4
(add-hook 'org-mode-hook (lambda () (setq tab-width 4)))

; }}}

;; clocking {{{
;; can only have one clock running (which makes sense..); if start clock in another file, current will be stopped automatically
;; remember clock if exit emacs
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; will become dangling clock if exit emacs (to automatically resume: (setq org-clock-persist t))
;; if inactive in emacs for x minutes, prompt to resolve idle time
(setq org-clock-idle-time 15)

;; http://orgmode.org/manual/Resolving-idle-time.html#Resolving-idle-time
;; shift something always leaves you clocked out
;; k to re-clock in and keep some (or all if hit RET) of the minutes
;; K like above but clock out
;; i or q will ignore question and start new clock (if restarting emacs... maybe will keep all idle time if idle time prompt appears)
;; g- got back x minutes ago (clocks you out at beginning of idle period then back in x minutes ago)
;; C to cancel clock
;; s to clock out at beginning of idle (subtracting all minutes) and clock back in now
;; S clocks out at beginning of idle

; }}}

;; speed commands {{{
(setq org-use-speed-commands t)

;; http://ergoemacs.org/emacs/elisp_insert-date-time.html
;; because org-time-stamp-inactive requires hitting enter to get default (2 universal prefixes will do automatically but contains time which don't want) and can't figure out how to auto insert today
(defun insert-date (&optional addTimeStamp-p)
  "Insert current date and or time.

• In this format yyyy-mm-dd weekday.
• When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00
• Replaces text selection.

See also `current-date-time-string'."
  (interactive "P")
  (when (use-region-p) (delete-region (region-beginning) (region-end) ) )
  (cond
   ((equal addTimeStamp-p nil ) (insert (format-time-string "[%Y-%m-%d %a]")))
   (t (insert (current-date-time-string))) ) )

;; ? to show
;; t  to cycle todo (will stop a clock)
;; tab or c to toggle
;; I and O to clock in and out
;; a archive subtree with confirmation (moves to archive file; will ask for confirmation)
;; e to set effort
;; W to add a warning in x minutes
;; v to open agenda
   ;; L for timeline for current buffer
   ;; a to see agenda for day/Week (deadlines) (can mark as done here)
;; w to refile (move current subtree to under another heading)
;; 0-3 priority
;; (org-speed-move-safe (quote org-forward-heading-same-level))
(setq org-speed-commands-user
        '(("e" . (org-speed-move-safe 'outline-previous-visible-heading))
          ("n" . (org-speed-move-safe 'outline-next-visible-heading))
          ;; swapping down and up
          ("E" . (org-shiftmetaup))
          ("N" . (org-shiftmetadown))
          ;; add tags
          (";" . (org-set-tags-command))
          ("h" . (org-metaleft))
          ("i" . (org-metaright))
          ;; will shift entire subtree
          ("<" . (org-shiftmetaleft))
          (">" . (org-shiftmetaright))
          ;; these two won't work
          ;; ("s" . (org-schedule))
          ;; ("d" . (org-deadline))
          ;; d to add today's date as an inactive timestamp
          ("d" . (progn (evil-next-line) (insert-date) (org-return) (org-return) (evil-previous-line)))
          ;; clock in under section where last clocked in then out
          ("l" . (org-clock-in-last))
          ("q" . (org-clock-cancel))
          ;; this is default i; default o is open link
          ("o" . (progn (forward-char 1) (call-interactively (quote org-insert-heading-respect-content))))))

; }}}

;; is there actually any reason to make a new minor mode instead of just using
;; evil-define-key and org-mode-map?
;; evil-org-mode {{{
;; create evil org minor mode and functions {{{
;; https://github.com/edwtjo/evil-org-mode
(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvilOrg"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

;; made smarter (added check if on heading)
(defun clever-insert-item ()
  "Clever insertion of org item or heading."
  (if (org-on-heading-p)
    ;; is this any different than after-current?
    (org-insert-heading-respect-content)
    (if (org-in-item-p)
      (org-insert-item)
      (insert "\n"))))

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
  FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

; }}}

;; capture {{{
;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
;; http://orgmode.org/manual/Template-elements.html#Template-elements
(setq org-default-notes-file "~/org/notes.org")
;; universal prefix to visit template; twice to go to last stored capture item in its buffer
;; for log day (adding things to log without going there)
(define-key evil-normal-state-map (kbd "SPC c") 'org-capture)
;; go to heading clocked into
(define-key evil-normal-state-map (kbd "SPC C") 'org-clock-goto)

;; define this after evil-org-mode and takes precedence it seems
(define-minor-mode evil-capture-mode
  "Buffer local minor mode for evil-capture"
  :init-value nil
  :lighter " EvilCapture"
  :keymap (make-sparse-keymap) ; defines evil-capture-mode-map
  :group 'evil-capture)

(add-hook 'org-capture-mode-hook 'evil-capture-mode) ;; only load with org-mode
(evil-define-key 'normal evil-capture-mode-map
  (kbd "RET") 'org-capture-finalize
  ;; abort
  "q" 'org-capture-kill)

;; (org-capture-refile)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ag-sys/Else/everything/log.org" "General Tasks/ Maybe Refile")
             "* TODO %?\n  %i\n  %a")
        ("c" "Currently Clocked in" entry (clock)
             "* .%? (Entered on %U)")))

;; %a will be expanded to link from whence called capture; see http://orgmode.org/manual/Template-expansion.html#Template-expansion
;; %U for inactive date, meaning will not show up in agenda
;; use certain template without interactive selection
;;  (define-key global-map "\C-cx"
;;     (lambda () (interactive) (org-capture nil "x")))
; }}}

;; can dot repeat with
;;  mappings {{{

;; 2 way instead of 3 way cycle; also works WITHIN heading, keeping position!! fuck org-cycle!!
;; sadly drawers will always be open when using though
;; http://emacs.stackexchange.com/questions/3998/how-to-remap-control-up-in-org-mode
(defun org-take-back-tab-bindings ()
  (define-key org-mode-map [remap outline-toggle-children] nil))
(eval-after-load "org" #'(org-take-back-tab-bindings))

(add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode
;; insert tab character
(evil-define-key 'insert evil-org-mode-map
  ;; (kbd "RET") 'clever-insert-item
  ;; (kbd "<tab>") (insert "<tab>"))
  (kbd "<tab>") (kbd "C-q <tab>"))

;; same bindings for the most part as speed commands; not using a state because don't usually want to do more than one command at a time; if do, can use speed commands
(evil-define-key 'normal evil-org-mode-map
  (kbd "<tab>")  'outline-toggle-children
  ;; navigate up and down between visible headings
  (kbd "m e") 'outline-previous-visible-heading
  (kbd "m n") 'outline-next-visible-heading
  (kbd "m w") 'org-forward-sentence
  (kbd "m E") 'org-shiftmetaup
  (kbd "m N") 'org-shiftmetadown
  (kbd "m h") 'org-metaleft
  (kbd "m i") 'org-metaright
  "<"         'org-shiftmetaleft
  ">"         'org-shiftmetaright
  ;; when adding a deadline can type +1 to add a day; day of week (tues); day of month (20)
  (kbd "m d") 'org-deadline
  (kbd "m s") 'org-schedule
  (kbd "m l") 'org-clock-in-last
  (kbd "m q") 'org-clock-cancel
  ;; follow links
  (kbd "RET") 'org-open-at-point
  (kbd "m T") 'org-insert-todo-heading-respect-content
  ;; toggle todo
  (kbd "m t") 'org-todo
  (kbd "m c") 'org-toggle-checkbox
  ;; archive
  (kbd "m a") 'org-archive-subtree-default-with-confirmation
  ;; open agenda
  (kbd "m v") 'org-agenda
  ;; add current file to agenda files (so todos and deadlines will be listed in agenda view; can also add directories)
  (kbd "m A") 'org-agenda-file-to-front
  ;; go to next agenda file
  (kbd "m C") 'org-cycle-agenda-files
  ;; clock in and out under current heading; 3 universal prefixes to start from last clock out time of session; 2 for clock-in-last
  ;; 1 universal prefix to select task from recent
  (kbd "m I") 'org-clock-in
  (kbd "m O") 'org-clock-out
  ;; show outline view
  ;; (kbd "SPC u") 'org-toc-show
  (kbd "SPC u") 'helm-org-in-buffer-headings
  "ge"          'outline-up-heading
  "gn"          'outline-forward-same-level
  "zR"          'show-all
  "zZ"          'hide-other
  (kbd "m /")   'org-sparse-tree
  ;; not the same
  "zM"          'hide-body
  "T" '(lambda () (interactive) (evil-org-eol-call (lambda() (org-insert-todo-heading nil))))
  ;; will act like o or insert -, etc.
  "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
  "O" 'org-insert-heading
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "-" 'org-cycle-list-bullet)
  ;; org-insert-drawer

; }}}

; }}}

; }}}
