;; * Docs
;; https://stumpwm.github.io/git/stumpwm-git.pdf
;; https://stumpwm.github.io/git/stumpwm-git.html

;; * Setup
(in-package :stumpwm-user)

;; first in case something is broken
(define-key *root-map* (kbd ".") "loadrc")

(defvar *first-load* t)

;; load quicklisp
(load "~/.sbclrc")

;; install/load libraries
;; stump already has alexandria but useful here for sly when editing outside of
;; stumpwm
(ql:quickload '(:jsown :alexandria))

;; https://github.com/stumpwm/stumpwm/issues/659
;; requires clx-truetype, which has been removed from quick lisp
;; need to clone to local-projects if want to use
;; cl-truetype
;; (clx-truetype:cache-fonts)
;; (load-module "ttf-fonts")

;; (set-font (make-instance 'xft:font
;;                          :family "Office Code Pro"
;;                          :subfamily "Regular"
;;                          :size 10
;;                          :antialias t))

;; modules
;; may need to change if installed with guix; got errors even after changing so
;; using roswell now
;; (add-to-load-path "~/.guix-profile/share/common-lisp/sbcl/stumpwm-swm-gaps/")
;; (setf *module-dir* "~/.stumpwm.d/modules")
;; (init-load-path *module-dir*)

(load-module "swm-gaps")

;; * Todos
;; ** Lisp
;; - defpackage and import used functions

;; ** Visual
;; - set *colors*
;; - wal -R -n in .xinitrc causes startup to fail
;; - polybar with top gap (not enough room made when polybar given y offset)

;; - with setting for gaps, stumpwm crashes on window create
;; https://github.com/stumpwm/stumpwm-contrib/blob/master/util/swm-gaps/README.org
;; https://github.com/stumpwm/stumpwm-contrib/issues/205

;; - make issue for border color; why is unfocused color used when single frame?
;;   (both settings say only for 1+ frame; no defined behavior for 1 frame? why?)
;; - menu font and size; ttf plugin
;; - add font that supports lisp icon in polybar
;; - polybar fails to load second after resourcing config (it starts but will
;;   never actually show up again it seems)
;; - some visual indicator of windows in frame
;; - menu colors

;; ** Major
;; - better auto-tiling; see dynamic tiling PR that was merged
;; https://github.com/stumpwm/stumpwm/issues/811
;; https://github.com/stumpwm/stumpwm/pull/851
;; - reload xenv.sh when reloading init (will this affect environment?)
;; - empty desktop keybindings
;; - fix sly in Emacs
;; - multi-monitor setup
;; - connect to via slynk; be able to edit config file with sly
;; - hooks use cases

;; TODO tdrop
;; https://github.com/stumpwm/stumpwm/wiki/Tips-And-Tricks#que-commands-for-a-window-that-doesnt-yet-exist
;; https://old.reddit.com/r/stumpwm/comments/997md4/with_open_windows_doing_things_to_windows_as_they/
;; https://old.reddit.com/r/stumpwm/comments/7xzhtn/float_window_rule/

;; ** Rules
;; - auto-float popups, wpg, steam

;; ** Create commands
;; TODO commands to create
;; - move-focus but cycle through windows in frame if only one frame
;; - command to resize in direction
;; - sticky
;; - hide all on group
;; - toggle floating
;; - toggle only and auto-tiling

;; ** Modules
;; TODO stumpwm prescient
;; TODO https://github.com/stumpwm/stumpwm-contrib/blob/master/util/winner-mode/README.org
;; TODO maybe try golden ratio
;; https://github.com/stumpwm/stumpwm-contrib/blob/master/util/urgentwindows/README.org

;; ** Minor
;; TODO https://github.com/juki-pub/stumpbuffer
;; TODO scratchpad keybinding (https://www.youtube.com/watch?v=tKt_rVO960Q)
;; TODO stumpwm transient with stumpish? probably not necessary
;; TODO bind key to banish (move point out of the way)
;; TODO reload or restart-hard to reload stumpwm
;; TODO see todos in wm_action

;; ** Look Through Configs
;; https://github.com/jorams/dotfiles/blob/master/stumpwmrc.lisp
;; https://github.com/PuercoPop/stumpwm-config/blob/master/general-settings.lisp
;; TODO ask

;; * Helpers
(defmacro define-keys (map &rest bindings)
  "In map, define multiple bindings with automatic kbd usage."
  `(progn ,@(loop for (key def) on bindings by 'cddr
                  collect `(define-key ,map (kbd ,key) ,def))))

;; * Group Setup
(when *first-load*
  (grename "一")
  (gnew "二")
  (gnewbg "三")
  (gnewbg "四")
  (gnewbg "五")
  (gnewbg "六")
  (gnewbg "七")
  (gnewbg "八")
  (gnewbg "九"))

;; * Settings
;; ** Visual
(which-key-mode)

;; show menus at top
(setf *message-window-gravity* :top)
(setf *message-window-input-gravity* :top-right)
(setf *input-window-gravity* :top)

(defparameter *border-width* (parse-integer (getenv "WM_BORDER")))
(setf *maxsize-border-width* *border-width*)
(setf *transient-border-width* *border-width*)
(setf *normal-border-width* *border-width*)
(setf *window-border-style* :tight)
(set-msg-border-width *border-width*)

(alexandria:when-let*
    ((file (uiop:file-exists-p "~/.cache/wal/colors.json"))
     (file-text (uiop:read-file-string file))
     (colors (jsown:parse file-text))
     (foreground (jsown:val (jsown:val colors "special") "foreground"))
     (background (jsown:val (jsown:val colors "special") "background"))
     (color1 (jsown:val (jsown:val colors "colors") "color1"))
     (color15 (jsown:val (jsown:val colors "colors") "color15")))
  (setf *text-color* foreground)
  ;; input and message bar
  (set-bg-color background)
  (set-fg-color foreground)
  (set-border-color color15)
  ;; window borders
  (set-win-bg-color background)
  (set-unfocus-color color1)
  (set-focus-color color15))

;; gaps
;; (setf swm-gaps:*gaps-on* t)
;; (setf swm-gaps:*inner-gaps-size* (getenv "WM_GAP"))

;; * Auto-tile
;;; Basic tiling in stumpwm. Defines a new command, TILE, to tile
;;; windows in the current group. In addition, any group name added to
;;; the list *AUTOTILING-GROUPS* will tile automatically its windows.

;;; Initial author: jao@gnu.org.
;;; This code is in the public domain.

(defun do-auto-tile (group)
  (let* ((tlen (length (stumpwm::group-windows group)))
         (len (1- tlen)))
    (unless (zerop tlen)
      (stumpwm::call-interactively "only")
      (unless (current-window)
        (stumpwm::focus-next-window group)))
    (unless (zerop len)
      (stumpwm::split-frame group :column)
      (stumpwm::focus-next-frame group)
      (dotimes (n (1- len))
        (stumpwm::split-frame group :row (/ (- len n)))
        (stumpwm::focus-next-frame group))
      (stumpwm::focus-next-frame group))))

(defcommand auto-tile () ()
  "Tile windows in the current group."
  (do-auto-tile (current-group)))

(defvar *autotiling-groups* '()
  "Names of groups that will automatically tile their windows.")

(defun autotile-hook (&optional window)
  (unless (and window
               (stumpwm::window-transient-p window))
    (do-auto-tile (current-group))))

;; (add-hook *new-window-hook* #'autotile-hook)
;; (add-hook *destroy-window-hook* #'autotile-hook)

;; (defun maybe-balance-frames ()
;;   (when (< 1 (length (group-frames (current-group))))
;;     (balance-frames)))

;; (add-hook *new-window-hook*
;;           (lambda (window) (hsplit) (maybe-balance-frames)))
;; (add-hook *destroy-window-hook*
;;           (lambda (window) (remove-split) (maybe-balance-frames)))

;; * Example config
;;; Define window placement policy...

;; Clear rules
;; (clear-window-placement-rules)

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.
;; (define-frame-preference "Default"
;;   ;; frame raise lock (lock AND raise == jumpto)
;;   (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
;;   (1 t nil :class "XTerm"))

;; (define-frame-preference "Ardour"
;;   (0 t   t   :instance "ardour_editor" :type :normal)
;;   (0 t   t   :title "Ardour - Session Control")
;;   (0 nil nil :class "XTerm")
;;   (1 t   nil :type :normal)
;;   (1 t   t   :instance "ardour_mixer")
;;   (2 t   t   :instance "jvmetro")
;;   (1 t   t   :instance "qjackctl")
;;   (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

;; (define-frame-preference "Shareland"
;;   (0 t   nil :class "XTerm")
;;   (1 nil t   :class "aMule"))

;; (define-frame-preference "Emacs"
;;   (1 t t :restore "emacs-editing-dump" :title "...xdvi")
;;   (0 t t :create "emacs-dump" :class "Emacs"))

;; * Programs/Daemons
;; ** Sxhkd
(run-shell-command
 "xrestart sxhkd ~/.config/sxhkd/sxhkdrc ~/.config/sxhkd/wm_sxhkdrc")

;; ** Polybar
;; TODO fails after first time
(when *first-load*
  (run-shell-command "xrestart polybar stumpwm"))

;; * Slynk Integration
;; TODO load things so indentation doesn't break
;; TODO fix inferior-lisp-program

;; https://old.reddit.com/r/stumpwm/comments/ouuh1c/how_to_connect_sly_to_stumpwm_to_modifying_it_in/
;; (defcommand slynk (port) ((:string "Port number: "))
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (slynk:create-server :port (parse-integer port) :dont-close t))
;;    :name "slynk-manual"))

;; * End
(setf *first-load* nil)
