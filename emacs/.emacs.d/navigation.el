;; http://emacs.stackexchange.com/questions/7638/how-to-detect-current-workgroup-in-workgroups2/7665#7665

;; could do evil-normal-state-local-map to be buffer local
;; however, would have to source this file on buffer change instead of on workgroup change
(defun lit/nmap-find-file (key file &rest maps)
  "A more convenient way to define 'quickmark' keys."
  (while key
    (define-key evil-normal-state-map key `(lambda () (interactive) (find-file ,file)))
    (setq key (pop maps)
          file (pop maps))))

;; just set default quickmarks here so that will be reset if overridden by a workgroup
;; general quickmarks
(lit/nmap-find-file
     ",a" "~/ag-sys/Else/everything/arch_and_program_info.txt"
     ",b" "~/ag-sys/Else/everything/\#browse.txt"
     ",B" "~/.config/bspwm/bspwmrc"
     ",c" "~/.config/ranger/rc.conf"
     ",d" "~/ag-sys/Else/everything/another/ideas.txt"
     ",e" "~/ag-sys/Else/everything/everything_index.txt"
     ",E" "~/.emacs.d/awaken.org"
     ",g" "~/.pentadactyl/groups.penta"
     ",i" "~/ag-sys/Else/everything/interaction.txt"
     ",I" "~/dotfiles/post_install/post_install.txt"
     ",j" "~/ag-sys/Else/everything/journal.txt"
     ",l" "~/ag-sys/Else/everything/log.org"
     ",m" "~/.muttrc"
     ",M" "~/ag-sys/Else/everything/other/music/listen/music.txt"
     ",n" "~/.emacs.d/navigation.el"
     ",p" "~/.pentadactylrc"
     ",P" "~/ag-sys/Else/everything/policy.txt"
     ",r" "~/ag-sys/Else/everything/\#remapping.txt"
     ",R" "~/.README.md"
     ",t" "~/.tmux.conf"
     ",v" "~/.vimrc"
     ",x" "~/.xinitrc"
     ",z" "~/.zshrc"
     ",2" "~/ag-sys/Else/everything/\#20xx.txt")

(let ((workgroup-title (wg-workgroup-name (wg-current-workgroup))))
  (cond
   ((equal workgroup-title "emacs")
    (lit/nmap-find-file ",f" "~/.emacs.d/awaken.org"
                        ",s" "~/.emacs.d/navigation.el"
                        ",t" "~/.emacs.d/init.el"))
   ((equal workgroup-title "main")
    (lit/nmap-find-file ",," "~/ag-sys/Else/everything/log.org"
                        ",f" "~/ag-sys/Else/everything/everything_index.txt"
                        ",s" "~/ag-sys/Else/everything/arch_and_program_info.txt"
                        ",t" "~/ag-sys/Else/everything/interaction.txt"
                        ",F" "~/ag-sys/Else/everything/\20xx.txt"
                        ",p" "~/ag-sys/Else/everything/policy.txt"
                        ",p" "~/ag-sys/Else/everything/other/computer/linux/arch/install.txt"))
   ((equal workgroup-title "prog")
    (lit/nmap-find-file ",," "~/ag-sys/prog/common_lisp/LoL_notes.txt"
                        ",f" "~/ag-sys/prog/common_lisp/cl_notes.org"))
   ((equal workgroup-title "cons")
    (lit/nmap-find-file ",f" "~/ag-sys/Else/everything/another/consume/book_notes.txt"
                        ",s" "~/ag-sys/Else/everything/another/consume/nim.org"))
   ((or (equal workgroup-title "wr") (equal workgroup-title "wr2"))
    (lit/nmap-find-file ",," "~/ag-sys/Else/everything/another/_prose/structure_standards.txt"
                        ",f" "draft_a.org"
                        ",s" "plot.org"
                        ",t" "~/ag-sys/Else/everything/another/_prose/pots/misc.org"
                        ",w" "world.org"
                        ",m" "misc.org"
                        ",b" "bio.org"
                        ",l" "~/ag-sys/Else/everything/another/_prose/lts/misc.org"
                        ",p" "~/ag-sys/Else/everything/another/_prose/pp/todo.txt"
                        ",i" "~/ag-sys/Else/everything/another/ideas.txt"
                        ",c" "~/ag-sys/Else/everything/another/_prose/lndn/landon.txt"
                        ",g" "~/ag-sys/Else/everything/another/_prose/gen_misc.txt"))))
