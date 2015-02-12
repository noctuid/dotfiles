" Navigation (tab, buffer, file)

" Quicker Tab Navigation"{{{
if tabpagenr() <= 10
	nnoremap <space>a 1gt
	nnoremap <space>r 2gt
	nnoremap <space>s 3gt
	nnoremap <space>t 4gt
	nnoremap <space>d 5gt
	nnoremap <space>h 6gt
	nnoremap <space>n 7gt
	nnoremap <space>e 8gt
	nnoremap <space>i 9gt
	nnoremap <space>o 10gt
elseif tabpagenr() > 10
	nnoremap <space>a 11gt
	nnoremap <space>r 12gt
	nnoremap <space>s 13gt
	nnoremap <space>t 14gt
	nnoremap <space>d 15gt
	nnoremap <space>h 16gt
	nnoremap <space>n 17gt
	nnoremap <space>e 18gt
	nnoremap <space>i 19gt
	nnoremap <space>o 20gt
endif

"}}}

" Thanks to Ingo Karkat for answering my question: http://stackoverflow.com/questions/21125170/grabbing-the-current-tab-name
cnoreabbr <expr> tabname t:taboo_tab_name
" cnoreabbr <expr> buffername expand('%:t')

if exists("t:taboo_tab_name")
	" too much manual work without being able to easily edit in one place;
	" using ,<letter> mappings below for quickmarks now exclusively instead of unite bookmarking files for specific tab names
	" nnoremap <buffer> <space>u :Unite -quick-match bookmark:tabname<C-]><cr>
	" nnoremap <buffer> <space>U :UniteBookmarkAdd<cr>tabname<c-]><cr>buffername<c-]>

	if t:taboo_tab_name == "main"
		cd ~/ag-sys/Else/everything
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/log.txt<cr>
		" first second third most used
		nnoremap <buffer> ,f :e ~/ag-sys/Else/everything/everything_index.txt<cr>
		nnoremap <buffer> ,s :e ~/ag-sys/Else/everything/arch_and_program_info.txt<cr>
		nnoremap <buffer> ,t :e ~/ag-sys/Else/everything/interaction.txt<cr>
		nnoremap <buffer> ,F :e ~/ag-sys/Else/everything/\#20xx.txt<cr>
		nnoremap <buffer> ,p :e ~/ag-sys/Else/everything/policy.txt<cr>
		nnoremap <buffer> ,i :e ~/ag-sys/Else/everything/other/computer/linux/arch/install.txt<cr>

	elseif t:taboo_tab_name == "browse"
		cd ~/dotfiles/browsing
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/\#browse.txt<cr>
		nnoremap <buffer> ,f :e ~/.pentadactylrc<cr>
		nnoremap <buffer> ,s :e ~/.pentadactyl/groups.penta<cr>
		nnoremap <buffer> ,t :e ~/.pentadactyl/navigation.penta<cr>
		nnoremap <buffer> ,F :e ~/.pentadactyl/autocommands.penta<cr>
		" readme; basic documentation
		nnoremap <buffer> ,r :e ~/README.mkd<cr>

	elseif t:taboo_tab_name == "prog"
		cd ~/ag-sys/prog
		nnoremap <buffer> ,f ~/ag-sys/prog/common_lisp/cl_notes.txt
		nnoremap <buffer> ,s ~/ag-sys/prog/rust/rust_notes.txt
		nnoremap <buffer> ,t ~/ag-sys/prog/haskell/\#info.txt
		nnoremap <buffer> ,c ~/ag-sys/prog/c/info.txt
		nnoremap <buffer> ,p ~/ag-sys/prog/python/\#info.txt

	elseif t:taboo_tab_name == "config"
		cd ~/dotfiles
		nnoremap <buffer> ,, :e ~/dotfiles/README.md<cr>
		nnoremap <buffer> ,f :e ~/.navigation.vim<cr>
		nnoremap <buffer> ,r :e ~/dotfiles/README.md<cr>
		nnoremap <buffer> <space>p :Unite -start-insert file<cr>

		nnoremap <Plug>CycleToMail :TabooRename conf-mail<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToMail
		silent! call repeat#set("\<Plug>CycleToMail", v:count)
	" don't really use; remove? {{{
	elseif t:taboo_tab_name == "mail"
		cd ~/dotfiles/mail_and_cal
		nnoremap <buffer> ,f :e ~/.muttrc<cr>
		nnoremap <buffer> ,s :e ~/.mbsyncrc<cr>
		nnoremap <buffer> ,t :e ~/.msmtprc<cr>

		nnoremap <Plug>CycleToMedia :TabooRename conf-media<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToMedia
		silent! call repeat#set("\<Plug>CycleToMedia", v:count)
	elseif t:taboo_tab_name == "music"
		cd ~/dotfiles/music
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/other/music/listen/music.txt<cr>
		nnoremap <buffer> ,f :e ~/.mpv/input.conf<cr>
		nnoremap <buffer> ,s :e ~/.vimusrc<cr>
		nnoremap <buffer> ,t :e ~/.mpd/mpd.conf<cr>
		nnoremap <buffer> ,F :e ~/.mpv/config<cr>

		nnoremap <Plug>CycleToRan :TabooRename conf-ranger<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToRan
		silent! call repeat#set("\<Plug>CycleToRan", v:count)
	elseif t:taboo_tab_name == "ranger"
		cd ~/.config/ranger
		nnoremap <buffer> ,f :e ~/.config/ranger/rc.conf<cr>
		nnoremap <buffer> ,s :e ~/.config/ranger/rifle.conf<cr>
		nnoremap <buffer> ,t :e ~/.config/ranger/commands.py<cr>
		nnoremap <buffer> <space>p :Unite -quick-match file<cr>

		nnoremap <Plug>CycleToConf :TabooRename config<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToConf
		silent! call repeat#set("\<Plug>CycleToConf", v:count)
	" }}}
	elseif t:taboo_tab_name == "bin"
		cd ~/bin
		nnoremap <buffer> <space>p :Unite -start-insert file<cr>

	elseif t:taboo_tab_name == "remap"
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/\#remapping.txt<cr>
		nnoremap <buffer> ,f :e ~/.config/sxhkd/sxhkdrc<cr>
		nnoremap <buffer> ,s :e ~/.Xmodmap<cr>
		nnoremap <buffer> ,t :e ~/.config/xchainkeys/xchainkeys.conf<cr>
		nnoremap <buffer> ,r :e ~/README.md<cr>

	elseif t:taboo_tab_name == "blag"
		cd ~/_repos/blog/angelic-sedition.github.io/source/_posts
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/another/blag/octopress/octopress_blogging.txt<cr>
		nnoremap <buffer> ,f :e ~/ag-sys/Else/everything/another/blag/octopress/octopress_setup.txt<cr>
		nnoremap <buffer> <space>p :Unite -start-insert file<cr>

	elseif t:taboo_tab_name == "wr"
		cd ~/ag-sys/Else/everything/another/_prose/pots

		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/another/_prose/structure_standards.txt<cr>
		nnoremap <buffer> ,f :e draft_a.txt<cr>
		nnoremap <buffer> ,s :e plot.txt<cr>
		nnoremap <buffer> ,t :e ~/ag-sys/Else/everything/another/_prose/pots/misc.txt<cr>
		nnoremap <buffer> ,w :e world.txt<cr>
		nnoremap <buffer> ,m :e misc.txt<cr>
		nnoremap <buffer> ,b :e bio.txt<cr>

		nnoremap <buffer> ,l :e ~/ag-sys/Else/everything/another/_prose/lts/misc.txt<cr>
		nnoremap <buffer> ,p :e ~/ag-sys/Else/everything/another/_prose/pp/todo.txt<cr>
		nnoremap <buffer> ,i :e ~/ag-sys/Else/everything/another/ideas.txt<cr>
		nnoremap <buffer> ,c :e ~/ag-sys/Else/everything/another/_prose/lndn/landon.txt<cr>
		nnoremap <buffer> ,g :e ~/ag-sys/Else/everything/another/_prose/gen_misc.txt<cr>
		
		" related scripts
		nnoremap <buffer> ,v :ViewTxtPdf<cr>
		nnoremap <buffer> ,z :Clam ~/bin/writing/writing_stats -c %<cr>:resize 3<cr>
		nnoremap <buffer> ,Z :Clam ~/bin/writing/word_use_stats -r %<cr>

	elseif t:taboo_tab_name == "cons"
		nnoremap <buffer> ,f :e ~/ag-sys/Else/everything/another/consume/book_notes.txt<cr>
		nnoremap <buffer> ,s :e ~/ag-sys/Else/everything/other/music/listen/music.txt<cr>

	elseif t:taboo_tab_name == "cube"
		cd ~/ag-sys/Else/everything/other/skill_toys/cubes/
		nnoremap <buffer> <space>p :Unite -start-insert file<cr>

	elseif t:taboo_tab_name == "game"
		cd ~/ag-sys/Else/everything/other/Gaming
		nnoremap <buffer> <space>p :Unite -start-insert file<cr>

	endif
endif
