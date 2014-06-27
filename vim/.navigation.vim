" Navigation (tab, buffer, file)

augroup BufferBindings
  au!
  au BufEnter * so ~/.navigation.vim
augroup END

nnoremap <leader>f :Unite --start-insert file<cr>

" quicker tab navigation"{{{
" nnoremap N gT
" nnoremap E gt
nnoremap <space>w 1gt
nnoremap <space>f 11gt
nnoremap <space>p 12gt
if tabpagenr() < 11
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
elseif tabpagenr() >= 10
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

" 'session' setup (shorten)
nnoremap ,ks 1gt:e ~/.vimrc<cr>2gt:e ~/ag-sys/Else/everything/@log.txt<cr>3gt:TabooRename main<cr>:e ~/ag-sys/Else/everything/everything_index.txt<cr>4gt:TabooRename browse<cr>:e ~/ag-sys/Else/everything/\#browse.txt<cr>5gt:TabooRename config<cr>:e ~/dotfiles/README.md<cr>6gt:TabooRename bin<cr>7gt:TabooRename remap<cr>:e ~/ag-sys/Else/everything/\#remapping.txt<cr>2gt

" faster tab name setup
nnoremap ,kb :TabooRename browse<cr>:e ~/ag-sys/Else/everything/\#browse.txt<cr>
nnoremap ,km :TabooRename main<cr>:e ~/ag-sys/Else/everything/@log.txt<cr>

" general quickmarks
nnoremap <buffer> ,a :e ~/ag-sys/Else/everything/arch_and_program_info.txt<cr>
nnoremap <buffer> ,b :e ~/ag-sys/Else/everything/\#browse.txt<cr>
nnoremap <buffer> ,B :e ~/.config/bspwm/bspwmrc<cr>
nnoremap <buffer> ,c :e ~/.config/ranger/rc.conf<cr>
nnoremap <buffer> ,d :e ~/ag-sys/Else/everything/\#another/\#idea.txt<cr>
nnoremap <buffer> ,e :e ~/ag-sys/Else/everything/everything_index.txt<cr>
nnoremap <buffer> ,g :e ~/.pentadactyl/groups.penta<cr>
nnoremap <buffer> ,i :e ~/ag-sys/Else/everything/\#interaction.txt<cr>
nnoremap <buffer> ,I :e ~/post_install.txt<cr>
nnoremap <buffer> ,j :e ~/ag-sys/Else/everything/journal.txt<cr>
nnoremap <buffer> ,l :e ~/ag-sys/Else/everything/@log.txt<cr>
nnoremap <buffer> ,m :e ~/.muttrc<cr>
nnoremap <buffer> ,M :e ~/ag-sys/Else/everything/music/\#music.txt<cr>
nnoremap <buffer> ,n :e ~/.navigation.vim<cr>
nnoremap <buffer> ,p :e ~/.pentadactylrc<cr>
nnoremap <buffer> ,r :e ~/ag-sys/Else/everything/\#remapping.txt<cr>
nnoremap <buffer> ,R :e ~/.README.md<cr>
nnoremap <buffer> ,t :e ~/.tmux.conf<cr>
nnoremap <buffer> ,v :e ~/.vimrc<cr>
nnoremap <buffer> ,x :e ~/.xinitrc<cr>
nnoremap <buffer> ,y :Unite -start-insert buffer file_mru<cr>.yml<esc>
nnoremap <buffer> ,Y :e ~/ag-sys/Else/everything/lpol.txt<cr>
nnoremap <buffer> ,z :e ~/.zshrc<cr>

" Thanks to Ingo Karkat for answering my question: http://stackoverflow.com/questions/21125170/grabbing-the-current-tab-name
cnoreabbr <expr> tabname t:taboo_tab_name
cnoreabbr <expr> buffername expand('%:t')
" stop complaining if not named
if exists("t:taboo_tab_name")
	nnoremap <buffer> <space>u :Unite -quick-match bookmark:tabname<C-]><cr>
	nnoremap <buffer> <space>U :UniteBookmarkAdd<cr>tabname<c-]><cr>buffername<c-]>

	if t:taboo_tab_name == "main"
		cd ~/ag-sys/Else/everything
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/@log.txt<cr>
		nnoremap <buffer> ,f :e ~/ag-sys/Else/everything/everything_index.txt<cr>
		nnoremap <buffer> ,s :e ~/ag-sys/Else/everything/arch_and_program_info.txt<cr>
		nnoremap <buffer> ,t :e ~/ag-sys/Else/everything/\#interaction.txt<cr>

	elseif t:taboo_tab_name == 'browse'
		cd ~/dotfiles/browsing
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/\#browse.txt<cr>
		nnoremap <buffer> ,f :e ~/.pentadactylrc<cr>
		nnoremap <buffer> ,s :e ~/.pentadactyl/groups.penta<cr>
		nnoremap <buffer> ,t :e ~/.quickpenta<cr>
		nnoremap <buffer> ,F :e ~/.pentadactyl/autocommands<cr>
		" readme; basic documentation
		nnoremap <buffer> ,r :e ~/README.mkd<cr>

	elseif t:taboo_tab_name == 'prog'
		cd ~/ag-sys/prog
		nnoremap <buffer> ,f ~/ag-sys/prog/python/\#info.txt
		nnoremap <buffer> ,s ~/ag-sys/prog/haskell/\#info.txt
		nnoremap <buffer> ,t ~/ag-sys/prog/cpp/\#info.txt

	elseif t:taboo_tab_name == "config"
		cd ~/dotfiles
		nnoremap <buffer> ,f :e ~/.navigation.vim<cr>
		nnoremap <buffer> ,r :e ~/dotfiles/README.md<cr>
		nnoremap <buffer> <space>u :Unite -start-insert file<cr>

		nnoremap <Plug>CycleToMail :TabooRename conf-mail<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToMail
		silent! call repeat#set("\<Plug>CycleToMail", v:count)
	elseif t:taboo_tab_name == "conf-mail"
		cd ~/dotfiles/mail
		nnoremap <buffer> ,f :e ~/.muttrc<cr>
		nnoremap <buffer> ,s :e ~/.mbsyncrc<cr>
		nnoremap <buffer> ,t :e ~/.msmtprc<cr>

		nnoremap <Plug>CycleToMedia :TabooRename conf-media<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToMedia
		silent! call repeat#set("\<Plug>CycleToMedia", v:count)
	elseif t:taboo_tab_name == "conf-media"
		cd ~/dotfiles/music
		nnoremap <buffer> ,f :e ~/.mpv/input.conf<cr>
		nnoremap <buffer> ,s :e ~/.vimusrc<cr>
		nnoremap <buffer> ,t :e ~/.mpd/mpd.conf<cr>
		nnoremap <buffer> ,F :e ~/.mpv/config<cr>

		nnoremap <Plug>CycleToRan :TabooRename conf-ranger<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToRan
		silent! call repeat#set("\<Plug>CycleToRan", v:count)
	elseif t:taboo_tab_name == "conf-ranger"
		cd ~/.config/ranger
		nnoremap <buffer> ,f :e ~/.config/ranger/rc.conf<cr>
		nnoremap <buffer> ,s :e ~/.config/ranger/rifle.conf<cr>
		nnoremap <buffer> ,t :e ~/.config/ranger/commands.py<cr>
		nnoremap <buffer> <space>u :Unite -quick-match file<cr>

		nnoremap <Plug>CycleToConf :TabooRename config<cr>:so ~/.navigation.vim<cr>
		nmap <buffer> <space>c <Plug>CycleToConf
		silent! call repeat#set("\<Plug>CycleToConf", v:count)

	elseif t:taboo_tab_name == 'bin'
		cd ~/bin
		nnoremap <buffer> <space>u :Unite -start-insert file<cr>

	elseif t:taboo_tab_name == "remap"
		nnoremap <buffer> ,, :e ~/ag-sys/Else/everything/\#remapping.txt<cr>
		nnoremap <buffer> ,f ~/.config/sxhkd/sxhkdrc
		nnoremap <buffer> ,s ~/.Xmodmap
		nnoremap <buffer> ,t ~/.config/xchainkeys/xchainkeys.conf
		nnoremap <buffer> ,r :e ~/README.md

	elseif t:taboo_tab_name == "pp"
		cd ~/ag-sys/Else/everything/\#another/\#prose/pp
		nnoremap <buffer> ,, :e
		nnoremap <buffer> ,r :e ~/ag-sys/Else/everything/\#another/\#prose/pp/random.txt<cr>

	endif
" else
endif
