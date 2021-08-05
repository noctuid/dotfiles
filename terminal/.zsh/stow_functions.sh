# as of stow 2.2.2, READMEs are automatically ignored
restow() {
	cd ~/dotfiles && \
	# so can have readmes in each dir without conflict
	stow -Rvt ~/ aesthetics browsing common common_lisp emacs \
		 games input mail media music remap scripts startup terminal vim wm \
		 .aesthetics .browsing .common .emacs .mail .media .music
}

unstow() {
	cd ~/dotfiles && \
	stow -Dvt ~/ aesthetics browsing common common_lisp emacs games input mail \
		 media music remap scripts startup terminal wm vim .aesthetics \
		 .browsing .common .emacs .mail .media .music
}
