#!/bin/sh
mutt_kz=$(mutt -v | grep mutt-kz)
sidebar=$(mutt -v | grep sidebar)

if [ "$mutt_kz" != "" ]; then
	# https://github.com/karelzak/mutt-kz/issues/82
	# thanks to Dabsen
	# mutt-kz only; colours delim
	echo "color sidebar color223 color235"
elif [ "$sidebar" != "" ];then
	# mutt-kz doesn't have these settings
	# only show last part of path; this is how it is by default in mutt-kz
	echo "set sidebar_shortpath = yes"
	# set sidebar_format = “%B%?F? [%F]?%* %?N?%N/?%4S”
	# don't indent subfolders; mutt-kz indents
	echo "set sidebar_folderindent = no"
fi

# for any version of mutt with the sidebar patch
if [ "$sidebar" != "" ]; then
	echo "macro index tb '<enter-command>toggle sidebar_visible<enter><refresh>'"
	echo "macro pager tb '<enter-command>toggle sidebar_visible<enter><redraw-screen>'"
	echo "set sidebar_delim = ' │'"
	echo "set sidebar_visible = yes"
	echo "set sidebar_width   = 34"
	echo "color sidebar_new color221 color233"
	# color sidebar_flagged
fi

# from man page:
# sidebar_format
#        Type: string
#        Default: “%B%?F? [%F]?%* %?N?%N/?%4S”
#
#        Format string for the sidebar. The  sequences  `%N',  `%F'  and  `%S'  will  be
#        replaced  by  the  number  of new or flagged messages or the total size of them
#        mailbox. `%B' will be replaced with the name of the mailbox.  The `%!' sequence
#        will  be  expanded to `!' if there is one flagged message; to `!!' if there are
#        two flagged messages; and to `n!' for n flagged messages, n>2.
