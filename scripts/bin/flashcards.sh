#!/bin/sh
# todo:
# record which need more reviewing

card_dir=~/.cards
card_set=general
create_set=no
edit_set=no
review_set=yes
# side to show first
side=left

function print_help() {
echo "
If no cardset is given, will list existing cardsets.
-d dir  specify an alternate directory for cards
-n      to create a set instead of reviewing an existing set
-e      to edit a set instead of reviewing one
-s side to pick side to show first
-r      show right side first (default is left)

"
}

function review_card() {
# order non-blank/non-"comment" lines randomly
IFS=$'\n'; review_lines=($(grep -vw '^\s*$\|#' $card_set | shuf))
for line in "${review_lines[@]}";do
	if [ "$side" == "left" ]; then
		echo $line | awk -F ':' '{ print $1 }'
		read -n 1 && printf "\n"
		echo $line | awk -F ':' '{ print $2 }' | tr -d ' '
		echo "------------"
	else
		echo $line | awk -F ':' '{ print $2 }' | tr -d ' '
		read -n 1 && printf "\n"
		echo $line | awk -F ':' '{ print $1 }'
		echo "------------"
	fi
	# read -p "Quit? [q]"-n 1 -r
	read -n 1 -r
	if [[ $REPLY =~ ^[Qq]$ ]]
	then
		exit
	fi
done
}

while getopts d:s:nehr opt
do
	case $opt in
	d) card_dir=$optarg;;
	n) create_set=yes
	   review_set=no;;
	e) edit_set=yes
	   review_set=no;;
	s) side=$optarg;;
	r) side=right;;
	h) print_help;;
	*) print_help
        exit 1;;
    esac
done

mkdir -p $card_dir
cd $card_dir
if [ -n "$1" ]; then
	# last arg
	card_set="${@: -1}"
	if [ "$create_set" == "yes" ];then
		touch $card_set
	fi
	if [ "$edit_set" == "yes" ];then
		vim $card_set
		exit
	fi
	if [ "$review_set" == "yes" ];then
		review_card
	fi
else
	ls
fi
