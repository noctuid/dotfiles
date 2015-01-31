#!/bin/bash

function print_help() {
	echo "
Creates a pdf from a text file with basic syntax (see README) using xelatex.

-o <filename>  specify an output name (default: output)
-e             add example lorem ipsum text at end
-v             open pdf afterwards using rifle
-h             print help and exit

"

	if [ "$1" == "illegal_opt" ]; then
		exit 1
	else
		exit 0
	fi
}

example_text=false
view_pdf=false
output_name="output"
while getopts :o:veh opt
do
	case $opt in
	o) output_name=$OPTARG;;
	e) example_text=true;;
	v) view_pdf=true;;
	h) print_help;;
	*) print_help illegal_opt;;
    esac
done

ENDTEXT=""
# first italic marker?
FIT=true
# first bold marker?
FBF=true
# first caret?
FCT=true
# first plus?
FPL=true
ESCAPED=false

file=${!#}

restore_defaults() {
	# deal with \ without a preceding \ as final character on line
	if $ESCAPED; then
		# assume want backslask at end of line?
		ENDTEXT="${ENDTEXT}\\textbackslash{}"
	fi
	# deal with unmatched _, *, ^, and + on line
	if ! $FIT; then
		ENDTEXT="${ENDTEXT}}"
	fi
	if ! $FBF; then
		ENDTEXT="${ENDTEXT}}"
	fi
	if ! $FCT; then
		ENDTEXT="${ENDTEXT}}"
	fi
	if ! $FPL; then
		ENDTEXT="${ENDTEXT}}"
	fi
	FIT=true
	FBF=true
	FCT=true
	FPL=true
	ESCAPED=false
}

# -r so doesn't eat backslash
while read -r line; do
	length=${#line}
	for (( i=0; i<$length; i++ )); do
		char=${line:$i:1}
		# first character on line
		if [ "$i" == "0" ]; then
			if [ "$char" == "^" ]; then
				ENDTEXT="${ENDTEXT}\\chapter{"
				FCT=false
				# go to next char
				continue
			elif [ "$char" == "+" ]; then
				ENDTEXT="${ENDTEXT}\\section{"
				FPL=false
				continue
			fi
		fi
		if ! $ESCAPED; then
			if [ "$char" == "\\" ]; then
				ESCAPED=true
			elif [ "$char" == "#" ]; then
				# skip rest of line; it's a comment
				ENDTEXT="${ENDTEXT}

"
				restore_defaults
				continue 2

			# italicized text
			elif [ "$char" == "_" ] && $FIT; then
				ENDTEXT="${ENDTEXT}\\textit{"
				FIT=false
			elif [ "$char" == "_" ] && ! $FIT; then
				ENDTEXT="${ENDTEXT}}"
				FIT=true

			# emboldened text
			elif [ "$char" == "*" ] && $FBF; then
				ENDTEXT="${ENDTEXT}\\textbf{"
				FBF=false
			elif [ "$char" == "*" ] && ! $FBF; then
				ENDTEXT="${ENDTEXT}}"
				FBF=true

			# escaping for LaTeX
			elif [ "$char" == "^" ]; then
				ENDTEXT="${ENDTEXT}\\textasciicircum{}"
			elif [ "$char" == "&" ]; then
				ENDTEXT="${ENDTEXT}\\&"
			elif [ "$char" == "%" ]; then
				ENDTEXT="${ENDTEXT}\\%"
			elif [ "$char" == "{" ]; then
				ENDTEXT="${ENDTEXT}\\{"
			elif [ "$char" == "}" ]; then
				ENDTEXT="${ENDTEXT}\\}"
			elif [ "$char" == "~" ]; then
				ENDTEXT="${ENDTEXT}\\textasciitilde{}"

			else
				ENDTEXT="$ENDTEXT$char"
			fi
		else
			# escaping for LaTeX
			if [ "$char" == "#" ]; then
				ENDTEXT="${ENDTEXT}\\#"
			elif [ "$char" == "_" ]; then
				ENDTEXT="${ENDTEXT}\\_"
			elif [ "$char" == "\\" ]; then
				ENDTEXT="${ENDTEXT}\\textbackslash{}"

			else
				ENDTEXT="$ENDTEXT$char"
			fi
			ESCAPED=false
		fi
	done
	restore_defaults
	# add newline back along with extra to specify new paragraph
	ENDTEXT="${ENDTEXT}

"
done < "$file"

# replace three periods with real ellipsis
ENDTEXT=${ENDTEXT//.../…}
# replace two periods with underscores (placeholder)
ENDTEXT=${ENDTEXT//../\\_\\_\\_\\_\\_}

if $example_text; then
	ENDTEXT="${ENDTEXT}\\lipsum\\lipsum"
fi

# finalize
ENDTEXT="\\begin{document}
$ENDTEXT
\\end{document}
"

mkdir -p output && cd output

echo "$ENDTEXT" > output.txt
cp ~/bin/writing/template.tex ${output_name}.tex
# append text
cat output.txt >> ${output_name}.tex

# make pdf
xelatex ${output_name}.tex

if $view_pdf; then
	rifle ${output_name}.pdf
fi
