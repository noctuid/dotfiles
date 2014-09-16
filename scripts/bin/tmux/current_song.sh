song=$(mpc current -f "%artist% - %title%")
# cut out anything in parens to shorten title length
songy=${song%(*}
echo "$songy"
