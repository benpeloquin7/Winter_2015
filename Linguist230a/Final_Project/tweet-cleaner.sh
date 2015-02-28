###:---------tweet-cleaner.sh-----------:###
#Removes http links, twitter handles, new lines and back slashes

path="data-hold/account-store/"
tweets=$1

if [[ -e "cleantest.txt" ]]; then
rm cleantest.txt
fi

csvfix order -f 4 $path$tweets | grep -v "RT" |\
sed -E 's/http.+\b//g' | sed -E 's/@.+\b//g' |\
tr -d '\"' | tr -d '\n' | tr [A-Z] [a-z] | tr '  +' ' ' |\
iconv -c -f utf-8 -t ascii > cleantest.txt

