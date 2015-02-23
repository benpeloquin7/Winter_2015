path="data-hold/account-store/"
tweets=$1

csvfix order -f 4 $path$tweets | grep -v "RT" |\
tr '[A-Z]' '[a-z]' | tr -d "\"" > cleantest.txt
