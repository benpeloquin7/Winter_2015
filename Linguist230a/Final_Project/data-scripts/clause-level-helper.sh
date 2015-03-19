###:::-----clause-level-helper4.sh--------:::###
current_file=$1
account_name=$(echo $current_file | grep -oE "@.+" | sed 's/.csv//g')

#All lines that contain at least one (!)
csvfix order -f 4 $current_file | grep -v 'RT' |\
	sed -E 's/http.+\b/H/g' | iconv -c -f utf-8 -t ascii |\
	grep -ioE '[^!\.\?]+!+' | tr -d '\"' | tr -d '|' > $account_name.txt


for numTarget in {1..6}; do
	#All occurances of correct lengthening
	numTemp=$((numTarget - 1))
	cat $account_name.txt | grep -ioE '[^!\.\?]+(!)\1'"{$numTemp}"'$' > $account_name$numTarget.txt
	while read -r line; do
        	numChars=$(($(echo $line | tr -d "\n" | wc -c) - $(echo $numTarget)))
        	numTokens=$(echo $line | wc -w)
        	printf '%s|%s|%i|%i|%i\n' "$account_name" "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
	done < $account_name$numTarget.txt
	rm $account_name$numTarget.txt
done
rm $account_name.txt
