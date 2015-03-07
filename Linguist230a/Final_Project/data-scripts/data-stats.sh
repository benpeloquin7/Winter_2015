###:----some basic statistics around ! 3.5.15------:###

account=$1
#Aggreate all tweets into a single .csv with cleaning
csvfix order -f 1,2,3,4 ../data-hold/account-store/$account.csv | grep -v "RT" |\
sed -E 's/http.+\b//g' | tr -d '\"' | tr ' +' ' ' |\
iconv -c -f utf-8 -t ascii > $account.csv

csvfix echo -smq -osep '|' $account.csv > $account.psv

while read -r line; do
        tweet_ID=$(echo $line | awk -F '|' '{print $1}')
        date_time=$(echo $line | awk -F '|' '{print $2}')
        screen_name=$(echo $line | awk -F '|' '{print $3}')
        text=$(echo $line | awk -F '|' '{print $4}')
        numChars=$(($(echo $text | wc -c) - 2))
        numTokens=$(echo $text | wc -w)
        target=$(echo $text | grep -oE "!")
        target=$(echo $target | tr -d ' ')
        numTarget=$(echo ${#target})
        printf '%s|%s|%s|%s|%i|%i|%s|%i\n' "$tweet_ID" "$date_time" "$screen_name" "$text" "$numChars" "$numTokens" "$target" "$numTarget" >> $account\-data.psv
done < $account.psv
