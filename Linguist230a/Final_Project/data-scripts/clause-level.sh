###:::-----clause-level.sh--------:::###
if [[ -e aggregate.psv ]]; then
rm aggregate.psv
fi

echo "!"
csvfix order -f 4 ../data-hold/account-store/@KimKardashian.csv | grep -v 'RT' |\
sed -E 's/http.+\b//g' | sed -E 's/@.+b//g' | iconv -c -f utf-8 -t ascii |\
grep -oiE '\b[^!]+(!){1}' > 1.txt

while read -r line; do
        numTarget=1
        numChars=$(($(echo $line | wc -c) - $(echo $numTarget)))
        numTokens=$(echo $line | wc -w)
        printf '%s|%i|%i|%i\n' "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
done < 1.txt

echo "!!"
csvfix order -f 4 ../data-hold/account-store/@KimKardashian.csv | grep -v 'RT' |\
sed -E 's/http.+\b//g' | sed -E 's/@.+b//g' | iconv -c -f utf-8 -t ascii |\
grep -oiE '\b[^!]+(!){2}' > 2.txt

while read -r line; do
        numTarget=2
        numChars=$(($(echo $line | wc -c) - $(echo $numTarget)))
        numTokens=$(echo $line | wc -w)
        printf '%s|%i|%i|%i\n' "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
done < 2.txt

echo "!!!"
csvfix order -f 4 ../data-hold/account-store/@KimKardashian.csv | grep -v 'RT' |\
sed -E 's/http.+\b//g' | sed -E 's/@.+b//g' | iconv -c -f utf-8 -t ascii |\
grep -oiE '\b[^!]+(!){3}' > 3.txt

while read -r line; do
        numTarget=3
        numChars=$(($(echo $line | wc -c) - $(echo $numTarget)))
        numTokens=$(echo $line | wc -w)
        printf '%s|%i|%i|%i\n' "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
done < 3.txt

echo "!!!!"
csvfix order -f 4 ../data-hold/account-store/@KimKardashian.csv | grep -v 'RT' |\
sed -E 's/http.+\b//g' | sed -E 's/@.+b//g' | iconv -c -f utf-8 -t ascii |\
grep -oiE '\b[^!]+(!){3}' > 4.txt

while read -r line; do
        numTarget=4
        numChars=$(($(echo $line | wc -c) - $(echo $numTarget)))
        numTokens=$(echo $line | wc -w)
        printf '%s|%i|%i|%i\n' "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
done < 4.txt

echo "!!!!!"
csvfix order -f 4 ../data-hold/account-store/@KimKardashian.csv | grep -v 'RT' |\
sed -E 's/http.+\b//g' | sed -E 's/@.+b//g' | iconv -c -f utf-8 -t ascii |\
grep -oiE '\b[^!]+(!){3}' > 5.txt

while read -r line; do
        numTarget=5
        numChars=$(($(echo $line | wc -c) - $(echo $numTarget)))
        numTokens=$(echo $line | wc -w)
        printf '%s|%i|%i|%i\n' "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
done < 5.txt

echo "!!!!!!"
csvfix order -f 4 ../data-hold/account-store/@KimKardashian.csv | grep -v 'RT' |\
sed -E 's/http.+\b//g' | sed -E 's/@.+b//g' | iconv -c -f utf-8 -t ascii |\
grep -oiE '\b[^!]+(!){6}' > 6.txt

while read -r line; do
        numTarget=6
        numChars=$(($(echo $line | wc -c) - $(echo $numTarget)))
        numTokens=$(echo $line | wc -w)
        printf '%s|%i|%i|%i\n' "$line" "$numTarget" "$numChars" "$numTokens" >> aggregate.psv
done < 6.txt
