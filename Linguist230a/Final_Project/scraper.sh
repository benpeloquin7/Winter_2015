###:------scraper.sh-------:###

#####################
###Data collection###
#####################

mkdir -p data-hold

#Curl down top 100 twitter accounts webpage into twittercounter100.html
curl -s -o data-hold/twittercounter100.html http://twittercounter.com/pages/100

#Pull out twitter handles store in top100 accounts
cat data-hold/twittercounter100.html | pup 'a.uname text{}'\
> data-hold/top100_accounts.txt

#Store each of the top 100 in a uniq CSV file
#Account specific data in account-store directory
mkdir -p data-hold/account-store
for account in $(cat data-hold/top100_accounts.txt); do
t timeline -n 1000 --csv $account > data-hold/account-store/$account.csv
#avoid twitter api rate limit, slow api calls to one a minute
sleep 35s
done
