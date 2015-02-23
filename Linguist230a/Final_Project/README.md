###:-----README.md------:###
twitter profiles pulled from:
http://twittercounter.com/pages/100


#Some code for tweet cleaning and word counting (obama example)
csvfix order -f 4 @BarackObama.csv | grep -v 'RT' |\
tr '[A-Z]' '[a-z]' | tr -d '\?\",\.:()-'| tr ' ' '\n' |\
sort | uniq -c | sort -r | head -n 25

#some n-grams code help in R
http://english.boisestate.edu/johnfry/files/2013/04/bigram-2x2.pdf
