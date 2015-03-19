###::::---------------README.md---------------::::###

*data-hold/
	*BRM-emot-submit.csv
	-->BRM emotional rating data (Valence, Arousal, Dominance for ~14,000 word lemmas. 	http://www.humanities.mcmaster.ca/~vickup/Warriner_et_al%20emot	%20ratings.csv

	*account-store/
	--> English tweets

	*Non_English/
	--> Non-english tweets

	*top100_accounts.txt
	--> scraped account names from twittercounter100.html

	*twittercounter100.html
	--> scraped html doc with top 100 accounts
*data-scripts/
	*ExpressiveLenthening_Analaysis.R
	--> complete statistical analysis for final paper

	*aggregate.psv
	--> cleaned data pipe separated data file for all english tweets
	
	*clause-level.sh
	--> creates aggregate.psv, calls clause-level-helper.sh

	*clause-level-helper.sh
	--> parses tweets for clauses with sentence final (!) from degree 1 (!) to 6 (!!!!!!)
	--> cleans tweets of non -UTF-8 chars along with other symbols (lines 6 - 8)
	--> includes:
		numChars
		numTokens
		account
		clause text
	*data-stats.sh
	--> basic summary statistics on data set

	*toWWW.sh
	--> sends to my WWW folder (non-essential convenience script)

*scraper.sh
--> primary web scraping script
--> uses command line tool t https://github.com/sferik/t
--> populates data-hold with account level data 
