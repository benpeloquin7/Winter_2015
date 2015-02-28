###:------------main.sh-----------:###
output=$(python n-grams.py)

echo $output | sed 's/[\.!?]\s*./\U&\E/g'
