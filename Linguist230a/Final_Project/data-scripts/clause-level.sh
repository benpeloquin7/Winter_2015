#####:::-------clause-level.sh-------:::#####

if [[ -e aggregate.psv ]]; then
rm aggregate.psv
fi

touch aggregate.psv

for file in $(ls ../data-hold/account-store/*); do
current_account=$(echo $file | grep -oE "@.+" | sed -E 's/\.csv//g')
echo "current account: $current_account"
bash clause-level-helper.sh $file
done
