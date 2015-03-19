file_to_move=$1
if [[ -e $file_to_move ]]; then
mv $file_to_move ~/WWW/$file_to_move
else
echo "Error: No file named $file_to_move"
fi
