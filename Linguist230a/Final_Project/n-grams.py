#:--------n-grams.py-----------#
from random import randint
from math import trunc

nSize = 7

#tokenized text
tokens = []
with open('cleantest.txt', 'r') as f:
	for line in f:
		for word in line.split():
			tokens.append(word)

#Wrap-around
for i in range(0, nSize - 1):
	tokens.append(tokens[i])

#Set of prefixes
uniq_tokens = set(tokens)

#Fill map
gram_map = dict()
for i in range(0, len(tokens) - nSize - 1):
	temp = []
	for j in range(1, nSize):
		temp.append(tokens[i+j])
    	if (gram_map.has_key(tokens[i])):
        	gram_map[tokens[i]].append(temp)
    	else:
        	gram_map[tokens[i]] = []
		gram_map[tokens[i]].append(temp)

#Set random number generator
#random.seed(0)
start_key = "the"
current_key = start_key

sent = ""
sent += "...%s " % start_key 

#Generate n-gram tweet
for i in range(0, trunc(140 / nSize)):
	#Randomly select a vector of words in map
	random_value = randint(0, len(gram_map[current_key])) - 1
        next_tokens = gram_map[current_key][random_value]
	
	#Append to sentence string
	sent_append =' '.join(next_tokens)
        sent += " %s" % sent_append
	
	#If next tokens has length of one, set to that word
	if (len(next_tokens) == 1):
                current_key = next_tokens
        else:
	#Otherwise get the last word in values vector
                current_key = next_tokens[len(next_tokens)-1]
        
sent += "..."

print(sent)
