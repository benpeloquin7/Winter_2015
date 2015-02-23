data = scan(what="c", sep="\n", file="cleantest.txt")
tokens = unlist(strsplit(data, "[^a-z]+"))
tokens2 = c(tokens[-1], ".")
tokens3 = c(tokens2[-1], ".")
trigrams = paste(tokens, tokens2, tokens3)
freq = sort(table(trigrams), decreasing=T)

head(freq, n=25)
