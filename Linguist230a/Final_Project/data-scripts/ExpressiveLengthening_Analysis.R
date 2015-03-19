rm(list=ls())
source("~/Desktop/Winter_2014/Linguist230a/Final_Project/")
library("rjson")
library("ggplot2")
library(lme4)
library(languageR)
library(stringr)

########################################################################################
#function definitions ----------------------------------------------------------------->

#lengthening.counts()
#--------> returns number of lengthened items for a string argument
lengthening.counts = function(str) {
  #remove none alpha-numerics and tokenize
  str=str_replace_all(str, "[^[:alnum:] ]", "")
  str=unlist(sapply(str, FUN=function(x){strsplit(x, split=" ")}))
  
  #boolean lengthened function 3+ chars in a row
  lengthned = function(w) {
    return(grepl('(.)\\1{2}', w))
  }
  values=sapply(str, lengthned)
  values=unlist(values)
  return(sum(values))
}

#avg.Arousal()
#--------> returns average arousal metric for string input
# token matches with mean Arousal ratinsg from e.data
avg.Arousal = function(str) {
  #remove none alpha-numerics and tokenize
  str=str_replace_all(str, "[^[:alnum:] ]", "")
  str=unlist(sapply(str, FUN=function(x){strsplit(x, split=" ")}))
  
  word.value = function(loc) {
    if (!is.na(loc)) {
      return(e.data[loc, "A.Mean.Sum"])
    }  
  }
  #loc function for individual words
  loc=function(w) {
    loc=which(e.data[,"Word"]==w)
    if (length(loc) > 0) {
      return(loc)
    } else return(NA)
  }
  #vector of word locations
  wrd.locs = sapply(str, FUN=function(w){loc(w)})
  wrd.locs = wrd.locs[ !is.na(wrd.locs)]
  #vector of values for words
  values=sapply(wrd.locs, word.value)
  values = as.numeric(values)
  return(mean(values))  
}

#avg.Valence()
#--------> returns average Valence metric for string input
# token matches with mean Valence ratings from e.data
avg.Valence = function(str) {
  #remove none alpha-numerics and tokenize
  str=str_replace_all(str, "[^[:alnum:] ]", "")
  str=unlist(sapply(str, FUN=function(x){strsplit(x, split=" ")}))
  
  word.value = function(loc) {
    if (!is.na(loc)) {
      return(e.data[loc, "V.Mean.Sum"])
    }  
  }
  #loc function for individual words
  loc=function(w) {
    loc=which(e.data[,"Word"]==w)
    if (length(loc) > 0) {
      return(loc)
    } else return(NA)
  }
  #vector of word locations
  wrd.locs = sapply(str, FUN=function(w){loc(w)})
  wrd.locs = wrd.locs[ !is.na(wrd.locs)]
  #vector of values for words
  values=sapply(wrd.locs, word.value)
  values = as.numeric(values)
  return(mean(values))  
}

#avg.Dominance()
#--------> returns average Dominance metric for string input
# token matches with mean Dominance ratings from e.data
avg.Dominance = function(str) {
  #remove none alpha-numerics and tokenize
  str=str_replace_all(str, "[^[:alnum:] ]", "")
  str=unlist(sapply(str, FUN=function(x){strsplit(x, split=" ")}))
  
  word.value = function(loc) {
    if (!is.na(loc)) {
      return(e.data[loc, "D.Mean.Sum"])
    }  
  }
  #loc function for individual words
  loc=function(w) {
    loc=which(e.data[,"Word"]==w)
    if (length(loc) > 0) {
      return(loc)
    } else return(NA)
  }
  #vector of word locations
  wrd.locs = sapply(str, FUN=function(w){loc(w)})
  wrd.locs = wrd.locs[ !is.na(wrd.locs)]
  #vector of values for words
  values=sapply(wrd.locs, word.value)
  values = as.numeric(values)
  return(mean(values))  
}

#avg.Subjectivity()
#--------> returns counts of idenfied (strong) subjective words matched in subj.data 
# subj.data taken from Wilson et. al 2005 Opinion Finder lexicon
avg.Subjectivity = function(str) {
  #remove none alpha-numerics and tokenize
  str=str_replace_all(str, "[^[:alnum:] ]", "")
  str=unlist(sapply(str, FUN=function(x){strsplit(x, split=" ")}))
  
  #loc function for individual words
  loc=function(w) {
    loc=which(subj.data[,"word"]==w)[1]
    if (length(loc) > 0) {
      return(loc)
    } else return(NA)
  }
  word.value = function(loc) {
    if (!is.na(loc)) {
      return(subj.data[loc, "type"])
    }  
  }
  #vector of word locations
  wrd.locs = sapply(str, FUN=function(w){loc(w)})
  wrd.locs = wrd.locs[ !is.na(wrd.locs)]
  #vector of values for words
  values=sapply(wrd.locs, word.value)
  values = as.numeric(values)
  return(sum(values))  
}

#lengthened.len()
#lengthened terms increase as a function of sentence final lengthening used
#with data filtered to 
lengthened.len = function(str) {
  str=str_replace_all(str, "[^[:alnum:] ]", "")
  str=unlist(sapply(str, FUN=function(x){strsplit(x, split=" ")}))
  
  #length of elongated term
  find.len = function(w) {
    if (grepl('(.)\\1{2}', w)) {
      return(nchar(w))
    }    
  }
  #values vector
  values = sapply(str, find.len)
  values = as.numeric(values[ values != "NULL" ])
  return(mean(values))
}

#skew()
#optional skew function for Valence and Dominance transform x < 5 to 10 - x
skew = function(n) {
  if (n < 5) { return(10 - n) }
  else { return(n) }
}

#function definitions ----------------------------------------------------------------->
########################################################################################

########################################################################################
#initialize data ---------------------------------------------------------------------->
#full data set
data=read.delim("~/Desktop/Winter_2014/Linguist230a/Final_Project/aggregate4.psv",
                header = F, sep = "|", quote = "\"",
                dec = ".", fill = TRUE, comment.char = "")
colnames(data) = c("account", "text", "numTarget", "numChars", "numTokens")
head(data)

#emotionality data from BRM:
#http://www.humanities.mcmaster.ca/~vickup/Warriner_et_al%20emot%20ratings.csv
e.data = read.delim("~/Desktop/Winter_2014/Linguist230a/Final_Project/BRM-emot-submit.csv",
                    header = T, sep = ",", comment.char = "")
head(e.data)

#subjectivity data
subj.data = read.delim("~/Desktop/subjectivity_clues_hltemnlp05/subjclueslen1-HLTEMNLP05.tff",
                    header = F, sep = " ", comment.char = "")

#data cleaning function remove 
remove = function(str) {
  return(gsub('.+=','',str))
}
#clean data
subj.data = sapply(seq(from=1, to=6), FUN=function(x) {
  subj.data[,x] = sapply(subj.data[,x], remove)  
})
colnames(subj.data) = c("type", "len", "word", "pos", "stemmed", "prior_polarity")
head(subj.data)
#relable types
subj.data[,"type"] = sapply(subj.data[,"type"], FUN=function(w) {
  ifelse(w=="weaksubj", 0, 1)
})

###########
##avgTlen##
data$avgTLen = data$numChars / data$numTokens
#convert text to lower
data[,"text"] = tolower(data[,"text"])

########################
##emotionality ratings##
#CAUTION---> SLOW RUNNING 
data$Arousal = sapply(data$text, avg.Arousal)
data$Valence = sapply(data$text, avg.Valence)
data$Dominance = sapply(data$text, avg.Dominance)

#######################
#count of lengthenings#
data$len.count = sapply(data$text, FUN=function(str) { lengthening.counts(str) })

##############################
#percent of string lengthened#
data$percent.lengthened = data$len.count / data$numTokens

##############################
#count subjectivity strong#
data$subjectivity.count = sapply(data$text, FUN=function(str) { avg.Subjectivity(str) })
data$percent.subj = data$subjectivity.count / data$numTokens

#initialize data ---------------------------------------------------------------------->
########################################################################################

########################################################################################
#Analysis space ----------------------------------------------------------------------->
#optional data housekeeping (trim NA data)
#p.data.cleaned = data[which(!is.na(data$Arousal)),]

#######################################################
#Breif analysis on e.data----------------------------->
plot.new()
e.data$Word = sapply(e.data$Word, toString)
e.data$nChar = sapply(e.data$Word, nchar)
#e.data[which(e.data[,"nChar"] == max(e.data$nChar)), "Word"]
#nchar(e.data[which(e.data[,"A.Mean.Sum"] == max(e.data[,"A.Mean.Sum"])), "Word"])

#Plot and linear models
plot(e.data$nChar, e.data$A.Mean.Sum,
     main="Emotional Arousal ratings by\nword character length",
     ylab="character length", xlab="mean arousal rating",
     pch=10, col="blue")
m0 = lm(e.data$A.Mean.Sum ~ 1)
m1 = lm(e.data$A.Mean.Sum ~ e.data$nChar)
abline(m1, col="red")
anova(m0, m1)
#Breif analysis on e.data----------------------------->
#######################################################

#####################################################################
#models------------------------------------------------------------->
#character counts
#Hierarchical linear regression with DV numChars, FE numTarget, RE numTarget | account
m0.char = glmer(numChars ~ 1 + (1 + numTarget | account), family="poisson", data=data)
m1.char = glmer(numChars ~ numTarget + (1 + numTarget |account), family="poisson", data=data)
summary(m1.char)
anova(m0.char, m1.char)

#token count
m0.tokens = glmer(numTokens ~ 1 + (1 + numTarget | account), data=data)
m1.tokens = glmer(numTokens ~ numTarget + (1 + numTarget |account), data=data)
summary(m0.tokens)
anova(m0.tokens, m1.tokens)

#Avg token len
m0.tLen = glmer(avgTLen ~ 1 + (1 + numTarget | account), data=data)
m1.tLen = glmer(avgTLen ~ numTarget + (1 + numTarget |account), data=data)
summary(m1.tLen)
anova(m0.tLen, m1.tLen)

#arousal ratings
avgArousal = aggregate(Arousal ~ numTarget, data=data, mean)
m0.A = lmer(Arousal ~ 1 + (1 + numTarget | account), data=data)
m1.A = lmer(Arousal ~ numTarget + (1 + numTarget | account), data=data)
summary(m1.A)
anova(m0.A, m1.A)

#t.test arousal under 2 lengthened vs over 2 lengthened
avgArousal.under2= data[which(data$numTarget < 2), 7]
avgArousal.over2 = data[which(data$numTarget >= 2), 7]
t.test(avgArousal.under2, avgArousal.over2)

#percent of clauses with other lengthened items
#Hierarchical linear regression with DV percent.lengthend, FE numTarget, RE numTarget | account
agg.lengthened = aggregate(percent.lengthened ~ numTarget, data=data, mean)
m0.lengthened = lmer(percent.lengthened ~ 1 + (1 + numTarget | account), data=data)
m1.lengthened = lmer(percent.lengthened ~ numTarget + (1 + numTarget | account), data=data)
summary(m1.lengthened)
anova(m0.lengthened, m1.lengthened)

#Degree of co-occurring lengthening
data.lengthened.only = data[which(data[,"len.count"] > 0),]
data.lengthened.only$lengthened.terms = sapply(data.lengthened.only$text, lengthened.len)
head(data.lengthened.only)
agg.dgrLen = aggregate(lengthened.terms ~ numTarget, data=data.lengthened.only, mean)
m0.dgrLen = lmer(lengthened.terms ~ 1 + (1 + numTarget | account), data=data.lengthened.only)
m1.dgrLen = lmer(lengthened.terms ~ numTarget + (1 + numTarget | account), data=data.lengthened.only)
summary(m1.dgrLen)
anova(m0.dgrLen, m1.dgrLen)
#models------------------------------------------------------------->
#####################################################################
#Analysis space ----------------------------------------------------------------------->
########################################################################################

########################################################################################
#plots -------------------------------------------------------------------------------->

############################################################
#Figure 1 ------------------------------------------------->
#Vectors by extent of expressive lengthening
num.1 = as.integer(data[which(data$numTarget == 1), 4])
num.2 = as.integer(data[which(data$numTarget == 2), 4])
num.3 = as.integer(data[which(data$numTarget == 3), 4])
num.4 = as.integer(data[which(data$numTarget == 4), 4])
num.5 = as.integer(data[which(data$numTarget == 5), 4])
num.6 = as.integer(data[which(data$numTarget == 6), 4])
#Bar plot of frequency of expressive lengthening
bp = barplot(c(length(num.1), length(num.2), length(num.3),
               length(num.4), length(num.5), length(num.6)),
             names=c(1:6), main="Counts by expressive lengthening\n",
             ylab="counts", xlab="expressive lengthening", ylim=c(0,20000),
             col="light blue")
text(bp, 0, round(c(length(num.1), length(num.2), length(num.3),
                    length(num.4), length(num.5), length(num.6))), cex=.7, pos=3)
#Figure 1 ------------------------------------------------->
############################################################

############################################################
#Figure 2 ------------------------------------------------->
#Number of characters density plots
plot(density(data[which(data$numTarget==1),]$numChars), col="blue", ylim=c(0,.05),
     main="Clause length ~\nExtent of expressive lengthening", xlab="clause length - characters")
lines(density(data[which(data$numTarget==2),]$numChars), col="red")
lines(density(data[which(data$numTarget==3),]$numChars), col="green")
lines(density(data[which(data$numTarget==4),]$numChars), col="purple")
lines(density(data[which(data$numTarget==5),]$numChars), col="orange")
lines(density(data[which(data$numTarget==6),]$numChars), col="black")
abline(v=mean(num.1), col="blue", lty=2)
abline(v=mean(num.2), col="red", lty=2)
abline(v=mean(num.3), col="green", lty=2)
abline(v=mean(num.4), col="purple", lty=2)
abline(v=mean(num.5), col="orange", lty=2)
abline(v=mean(num.6), col="black", lty=2)
legend("topright",c("1","2","3","4","5", "6"), lty=c(1,1),
       lwd=c(2.5,2.5),col=c("blue","red", "green", "purple", "orange", "black"))
#Figure 2 ------------------------------------------------->
############################################################

############################################################
#Figure 3 ------------------------------------------------->
#Average Arousal rating
agg.emo = aggregate(Arousal ~ numTarget, data = data, mean)
bp.emo = barplot(agg.emo[,2], col=c("blue","red", "green", "purple", "orange", "grey"),
                 names=c(1,2,3,4,5,6),
                 main="Average Arousal rating by\ndegree of lengthening",
                 xlab="Degree of expressive lengthening",
                 ylab="Average Arousal ratings (scale is 1 - 9)", ylim=c(0,5))
text(bp.emo, 1, round(c(agg.emo[1,2], agg.emo[2,2], agg.emo[3,2],
                        agg.emo[4,2], agg.emo[5,2], agg.emo[6,2]), digits=2),
     cex=.7, pos=3)
#Figure 3 ------------------------------------------------->
############################################################

############################################################
#Figure 4 ------------------------------------------------->
#percent lengthened plot
agg1 = aggregate(percent.lengthened~numTarget, data=data, mean)
plot(agg1[,1], (100*agg1[,2]), type="b",col="red",
     main="Likelihood of observing\na lengthened token in preceding clause",
     xlab = "Degree of expressive lengthening",
     ylab="% likelihood of observing lengthened token", ylim=c(0,9))
#Figure 4 ------------------------------------------------->
############################################################

############################################################
#Figure 5 ------------------------------------------------->
data.lengthened.only = data[which(data[,"len.count"] > 0),]
data.lengthened.only$lengthened.terms = sapply(data.lengthened.only$text, lengthened.len)
plot(agg.dgrLen[,2],
     ylim = c(7,12), main="Average character length of\nlengthened tokens",
     type="b",col="blue",
     xlab="Degree of sentence final lengthening",
     ylab="Average length of lengthened tokens")
#Figure 5 ------------------------------------------------->
############################################################

##############
#Other Anlyses
###Only clauses that contain a lengthened token in the prior to sentence final (!)
data.lengthened.only = data[which(data[,"len.count"] > 0),]
aggregate(avgTLen ~ numTarget, data=data.lengthened.only, mean)
length(which(data.lengthened.only[,"numTarget"]==4))
bp.2 = barplot(c(length(which(data.lengthened.only$numTarget == 1)), length(which(data.lengthened.only$numTarget == 2)),
                 length(which(data.lengthened.only$numTarget == 3)), length(which(data.lengthened.only$numTarget == 4)),
                 length(which(data.lengthened.only$numTarget == 5)), length(which(data.lengthened.only$numTarget == 6))),
               names=c(1:6), main="Counts in clauses with\nat last one lengthened token",
               ylab="Lengthend token counts", xlab="Degree of expressive lengthening", ylim=c(0,600),
               col="light blue")
text(bp.2, 0, round(c(length(which(data.lengthened.only$numTarget == 1)), length(which(data.lengthened.only$numTarget == 2)),
                      length(which(data.lengthened.only$numTarget == 3)), length(which(data.lengthened.only$numTarget == 4)),
                      length(which(data.lengthened.only$numTarget == 5)), length(which(data.lengthened.only$numTarget == 6)))),
     cex=.7, pos=3)

#plots -------------------------------------------------------------------------------->
########################################################################################