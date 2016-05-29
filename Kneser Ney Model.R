##Code used to read files, clean data and create tables of unigrams,
##bigrams and trigrams, then calculate probabiltities used in
##Kneser Ney prediction model

setwd("C:/Users/Craig/Desktop/Coursera/Capstone Project/Sample Files")
library(tm)
library(slam)
library(plyr)
library(reshape2)
library(ggplot2)
library(stringr)

##Read in files
newsfile <- file("en_US.news.txt","rb")
news<-readLines(newsfile,encoding="UTF-8",skipNul=TRUE)
close(newsfile)

twitterfile <- file("en_US.twitter.txt","rb")
twitter<-readLines(twitterfile,encoding="UTF-8",skipNul=TRUE)
close(twitterfile)

blogsfile <- file("en_US.blogs.txt","rb")
blogs<-readLines(blogsfile,encoding="UTF-8",skipNul=TRUE)
close(blogsfile)

swearwordfile<-read.csv("swearWords.csv",header=FALSE)
swearwords<-NULL
for(i in 1:length(swearwordfile))
        swearwords[i]<-as.character(swearwordfile[1,i])

##Select samples
set.seed(1000)
nsample<-rbinom(length(news),1,.01)
newssample<-news[nsample==1]
newstest<-news[nsample==0]

tsample<-rbinom(length(twitter),1,.01)
twittersample<-twitter[tsample==1]
twittertest<-twitter[tsample==0]

bsample<-rbinom(length(blogs),1,.01)
blogssample<-blogs[bsample==1]
blogstest<-blogs[bsample==0]

##Define functions
CleanCorpus <- function(x) {
        sampleCorpus<-Corpus(VectorSource(x))
        sampleCorpus<-tm_map(sampleCorpus, removePunctuation)
        sampleCorpus<-tm_map(sampleCorpus, tolower)
        sampleCorpus<-tm_map(sampleCorpus, removeNumbers)
        sampleCorpus<-tm_map(sampleCorpus, stripWhitespace)
        sampleCorpus<-tm_map(sampleCorpus, removeWords, swearwords) ##swearwords from bannedwordlist.com
        sampleCorpus<-tm_map(sampleCorpus, PlainTextDocument)}
##Create corpus
sample<-c(newssample, twittersample, blogssample)
sample<-iconv(sample, "latin1", "ASCII", sub="")
Corpus<-CleanCorpus(sample)

##Function to create bigrams
BigramTokenizer <- function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names=FALSE)
##Function to create trigrams
TrigramTokenizer <- function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse=" "), use.names=FALSE)

##Create n-grams
##Single word counts
tdm<-TermDocumentMatrix(Corpus, control=list(wordLengths=c(1,Inf)))
##Bigrams
tdm2<-TermDocumentMatrix(Corpus, control=list(tokenize=BigramTokenizer))
##Trigrams
tdm3<-TermDocumentMatrix(Corpus, control=list(tokenize=TrigramTokenizer))
##Count the number of each word, bigram and trigram
freq<-row_sums(tdm)
freq2<-row_sums(tdm2)
freq3<-row_sums(tdm3)
df<-data.frame(word=names(freq), freq)
df2<-data.frame(word=names(freq2), freq2)
df3<-data.frame(word=names(freq3), freq3)
df<-arrange(df,desc(freq))
df2<-arrange(df2,desc(freq2))
df3<-arrange(df3,desc(freq3))

##Kneser Ney

##Break up bigrams
bigrams<-nrow(df2)
df2$unigram<-word(df2$words,1)
df2$lastword<-word(df2$words,2)

##Break up trigrams
trigrams<-nrow(df3)
df3$bigram<-word(df3$words,1,2)
df3$lasttwo<-word(df3$words,2,3)
df3$firstword<-word(df3$words,1)
df3$middle<-word(df3$words,2)
df3$lastword<-word(df3$words,3)

##Perform unigram calculations
samplerows<-46678
pb <- txtProgressBar(min = 0, max = samplerows, style = 3)
for(i in 1:samplerows) {
        Sys.sleep(0.001) 
        setTxtProgressBar(pb, i)
        ##Calculate number of times each unigram appears as a novel continuation
        df$KN[i]<-sum(df2$lastword==df$words[i])
        ##calculate number of types that can follow each unigram - used to set lambdas
        df$KNfollow[i]<-sum(df2$unigram==df$words[i])
}
close(pb)
df$KNP<-df$KN/bigrams
df$lambda<-0.75/df$freq*df$KNfollow

##Perform bigram calculations
samplerows<-212740
pb <- txtProgressBar(min = 0, max = samplerows, style = 3)
for(i in 1:samplerows) {
        Sys.sleep(0.001) 
        setTxtProgressBar(pb, i)
        ##Calculate number of times each bigram appears as a novel continuation
        df2$KN[i]<-sum(df3$lasttwo==df2$words[i])
        ##Calculate number of types that can follow each bigram - used to set lambdas
        df2$KNfollow[i]<-sum(df3$bigram==df2$words[i])
        ##Calculate bigram probabilities for trigram KN model
        df2$KNP[i]<-max((df2$KN[i]-0.75),0)/sum(df3$middle==df2$unigram[i])+df$lambda[df$words==df2$unigram[i]]*df$KNP[df$words==df2$lastword[i]]
}
df2$lambda<-0.75/df2$freq*df2$KNfollow

##Perform trigram calculations
samplerows<-154364
pb <- txtProgressBar(min = 0, max = samplerows, style = 3)
for(i in 1:samplerows) {
        Sys.sleep(0.001) 
        setTxtProgressBar(pb, i)
        ##calculate trigram probabilities
        df3$KNP[i]<-max((df3$freq[i]-0.75),0)/df2$freq[df2$words==df3$bigram[i]]+df2$lambda[df2$words==df3$bigram[i]]*df2$KNP[df2$words==df3$lasttwo[i]]
}
close(pb)

##Sort tables by KN prob
df<-df[order(-df$KNP),]
df2<-df2[order(-df2$KNP,-df2$freq),]
df3<-df3[order(-df3$KNP,-df3$freq),]

##Save only columns to be used in prediction model
df<-df[,c("words","KNP","lambda")]
names(df)[1]<-"lastword"
df2<-df2[,c("words","unigram","lastword","KNP","lambda")]
df3<-df3[,c("bigram","lasttwo","lastword","KNP")]

##Save tables to be used in prediction model
write.table(df, file = "df.csv", sep = ",", col.names = c("lastword","KNP","lambda"))
write.table(df2, file = "df2.csv", sep = ",", col.names = c("words","unigram","lastword","KNP","lambda"))
write.table(df3, file = "df3.csv", sep = ",", col.names = c("bigram","lasttwo","lastword","KNP"))