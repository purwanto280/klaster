install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("sentimentr")
install.packages("plyr")
install.packages("RTextTools")
install.packages("devtools")
require(devtools)
install.packages("e1071")
install_github("sentiment140","okugami79")
install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")


library(e1071)
library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(plyr)
library(RTextTools)
library(sentiment)
library(Rstem)

setup_twitter_oauth("CKGwy5jS454hqvK0A3wkOUL7x","TJiOjYrEff7ln0SUbPZYF20rZMcFoJrgkgVZx0iAHK5z38WxYW","927709859411140608-WNpHxKi9y5VP6Xi8rwEpfJTzkN1Fsd7","2IyrKZ3YkNztKPSnMOvRcqSYJ961tu1Ev9ub37tvDL1u9")

#WORDCLOUD

tweets <- userTimeline("affsuzukicup", n = 250)
show(tweets)
n.tweet <- length(tweets)
# convert tweets to a data frame
tweets.df <- twListToDF(tweets)

myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

term.freq <- rowSums(as.matrix(tdm))
tdm <- TermDocumentMatrix(myCorpus)
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)


#k-means clustering
d <- dist(term.freq, method="euclidian")
carsCluster <- kmeans(term.freq, 3)
clusplot(as.matrix(d), carsCluster$cluster, color=T, shade=T, labels=3, lines=0)
