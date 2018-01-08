install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("plyr")
install.packages("RTextTools")

install.packages("e1071")

library(e1071)

library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(wordcloud)
library(plyr)
library(RTextTools)
library(e1071)

setup_twitter_oauth("dOrn0JcEcmYslZq30swkkXDjN","gzoeRgOIYoquT2Kj5gzcH3TqcNnDYku3qSLnzGMil1x3QsjccQ","484467466-ScpeNMUbMXcph9ncPLHTgeXc5q6pAf4AISQ58F6C","zJV3iq6s78YDjOmDWI08ejpcQRlL7nrDWyBsYlGThyIiv")

tweets <- userTimeline("Trump", n = 10)
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
myCorpus
term.freq <- rowSums(as.matrix(tdm))
tdm <- TermDocumentMatrix(myCorpus)
tdmat <- as.matrix(removeSparseTerms(tdm, sparse=0.3))
# compute distances
distMatrix <- dist(scale(tdm))
fit <- hclust(distMatrix, method="ward.D2")
plot(fit)
fit <- hclust(distMatrix, method="single")
plot(fit)