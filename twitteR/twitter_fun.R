install.packages("twitteR")
library(twitteR)
install.packages("tm")
library(tm)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
# First set up
setup_twitter_oauth("HGRa7YdkbLE5ofiPuBhvivXC2", "owCXFqyEcqj0anRQLlUdNADKecxyJMPXN1jVUlad3EIxHGYFm7")
tweets = searchTwitter('#LGBT',n = 5)


wordcloud = function(text){
  text = "computer"
  install.packages("SnowballC")
  library("SnowballC")
  # Wordcloud
  my_tweets = searchTwitter(text,n = 100, lang = "en")
  
  my_text = sapply(my_tweets, function(x) x$getText())
  
  #remove strange characters
  my_text = iconv(my_text, to = "utf-8", sub="")
  
  # create a corpus
  my_corpus = Corpus(VectorSource(my_text))
  
  
  # document matrix, remove punctuation, remove numbers, turn to lower case
  tdm = TermDocumentMatrix(my_corpus,control = list(removePunctuation = TRUE,stopwords = c(text, stopwords("english")),removeNumbers = TRUE, tolower = TRUE))
  
  # define tdm as matrix
  m = as.matrix(tdm)
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing=TRUE) 
  # create a data frame with words and their frequencies
  dm = data.frame(word=names(word_freqs), freq=word_freqs)
  
  #plot the wordcloud
  # plot wordcloud
  wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "GnBu"))
  
}

wordcloud("computer")

# Wordcloud
stat_tweets = searchTwitter("statistics",n = 500, lang = "en")

stat_text = sapply(stat_tweets, function(x) x$getText())

#remove strange characters
stat_text = iconv(stat_text, to = "utf-8-mac")

# create a corpus
stat_corpus = Corpus(VectorSource(stat_text))

install.packages("SnowballC")
library("SnowballC")
# document matrix, remove punctuation, remove numbers, turn to lower case
tdm = TermDocumentMatrix(stat_corpus,control = list(removePunctuation = TRUE,stopwords = c("statistics", stopwords("english")),removeNumbers = TRUE, tolower = TRUE))

# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#plot the wordcloud
# plot wordcloud
wordcloud(dm$word)

# save the image in png format
png("MachineLearningCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "GnBu"))
dev.off()

