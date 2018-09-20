setwd("/Users/yashsinghai/Desktop/Text mining class files/Class case studies/2. Sentiment Analysis")
getwd()

install.packages(c("devtools", "rjson", "bit64", "httr"))

library(devtools)
install.packages("curl")
require("curl")
install_github("twitteR", username="geoffjentry")
library(twitteR)
library(plyr)
library(stringr)
install.packages("tm")
library(tm)

##Authentication for twitter developers account
consumer_key      <- "m4gos0ZsT9AmU7lm36jx7Ygnu"
consumer_secret   <- "iyvSAItDj4jGgGwnPZ8PpYT05ike11DTVoOnzxG2tJBRSkJExH"
access_token      <- "2371441225-3vhQ9d4wPPe2eXLqt4JDHFJkVCtc7WhkUNohrHz"
access_secret     <- "IekDC8pV51lMLYw6926DOSmXPJGEXwzgWyjdsh1w9KjvE"

setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)

##Searching for the string
search.string <- "Trump"
no.of.tweets <- 1000

input_tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
input_tweets[1:10] 


##Extract a specific set of words as a text string
tweet_trump=sapply(input_tweets,function(x) x$getText()) 


##Score sentiment function
score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

##Positive and negative work analysis
pos=readLines("positive-words.txt") 
neg=readLines("negative-words.txt") 
View(pos)
View(neg)

##Dry run
sample= "I heard that the new iphone is horrible, though my mom hates it."
result=score.sentiment(sample,pos,neg)
View(result)



##Score computation from the tweets
scores <- NULL
for(i in 1:no.of.tweets){
  P.scores = score.sentiment(tweet[i], pos, neg)
  scores <- rbind(scores, P.scores)
#  print(P.scores)
}
View(scores)
#analysis = score.sentiment(tweet_trump, pos, neg)
#View(head(analysis, 10))


##Categorize each tweet as positive, negative and neutral
scores$very.pos = as.numeric(scores$score > 0)
scores$very.neg = as.numeric(scores$score < 0)
scores$very.neu = as.numeric(scores$score == 0)


##Number of positive, negative and neutral tweets
numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)
numneu = sum(scores$very.neu)

##Final results woth pie plot
s <- c(numpos,numneg,numneu)
lbls <-c("POSITIVE","NEGATIVE","NEUTRAL")
pct <- round(s/sum(s)*100)
lbls <- paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")

pie(s,labels = lbls, col = rainbow(length(lbls)),main="OPINION")

