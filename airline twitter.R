
# Install and Activate Packages
install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
install.packages("plyr")
installed.packages("ggplot2")
install.packages("doBy")
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(plyr)
library(ggplot2)
library(doBy)

# loading and saving the tweets into R
delta.tweets = searchTwitter('@delta', n=1500)
tweets.df.delta <- twListToDF(delta.tweets)
save(tweets.df.delta, file=file.path('C:/Users/mohit/Desktop/airline', 'tweets.df.delta.after.RData' ), ascii=T)

united.tweets = searchTwitter('@united', n=1500)
tweets.df.united <- twListToDF(united.tweets)

save(tweets.df.united, file=file.path('C:/Users/mohit/Desktop/airline', 'tweets.df.united.RData' ), ascii=T)

jetblue.tweets = searchTwitter('@jetblue', n=1500)
tweets.df.jetblue <- twListToDF(jetblue.tweets)
save(tweets.df.jetblue, file=file.path('C:/Users/mohit/Desktop/airline', 'tweets.df.jetblue.RData' ), ascii=T)

southwest.tweets = searchTwitter('@SouthwestAir', n=1500)
tweets.df.southwest <- twListToDF(southwest.tweets)
save(tweets.df.southwest, file=file.path('C:/Users/mohit/Desktop/airline', 'tweets.df.southwest.RData' ), ascii=T)

american.tweets = searchTwitter('@AmericanAir', n=1500)
tweets.df.american <- twListToDF(american.tweets)
save(tweets.df.american, file=file.path('C:/Users/mohit/Desktop/airline', 'tweets.df.american.RData' ), ascii=T)

#Sanity check
tweets.df.delta[[1]][4]
#str(tweets.df.delta)
abc = tweets.df.delta[grep("food",tweets.df.delta$text),]
abc$text

# Convert the tweets from list to array format
delta.text = laply(delta.tweets, function(t) t$getText())
united.text = laply(united.tweets, function(t) t$getText())
southwest.text = laply(southwest.tweets, function(t) t$getText())
jetblue.text = laply(jetblue.tweets,function(t) t$getText())
american.text = laply(american.tweets,function(t) t$getText())
length(delta.text)
head(delta.text, 5)

# Downloading Hu and Liu's opinion lexicon
class(delta.text)
hu.liu.pos = scan('C:/Users/mohit/Desktop/positive-words.txt',what='character', comment.char=';')
hu.liu.neg = scan('C:/Users/mohit/Desktop/negative-words.txt',what='character', comment.char=';')
class(hu.liu.neg)
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')


# Function to score sentiments
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences,function(sentence, pos.words, neg.words){
    
    # clean up sentences with R's regex-driven global substitute, gsub():
 
    sentence = gsub('http\\S+\\s*', '', sentence) ## Remove URLs
    
    sentence = gsub('\\b+RT', '', sentence) ## Remove RT
    
    sentence = gsub('#\\S+', '', sentence) ## Remove Hashtags
    
    sentence = gsub('@\\S+', '', sentence) ## Remove Mentions
    
    sentence = gsub('[[:cntrl:]]', '', sentence) ## Remove Controls and special characters
    
    sentence = gsub("\\d", '', sentence) ## Remove Controls and special characters
    
    sentence = gsub('[[:punct:]]', '', sentence) ## Remove Punctuations
    
    sentence = gsub("^[[:space:]]*","",sentence) ## Remove leading whitespaces
    
    sentence = gsub("[[:space:]]*$","",sentence) ## Remove trailing whitespaces
    
    sentence = gsub(' +',' ',sentence) ## Remove extra whitespaces
    
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # compare our words to the dictionaries of positive & negative terms
    qwe = match("words", pos.words)
    qwe2 = match(words, neg.words)
    
    rt = !is.na(qwe)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


# algorithm sanity check
sample = c("Had the best time of my life in New York! What a wonderful experience!","The food was absolutely horrible! It was so bland!","Wow!! You learnt how to make Coffee?? That's amazing! I'm so impressed.")

result = score.sentiment(sample, pos.words, neg.words)
class(result)
str(result)
colnames(result)
rownames(result)
result $ score

# Computing sentiment scores for each airline

delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
str(delta.scores)
score_of_delta=delta.scores $ score
hist(score_of_delta)

united.scores = score.sentiment(united.text, pos.words,neg.words, .progress='text')
score_of_united=united.scores $ score
hist(score_of_united)

southwest.scores = score.sentiment(southwest.text, pos.words,neg.words, .progress='text')
score_of_southwest=southwest.scores $ score
hist(score_of_southwest)

jetblue.scores = score.sentiment(jetblue.text, pos.words,neg.words, .progress='text')
score_of_jetblue=jetblue.scores $ score
hist(score_of_jetblue)

american.scores = score.sentiment(american.text, pos.words,neg.words, .progress='text')
score_of_american=american.scores $ score
hist(score_of_american)

delta.scores$airline = 'Delta'
delta.scores$code = 'DL'

united.scores$airline = 'United'
united.scores$code = 'UA'

southwest.scores$airline = 'Southwest'
southwest.scores$code = 'WN'

jetblue.scores$airline = 'JetBlue'
jetblue.scores$code = 'B6'

american.scores$airline = 'American'
american.scores$code = 'AA'

all.scores = rbind( delta.scores, united.scores, southwest.scores, 
                    jetblue.scores, american.scores )
class(all.scores)

# plot the scores

g.hist = ggplot(data=all.scores, mapping=aes(x=score, fill=airline) )
# add a bar graph layer. Let it bin the data and compute frequencies
# (set binwidth=1 since scores are integers)
g.hist = g.hist + geom_bar( binwidth=1 )

# make a separate plot for each airline
g.hist = g.hist + facet_grid(airline~.)

# plain display, nice colors
g.hist = g.hist + theme_bw() + scale_fill_brewer() 

print(g.hist)
ggsave(file.path('C:/Users/mohit/Desktop/airline/output', 'twitter_score_histograms.pdf'), g.hist, width=6, height=5.5)

# Classify the scores into positive and negative sentiments

all.scores$very.pos.bool = all.scores$score >= 2
all.scores$very.neg.bool = all.scores$score <= -2

all.scores$very.pos = as.numeric(all.scores$very.pos.bool)
all.scores$very.neg = as.numeric(all.scores$very.neg.bool)

twitter.df = ddply(all.scores, c('airline', 'code'), summarise,
                  very.pos.count=sum( very.pos ),
                  very.neg.count=sum( very.neg ) )
twitter.df


twitter.df$very.tot = twitter.df$very.pos.count +   twitter.df$very.neg.count
twitter.df$score = round( 100 * twitter.df$very.pos.count / twitter.df$very.tot )
orderBy(~-score, twitter.df)


# Carry out sentiment analysis on the subtweets

#delta subtweets
food_delta = tweets.df.delta[grep("food",tweets.df.delta$text),]
food.delta = food_delta$text
delta.scores.food = score.sentiment(food.delta, pos.words,neg.words, .progress='text')
delta.scores.food$score
hist(delta.scores.food$score)


baggage_delta=tweets.df.delta[grep("baggage",tweets.df.delta$text),]
baggage.delta = baggage_delta$text
delta.scores.baggage = score.sentiment(baggage.delta, pos.words,neg.words, .progress='text')
delta.scores.baggage$score
hist(delta.scores.baggage$score)

comfort_delta=tweets.df.delta[grep("comfort",tweets.df.delta$text),]
comfort.delta = comfort_delta$text
delta.scores.comfort = score.sentiment(comfort.delta, pos.words,neg.words, .progress='text')
delta.scores.comfort$score
hist(delta.scores.comfort$score)

delta.scores.food$Category = 'Food'
delta.scores.baggage$Category = 'Baggage'
delta.scores.comfort$Category = 'Comfort'

all.scores.delta = rbind( delta.scores.food, delta.scores.baggage, delta.scores.comfort)

all.scores.delta$very.pos.bool =all.scores.delta$score >= 1
all.scores.delta$very.neg.bool = all.scores.delta$score <= 0

all.scores.delta$very.pos = as.numeric(all.scores.delta$very.pos.bool)
all.scores.delta$very.neg = as.numeric(all.scores.delta$very.neg.bool)


twitter.df.delta = ddply(all.scores.delta, c('Category'), summarise,
                   very.pos.count=sum( very.pos ),
                   very.neg.count=sum( very.neg ) )
twitter.df.delta

twitter.df.delta$very.tot = twitter.df.delta$very.pos.count +   twitter.df.delta$very.neg.count
twitter.df.delta$score = round( 100 * twitter.df.delta$very.pos.count / twitter.df.delta$very.tot )
orderBy(~-score, twitter.df.delta)


# United subtweets

food_united = tweets.df.united[grep("food",tweets.df.united$text),]
food.united = food_united$text
united.scores.food = score.sentiment(food.united, pos.words,neg.words, .progress='text')
united.scores.food$score
hist(united.scores.food$score)


baggage_united=tweets.df.united[grep("baggage",tweets.df.united$text),]
baggage.united = baggage_united$text
united.scores.baggage = score.sentiment(baggage.united, pos.words,neg.words, .progress='text')
united.scores.baggage$score
hist(united.scores.baggage$score)

comfort_united=tweets.df.united[grep("comfort",tweets.df.united$text),]
comfort.united = comfort_united$text
united.scores.comfort = score.sentiment(comfort.united, pos.words,neg.words, .progress='text')
united.scores.comfort$score
hist(united.scores.comfort$score)

united.scores.food$Category = 'Food'
united.scores.baggage$Category = 'Baggage'
united.scores.comfort$Category = 'Comfort'

all.scores.united = rbind( united.scores.food, united.scores.baggage, united.scores.comfort)

all.scores.united$very.pos.bool =all.scores.united$score >= 1
all.scores.united$very.neg.bool = all.scores.united$score <= 0

all.scores.united$very.pos = as.numeric(all.scores.united$very.pos.bool)
all.scores.united$very.neg = as.numeric(all.scores.united$very.neg.bool)


twitter.df.united = ddply(all.scores.united, c('Category'), summarise,
                         very.pos.count=sum( very.pos ),
                         very.neg.count=sum( very.neg ) )
twitter.df.united

twitter.df.united$very.tot = twitter.df.united$very.pos.count +   twitter.df.united$very.neg.count
twitter.df.united$score = round( 100 * twitter.df.united$very.pos.count / twitter.df.united$very.tot )
orderBy(~-score, twitter.df.united)

# Southwest subtweets

food_southwest = tweets.df.southwest[grep("food",tweets.df.southwest$text),]
food.southwest = food_southwest$text
southwest.scores.food = score.sentiment(food.southwest, pos.words,neg.words, .progress='text')
southwest.scores.food$score
hist(southwest.scores.food$score)


baggage_southwest=tweets.df.southwest[grep("baggage",tweets.df.southwest$text),]
baggage.southwest = baggage_southwest$text
southwest.scores.baggage = score.sentiment(baggage.southwest, pos.words,neg.words, .progress='text')
southwest.scores.baggage$score
hist(southwest.scores.baggage$score)

comfort_southwest=tweets.df.southwest[grep("comfort",tweets.df.southwest$text),]
comfort.southwest = comfort_southwest$text
southwest.scores.comfort = score.sentiment(comfort.southwest, pos.words,neg.words, .progress='text')
southwest.scores.comfort$score
hist(southwest.scores.comfort$score)

southwest.scores.food$Category = 'Food'
southwest.scores.baggage$Category = 'Baggage'
southwest.scores.comfort$Category = 'Comfort'

all.scores.southwest = rbind( southwest.scores.food, southwest.scores.baggage, southwest.scores.comfort)

all.scores.southwest$very.pos.bool =all.scores.southwest$score >= 1
all.scores.southwest$very.neg.bool = all.scores.southwest$score <= 0

all.scores.southwest$very.pos = as.numeric(all.scores.southwest$very.pos.bool)
all.scores.southwest$very.neg = as.numeric(all.scores.southwest$very.neg.bool)


twitter.df.southwest = ddply(all.scores.southwest, c('Category'), summarise,
                          very.pos.count=sum( very.pos ),
                          very.neg.count=sum( very.neg ) )
twitter.df.southwest

twitter.df.southwest$very.tot = twitter.df.southwest$very.pos.count +   twitter.df.southwest$very.neg.count
twitter.df.southwest$score = round( 100 * twitter.df.southwest$very.pos.count / twitter.df.southwest$very.tot )
orderBy(~-score, twitter.df.southwest)

# Jetblue Subtweets

food_jetblue = tweets.df.jetblue[grep("food",tweets.df.jetblue$text),]
food.jetblue = food_jetblue$text
jetblue.scores.food = score.sentiment(food.jetblue, pos.words,neg.words, .progress='text')
jetblue.scores.food$score
hist(jetblue.scores.food$score)


baggage_jetblue=tweets.df.jetblue[grep("baggage",tweets.df.jetblue$text),]
baggage.jetblue = baggage_jetblue$text
jetblue.scores.baggage = score.sentiment(baggage.jetblue, pos.words,neg.words, .progress='text')
jetblue.scores.baggage$score
hist(jetblue.scores.baggage$score)

comfort_jetblue=tweets.df.jetblue[grep("seat",tweets.df.jetblue$text),]
comfort.jetblue = comfort_jetblue$text
jetblue.scores.comfort = score.sentiment(comfort.jetblue, pos.words,neg.words, .progress='text')
jetblue.scores.comfort$score
hist(jetblue.scores.comfort$score)

jetblue.scores.food$Category = 'Food'
jetblue.scores.baggage$Category = 'Baggage'
jetblue.scores.comfort$Category = 'Comfort'

all.scores.jetblue = rbind( jetblue.scores.food, jetblue.scores.baggage, jetblue.scores.comfort)

all.scores.jetblue$very.pos.bool =all.scores.jetblue$score >= 1
all.scores.jetblue$very.neg.bool = all.scores.jetblue$score <= 0

all.scores.jetblue$very.pos = as.numeric(all.scores.jetblue$very.pos.bool)
all.scores.jetblue$very.neg = as.numeric(all.scores.jetblue$very.neg.bool)


twitter.df.jetblue = ddply(all.scores.jetblue, c('Category'), summarise,
                             very.pos.count=sum( very.pos ),
                             very.neg.count=sum( very.neg ) )
twitter.df.jetblue

twitter.df.jetblue$very.tot = twitter.df.jetblue$very.pos.count +   twitter.df.jetblue$very.neg.count
twitter.df.jetblue$score = round( 100 * twitter.df.jetblue$very.pos.count / twitter.df.jetblue$very.tot )
orderBy(~-score, twitter.df.jetblue)

# american subtweets

food_american = tweets.df.american[grep("food",tweets.df.american$text),]
food.american = food_american$text
american.scores.food = score.sentiment(food.american, pos.words,neg.words, .progress='text')
american.scores.food$score
hist(american.scores.food$score)


baggage_american=tweets.df.american[grep("baggage",tweets.df.american$text),]
baggage.american = baggage_american$text
american.scores.baggage = score.sentiment(baggage.american, pos.words,neg.words, .progress='text')
american.scores.baggage$score
hist(american.scores.baggage$score)

comfort_american=tweets.df.american[grep("seat",tweets.df.american$text),]
comfort.american = comfort_american$text
american.scores.comfort = score.sentiment(comfort.american, pos.words,neg.words, .progress='text')
american.scores.comfort$score
hist(american.scores.comfort$score)

american.scores.food$Category = 'Food'
american.scores.baggage$Category = 'Baggage'
american.scores.comfort$Category = 'Comfort'

all.scores.american = rbind( american.scores.food, american.scores.baggage, american.scores.comfort)

all.scores.american$very.pos.bool =all.scores.american$score >= 1
all.scores.american$very.neg.bool = all.scores.american$score <= 0

all.scores.american$very.pos = as.numeric(all.scores.american$very.pos.bool)
all.scores.american$very.neg = as.numeric(all.scores.american$very.neg.bool)


twitter.df.american = ddply(all.scores.american, c('Category'), summarise,
                           very.pos.count=sum( very.pos ),
                           very.neg.count=sum( very.neg ) )
twitter.df.american

twitter.df.american$very.tot = twitter.df.american$very.pos.count +   twitter.df.american$very.neg.count
twitter.df.american$score = round( 100 * twitter.df.american$very.pos.count / twitter.df.american$very.tot )
orderBy(~-score, twitter.df.american)

