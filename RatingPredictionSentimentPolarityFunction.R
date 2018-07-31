library(readr)
library(dplyr)
library(stringr)
library(jsonlite)
library(tm)
library(wordcloud)

rest_reviews <- readRDS("rest_reviews")

pos.words <- read.csv('pve.csv')
neg.words <- read.csv('nve.csv')
pos.words <- scan('pve.csv',what = 'character')
neg.words <- scan('nve.csv',what = 'character')


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an “l” for us
  # we want a simple array (“a”) of scores back, so we use 
  # “l” + “a” + “ply” = “laply”:
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R’s regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
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

result <- score.sentiment(rest_reviews$text,pos.words,neg.words)
summary(result$score)

hist(result$score,col = 'yellow', main = 'Score of reviews' ,
      ylab =  'Count of reviews')

count(result$score)

rest_reviews = cbind(rest_reviews, as.matrix(result))
rest_reviews$date=NULL
rest_reviews$name=NULL
rest_reviews$city=NULL
rest_reviews$state=NULL
rest_reviews$text=NULL
rest_reviews$text=NULL

data <- rest_reviews
data$business_id=NULL
data$review_id=NULL


library(caret)

set.seed ( 32343)
inTrain <- createDataPartition ( y=data$stars, p=.70, list =FALSE)
training <- data[inTrain,]
testing <- data[-inTrain,]
prop.table(table(training$stars))
prop.table(table(testing$stars))

modelFit <- train(as.factor(stars)~., data=training, method="multinom")
##modelFit
predictions <- predict(modelFit, newdata=testing)
## print(modelFit)
## plot(modelFit$finalModel, uniform=TRUE)
library(gmodels)
CrossTable(predictions, testing$stars, 
           dnn = c("Predicted" , "Actual") , 
           prop.t=FALSE, prop.c=FALSE, prop.r=FALSE , prop.chisq = FALSE)

confusionMatrix(predictions, testing$stars)
