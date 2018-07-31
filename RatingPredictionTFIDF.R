library(readr)
library(dplyr)
library(stringr)
library(jsonlite)
library(tm)
library(wordcloud)

rest_reviews <- readRDS("rest_reviews")
data <- rest_reviews
#star <- subset(data, data$stars == 5 | data$stars == 4, select = c(text, review_id))
#r <- star

t <- data.frame(data$review_id, data$text, stringsAsFactors = FALSE)
names(t) <- c("doc_id", "text")

corpus <- Corpus(DataframeSource(t))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
## layout(matrix(c(1, 2), nrow=2), heights=c(1, 4)) par(mar=rep(0, 4))
## plot.new() text(x=0.5, y=0.5, 'Frequest Words used in 4 or 5 reviews')
#wordcloud(corpus, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
#          rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))

rm(t)
review_dtm_tfidf <- DocumentTermMatrix(corpus, 
                                       control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.94)

rest_reviews = cbind(rest_reviews, as.matrix(review_dtm_tfidf))

rest_reviews$date=NULL
rest_reviews$name=NULL
rest_reviews$city=NULL
rest_reviews$state=NULL
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

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

modelFit <- train(as.factor(stars)~., data=training, 
                  method = "rf")
##modelFit
predictions <- predict(modelFit, newdata=testing)
##confusionMatrix(predictions, testing$review_stars_bin)
## print(modelFit)
## plot(modelFit$finalModel, uniform=TRUE, main="classification Tree")
library(gmodels)
CrossTable(predictions, testing$stars, 
           dnn = c("Predicted" , "Actual") , 
           prop.t=FALSE, prop.c=FALSE, prop.r=FALSE , prop.chisq = FALSE)

confusionMatrix(predictions, testing$stars)


#######################################################################
