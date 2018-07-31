library(dplyr)
library(tidytext)

rest_reviews <- readRDS("rest_reviews")


###WORDCLOUD FOR 1 STAR REVIEWS
one.star <- subset(rest_reviews, rest_reviews$stars == "1",  select = c(text, review_id))

t <- data.frame(one.star$review_id, one.star$text, stringsAsFactors = FALSE)
names(t) <- c("doc_id", "text")

corpus <- Corpus(DataframeSource(t))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#layout(matrix(c(1, 2), nrow=2), heights=c(1, 4)) par(mar=rep(0, 4))
#plot.new() text(x=0.5, y=0.5, 'Frequent Words used in 1 star reviews')
#wordcloud(corpus, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
#         rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
rm(t)
review_dtm_tfidf <- DocumentTermMatrix(corpus, 
                                       control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.99)

tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
tdm <- removeSparseTerms(tdm, 0.99)
m <- as.matrix(tdm)

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


###WORDCLOUD FOR 5 STAR REVIEWS

one.star <- subset(rest_reviews, rest_reviews$stars == "5",  select = c(text, review_id))

t <- data.frame(one.star$review_id, one.star$text, stringsAsFactors = FALSE)
names(t) <- c("doc_id", "text")

corpus <- Corpus(DataframeSource(t))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#layout(matrix(c(1, 2), nrow=2), heights=c(1, 4)) par(mar=rep(0, 4))
#plot.new() text(x=0.5, y=0.5, 'Frequent Words used in 1 star reviews')
#wordcloud(corpus, scale = c(3, 0.5), max.words = 50, min.freq = 5, random.order = , 
#         rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(8, "Dark2"))
rm(t)
review_dtm_tfidf <- DocumentTermMatrix(corpus, 
                                       control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.99)

tdm <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
tdm <- removeSparseTerms(tdm, 0.99)
m <- as.matrix(tdm)

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


