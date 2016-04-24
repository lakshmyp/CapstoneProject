

##Importing Data

news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8")
twitter <- readLines("final/en_US/en_US.twitter.txt",encoding = "UTF-8")
blogs <- readLines("final/en_US/en_US.blogs.txt",encoding = "UTF-8")




# save the data to files

save(news, file="news.RData")
save(twitter, file="twitter.RData")
save(blogs, file="blogs.RData")


## Sample the data

set.seed(1234)

sample_news <- sample(news, 35000)
sample_twitter <- sample(twitter, 25000)
sample_blogs <- sample(blogs, 25000)


## Save Sample data

samples <- c(sample_news, sample_twitter, sample_blogs)

#samples2 <- paste(sample_news, sample_twitter, sample_blogs, collapse = " ")

## Remove Special characters
samples <- iconv(samples, "UTF-8", "ASCII", sub ="")

#samples2 <- iconv(samples2, "UTF-8", "ASCII", sub ="")

save(samples, file = "samples.RData")


library(tm)
library(RWeka)
library(slam)

## Profanity filtering

#List of swear words found online 

load("samples.RData")

profanity <- read.csv("profanity.csv",skip = 3, colClasses = "character")
profanity <- profanity[,2]
profanity <- gsub(",", "", profanity)



corpustext <- VCorpus(VectorSource(samples))

corpustext <- tm_map(corpustext, removeNumbers)
corpustext <- tm_map(corpustext, content_transformer(tolower))


toNothing <- content_transformer(function(x, pattern) gsub(pattern, "", x))
corpustext <- tm_map(corpustext, toNothing, "^rt")


toYou <- content_transformer(function(x, pattern) gsub(pattern, " you ", x))
corpustext <- tm_map(corpustext, toYou, " u ")

corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, "will not", x)), "won't")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " not", x)), "n't")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " will", x)), "'ll")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " are", x)), "'re")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " have", x)), "'ve")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " am", x)), "'m")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " ", x)), "-")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, "it is", x)), "it's")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " he is", x)), " he's")
corpustext <- tm_map(corpustext, 
                     content_transformer(function(x, pattern) gsub(pattern, " she is", x)), " she's")


corpustext <- tm_map(corpustext, removePunctuation)
corpustext <- tm_map(corpustext, removeWords, profanity)
corpustext <- tm_map(corpustext, stripWhitespace)
corpustext <- tm_map(corpustext, PlainTextDocument)


saveRDS(corpustext, "corpustext.Rda")
corpustext <- readRDS("corpustext.Rda")


## Stemming

corpstem <- tm_map(corpustext, stemDocument)
corpstem <- tm_map(corpstem, stripWhitespace)
corpstem <- tm_map(corpstem, PlainTextDocument)

saveRDS(corpustext, "corpstem.Rda")
corpstem <- readRDS("corpstem.Rda")


## Creating the matrix and it's transpose
dtm <- DocumentTermMatrix(corpstem, 
                          control = list(bounds = list(global = c(10,Inf)), 
                                         wordLengths = c(1,16)))



OnWDF <- sort(col_sums(dtm), decreasing = TRUE)
OnWDF <- as.data.frame(OnWDF)
OnWDF$phrase <- row.names(OnWDF)


saveRDS(OnWDF, file = "OnWDF.Rda")
OnWDF <- readRDS(file ="OnWDF.Rda")


## Tokenize
## N-grams 

ThreeWordTokens <- function(x) NGramTokenizer(x, Weka_control(min=3, max =3))
Threewords <- TermDocumentMatrix(corpstem, control = list(tokenize = ThreeWordTokens, 
                                                          bounds = list(global = c(2,Inf))))


findFreqTerms(Threewords, lowfreq = 50)
ThW <- rollup(Threewords, 2, na.rm = TRUE, FUN = sum)


TwoWordTokens <- function(x) NGramTokenizer(x, Weka_control(min=2, max =2))
Twowords <- TermDocumentMatrix(corpstem, control = list(tokenize = TwoWordTokens, 
                                                        bounds = list(global = c(3,Inf))))
findFreqTerms(Twowords, lowfreq = 50)
TwW <- rollup(Twowords, 2, na.rm = TRUE, FUN = sum)



FourWordTokens <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
Fourwords <- TermDocumentMatrix(corpstem, control = list(tokenize = FourWordTokens, 
                                                         bounds = list(global = c(2, Inf))))
findFreqTerms(Fourwords, lowfreq = 10)

FoW <- rollup(Fourwords, 2, na.rm = TRUE, FUN = sum)   


##  put all unigrams in a data table


## put all trigrams in a table 


ThWDF <- sort(row_sums(ThW), decreasing = TRUE)
ThWDF <- as.data.frame(ThWDF)
ThWDF$phrase <- row.names(ThWDF)


saveRDS(ThWDF, file = "ThWDF.Rda")
ThWDF <- readRDS(file ="ThWDF.Rda")


for(i in 1:nrow(ThWDF)){
  elems <- unlist(strsplit(ThWDF$phrase[i], " "))
  ThWDF$word1[i] <- elems[1]
  ThWDF$word2[i] <- elems[2]
  ThWDF$word3[i] <- elems[3]
   
}

saveRDS(ThWDF, file = "ThWDF.Rda")

## put all bigrams in a table

TwWDF <- sort(row_sums(TwW), decreasing = TRUE)
TwWDF <- as.data.frame(TwWDF)
TwWDF$phrase <- row.names(TwWDF)


saveRDS(TwWDF, file = "TwWDF.Rda")
TwWDF <- readRDS(file ="TwWDF.Rda")


for(i in 1:nrow(TwWDF)){
  elems <- unlist(strsplit(TwWDF$phrase[i], " "))
  TwWDF$word1[i] <- elems[1]
  TwWDF$word2[i] <- elems[2]

}

saveRDS(TwWDF, file = "TwWDF.Rda")

## 4 word 

FoWDF <- sort(row_sums(FoW), decreasing = TRUE)
FoWDF <- as.data.frame(FoWDF)
FoWDF$phrase <- row.names(FoWDF)


saveRDS(FoWDF, file = "FoWDF.Rda")
FoWDF <- readRDS(file ="FoWDF.Rda")


for(i in 1:nrow(FoWDF)){
  elems <- unlist(strsplit(FoWDF$phrase[i], " "))
  FoWDF$word1[i] <- elems[1]
  FoWDF$word2[i] <- elems[2]
  FoWDF$word3[i] <- elems[3]
  FoWDF$word4[i] <- elems[4]
}

saveRDS(FoWDF, file = "FoWDF.Rda")

## Prediction

sampletext <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"


### Clean text 
##text into format of corpus
samplecorp <- VCorpus(VectorSource(sampletext))
samplecorp <- tm_map(samplecorp, removeNumbers)
samplecorp <- tm_map(samplecorp, removePunctuation)
samplecorp <- tm_map(samplecorp, content_transformer(tolower))
#samplecorp <- tm_map(samplecorp, removeWords, profanity)
#samplecorp <- tm_map(samplecorp, stemDocument)
samplecorp <- tm_map(samplecorp, stripWhitespace)
samplecorp <- tm_map(samplecorp, PlainTextDocument)


## Split words

sampletext <- samplecorp[[1]]$content
#sampletext <- paste("<s>", sampletext, "</s>")

#Word count is over?
sample_unlisted <- unlist(strsplit(sampletext, " "))
n <- length(sample_unlisted)


##Last 3 words

if (n>2){
result4 <- FoWDF$word4[ (sample_unlisted[n-2]==FoWDF$word1) & (sample_unlisted[n-1]==FoWDF$word2)& (sample_unlisted[n]==FoWDF$word3)]
result3 <- ThWDF$word3[ (sample_unlisted[n-1]==ThWDF$word1) & (sample_unlisted[n]==ThWDF$word2)]
result2 <- TwWDF$word2[ (sample_unlisted[n]==TwWDF$word1)]
}

if (n = 2){
  result3 <- ThWDF$word3[ (sample_unlisted[n-1]==ThWDF$word1) & (sample_unlisted[n]==ThWDF$word2)]
  result2 <- TwWDF$word2[ (sample_unlisted[n-1]==TwWDF$word1)]
}

if (n =1){
  result2 <- TwWDF$word2[ (sample_unlisted[n-1]==TwWDF$word1)]
}


result <- c(result4, result3, result2)
result <- unique(result)
head(result)



## Last 2 words

##Last word 

## Search last 3 words in 4-grams' first 3 words 





