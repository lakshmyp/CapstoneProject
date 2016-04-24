library(shiny)
library(tm)
library(RWeka)
library(slam)
library(gdata)

profanity <- read.csv("profanity.csv",skip = 3, colClasses = "character")
profanity <- profanity[,2]
profanity <- gsub(",", "", profanity)


clean <- function(text) {
  samplecorp <- VCorpus(VectorSource(text))
  samplecorp <- tm_map(samplecorp, removeNumbers)
  samplecorp <- tm_map(samplecorp, content_transformer(tolower))
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, "will not", x)), "won't")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " not", x)), "n't")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " will", x)), "'ll")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " are", x)), "'re")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " have", x)), "'ve")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " am", x)), "'m")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " ", x)), "-")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, "it is", x)), "it's")
  samplecorp <- tm_map(samplecorp, content_transformer(function(x, pattern) gsub(pattern, " he is", x)), " he's")
  samplecorp <- tm_map(samplecorp,content_transformer(function(x, pattern) gsub(pattern, " she is", x)), " she's")
  samplecorp <- tm_map(samplecorp, removePunctuation)
  samplecorp <- tm_map(samplecorp, stripWhitespace)
  samplecorp <- tm_map(samplecorp, PlainTextDocument)
  samplecorp <- tm_map(samplecorp, removeWords, profanity)
  text <- samplecorp[[1]]$content
  text_unlisted <- unlist(strsplit(text, " "))
  text_unlisted
}


predict <- function(textun){
  n <- length(textun)
  
  tab <- readRDS(file ="onwdf.rda")
  top5words <- tab$phrase[1:5]
  
  if (n>2){
    result <- predictbig(textun, n)
  }
  
  else{
    if (n == 2) result <- predict2(textun)
    else if (n == 1) result <- predict1(textun)
    else  result <- NULL
  }
  
  if(length(result) <5){
    result <- c(result, top5words)
  }
  
  head(result)
  
}

predictbig <- function(text, n){
 
  tab1 <- readRDS(file ="fowdf.rda")
  tab2 <-readRDS(file ="thwdf.rda")
  tab3 <- readRDS(file ="twwdf.rda")
  
  result4 <- tab1$word4[ 
    (startsWith(tab1$word1, text[n-2])) & 
      (startsWith(tab1$word2,text[n-1]))& 
      (startsWith(tab1$word3, text[n]))]
  
  result3 <- tab2$word3[
    (startsWith(tab2$word1,text[n-1])) 
    & (startsWith(tab2$word2,text[n])) ]
  
  result2 <- tab3$word2[ startsWith(tab3$word1, text[n])]
  result <- c(result4, result3, result2)
  result <- unique(result)
  result
}

predict2 <- function(text, n=2){
 
  tab2 <-readRDS(file ="thwdf.rda")
  tab3 <- readRDS(file ="twwdf.rda")
  result3 <- tab2$word3[ (startsWith(tab2$word1,text[n-1])) & (startsWith(tab2$word2,text[n])) ]
  result2 <- tab3$word2[startsWith(tab3$word1, text[n])]
  result <- c(result3, result2)
  result <- unique(result)
  result
}


predict1 <- function(text, n =1){
  library(gdata)
  tab3 <- readRDS(file ="twwdf.rda")
  result2 <- tab3$word2[ startsWith(tab3$word1, text)]
  result <- unique(result2)
  result
}



shinyServer(
  function(input,output){
    

   my_data <- reactive({
     
     if(input$goButton == 0) {return()}
     
     isolate({
       input$goButton
       predict(clean(input$text1))
      })
   })
   
   output$oid1 <- renderPrint({my_data()[1]})
   output$oid2 <- renderPrint({my_data()[2:5]})

  }
)
    


  
