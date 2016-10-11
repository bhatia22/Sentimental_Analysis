candidates=list("Hillary Clinton"="hillary+clinton","Donald Trump"="donald+trump","Gary Johnson"="gary+johnson","Jill Stein"="jill+stein")

library(shiny)
library(twitteR)
library(ROAuth)
library(RCurl)
library(stringr)
library(tm)
library(plyr)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )
source("cleantweets.R")
source("wordcloudfunc.R")
source("multiplot.R")
# -------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style("body {background-image: url(http://wallpoper.com/images/00/36/45/34/birds-twitter_00364534.jpg); }")
  ),
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #ad1d28;
                    }
                    
                    "))
    ),
  
  headerPanel("Sentiment Analysis Using Twitter") ,
  sidebarPanel(
    textInput("name3", "Enter the first name",value = "Hillary")
    ,
    textInput("name4", "Enter the second name",value = "Donald")
    ,
    sliderInput("samplesize","Number of Tweets", min=1,max=2400,value = 10)
    ,
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 2, end = Sys.Date()
    ),
    actionButton(inputId = "start", label="Analyze!")
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Sentiment Plot", plotOutput("distplot")),
      tabPanel("Word Cloud", plotOutput("wordhillary"),plotOutput("wordtrump"))
      
    )
  )
)




# -------------------------------------------------------------------
server <- function(input, output) {
  
  
  consumer_key = "hQfZMA6uD0wmWrhPpXCCdG8Gb"
  consumer_secret = "OGtN0cLq0Nb0JO8RoWPNXqkOaaU8WZY3evMdxeZmXegXf079qE"
  token_secret = "PsE6WOrXwrfNsY1NlFIxEfkf2wvhep7Cxa4fuCSXb1zqS"
  access_token = "782594344532541440-UOcNYPDsLYeFSS3aGutPj5f3dtHt68m"
  
 
 
  authenticate <- OAuthFactory$new(consumerKey = consumer_key,
                                   consumerSecret = consumer_secret,
                                   requestURL="https://api.twitter.com/oauth/request_token",
                                   accessURL="https://api.twitter.com/oauth/access_token",
                                   authURL="https://api.twitter.com/oauth/authorize")
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, token_secret)
  token <- get("oauth_token", twitteR:::oauth_cache) #Save the credentials info
  token$cache()

  save(authenticate, file="twitter authentication.Rdata")
  load("twitter authentication.Rdata")
  
  observeEvent(input$start,{
    
   tweet1 <- searchTwitter(input$name3, n=input$samplesize, lang='en', since=input$date, until=input$date2)
   tweet2 <- searchTwitter(input$name4, n=input$samplesize, lang='en', since=input$date, until=input$date2)
  
   
  
  tweet1_txt <- sapply(tweet1, function(x) x$getText())
  tweet2_txt <- sapply(tweet2, function(x) x$getText())
  NumTweets <- c(length(tweet1_txt), length(tweet2_txt))
  tweets <- c(tweet1_txt, tweet2_txt)
  tweets <- sapply(tweets,function(x) iconv(x, "latin1", "ASCII", sub=""))
  writeLines(tweet1_txt,"tweet1.txt")
  writeLines(tweet2_txt,"tweet2.txt")
  paste(tweets, collapse=" ")
  tweets=cleantweets(tweets)
  
  output$wordhillary <-renderPlot(wordcloudfunc(tweet1_txt))
  output$wordtrump<-renderPlot(wordcloudfunc(tweet2_txt))
  

  mySentiment <- get_nrc_sentiment(cleantweets(tweet1_txt))
  head(mySentiment)
  tweets1 <- cbind(cleantweets(tweet1_txt), mySentiment)
  tweets1
  sentimentTotals <- data.frame(colSums(tweets1[,c(2:11)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL

  p1=ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score on Tweets")
  
  mySentiment <- get_nrc_sentiment(cleantweets(tweet2_txt))
  head(mySentiment)
  tweets2 <- cbind(cleantweets(tweet2_txt), mySentiment)
  tweets2
  sentimentTotals <- data.frame(colSums(tweets2[,c(2:11)]))
  names(sentimentTotals) <- "count"
  sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
  rownames(sentimentTotals) <- NULL
  
  p2=ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
    geom_bar(aes(fill = sentiment), stat = "identity") +
    theme(legend.position = "none") +
    xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score on Donald's Tweets")
  
  graph1=multiplot(p1,p2,cols=2)
  
 

output$distplot <- renderPlot({multiplot(p1,p2,cols=2)})})

}

shinyApp(ui = ui, server = server)
