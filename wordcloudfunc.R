wordcloudfunc=function(tweethandle,n){
  nohandles <- str_replace_all(cleantweets(tweethandle), "@\\w+", "")
  wordCorpus <- Corpus(VectorSource(nohandles))
  wordCorpus <- tm_map(wordCorpus, removePunctuation)
  wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
  wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
  wordCorpus <- tm_map(wordCorpus, removeWords, c("amp", "2yo", "3yo", "4yo"))
  wordCorpus <- tm_map(wordCorpus, stripWhitespace)
  wordCorpus <- tm_map(wordCorpus, stemDocument)
  col=brewer.pal(6,"Dark2")
  
  y=wordcloud(wordCorpus, min.freq=n/30, scale=c(7,1),rot.per = 0.25,
              random.color=T, max.word=100, random.order=F,colors=col)
  return(y)
}