cleantweets=function(tweetname){
  
  clean_tweet = gsub("&amp", "", tweetname)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet)
  clean_tweet <- str_replace_all(clean_tweet," "," ")
  clean_tweet= gsub("http[^[:space:]]*", "", clean_tweet)
  clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
  clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
  clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
  clean_tweet
}