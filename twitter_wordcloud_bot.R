library(curl)
library(twitteR)
library(slam)
library(tm)
library(RColorBrewer)
library(wordcloud2)
library(webshot)
library(htmlwidgets)

#Insert Your Own keys here
consumer_key <- "consumerKey"
consumer_secret <- "consumerSecret"
access_token <- "accessToken"
access_secret <- "accessSecret"

#Set up connection to twitter API.
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
  
#Load list of Stop Words to be removed from the word cloud.
cwords <- read.table("commonwords.txt")
cwordsvec <- unlist(as.character(cwords$V1))

#Create a data frame of trends
#First argument to getTrends() is the location id from the twitter API, i only take the top 5 trends worldwide and top three from New York and London.
#You can play with these values as you wish.
trendsdf <- rbind(getTrends(1)[1:5,],getTrends(2459115)[1:3,],getTrends(44418)[1:3,])
trendsdf$name <- gsub("[^#[:alnum:][:space:]]*","",trendsdf$name)
trendslist <- trendsdf$name[!(trendsdf$name == "#" | trendsdf$name == "")]
  
#Now For each trend we get the tweets, and create the WordCloud.  
for(i in 1:11){
    
  string <- trendslist[i]
  tweets <- searchTwitteR(string,1000, resultType="recent")
  
  tweetsdf <- twListToDF(tweets)
  tweetsdf$text <- gsub("@\\w+ *","",tweetsdf$text)
  tweetsdf$text <- gsub("http\\w+ *","",tweetsdf$text)
  tweetsdf$text <- gsub("[^[:alnum:][:space:]]*","",tweetsdf$text)
    
  #Create Corpus 
  corpus <- Corpus(VectorSource(tweetsdf$text))
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeWords,c("amp",string,tolower(string),stopwords("English"),cwordsvec))
    
  dtm <- DocumentTermMatrix(corpus)
    
  freqdf <- data.frame(word=attr(sort(col_sums(dtm),decreasing = TRUE),"names"),
                         freq=sort(col_sums(dtm),decreasing = TRUE))
    
  File <- paste0(gsub(" ","",string),".png","")
    
  #Custom Palettes 
  pal1lightcol <- c("#EBE18C","#DC3F1C","#448D7A","#D8A027","#88A764")
  pal2mutretro <- c("#CCEAF4","#A9A9A9","#FF3B3D","#EFEFEF")
  pal3colorful <- c("#E44E40","#E9B000","#EB6E80","#018E97")
  pal4vibrantrad <- c("#6C3667","#88D317","#535353","#FFFFFF")
  pals <- list(pal1lightcol,pal2mutretro,pal3colorful,pal4vibrantrad)
  
  #Create Wordcloud
  #Wordcloud selects a random Color palette from the above palettes everytime it is called
  wcloud <- wordcloud2(freqdf, size=6, minSize = 4, color =rep_len(unlist(pals[sample(1:4,1)]), nrow(freqdf)),
                        backgroundColor = "black", ellipticity = .5) 
  
  #Since wordcloud2 creates a html widget, we take a screenshot of the html output to get an image  
  saveWidget(wcloud,"tmp.html",selfcontained = FALSE)
  webshot("tmp.html",File, delay = 20,cliprect = "viewport")
  
  #Tweet the Wordcloud
  tweettext <- paste0("#wordcloud for live trends ",dQuote(string))
  tweet(tweettext, mediaPath=File)
 }
  
