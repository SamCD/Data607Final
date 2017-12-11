library(ggplot2) # Data visualization
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library("tm")
library("googleway")
library(RCurl)
library("quanteda") 
library(tidytext)
library(plyr)

link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_1.csv')
mbti <- read.csv(text = link)
mbti <- mbti %>% 
  mutate(posts = strsplit(as.character(posts), "[|||]")) %>% 
  unnest(posts)
mbti <- mbti[!(is.na(mbti$posts) | mbti$posts==""), ]
mbti$isURL <- grepl('$http',mbti$posts,TRUE)
mbti <- subset(mbti,mbti$isURL == 0)

mbtiQ <- corpus(mbti,text_field = "posts")
mbtiDF <- tidy(mbtiQ)
mbtiDF$ntok <- ntoken(mbtiDF$text)
mbtiDF$ntyp <- ntype(mbtiDF$text)
mbtiDF$uniq <- 100.0 / (mbtiDF$ntok/mbtiDF$ntyp)
mbtiDF$avgWPS <- mbtiDF$ntok / nsentence(mbtiDF$text)
mbtiDF <- tibble::rowid_to_column(mbti, "ID")

#sentiment analysis
mbtiS <- dfm(mbtiQ, dictionary = data_dictionary_LSD2015)
mbtiS <- tidy(mbtiS)
mbtiPlot <- cbind(mbtiS) # make a copy to use later
mbtiS <- mutate(mbtiS, ID = as.numeric(rownames(mbtiS)))
mbtiN <- subset(mbtiS,mbtiS$term=="negative")
mbtiN$document <- as.integer(mbtiN$document)
mbtiDF <- inner_join(mbtiDF,mbtiN,by = c("ID" = "document"))[c("ID","type","posts","count","uniq","avgWPS")]
names(mbtiDF)[names(mbtiDF) == 'count'] <- 'negative'
mbtiP <- subset(mbtiS,mbtiS$term=="positive")
mbtiP$document <- as.integer(mbtiP$document)
mbtiDF <- inner_join(mbtiDF,mbtiP,by = c("ID" = "document"))[c("ID","type","posts","negative","count","uniq","avgWPS")]
names(mbtiDF)[names(mbtiDF) == 'count'] <- 'positive'
mbtiDF <- data.frame(mbtiDF)

link2 <- getURL("https://raw.githubusercontent.com/SamCD/Data607Final/master/Starbucks.csv")
samples <- read.csv(text = link2,header = FALSE)
key <- 'AIzaSyAq8j_r8PZJE2rtNTGjE4HbfMZbm7Njbxc'
resDF <- data.frame(rating = as.numeric(character()),text = character())
samples <- samples[sample(nrow(samples)),]
for (row in 1:nrow(head(samples),10)) {
  lat <- samples[row,2]
  lon <- samples[row,1]
  res <- google_places(location = c(lat, lon),
                       keyword = "Restaurant",
                       radius = 5000,
                       key = key)
  for (i in res$results$place_id){
    revs <- google_place_details(i,key=key)$result$reviews[,c("rating","text")]
    resDF <- rbind(resDF,revs)
  }
}

resQ <- corpus(resDF,text_field = "text")
resDF <- tidy(resQ)
resDF$ntok <- ntoken(resDF$text)
resDF$ntyp <- ntype(resDF$text)
resDF$uniq <- 100.0 / (resDF$ntok/resDF$ntyp)
resDF$avgWPS <- resDF$ntok / nsentence(resDF$text)
resDF <- tibble::rowid_to_column(resDF, "ID")
#sentiment analysis
resS <- dfm(resQ, dictionary = data_dictionary_LSD2015)
resS <- tidy(resS)
resPlot <- cbind(resS) #make a copy to use later
resS <- mutate(resS, ID = as.numeric(rownames(resS)))
resN <- subset(resS,resS$term=="negative")
resN$document <- as.integer(resN$document)
resDF <- inner_join(resDF,resN,by = c("ID" = "document"))[c("ID","rating","text","count")]
names(resDF)[names(resDF) == 'count'] <- 'negative'
resP <- subset(resS,resS$term=="positive")
resP$document <- as.integer(resP$document)
resDF <- inner_join(resDF,resP,by = c("ID" = "document"))[c("ID","rating","text","negative","count")]
names(resDF)[names(resDF) == 'count'] <- 'positive'
resDF <- data.frame(resDF)

count(mbtiDF,"type")
mbtiDF$isINFP <- (mbti$type=="INFP")

fit <- glm(isINFP~uniq+avgWPS+negative+positive,data=mbtiDF,family=binomial())
predict(fit, resDF,type="response")

mbtiPlotted = ggplot(mbtiPlot, mapping = aes(x = type, fill = term)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'MBTI Type') +
  scale_fill_discrete(name="Word Sentiment") +
  theme_few()

resPlotted = ggplot(resPlot, mapping = aes(x = rating, fill = term)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Rating Score') +
  scale_fill_discrete(name="Word Sentiment") +
  theme_few()

multiplot(mbtiPlotted,resPlotted)

#mbtiWC <- dfm(mbtiQ, remove = stopwords("english"), remove_punct = TRUE)
#set.seed(100)
#textplot_wordcloud(mbtiWC, min.freq = 6, random.order = FALSE,
#                   rot.per = .25, 
#                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
