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
library(rpart)

link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_1.csv')
mbti <- read.csv(text = link)
mbti <- mbti %>% 
  mutate(posts = strsplit(as.character(posts), "[|||]")) %>% 
  unnest(posts)
mbti <- mbti[!(is.na(mbti$posts) | mbti$posts==""), ]
mbti$isURL <- grepl('$http',mbti$posts,TRUE)
mbti <- subset(mbti,mbti$isURL == 0)
mbti$type <- strtrim(mbti$type,1)

entQ <- corpus(subset(mbti,mbti$type=="E"),text_field = "posts")
intQ <- corpus(subset(mbti,mbti$type=="I"),text_field = "posts")
entDF <- tidy(entQ)
intDF <- tidy(intQ)

entDF$ntok <- ntoken(entDF$text)
entDF$ntyp <- ntype(entDF$text)
entDF$uniq <- 100.0 / (entDF$ntok/entDF$ntyp)
entDF$avgWPS <- entDF$ntok / nsentence(entDF$text)
entDF <- tibble::rowid_to_column(entDF, "ID")

intDF$ntok <- ntoken(intDF$text)
intDF$ntyp <- ntype(intDF$text)
intDF$uniq <- 100.0 / (intDF$ntok/intDF$ntyp)
intDF$avgWPS <- intDF$ntok / nsentence(intDF$text)
intDF <- tibble::rowid_to_column(intDF, "ID")

#sentiment analysis
entS <- dfm(entQ, dictionary = data_dictionary_LSD2015)
entS <- tidy(entS)
entPlot <- cbind(entS) # make a copy to use later
entS <- mutate(entS, ID = as.numeric(rownames(entS)))
entN <- subset(entS,entS$term=="negative")
entN$document <- as.integer(entN$document)

intS <- dfm(intQ, dictionary = data_dictionary_LSD2015)
intS <- tidy(intS)
intPlot <- cbind(intS) # make a copy to use later
intS <- mutate(intS, ID = as.numeric(rownames(intS)))
intN <- subset(intS,intS$term=="negative")
intN$document <- as.integer(intN$document)

link2 <- getURL("https://raw.githubusercontent.com/SamCD/Data607Final/master/Starbucks.csv")
samples <- read.csv(text = link2,header = FALSE)
key <- 'AIzaSyAq8j_r8PZJE2rtNTGjE4HbfMZbm7Njbxc'
resDF <- data.frame(rating = as.numeric(character()),text = character())
samples <- samples[sample(nrow(samples)),]
for (row in 1:10) {
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
#resS <- tidy(resS)
resPlot <- cbind(resS) #make a copy to use later
resDF <- data.frame(resDF)

#mbtiDF <- data.frame(rbind(entDF,intDF))
#set.seed(1234)
#ind <- sample(2, nrow(mbtiDF), replace=TRUE, prob=c(0.67, 0.33))
#mbtiDF$ind <- ind
#tested <- subset(mbtiDF$ind,mbtiDF$ind!=1)
#model= glm(type ~ uniq+avgWPS, mbtiDF)
#p = predict(model, mbtiDF)
#plot(p - tested)

pie(table(entPlot$term))
pie(table(intPlot$term))

#mbtiPlotted <- ggplot(mbtiPlot, aes(x = term, fill  = )) +
#  geom_bar(position = "fill") +
#  labs(x = 'MBTI Type') +
#  scale_fill_discrete(name="Word Sentiment")

#resPlotted = ggplot(resPlot, mapping = aes(x = rating, fill = term)) +
#  geom_bar(stat='count', position='fill', inherit.aes = FALSE ) +
#  labs(x = 'Rating Score') +
#  scale_fill_discrete(name="Word Sentiment")

entWC <- dfm(entQ, remove = stopwords("english"), remove_punct = TRUE)
intWC <- dfm(intQ, remove = stopwords("english"), remove_punct = TRUE)

set.seed(100)
textplot_wordcloud(entWC, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),
                   scale=c(4,.5),
                   max.words =40)
textplot_wordcloud(entWC, min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"),
                   scale=c(4,.5),
                   max.words =40)


