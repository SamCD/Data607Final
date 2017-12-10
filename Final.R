library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyr)
library(dplyr)
library(stringr)
library("tm")
library("googleway")
library(RCurl)
library("quanteda") 
library(tidytext)

link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_1.csv')
mbti <- read.csv(text = link)
mbti <- mbti %>% 
  mutate(posts = strsplit(as.character(posts), "[|||]")) %>% 
  unnest(posts)
mbti <- mbti[!(is.na(mbti$posts) | mbti$posts==""), ]
#head(mbti)
mbti$isURL <- grepl('$http',mbti$posts,TRUE)

#entp <- VCorpus(VectorSource(subset(mbti,mbti$type=='ENTP' | mbti$isURL==0)$posts))
entpQ <- corpus(subset(mbti,mbti$type=='ENTP' | mbti$isURL==0)$posts)
summary(entpQ)
entpDF <- tidy(entpQ)
add_column(entpDF,tok = ntoken(entpDF$text))
entpDF$ntok <- ntoken(entpDF$text)
entpDF$ntyp <- ntype(entpDF$text)
entpDF$uniq <- 100.0 / (entpDF$ntok/entpDF$ntyp)
entpDF$avgWPS <- entpDF$ntok / nsentence(entpDF$text)
entpDF <- data.frame(entpDF)

entpDFM <- dfm(entpQ,remove = c(stopwords("english"),"'"))
entpDF <- data.frame(tidy(entpDFM))
head(entpDF)

summary(entpQ)
entpQ[1]
ntoken(char_tolower(entpQ[1]))
ntype(char_tolower(entpQ[1]))
entpQ[["Uniqueness"]] <- 100.0 / (ntoken(char_tolower(texts(entpQ))) / ntype(char_tolower(texts(entpQ))))
docvars(entpQ)


#entp <- tm_map(entp, content_transformer(tolower))
entp <- tm_map(entp, removeNumbers)
entp <- tm_map(entp, removeWords, stopwords("english"))
entpDF <- data.frame(text=unlist(sapply(entp, `[`, "content")), stringsAsFactors=F)

link2 <- getURL("https://raw.githubusercontent.com/SamCD/Data607Final/master/Starbucks.csv")
samples <- read.csv(text = link2,header = FALSE)

resDF <- data.frame(rating = as.numeric(character()),text = character())
x <- 500
while(x>0) {
lat <- as.numeric(format(round(runif(1,-90,90), 3), nsmall = 3))
lon <- as.numeric(format(round(runif(1,-180,180), 3), nsmall = 3))
rad <- as.numeric(format(round(runif(1,1,5000), 0), nsmall = 0))
key <- 'AIzaSyAq8j_r8PZJE2rtNTGjE4HbfMZbm7Njbxc'
res <- google_places(location = c(lat, lon),
                     keyword = "Restaurant",
                     radius = rad,
                     key = key)
head(res)
for (i in res$results$place_id){
  revs <- google_place_details(i,key=key)$result$reviews[,c("rating","text")]
  resDF <- rbind(resDF,revs)
}
x <- x - 1
print(x)
}

head(resDF,100)