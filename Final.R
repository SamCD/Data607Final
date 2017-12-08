library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyr)
library(dplyr)
library(stringr)
library("tm")
library("googleway")

link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_1.csv')
mbti_1 <- read.csv(text = link)
link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_2.csv')
mbti_2 <- read.csv(text = link)
link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_3.csv')
mbti_3 <- read.csv(text = link)
link <- getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/mbti_4.csv')
mbti_4 <- read.csv(text = link)

mbti <- rbind(mbti_1,mbti_2,mbti_3,mbti_4)

mbti <- mbti %>% 
  mutate(posts = strsplit(as.character(posts), "[|||]")) %>% 
  unnest(posts)
mbti$isURL <- grepl('$http',mbti$posts,TRUE)
entp <- VCorpus(VectorSource(subset(mbti,mbti$type=='ENTP' | mbti$isURL==0)$posts))
entp <- tm_map(entp, content_transformer(tolower))
entp <- tm_map(entp, removeNumbers)
entp <- tm_map(entp, removeWords, stopwords("english"))
entpDF <- data.frame(text=unlist(sapply(entp, `[`, "content")), stringsAsFactors=F)

key <- 'AIzaSyAq8j_r8PZJE2rtNTGjE4HbfMZbm7Njbxc'
res <- google_places(location = c(-37.918, 144.968),
                     keyword = "Restaurant",
                     radius = 5000,
                     key = key)
resDF <- google_place_details(head(res$results$placeid,1))
for (i in res$results$placeid){
  x <- google_place_details(i)
  resDF <- rbind(resDF,x)
}