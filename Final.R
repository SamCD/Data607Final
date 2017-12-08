library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyr)
library(dplyr)
library(stringr)
library("tm")
library("googleway")

INFJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/INFJ.csv'))
ENTP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ENTP.csv'))
INTP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/INTP.csv'))
INTJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/INTJ.csv'))
ENTJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ENTJ.csv'))
ENFJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ENFJ.csv'))
INFP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/INFP.csv'))
ENFP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ENFP.csv'))
ISFP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ISFP.csv'))
ISTP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ISTP.csv'))
ISFJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ISFJ.csv'))
ISTJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ISTJ.csv'))
ESTP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ESTP.csv'))
ESFP <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ESFP.csv'))
ESTJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ESTJ.csv'))
ESFJ <- read.csv(getURL('https://raw.githubusercontent.com/SamCD/Data607Final/master/ESFJ.csv'))

mbti <- rbind(INFJ, ENTP, INTP, INTJ, ENTJ, ENFJ, INFP, ENFP, ISFP, ISTP, ISFJ, ISTJ, ESTP, ESFP, ESTJ, ESFJ)

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