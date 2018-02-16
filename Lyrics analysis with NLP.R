#use rvest package to scrape the content from the web
#Corpus and Document Term Matrix using the tm text mining package and then use the tm_map() function to do the cleaning



#load important libraries to be used
library("dplyr")  #data manipulation
library("ggplot2")  #data visualization
library("tidytext")  #text mining
library("wordcloud") #creative visualization
library("gridExtra") #viewing multiple plots together

prince_orig <- read.csv(file.choose(), stringsAsFactors = FALSE, na.strings = "")
names(prince_orig)

prince <- prince_orig %>% select(lyrics = text, song, year, album, peak, us_pop = US.Pop, us_rnb = US.R.B)
glimpse(prince[139,])
dim(prince)
str(prince$lyrics[139], nchar.max =300)

#Data Conditioning
fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("'s", "", doc)
  return(doc)
}

#fix (expand) contractions
prince$lyrics <- sapply(prince$lyrics, fix.contractions)
# function to remove special characters (i..e anything but numbers and alphabets)
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
prince$lyrics <- sapply(prince$lyrics, removeSpecialChars)
# convert everything to lower case
prince$lyrics <- sapply(prince$lyrics, tolower)

str(prince[139, ]$lyrics, nchar.max = 300)
summary(prince)
