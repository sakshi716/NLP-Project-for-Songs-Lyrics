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
# covert year & peak to numeric data type
prince$year <- sapply(prince$year, as.numeric)
prince$peak <- sapply(prince$peak, as.numeric)

str(prince[139, ]$lyrics, nchar.max = 300)
summary(prince)

#Create decade column
prince <- prince %>% mutate(decade = 
                              ifelse(prince$year %in% 1978:1979, "1970s", 
                              ifelse(prince$year %in% 1980:1989, "1980s", 
                              ifelse(prince$year %in% 1990:1999, "1990s", 
                              ifelse(prince$year %in% 2000:2009, "2000s", 
                              ifelse(prince$year %in% 2010:2015, "2010s", 
                              "NA"))))))

#create the chart level column
prince <- prince %>%
  mutate(chart_level = 
           ifelse(prince$peak %in% 1:10, "Top 10", 
                  ifelse(prince$peak %in% 11:100, "Top 100", "Uncharted")))

#create binary field called charted showing if a song hit the charts at all
prince <- prince %>%
  mutate(charted = 
           ifelse(prince$peak %in% 1:100, "Charted", "Uncharted"))

#save the new dataset to .csv for use in later tutorials
write.csv(prince, file = "prince_new.csv")

#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

#Plotting released songs by decade and chartered
prince %>%
  filter(decade != "NA") %>%
  group_by(decade, charted) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = charted), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Released Songs") +
  labs(x = NULL, y = "Song Count")

#Plotting released songs by decade and chart level
charted_songs_over_time <- prince %>% filter(peak > 0) %>% group_by(decade, chart_level) %>% summarise(number_of_songs = n())
  
charted_songs_over_time %>% ggplot() + 
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Charted Songs") +
  labs(x = NULL, y = "Song Count")

#Overall songs plot
prince %>%
  group_by(decade, chart_level) %>%
  summarise(number_of_songs = n()) %>%
  ggplot() +
  geom_bar(aes(x = decade, y = number_of_songs, 
               fill = chart_level), stat = "identity")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("All Songs in Data")

#quick peek of the songs that hit No. 1 on the charts
library(knitr) # for dynamic reporting
install.packages("kableExtra")
library(kableExtra) # create a nicely formated HTML table
install.packages("formattable")
library(formattable) # for the color_tile function
prince %>%
  filter(peak == "1") %>%
  select(year, song, peak) %>%
  arrange(year) %>%
  mutate(year = color_tile("lightblue", "lightgreen")(year)) %>%
  mutate(peak = color_tile("lightgreen", "lightgreen")(peak)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Prince's No. 1 Songs") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

#TEXT MINING
install.packages("tidytext")
library("tidytext")
#Step 1: Removing superfluous words
undesirable_words <- c("prince", "chorus", "repeat", "lyrics", 
                       "theres", "bridge", "fe0f", "yeah", "baby", 
                       "alright", "wanna", "gonna", "chorus", "verse", 
                       "whoa", "gotta", "make", "miscellaneous", "2", 
                       "4", "ooh", "uurh", "pheromone", "poompoom", "3121", 
                       "matic", " ai ", " ca ", " la ", "hey", " na ", 
                       " da ", " uh ", " tin ", "  ll", "transcription",
                       "repeats")

#Step 2: Tokenization (break sentences into individual words)
prince %>% unnest_tokens("token_words", lyrics)

#Step 3: Remove stop words
#head(sample(stop_words$word, 15), 15)
#unnest and remove stop, undesirable and short words
prince_words_filtered <- prince %>%
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words) %>%
  distinct() %>%
  filter(!word %in% undesirable_words) %>%
  filter(nchar(word) > 3)

class(prince_words_filtered)
glimpse(prince_words_filtered)

prince_words_filtered %>% 
  filter(word == "race") %>% 
  select(word, song, year, peak, decade, chart_level, charted) %>% 
  arrange() %>% 
  top_n(10, song) %>%
  mutate(song = color_tile("lightblue", "lightblue") (song)) %>%
  mutate(word = color_tile("lightgreen", "lightgreen") (word)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Tokenized Format Example") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)
  
#Word count per Song
full_word_count <- prince %>%
  unnest_tokens(word, lyrics) %>%
  group_by(song, chart_level) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

full_word_count[1:10,] %>%
  ungroup(num_words, song) %>%
  mutate(num_words = color_bar("lightblue")(num_words)) %>%
  mutate(song = color_tile("lightpink","lightpink")(song)) %>%
  kable("html", escape = FALSE, align = "c", caption = "Songs With Highest Word Count") %>%
  kable_styling(bootstrap_options = 
                  c("striped", "condensed", "bordered"), 
                full_width = FALSE)

full_word_count %>%
  ggplot() +
  geom_histogram(aes(x = num_words, fill = chart_level )) +
  ylab("Song Count") + 
  xlab("Word Count per Song") +
  ggtitle("Word Count Distribution") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.grid.minor.y = element_blank())
