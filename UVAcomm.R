
# Author: Prabhat Rayapati
# Contact: pr2sn@virginia.edu

# Description: This purpose of this script is to create n-grams for UVA 
# commencement addresses from 1973 to 2016, available in the archives on 
# UVA's website. Additionally the script "babbles" a few paragraphs based on the
# ngrams created, and a graph of readability of these speeches over the years. 

# inspired by work in the class: DS6559 Text as Data by Michele Claibourn at UVA

# clear global environment variables
rm(list=ls())

# packages to install and load-------------------------------------------------

# install.packages("rvest")
library(rvest)
# install.packages("ngram")
library(ngram)
# install.packages("XML")
library(XML)
# install.packages("stringr")
library(stringr)
# install.packages("koRpus")
library(koRpus)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("wordcloud")
library(wordcloud)
# install.packages("tm")
library(tm)
# install.packages("RColorBrewer")
library(RColorBrewer)


# set/create a directory--------------------------------------------------------
setwd("~/Desktop")
# creating/changing directory to dir with UVA commencement speeches data 
if (!file.exists("UVAcomm")) {
  dir.create("UVAcomm")
}
setwd("~/Desktop/UVAcomm")



# Data -------------------------------------------------------------------------
# Scraping for speeches from url: https://majorevents.virginia.edu/finals/archive

# getting info from source page, its new so comes with UTF-8 character encoding
source.page <- read_html("https://majorevents.virginia.edu/finals/archive")

# Get URLs for the speeches, I used selectorgadget by hadley wickham
url1 <- source.page %>% 
  html_nodes(".collapse-text-text a") %>%  
  html_attr("href") 
# the first 14 nodes arent necessary
url1<-url1[-c(1:14)]

# few/most of them have only the file source of the page, lets add the url part
url1[c(4:32)]<-paste0("https://majorevents.virginia.edu/finals/",url1[c(4:32)])
url1[3]<-paste0("https://majorevents.virginia.edu/",url1[3])


# Get names for the speakers, I used selectorgadget by hadley wickham
speakers <- source.page %>% 
  html_nodes(".collapse-text-text a") %>%  
  html_text()

speakers<-speakers[-c(1:14)]
speakers<-gsub( ",.*$", "", speakers)


# dataframe of the commencement speeches and their dates
commYear<-as.numeric(str_extract(url1,"[[:digit:]]+")) 
commYear[1:2]<-c(2016,2016)

commSpeech<-data.frame(Year=commYear,speaker=speakers,url1,stringsAsFactors = FALSE)


# a list of nodes on each page for the speech text 
nodes<-c("#node-page-341 .even","#node-page-336 .even",
"#node-page-331 .even","#node-page-326 .even","#node-page-321 .even",
"#node-page-316 .even","#node-page-311 .even","#node-page-306 .even",
"#node-page-301 .even","#node-page-296 .even","#node-page-291 .even",
"#node-page-286 .even","#node-page-281 .even","#node-page-276 .even",
"#node-page-261 .even","#node-page-256 .even","#node-page-251 .even",
"#node-page-241 .even","#node-page-236 .even","#node-page-231 .even",
"#node-page-226 .even","#node-page-221 .even","#node-page-216 .even",
"#node-page-536 .even","#node-page-211 .even","#node-page-531 .even",
"#node-page-271 .even","#node-page-266 .even","#node-page-526 .even",
"#node-page-651 .even","#node-page-681 .even","#node-page-676 .even")
nodes<-rev(nodes)

# read the speeches in from the respective webpages
# save them each in text file named <speaker>-<year>.txt
for(i in 1:length(url1)){
  text <- read_html(commSpeech$url1[i]) %>% 
    html_nodes(nodes[i]) %>% 
    html_text() 
  filename <- paste0(commSpeech$speaker[i], "-", commSpeech$Year[i], ".txt")
  sink(file = filename) %>% 
    cat(text) 
  sink()
}

# list all the files in the directory
speechFiles<-list.files(getwd())

# the first four lines of each speech is just information of speaker
# lets remove them
speech<-rep(0,length(commYear))
for(i in 1:length(commYear)){
speech[i]<-paste(readLines(speechFiles[i])[-c(1:4)],collapse = "")
}
speechFiles
# some additional cleaning
speech<-gsub("\\t","",speech)
speech <-gsub("\"","", speech)

# n-gram --------------------------------------------------------------------- 

commSpeechAll<-paste(speech,collapse = "")

# bigram model
ng.speech<-ngram(commSpeechAll,n=2)
# get.ngrams(ng.speech)
# make the computer babble using the bigram language model we created
# change the seed to make new babble 
babble(ng.speech,100,seed = 110)

# readdability of speeches over years -----------------------------------------
# lets add the age and the grade to the commSpeech dataframe
# we will use the Flesch–Kincaid readability metric

for(i in 1:length(commYear)){
  tagged.speech <- tokenize(speech[i], format="obj", lang="en")
  hyphen.speech <- hyphen(tagged.speech)
  flesch_kincaid.speech <- readability(tagged.speech, hyphen=hyphen.speech, index="Flesch.Kincaid")
  commSpeech$Grade[i]<-flesch_kincaid.speech@Flesch.Kincaid$grade
  commSpeech$age[i]<-flesch_kincaid.speech@Flesch.Kincaid$age
}

# plotting ---------------------------------------------------------------------
p <- ggplot(commSpeech[-32,c(1,4)], aes(x = commSpeech$Year[-32], y = commSpeech$Grade[-32]))
p + geom_point(size=3, colour="purple")+
  labs(title = "Readability of Commencement Speeches", y = "Readability (grade level)", x = "Year")
  theme(panel.grid.minor = element_blank())
ggsave(file="~/Desktop/readability.png")

# WordCloud --------------------------------------------------------------------
speechAll<-str_split(commSpeechAll," ")
speechAll<-unlist(speechAll)
speechAll<-gsub("[[:punct:]]","",speechAll)
speechAll<-trimws(speechAll)
sum(speechAll=="the")

speechAll<-removeWords(speechAll,stopwords("en"))
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
speechAll = stringr::str_replace_all(speechAll, stopwords_regex, '')
speechAll<-paste(speechAll,collapse = " ")

speechAll <- Corpus(VectorSource(speechAll))
dtm <- TermDocumentMatrix(speechAll)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200,colors=brewer.pal(8, "Dark2"))

