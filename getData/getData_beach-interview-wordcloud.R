##-------------- 
# **************************************************************************** #
# ***************                Project Overview              *************** #
# **************************************************************************** #

# Author:      Dominick Lemas 
# Date:        March 27, 2019 
# IRB:
# Description: wordcloud data analysis for BEACH Interview project 
# Data: C:\Users\djlemas\Dropbox (UFL)\02_Projects\BEACH_INTERVIEW\03_Nvivo_Analysis\wordcloud
# Obj: Format data and basic analysis.

# **************************************************************************** #
# ***************                Directory Variables           *************** #
# **************************************************************************** #

# Directory Locations
work.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\03_Nvivo_Analysis\\wordcloud\\");work.dir
data.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\03_Nvivo_Analysis\\wordcloud\\");data.dir
out.dir=paste0(Sys.getenv("USERPROFILE"),"\\Dropbox (UFL)\\02_Projects\\BEACH_INTERVIEW\\03_Nvivo_Analysis\\wordcloud\\");out.dir

# Set Working Directory
setwd(work.dir)
list.files()

# **************************************************************************** #
# ***************                Library                       *************** #
# **************************************************************************** #
# install
# install.packages(c("tm","SnowballC","wordcloud","RColorBrewer"))  

# Load
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(dplyr)
library(readxl)
library(wordcloud2)

# **************************************************************************** #
# ***************  BIS Participants-WFQ.xlsx                                              
# **************************************************************************** # 

# file parameters
n_max=10000
bf.file.name="BIS Participants-WFQ.xlsx";bf.file.name


#Read Data
bf.group=read_xlsx(paste0(data.dir,bf.file.name), sheet = "Sheet1", range = NULL, col_names = TRUE,
                      col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                      guess_max = min(1000, n_max));bf.group


# **************************************************************************** #
# ***************  PRG Participants-WFQ.xlsx                                              
# **************************************************************************** # 

# file parameters
n_max=10000
prego.file.name="PRG Participants-WFQ.xlsx";prego.file.name


#Read Data
prego.group=read_xlsx(paste0(data.dir,prego.file.name), sheet = "Sheet1", range = NULL, col_names = TRUE,
                   col_types = NULL, na = "NA", trim_ws = TRUE, skip = 0, n_max = Inf,
                   guess_max = min(1000, n_max));prego.group


# **************************************************************************** #
# ***************  General data formatting                                             
# **************************************************************************** # 

# breastfeeding group
bf.group.trim=bf.group%>%
  select(Word,Count)%>%
  rename_all(tolower)
  bf.group.trim$group=c("bf")
  bf.group.trim$color=c("blue")

# pregnant group
preg.group.trim=prego.group%>%
    select(Word,Count)%>%
    rename_all(tolower)
   preg.group.trim$group=c("preg")
   preg.group.trim$color=c("orange")
   
  
# merge data
cloud=bind_rows(list(bf.group.trim, preg.group.trim))

# what is the ordering of redcap_events
cloud$group=as.factor(cloud$group)

# how many words overlap in both groups
length(intersect(cloud$word[cloud$group=="bf"],cloud$word[cloud$group=="preg"]))
word.overlap=unique(intersect(cloud$word[cloud$group=="bf"],cloud$word[cloud$group=="preg"]))

#how many words total
length(unique(cloud$word))

# ready for word cloud analysis
wordcloud(words = cloud$word, freq = cloud$count, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per=0.35,
          scale = c(3,0.5), random.color = FALSE,
          colors=brewer.pal(8, "Dark2")[factor(cloud$group)])

# wordcloud for unique words in each group
df=cloud%>%
  filter(!word %in% word.overlap)
word.overlap=unique(intersect(df$word[df$group=="bf"],df$word[df$group=="preg"]))
# ready for word cloud analysis
wordcloud(words = df$word, freq = df$count, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per=0.35,
          scale = c(3,0.5), random.color = FALSE,
          colors=brewer.pal(8, "Dark2")[factor(df$group)])

# wordcloud2
head(demoFreq)
wordcloud2(demoFreq, size=1.6)
wordcloud2(cloud, size=0.5,color=cloud$color)
letterCloud(cloud, word = "R", wordSize = 1)
wordcloud2(cloud, size = 0.5, minRotation = -pi/2, maxRotation = -pi/2, color=cloud$color)



# example: https://stackoverflow.com/questions/50337874/color-based-on-groups-in-wordcloud-r
https://www.r-bloggers.com/the-wordcloud2-library/

# creat mock data
set.seed(1)
d1 <- data.frame(word=c(stringi::stri_rand_strings(20, 5)), freq=c(sample.int(20,10,100)))
d1$group <- "group1"
d1$word <- paste("g1" ,d1$word, sep = "")

d2 <- data.frame(word=c(stringi::stri_rand_strings(20, 5)),freq=c(sample.int(20,10,100)))
d2$group <- "group2"
d2$word <- paste("g2" ,d2$word, sep = "")

# merge
d <- rbind(d1,d2)

# word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=TRUE, rot.per=0.35,
          scale = c(3,0.5), random.color = FALSE,
          colors=brewer.pal(8, "Dark2")[factor(d$group)])



