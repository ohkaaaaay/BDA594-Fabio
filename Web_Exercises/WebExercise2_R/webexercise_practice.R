# this is your first R exercise.
# type basic math calculation as the following (or copy the whole paragraph and paste into the Console).
4 + 3 # “+” plus sign
50/4 # “/” divided sign
6*6 # “*” multiply sign
9 + 3*3 # basic calculation procedure
9^3 # “^” is denotes power. 9^3 = 9*9*9
1 %/% 2

Age <- c(23, 20, 21, 22, 30)
Score <- c(83, 99, 80, 79, 90)

summary (Age)
summary (Score)

mydata = read.csv("Lung_Cancer_Death_2010-2013.csv")
View(mydata)



#Reading Tables and Conduct Cancer Data Analysis
#### Created by Jay Yang, HDMA@SDSU Sep,2016 -------- ####
#### load required libraries
library(ggplot2)
### Read csv data into R dataframe
cancer_data <- read.csv("Lung_Cancer_Death_2010-2013.csv")
### list all the field names (Variables in the dataset)
names(cancer_data)
### Subset data by specific column
## subset only the year 2010
data_2010 <- cancer_data[cancer_data$YEAR == 2010,]
## subset by other fields (ex. Geography and RegionName)
# data_LaMesa <- cancer_data[cancer_data$Geography == 'La Mesa',]
# data_EAST <- cancer_data[cancer_data$RegionName == 'EAST',]
### Generate some statistics
## aggregate region by names, then calculate the mean for three rates
mean_region <- aggregate(data_2010[, c("Male_Rate","Female_Rate","Age_Adjusted_Rate")], by=list(RegionName = data_2010$RegionName), FUN=mean, na.rm=TRUE)
print(mean_region)
## remove the row UNKNOWN
mean_region <- mean_region[mean_region$RegionName != "UNKNOWN",]
### Make a visual plot ! aes: aesthetic mappings, geom_bar: rectangle bars
ggplot(mean_region, aes(mean_region$RegionName,mean_region$Age_Adjusted_Rate)) + 
  geom_bar(stat="identity",aes(fill= factor(mean_region$RegionName)), width=0.5) +
  ggtitle("Lung Cancer in San Diego by Regions (Mean of Age Adjusted Rate)")+
  ylab("Mean of Age Adjusted Rate") +
  xlab("Regions of San Diego") +
  theme(legend.position="right") +
  theme(axis.text.x = element_text(angle=70, vjust=1, hjust=1))+
  list()
## show the plot & save it to file
ggsave("myFigure.png", width = 12, height = 6)



#Creating a Word Cloud for the definition of “Big Data” in this class
#these are the libraries used in the Word Cloud Tasks
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(NLP)
#Put your text files inside the temp folder(wordcloud) under working directory(D:\webexercise-2)
my_corpus = Corpus(DirSource("wordcloud"))
#You can add or remove STOPWORDS in the list
tdm = TermDocumentMatrix(my_corpus,
      control = list(removePunctuation = TRUE,
      stopwords = c("big", "data", stopwords("english")),
      removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud in R
wordcloud(dm$word, dm$freq, random.order=FALSE, random.color=FALSE, rot.per= 0, colors=brewer.pal(8, "Dark2"))
# save the image in png format – a PNG image will be created in the Working Directory
png("WordCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, random.color=FALSE, rot.per= 0, colors=brewer.pal(8, "Dark2"))
# dev.off will save the output PNG file into the working folder
dev.off()




#Reading Tables and Conduct Poison Data Analysis
#### Created by Elizabeth Fabio, Sep 2020 -------- ####
#### Load required libraries
library(ggplot2)
### Read csv data into R dataframe
poison_data <- read.csv("Poisoning_Death.csv")
### Generate some statistics
## Aggregate region by names, then calculate the mean for three rates
mean_year <- aggregate(poison_data[, c("Total_Male","Total_MaleRate", "Total_Female", "Total_FemaleRate", "TotalRate", "Total")], by=list(Year = poison_data$Year), FUN=mean, na.rm=TRUE)
print(mean_year)
### Make a visual plot ! aes: aesthetic mappings, geom_bar: rectangle bars
poison_plot <- ggplot(mean_year, aes(x = Year, y = Total)) +
  geom_bar(stat="identity", aes(fill = factor(Year))) +
  scale_x_continuous(breaks = NULL) +
  labs(title = "Poisoning in San Diego from 2011-2017", subtitle = "Mean from the Population", y = "Average", x = 'Year', fill = 'Legend')
poison_plot
## Show the plot & save it to file
ggsave("poisoning.png", width = 12, height = 6)



#Creating a Word Cloud from Wikipedia's page on Big Data (https://en.wikipedia.org/wiki/Big_data)
#Exported the page as a PDF and changed to a text file
#These are the libraries used in the Word Cloud Tasks
library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(NLP)
#Put your text files inside the temp folder(wordcloud) under working directory(D:\webexercise-2)
my_corpus = Corpus(DirSource("wordcloud"))
#You can add or remove STOPWORDS in the list
#Stopwords: Wikipedia references, websites, months, and the term "big data"
tdm = TermDocumentMatrix(my_corpus,
      control = list(removePunctuation = TRUE,
                stopwords = c("big", "data", "bigdata", "httpsenwikipediaorgwikibigdata", "wikipedia", "wikimedia", "–",
                              "httpswwwworldcatorgissn", "httpswwwncbinlmnihgovpmcarticlespmc", "httpspubmedncbinlmnihgov",
                              "httpsapisemanticscholarorgcorpusid", "january", "february", "march", "april", "may", "june",
                              "july", "august", "september", "october", "november", "december", stopwords("english")),
                removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
#Remove words that appear less than 5 times
dm <- subset(dm, freq > 5)
#Plot wordcloud in R
wordcloud(dm$word, dm$freq, random.order=FALSE, random.color=FALSE, rot.per= 0, colors=brewer.pal(8, "Dark2"))
#Save the image in png format – a PNG image will be created in the Working Directory
png("BigDataWiki.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, random.color=FALSE, rot.per= 0, colors=brewer.pal(8, "Dark2"))
#dev.off will save the output PNG file into the working folder
dev.off()

