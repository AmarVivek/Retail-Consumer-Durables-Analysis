#Setup the environment
library(SnowballC)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(data.table)
library(stringi)
library(qdap)
library(dplyr)
library(rJava)

setwd("C:/Users/Amar/Documents/Data Analytics/Assignment/Capstone Project")
tweets.df <- read.csv("Tweets_Samsung.csv")

#Convert char date into date format
tweets.df$Date <- as.Date(tweets.df$Date, format= "%d-%m-%y")

#Convert tweet text into character
tweets.df$Text <- as.character(tweets.df$Text)

# Remove character string between < >
tweets.df$Text <- genX(tweets.df$Text, " <", ">")

# Create document corpus with tweet text

myCorpus<- VCorpus(VectorSource(tweets.df$Text)) 

#Convert to Lowercase
myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))

#Remove urls
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#Remove everything expect English
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#Remove Stop Words
myStopWords<- c((stopwords('english')),c("rt", "use", "used", "via", "amp", "samsung","wwwyoureadercomuphpi","dldbzaecqb"))
myCorpus<- tm_map(myCorpus,removeWords , myStopWords)

#Remove Single letter words
removeSingle <- function(x) gsub(" . ", " ", x)   
myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))

#Remove extra white spaces
myCorpus<- tm_map(myCorpus, stripWhitespace)

#Keep a copy of the corpus
myCorpusCopy <- myCorpus

#Word Frequency Code

wordFreq <- function(corpus,word)
{
  results<- lapply(corpus,
                   function(x){ grep(as.character(x),pattern = paste0("\\<", word))})
  sum(unlist(results))
}
wordFreq(myCorpus,"Character")


tdm<- TermDocumentMatrix(myCorpus, control= list(wordLengths= c(1, Inf)))
tdm

idx <- which(dimnames(tdm)$Terms %in% c("apple", "best"))
as.matrix(tdm[idx,21:60])

(freq.terms <- findFreqTerms(tdm, lowfreq = 20))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq > 20)
df <- data.frame(term = names(term.freq), freq= term.freq)

ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts")) 

word.freq <-sort(rowSums(as.matrix(tdm)), decreasing= F)
pal<- brewer.pal(8, "Dark2")
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 2, random.order = F, colors = pal, max.words = 100)

#Code for Word Correlation
myCorpusdf <- data.frame(matrix(unlist(myCorpus),nrow=1091,byrow=T))

WordCorr <- apply_as_df(myCorpus[1:500], word_cor, word = "useful", r=.2)
plot(WordCorr)

qheat(vect2df(WordCorr[[1]], "word", "cor"), values=TRUE, high="red",
      digits=2, order.by ="cor", plot = FALSE) + coord_flip()

#Topic Modelling

df <- data.frame(text=sapply(myCorpus, `[[`, "content"), stringsAsFactors=FALSE)
head(unique(df[grep("tv", df$text), ]), n=10)

findAssocs(tdm, "tv", 0.2)

dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm , 1, sum)

NullDocs <- dtm[rowTotals==0, ]
dtm   <- dtm[rowTotals> 0, ]

if (length(NullDocs$dimnames$Docs) > 0) {
  tweets.df <- tweets.df[-as.numeric(NullDocs$dimnames$Docs),]
}

lda <- LDA(dtm, k = 5) # find 5 topic
term <- terms(lda, 7) # first 7 terms of every topic
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics<- topics(lda)
topics<- data.frame(date=(tweets.df$Date), topic = topics)
qplot (date, ..count.., data=topics, geom ="density", fill= term[topic], position="stack")

#Sentiment Analysis 

sentiments <- polarity(tweets.df$Text)
sentiments <- data.frame(sentiments$all$polarity)

sentiments[["polarity"]] <- cut(sentiments[[ "sentiments.all.polarity"]], c(-5,0.0,5), labels = c("negative","positive"))

table(sentiments$polarity)

#Sentiment Plot by date

sentiments$score<- 0
sentiments$score[sentiments$polarity == "positive"]<-1
sentiments$score[sentiments$polarity == "negative"]<- -1
sentiments$date <- as.IDate(tweets.df$Date)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

#Stream Graph for sentiment by date
Data<-data.frame(sentiments$polarity)
colnames(Data)[1] <- "polarity"
Data$Date <- tweets.df$Date
Data$text <- NULL
Data$Count <- 1

graphdata <- aggregate(Count ~ polarity + as.character.Date(Date),data=Data,FUN=length)
colnames(graphdata)[2] <- "Date"
str(graphdata)
graphdata$Date <- as.Date(graphdata$Date, format= "%m/%d/%yy")
graphdata$Date <- as.factor((graphdata$Date))
graphdata$Count <- as.numeric(graphdata$Count)
graphdata_df <- as.data.frame(graphdata)

streamgraph(data = graphdata, key = "polarity", value = "Count", date = "Date",
            scale="continuous", offset = "silhouette", interpolate = "cardinal",
            width = "200", height = "200",interactive = TRUE) %>%
  sg_legend(TRUE, "polarity: ") %>%
  sg_colors("PuOr") %>%
  sg_axis_x(tick_interval = 1, tick_units = "Date", tick_format = "%M")
