#  1. Creating Data for NLP

#library(stringi) # finds summary information for text files
require(tm)      # Text mining
require(RWeka)   # tokenizer - create unigrams, bigrams, trigrams
#library(NLP); library(openNLP) #automatically installed with tm?

#library(rJava)
#library(RWekajars)
#library(SnowballC) # Stemming
#library(RColorBrewer) # Color palettes
#library(qdap)
#library(ggplot2) #visualization

#read in blogs, twitter and news
Blogs1 <- readLines("C:/Users/KUIPERS/Desktop/RStudio/datasciencecoursera/NLP/final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
Twitter1 <- readLines("C:/Users/KUIPERS/Desktop/RStudio/datasciencecoursera/NLP/final/en_US/en_US.twitter.txt",encoding = "UTF-8", skipNul = TRUE, warn = FALSE)
News1 <- readLines("C:/Users/KUIPERS/Desktop/RStudio/datasciencecoursera/NLP/final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE, warn = FALSE)

# Take sample of each to limit size
set.seed(123)
Blogs2 <- sample(Blogs1, size=length(Blogs1)*.01, replace=FALSE) 
Twitter2 <- sample(Twitter1, size=length(Twitter1)*.01, replace=FALSE) 
News2 <- sample(News1, size=length(News1)*.01, replace=FALSE) 

data1 = c(Blogs2, Twitter2, News2)
length(data1)

# Create and Clean Corpus for each file
data2 <- sapply(data1, function(x) iconv(enc2utf8(x), sub = "byte"))
data2 <- (data2[!is.na(data2)])

# Use the tm package to convert to a Volatile Corpus and some basic data cleaning
Corpus1 <- VCorpus(VectorSource(Blogs2))
Corpus1 <- tm_map(Corpus1, tolower) # Make all words lower case 
Corpus1 <- tm_map(Corpus1, removePunctuation) # Remove all punctuation 
Corpus1 <- tm_map(Corpus1, removeNumbers) # Remove all numbers 
Corpus1 <- tm_map(Corpus1, stripWhitespace) # Remove all whitespace
#Corpus1 <- tm_map(Corpus1, PlainTextDocument) # ???Remove all make plain text???
profanity <-  c("([Ff][Uu][Cc][Kk]",
                "[Ss$][Hh][Ii][Tt]",
                "[Aa@][Ss$][Ss$]",
                "[Aa@][Ss$][Ss$][Hh][Oo][Ll][Ee]",
                "[Cc][Uu][Nn][Tt]",
                "[Dd][Aa][Mm][Nn]",
                "[Nn][Ii][Gg][Gg][Ee][Rr])", sep="|")
Corpus1 <- tm_map(Corpus1, removeWords, profanity)
# took 5 seconds 2.8 Mb

## Tokenizer function to get unigrams
unigram <- NGramTokenizer(Corpus1, Weka_control(min = 1, max = 1,delimiters = " \\r\\n\\t.,;:\"()?!"))
# took about 2 minutes
unigram <- data.frame(table(unigram))
unigram <- unigram[order(unigram$Freq,decreasing = TRUE),]

names(unigram) <- c("word1", "freq")
head(unigram)
unigram$word1 <- as.character(unigram$word1)

write.csv(unigram[unigram$freq > 1,],"unigram.csv",row.names=F)
unigram <- read.csv("unigram.csv",stringsAsFactors = F)
saveRDS(unigram, file = "unigram.RData")

## Tokenizer function to get bigrams
bigram <- NGramTokenizer(Corpus1, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t.,;:\"()?!"))
#about 2 minutes
bigram <- data.frame(table(bigram))
bigram <- bigram[order(bigram$Freq,decreasing = TRUE),]
names(bigram) <- c("words","freq")
head(bigram)
bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

write.csv(bigram[bigram$freq > 1,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")

## Tokenizer function to get trigrams
trigram <- NGramTokenizer(Corpus1, Weka_control(min = 3, max = 3,delimiters = " \\r\\n\\t.,;:\"()?!"))
# about 2 min
trigram <- data.frame(table(trigram))
trigram <- trigram[order(trigram$Freq,decreasing = TRUE),]
names(trigram) <- c("words","freq")
head(trigram)

trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))

trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)

write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")


## Tokenizer function to get quadgrams
quadgram <- NGramTokenizer(Corpus1, Weka_control(min = 4, max = 4,delimiters = " \\r\\n\\t.,;:\"()?!"))
quadgram <- data.frame(table(quadgram))
quadgram <- quadgram[order(quadgram$Freq,decreasing = TRUE),]

names(quadgram) <- c("words","freq")
head(quadgram)

quadgram$words <- as.character(quadgram$words)
str4 <- strsplit(quadgram$words,split=" ")    
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))

quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)

write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")



