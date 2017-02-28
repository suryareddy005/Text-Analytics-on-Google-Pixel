library(twitteR)
library(ROAuth) #Provides an interface to the OAuth 1.0 specification
library(RCurl) # To Read Data From Web
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RWeka)
library(ggplot2)
library(cluster)   
library(fpc)
library(proxy)     # Distance calculations
#install.packages("C:/Users/nspre/Downloads/Capstone_Project/sentiment_0.2.tar.gz", repos = NULL, type="source")
library(sentiment) #sentiment
library(readxl)    # Read Excel 
library(xlsx)      # Write Excel 
library(RTextTools)  # Topic modelling
library(topicmodels) # Topic modelling



#------------------------------------------------------------------------------------------------
#-------------------Reading Tweets From GooglePixel_New Excel -----------------------------------

GooglePixel <- read_excel("C:/Users/nspre/Downloads/Capstone_Project/GooglePixel_New.xlsx")
dim(GooglePixel)
GooglePixel<-unique(GooglePixel)
dim(GooglePixel)


#-------Prepare the text for sentiment analysis----------

# Create pixelCorpus
pixelCorpus=Corpus(VectorSource(GooglePixel$Text))
#summary(pixelCorpus)

# Function to Remove given pattern
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )

# Remove URL links
pixelCorpus = tm_map( pixelCorpus, toSpace, "https://t.co/[a-z,A-Z,0-9]*{8}")
# Take out retweet header, @user name and #hashtags
pixelCorpus = tm_map( pixelCorpus, toSpace, "RT @[a-z,A-Z,0-9]*: ")
pixelCorpus = tm_map( pixelCorpus, toSpace, "@[a-z,A-Z,0-9]*:")
pixelCorpus = tm_map( pixelCorpus, toSpace, "#[a-z,A-Z,0-9]*:")
# Remove punctuation
pixelCorpus <- tm_map(pixelCorpus, removePunctuation)
# Remove Numbers
pixelCorpus <- tm_map(pixelCorpus, removeNumbers)
# Convert to lower-case
pixelCorpus=tm_map(pixelCorpus,tolower)
# Remove particular unwanted words----- Remove Words are not 300 & 1019
pixelCorpus <- tm_map(pixelCorpus, removeWords, c("pixelpixel", "rt", "http", "ad", "vzwbuzz"))   

# Merging/Combining words that should stay together
for (j in seq(pixelCorpus))
{
  
  pixelCorpus[[j]] <- gsub("google pixel xl", "googlepixelxl", pixelCorpus[[j]])
  pixelCorpus[[j]] <- gsub("google pixel", "googlepixel", pixelCorpus[[j]])
  pixelCorpus[[j]] <- gsub("pixel xl", "pixelxl", pixelCorpus[[j]])
}
# Remove stopwords
pixelCorpus=tm_map(pixelCorpus,removeWords ,stopwords("en"))

# Remove stemwords ending with es,ing
pixelCorpus=tm_map(pixelCorpus, stemDocument)
pixelCorpus=tm_map(pixelCorpus, stripWhitespace)
# convert pixelCorpus to a Plain Text Document
pixelCorpus=tm_map(pixelCorpus,PlainTextDocument)

#-------------------End of Text Preparation ------------------
#*************************************************************


#*************************************************************
#--------------- Classify emotion, polarity-------------------
#*************************************************************

pixelCorpusdataframe<-data.frame(text=unlist(sapply(pixelCorpus, paste, collapse = "")), stringsAsFactors=T)
textdata <- pixelCorpusdataframe[pixelCorpusdataframe$text, ]

## Now we will transform all the words in lower case using catch.error function 
#textdata = sapply(textdata, try.error)
# Also we will remove NAs, if any exists
textdata = textdata[!is.na(textdata)]
# also remove names (column headings) from the text, as we do not want them in the sentiment analysis
names(textdata) = NULL

# Function returns an of bject of class data.frame with seven columns (anger, disgust, fear, joy, sadness, surprise, best_fit) and one row for each document:
# As a first step in this stage, let us first classify emotions
# In this we will be using Bayes' algorithm to classify emotion categories

pixel_class_emo = classify_emotion(textdata, algorithm="bayes", prior=1.0)

# we will fetch emotion category best_fit for our analysis purposes, visitors to this tutorials are encouraged to play around with other classifications as well.
emotion = pixel_class_emo[,7]

# Replace NA's (if any, generated during classification process) by word "unknown"
emotion[is.na(emotion)] = "unknown"


# classify polarity
pixel_class_pol = classify_polarity(textdata, algorithm="bayes")
# get polarity best fit
polarity = pixel_class_pol[,4]


# data frame with results
pixel_Sentiment_df = data.frame(text=textdata, emotion=emotion,
                                polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
pixel_Sentiment_df = within(pixel_Sentiment_df,
                            emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# Emotions Bar Plot 
ggplot(pixel_Sentiment_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

# Polarity Bar Plot 
ggplot(pixel_Sentiment_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")

# Prepare the data for creating a "Emotion word cloud".This includes removing common English stop words.

emos = levels(factor(pixel_Sentiment_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = textdata[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)

# Prepare the data for creating a "Polarity word cloud".This includes removing common English stop words.

polas = levels(factor(pixel_Sentiment_df$polarity))
nemo = length(polas)
polas.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = textdata[polarity == polas[i]]
  polas.docs[i] = paste(tmp, collapse=" ")
}
polas.docs = removeWords(polas.docs, stopwords("english"))
corpus = Corpus(VectorSource(polas.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = polas
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)



#******************************************************************
#-------------------- End of Emotions------------------------------
#------------------------------------------------------------------

#Extract the frequency of each bigram and analyse the twenty most frequent ones.
pixelOneTDM = TermDocumentMatrix(pixelCorpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
pixel_Onegram_Freq = sort(rowSums(as.matrix(pixelOneTDM)),decreasing = TRUE)
pixel_Onegram_FreqDF = data.frame(word=names(pixel_Onegram_Freq), freq=pixel_Onegram_Freq)
head(pixel_Onegram_FreqDF,30)

ggplot(head(pixel_Onegram_FreqDF,20), aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

col=brewer.pal(6,"Dark2")
wordcloud(pixelCorpus,  scale=c(5,0.6),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)


#------------------------------------------------------------------
#-------- Creating Bigram wordcloud -----------------
#Use Weka's n-gram tokenizer to create a TDM that uses as terms the bigrams that appear in the corpus.

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# Term Frequency Weighting
#pixel_Bigram = TermDocumentMatrix(pixelCorpus,
#                              control = list(tokenize = BigramTokenizer,weighting = function(x) weightTfIdf(x, normalize = FALSE)))
# term frequency - inverse document frequency (tf-idf) Weighting
pixel_Bigram = TermDocumentMatrix(pixelCorpus,
                                  control = list(tokenize = BigramTokenizer,weighting = function(x) weightTfIdf(x, normalize = FALSE)))

#Extract the frequency of each bigram and analyse the twenty most frequent ones.
pixel_Bigram_Freq = sort(rowSums(as.matrix(pixel_Bigram)),decreasing = TRUE)
pixel_Bigram_Freqdf = data.frame(word=names(pixel_Bigram_Freq), freq=pixel_Bigram_Freq)
head(pixel_Bigram_Freqdf)

ggplot(head(pixel_Bigram_Freqdf,20), aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

col=brewer.pal(6,"Dark2")
wordcloud(pixel_Bigram_Freqdf$word,pixel_Bigram_Freqdf$freq,  scale=c(5,0.6),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)

write.xlsx(pixel_Bigram_Freqdf, "C:/Users/nspre/Downloads/Capstone_Project/GooglePixel_BigramFreqdfNew.xlsx")

#------------------------------------------------------------------
#-------- End of Creating Bigram wordcloud ------------------------
#------------------------------------------------------------------



#********** Hirarchecal Clusters **********************************
#------------------------------------------------------------------
#  Start by removing sparse terms:   
pixel_Bigram_RSparse <- removeSparseTerms(pixel_Bigram, 0.985) # This makes a matrix that is 10% empty space, maximum.   
inspect(pixel_Bigram_RSparse) 

findFreqTerms(pixel_Bigram_RSparse, 100)
#findAssocs(pixel_Bigram_RSparse, "googlepixel",1)

pixel_Bigram_Dist <- proxy::dist(as.matrix(pixel_Bigram_RSparse), method="cosine")  # for Euclidian-euclidian   

fit <- hclust(d=pixel_Bigram_Dist, method="ward.D2")   
fit 

plot(fit, hang=-1) 

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the k clusters  

#------------------------------------------------------------------
#********** Hirarchecal Clusters Ends**********************************
#------------------------------------------------------------------



#------------------------------------------------------------------
#********** K Means Clusters **********************************
#------------------------------------------------------------------

#  Start by removing sparse terms:   
pixel_Bigram_RSparse <- removeSparseTerms(pixel_Bigram, 0.995) # This makes a matrix that is empty space, maximum.   
pixel_Bigram_Dist <- proxy::dist(as.matrix(pixel_Bigram_RSparse), method="cosine")  # for Euclidian-euclidian   

# Choosing K values using Hartigan rule
kbest= useful::FitKMeans(as.matrix(pixel_Bigram_Dist),max.clusters = 15, nstart = 25, seed = 1234)
kbest
useful::PlotHartigan(kbest)

### K-means clustering 
#pixel_Bigram_Dist <- dist(t(pixel_Bigram_RSparse), method="euclidian") 
set.seed(1234)
kfit <- kmeans(pixel_Bigram_Dist, 5, nstart = 100) 
# ploting Kmeans result using 2 Principal Components
useful::plot.kmeans(kfit,as.matrix(pixel_Bigram_Dist))

# Creating individual Data Frames for each cluster
kmeansDF=base::data.frame(word=names(kfit$cluster), freq=kfit$cluster)
kmeansDF1=kmeansDF[kmeansDF$freq==1,]
kmeansDF2=kmeansDF[kmeansDF$freq==2,]
kmeansDF3=kmeansDF[kmeansDF$freq==3,]
kmeansDF4=kmeansDF[kmeansDF$freq==4,]
kmeansDF5=kmeansDF[kmeansDF$freq==5,]

#-------------------------------------------------------------------------
#---------  Classifing emotions K1 Cluster -------------------------------

k1_emo = classify_emotion(kmeansDF$word, algorithm="bayes", prior=1.0)
# we will fetch emotion category best_fit for our analysis purposes, visitors to this tutorials are encouraged to play around with other classifications as well.
K1emotion = k1_emo[,7]
# Replace NA's (if any, generated during classification process) by word "unknown"
K1emotion[is.na(K1emotion)] = "unknown"

# classify polarity
k1_pol = classify_polarity(k1, algorithm="bayes")
# get polarity best fit
K1polarity = k1_pol[,4]

# data frame with results
k1_Sentiment_df = data.frame(text=k1, K1emotion=K1emotion,
                             K1polarity=K1polarity, stringsAsFactors=FALSE)

# sort data frame
k1_Sentiment_df = within(k1_Sentiment_df,
                         K1emotion <- factor(K1emotion, levels=names(sort(table(K1emotion), decreasing=TRUE))))

# Emotions Plot 
ggplot(k1_Sentiment_df, aes(x=K1emotion)) +
  geom_bar(aes(y=..count.., fill=K1emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")

# Polarity Plot 
ggplot(k1_Sentiment_df, aes(x=K1polarity)) +
  geom_bar(aes(y=..count.., fill=K1polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")




#-------- Creating Trigram wordcloud -----------------

#Use Weka's n-gram tokenizer to create a TDM that uses as terms the bigrams that appear in the corpus.

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
pixel_Trigram = TermDocumentMatrix(pixelCorpus.ng,
                                   control = list(tokenize = TrigramTokenizer))

#Extract the frequency of each bigram and analyse the twenty most frequent ones.
pixel_Trigram_Freq = sort(rowSums(as.matrix(pixel_Trigram)),decreasing = TRUE)
pixel_Trigram_Freqdf = data.frame(word=names(pixel_Trigram_Freq), freq=pixel_Trigram_Freq)
pixel_Trigram_Top_Freqdf= head(pixel_Trigram_Freqdf, 10)

ggplot(pixel_Trigram_Top_Freqdf, aes(word, freq)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

col=brewer.pal(6,"Dark2")
wordcloud(pixel_Trigram_Freqdf$word,pixel_Trigram_Freqdf$freq,  scale=c(5,0.6),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)


# Top 10 users with highest tweets 
head(GooglePixel[order(GooglePixel$`Retweet-Count`, decreasing=TRUE), c(2,11)], 10)
