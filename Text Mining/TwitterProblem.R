#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
install.packages("base64enc")
install.packages("httpuv")
install.packages("twitteR")
install.packages("RCurl")
install.packages("httr")
install.packages("syuzhet")
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)
library(rtweet)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
cred <- OAuthFactory$new(consumerKey='Provide Your Consumer API key', # Consumer Key (API Key) ### Due to security violation of Github removing it
                         consumerSecret='Provide Your Consumer API Secret key', #Consumer Secret (API Secret) ### Due to security violation of Github removing it
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
setup_twitter_oauth("Provide Your Consumer API key", # Consumer Key (API Key) ### Due to security violation of Github removing it
                    "Provide Your Consumer API Secret key", #Consumer Secret   ### Due to security violation of Github removing it
                    "Provide Your Access Token",  # Access Token  ### Due to security violation of Github removing it
                    "Provide Your Access Token Secret key")  ###Access Token Secret ####Due to security violation of Github removing it
Tweets <- userTimeline("narendramodi", n = 3200)
# store the tweets into dataframe
tweets.df = twListToDF(Tweets)
write.csv(tweets.df, "Tweets_modi.csv",row.names = F)
getwd()
################################################################################################################################################

makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 
# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negative wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names[1:120],freq_neg[1:120],scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}
words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
}
pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}
##### function to make cluster dendograms ##################################################################
clusdend = function(a){	# writing func clusdend() 	
  mydata.df = as.data.frame(inspect(a));	
  mydata1.df = mydata.df[, order(-colSums(mydata.df))];
  min1 = min(ncol(mydata.df), 40) 	# minimum dimn of dist matrix
  test = matrix(0,min1,min1)
  test1 = test
  for(i1 in 1:(min1-1)){ 
    for(i2 in i1:min1){
      test = sum(mydata1.df[ ,i1]-mydata1.df[ ,i2])^2
      test1[i1,i2] = test; test1[i2, i1] = test1[i1, i2] 	}
  }
  # making dissimilarity matrix out of the freq one
  test2 = test1
  rownames(test2) = colnames(mydata1.df)[1:min1]
  # now plot collocation dendogram
  d <- dist(test2, method = "euclidean") # distance matrix
  fit <- hclust(d, method="ward")
  windows()
  plot(fit) # display dendogram
} # clusdend() func ends

# lOADING +VE AND -VE words  
pos.words=scan("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Text Mining\\positive-words.txt", what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Text Mining\\negative-words.txt", what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray","superb","good") 			# including our own positive words to the existing list
neg.words = c(neg.words)
stopwords = readLines("C:\\Users\\sanu\\Downloads\\Desktop\\Documents\\Excelr\\Text Mining\\stop.txt")
#########################################################################################################################################
####We will remove hashtags, junk characters, other twitter handles and URLs 
####from the tags using gsub function so we have tweets for further analysis
# CLEANING TWEETS
tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)
tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

####Getting sentiments for each tweet
####Syuzhet breaks emotion into 10 different categories
# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emotions
emo_bar = colSums(emotions)##anger - 18,anticipation-116, disgust-7, fear-19,joy-146,
###sadness=40, surprise-48, trust-110,negative-43,positive-317

emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

####We are ready to visualize the emotions from NRC sentiments
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: narendramodi")
api_create(p,filename="Sentimentanalysis")
####Lets see which word contributes which emotion
# Create comparison word cloud data
wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" "),
  paste(tweets.df$text[emotions$positive > 0], collapse=" "),
  paste(tweets.df$text[emotions$negative > 0], collapse=" ")
)
wordcloud_tweet
# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))
# remove whitespace,punctuation, convert every word in lower case and remove stop words
##corpus = tm_map(corpus, stripwhitespace)     ### removes white space
corpus = tm_map(corpus, tolower)             ### converts to lower case
corpus = tm_map(corpus, removePunctuation)   ### removes punctuation marks  
corpus = tm_map(corpus, removeNumbers)       ### removes numbers in the documents  
corpus = tm_map(corpus, removeWords, c(stopwords("english"),stopwords))  
corpus = tm_map(corpus, stemDocument)
# create term document frequency matrix
tdm0 = TermDocumentMatrix(corpus)
inspect(tdm0)
# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(corpus,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))#,stemming=T))
inspect(tdm1)
a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }
# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]
# convert as matrix
tdm0 = as.matrix(tdm0)
tdm1 = as.matrix(tdm1)
tdm0new <- tdm0[nchar(rownames(tdm0)) < 11,]
tdm1new <- tdm1[nchar(rownames(tdm1)) < 11,]
# column name binding
colnames(tdm0) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust','positive','negative')
colnames(tdm0new) <- colnames(tdm0)
comparison.cloud(tdm0new, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown","purple","maroon"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
colnames(tdm1) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust','positive','negative')
colnames(tdm1new) <- colnames(tdm1)
comparison.cloud(tdm1new, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown","purple","maroon"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
##########################################################################################################
# Document term matrix 
dtm0 <- t(tdm0)
dtm1 <- t(tdm1)
# Word cloud - TF - Uni gram
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")
# Frequency Bar plot - TF - Uni gram
words_bar_plot(tdm0)
# Word cloud - TFIDF - Unigram
makewordc(tdm1)
# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)
# Positive word cloud - TF - Unigram
makeposwordc(tdm0)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")
# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)
# Positive word cloud - Unigram - TFIDF
makeposwordc(tdm1)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")
# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)
# Negative word cloud - TF - unigam
makenegwordc(tdm0) 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")
# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0)
# Negative word cloud - Unigram - TFIDF
makenegwordc(tdm1) 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")
# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)
# Bi gram word clouds
library(quanteda)
library(Matrix)
# Bi gram document term frequency 
dtm0_2 <- dfm(unlist(corpus),ngrams=3,verbose = F)
tdm0_2 <- t(dtm0_2)
a0 = NULL
for (i1 in 1:ncol(tdm0_2)){ if (sum(tdm0_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm0_2 = tdm0_2[, -a0]} else {tdm0_2 = tdm0_2};	dim(tdm0_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
dtm0_2 <- t(tdm0_2)
# Bi gram word cloud
makewordc(tdm0_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TF")
# Bi gram barplot on TF
words_bar_plot(tdm0_2)
## Bi gram on TFIDF
dtm1_2 <- tfidf(dtm0_2)
tdm1_2 <- t(dtm1_2)
a0 = NULL
for (i1 in 1:ncol(tdm1_2)){ if (sum(tdm1_2[, i1]) == 0) {a0 = c(a0, i1)} }
length(a0)		# no. of empty docs in the corpus
if (length(a0) >0) { tdm1_2 = tdm1_2[, -a0]} else {tdm1_2 = tdm1_2};	dim(tdm1_2)	# under TF weighing
a0 <- NULL;i1 <- NULL
dtm1_2 <- t(tdm1_2)
# Bi gram word cloud for TFIDF
makewordc(tdm1_2) # We have too see warnings to edit few words
title(sub = "BIGRAM - Wordcloud using TFIDF")
# Bigram barplot on TFIDF
words_bar_plot(tdm1_2)
# Cluster dendrogram on Uni gram - TF
clusdend=function(dtm0)
title(sub = "Dendrogram using TF")
# Cluster dendrogram on Uni gram - TFIDF
clusdend(dtm1)
title(sub = "Dendrogram using TFIDF")
# --- Can we segment the respondents (or cluster the documents) based on term usage? --- #
### --- kmeans proc ---- ###
# better cluster on TF dtm rather than tfidf dtm for solution stability #
wss = (nrow(dtm0)-1)*sum(apply(dtm0, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:8) wss[i] = sum(kmeans(dtm0, centers=i)$withinss)
windows()
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
k1 = 4		# based on the scree elbow plot
a3 = kmeans(dtm0, k1);	a3$size
a4 = kmeans(dtm1, k1)
round(a3$size/sum(a3$size), 2)		# segmt-sizes as proportions
# -- analyze each segment for what they're saying... --- #
for (i1 in 1:max(a3$cluster)) { 
  a4[[i1]] = t(dtm0[(a3$cluster == i1),])
} # loop ends
a4[[i2]]=t(dtm1[(a4$cluster == i2),])
# now plot wordclouds for by segment and see
par(ask = TRUE)
for (i2 in 1:max(a3$cluster)){	
  makewordc(a4[[i2]])	
  sub=paste("wordcloud-Clustering-",as.character(i2),"-",as.character(format(round(ncol(a4[[i2]])*100/nrow(dtm0),2),nsmall=3)),"%",collapse = " ")
  title(sub = sub)
  
} # loop ends
i2 <- NULL
par(ask = FALSE)		# close ask facility for graph making
# cluster dendograms cluster terms *within* documents
# in contrast, kmeans clusters documents themselves using word freqs across documents
# now try these examples:
for (i in 1:4){
  clusdend(t(a4[[i]]))
  title(sub = as.character(i))         
}
