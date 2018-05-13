###Text Mining Project###
###This project uses data from Kaggle related to Hillary Clinton's emails released by ###
###the State Department as a result of the Benghazi investigations ###
###to perform text mining and sentiment analysis ##

###More info can be found here: https://www.kaggle.com/kaggle/hillary-clinton-emails ###
###Data: https://www.kaggle.com/kaggle/hillary-clinton-emails/downloads/hillary-clinton-emails.zip/2 ###

###Set working directory and read in the csv file###-----------
setwd("C:/Users/William/Desktop/NCSU MSA/Fall 2015/Text Mining/Homework/Clinton E-mails")
Emails= read.csv("Emails.csv")
summary(Emails)
str(Emails)
summary(Emails$ExtractedSubject)
##End basic look at data##

###Necessary Packages###-----------
install.packages("tm", repos="http://R-Forge.R-project.org")
install.packages("SnowballC")
install.packages("stringi")
install.packages("arules")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("NbClust")
install.packages("qdap")
install.packages("proxy")
install.packages("cluster")
install.packages("fpc")
install.packages(c("RTextTools","topicmodels"))
install.packages("igraph")
install.packages("graph")
install.packages("syuzhet")
download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz") 
install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
install.packages("sentiment.tar.gz", repos=NULL, type="source")

library(tm)
library(SnowballC)
library(stringi)
library(arules)
library(wordcloud)
library(ggplot2)
library(NbClust)
library(qdap)
library(data.table)
library(stringr)
library(plyr)
library(dplyr)
library(magrittr)
library(tm)
library(proxy)
library(RColorBrewer)
library(cluster)
library(fpc)
library(RTextTools)
library(topicmodels)
library(igraph)
library(graph)
library('syuzhet')

##End installing and library packages##



###Creating a corpus for Email Body Text###----------
corpus = Corpus(VectorSource(Emails$ExtractedBodyText))
corpus
#It is calling each email a document

##Convert to lowercase##
corpus <- tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus,PlainTextDocument)
#Ensures the data is kept in the right type.

##Remove punctuation##
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,PlainTextDocument)

##Remove Stopwords##
corpus = tm_map(corpus,removeWords, c(stopwords("english"), "will", "fyi", "2009", "2010", "work", "call", "can", "get", "said", "also", "see", "want", "know", "talk", "just"))
corpus = tm_map(corpus,PlainTextDocument)
#summary(corpus)

##Stemming##
corpus = tm_map(corpus,stemDocument,language="english")
corpus = tm_map(corpus,PlainTextDocument)

##Remove Whitespace##
corpus <- tm_map(corpus, stripWhitespace)
head(corpus)


dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")), 
                      stringsAsFactors=F)
###End cleaning of body text###

###Create Document Term Matrix of Body Text###----------
frequencies = DocumentTermMatrix(corpus)
frequencies
findFreqTerms(frequencies,lowfreq=900)
freq = as.matrix(frequencies)
#End Creating Doc Term Matrix##

###Creating TDM & a bar graph of most frequent words in body text###----------
#Create Term Document Matrix#
tdm <- TermDocumentMatrix(corpus)
tt <- findFreqTerms(tdm,lowfreq=800)
tt
termFrequency <- rowSums(as.matrix(tdm[tt,]))
qplot(names(termFrequency), termFrequency,
      geom="bar", stat="identity", xlab = "Terms", ylab = "Term Frequency") +
  coord_flip()

###End Creating bar graph##

###Remove sparse terms for body text###------------
sparse = removeSparseTerms(frequencies,0.97)
#Going to take away all but the most frequently occuring terms
#A smaller nuumber will give you even fewer terms
sparse
# remove sparse terms
tdmat <- as.matrix(
  removeSparseTerms(tdm, sparse=0.951)
)

z <- write.csv(tdmat,"C:/Users/William/Desktop/NCSU MSA/Fall 2015/Text Mining/Homework/Clinton E-mails/Sparse.csv")
x <- write.csv(termFrequency,"C:/Users/William/Desktop/NCSU MSA/Fall 2015/Text Mining/Homework/Clinton E-mails/freq.csv")

###Cluster Dendrogram for body text---------------
distMatrix <- dist(scale(tdmat))
fit <- hclust(distMatrix, method="ward.D2")
plot(fit)
rect.hclust(fit, k = 15)
###End removing sparse terms###


###Creating a corpus for Email Subject###------------
corpus2 = Corpus(VectorSource(Emails$ExtractedSubject))
corpus2
#It is calling each email a document

##Convert to lowercase##
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 = tm_map(corpus2,PlainTextDocument)
#Not sure what this line does, but it seems to be keeping the data in the right type. It doesn't work well if you don't do this though

##Remove punctuation##
corpus2 = tm_map(corpus2,removePunctuation)
corpus2 = tm_map(corpus2,PlainTextDocument)

##Remove Stopwords##
corpus2 = tm_map(corpus2,removeWords, c(stopwords("english"), "will", "call", "can", "get", "said", "also", "see"))
corpus2 = tm_map(corpus2,PlainTextDocument)
#summary(corpus2)

##Stemming##
corpus2 = tm_map(corpus2,stemDocument,language="english")
corpus2 = tm_map(corpus2,PlainTextDocument)

##Remove Whitespace##
corpus2 <- tm_map(corpus2, stripWhitespace)
#head(corpus2)
#stopwords("english")
##End Cleaning of Subject Line###

###Create Document Term Matrix of Subject###----------
subfrequencies = DocumentTermMatrix(corpus2)
subfrequencies
findFreqTerms(subfrequencies,lowfreq=100)
subfreq = as.matrix(subfrequencies)

###Creating TDM & a bar graph of most frequent words in subject###----------
#Create Term Document Matrix#
tdmsub <- TermDocumentMatrix(corpus2)
tt <- findFreqTerms(tdmsub,lowfreq=80)
tt
termFrequencysub <- rowSums(as.matrix(tdmsub[tt,]))
qplot(names(termFrequencysub), termFrequencysub,
      geom="bar", stat="identity", xlab= "Terms", ylab= "Term Frequency") +
  coord_flip()
###End Creating bar graph for subject##

###Remove sparse terms for subject###----------
subsparse = removeSparseTerms(subfrequencies,0.97)
#Going to take away all but the most frequently occuring terms
#A smaller nuumber will give you even fewer terms
subsparse
# remove sparse terms
tdmatsub <- as.matrix(
  removeSparseTerms(tdmsub, sparse=0.951)
)

###Begin cluster dendrogram for subject###----------
# compute distances
distMatrixsub <- dist(scale(tdmatsub))
fitsub <- hclust(distMatrix, method="ward.D2")
plot(fitsub)
rect.hclust(fitsub, k = 8)
###End removing sparse terms###
###End cluster Dendrogram###

y <- write.csv(termFrequencysub,"C:/Users/William/Desktop/NCSU MSA/Fall 2015/Text Mining/Homework/Clinton E-mails/subjectfreq.csv")


###Begin finding highly associated words in Body Text###---------
#the (inclusive) lower correlation limits of each term in the range from zero to one.
##words highly associated with Obama##
obama = findAssocs(tdm, "obama", 0.54)

##words highly associated with president##
clinton = findAssocs(tdm,"clinton",0.54)
###End finding highly associated words###

findAssocs(tdm, "american",0.54)
findAssocs(tdm, "benghazi",0.83)
findAssocs(tdm, "libya",0.75)
findAssocs(tdm, "boehner",0.58)
findAssocs(tdm, "limbaugh", 0.7)
findAssocs(tdm, "obama", 0.7)
findAssocs(tdm, "iraq", 0.57)
findAssocs(tdm, "afghanistan", 0.65)
findAssocs(tdm, "haiti", 0.7)
findAssocs(tdm, "reid", 0.7)
findAssocs(tdm, "pelosi", 0.48)
findAssocs(tdm, "tucson", 0.95)
findAssocs(tdm, "illuminati", 0.85)
findAssocs(tdm, "beck", 0.8)
findAssocs(tdm, "gun", 0.85)


###Creating word clouds for body text and subject line###------------
#wordcloud for body text
wordcloud(corpus, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

#wordcloud for subject line
wordcloud(corpus2, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
###End creating word clouds###




###Begin Clustering###-----------
## do tf-idf
dtm_tfxidf <- weightTfIdf(sparse)
## do document clustering

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)

##Figure out how many clusters to use###----------
wssplot <- function(data, nc=20, seed=1234){
 wss <- (nrow(data)-1)*sum(apply(data,2,var))
 for (i in 2:nc){
 set.seed(seed)
 wss[i] <- sum(kmeans(data, centers=i)$withinss)}
 plot(1:nc, wss, type="b", xlab="Number of Clusters",
 ylab="Within groups sum of squares")}
wssplot(m)
###End Figuring out how many clusters to use 14 or 15###

###Cluster into 15 clusters (body)###----------
cl <- kmeans(m, 15)
table(cl$cluster)

###Show clusters (body) using the first 2 principal components###-----------
plot(prcomp(m)$x, col=cl$cl)
clusplot(m,cl$cluster,color=TRUE,shade=TRUE,labels=1,lines=1)
plotcluster(m, cl$cluster)

###Principal Components and Clustering (body)### ------------
#Not a really pretty graph----going to improve#
pca_comp <- prcomp(dtm_tfxidf)
pca_rep <- data_frame(
                      pc1 = pca_comp$x[,1],
                      pc2 = pca_comp$x[,2],
                      clust_id = as.factor(cl$cluster))

ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
  scale_color_brewer(palette = 'Set1') +
  geom_text(mapping = aes(label = clust_id), size = 2.5, fontface = 'bold') +
  labs(title = 'K-Means Cluster: 5 clusters on PCA Features',
       x = 'Principal Component Analysis: Factor 1',
       y = 'Principal Component Analysis: Factor 2') +
  theme_grey() +
  theme(legend.position = 'right',
        legend.title = element_blank())

###Clustering with Graphic Body###-----------
dtm <- as.DocumentTermMatrix(tdm)
rowTotals <- apply(dtm, 1, sum)
dtm.new <- dtm[rowTotals > 0, ]
lda <- LDA(dtm.new, k=6)
term <- terms(lda,4)
term
term <- apply(term, MARGIN = 2, paste, collapse = ", ")


topic <- topics(lda, 1)
str(topic)
topics <- data.frame(topic)
str(topics)
topics_new <- cbind("Topics" = rownames(topics$topics))
qplot(names(topics), ..count.. , data=topics, geom="bar", 
       fill=term[topic],position="dodge", xlab = "Topics", ylab="Count", main="Body Text Clusters",  colors=brewer.pal(8, "Dark2"))
###End pretty graphic###

###Clustering with graphic subject###---------
dtmsub <- as.DocumentTermMatrix(tdmsub)
rowTotalsSub <- apply(dtmsub, 1, sum)
dtm.newsub <- dtmsub[rowTotalsSub >0, ]
ldasub <- LDA(dtm.newsub, k=6)
termsub <- terms(ldasub,4)
termsub
termsub <- apply(termsub, MARGIN = 2, paste, collapse = ", ")

topicsub <- topics(ldasub, 1)
str(topicsub)
topicssub <- data.frame(topicsub)
str(topicssub)
qplot(names(topicssub), ..count.. , data=topicssub, geom="bar", 
      fill=term[topicsub], position="dodge")
#End Pretty graphic subject


                                 
###Sentiment Analysis###-----------

EmailsVector<- as.vector.factor(Emails$ExtractedBodyText)
d<-get_nrc_sentiment(EmailsVector)
td<-data.frame(t(d))

td_new <- data.frame(rowSums(td[2:7945]))

#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Vizualization
library("ggplot2")
qplot(sentiment, data=td_new2, weight=count, xlab= "Sentiment", ylab= "Count",geom="histogram",fill=I("blue"), col=I("red"), alpha=I(.2) )+ggtitle("Sentiment of Emails")


###Aliases###--------
aliases <-c("abedin huma", 
            "abedinh@state.gov", 
            "abein huma", 
            "abendin huma", 
            "adedin huma", 
            "huma abedin", 
            "huma@clintonemail.com", 
            "abedin@state.gov", 
            "abendinh@state.gov", 
            "adedinh@state.gov", 
            "sullivan jj@state.gov", 
            "sullivan jacbo j", 
            "sullivan jack", 
            "sullivan jacob", 
            "sullivan jacob h", 
            "sullivan jacob j", 
            "sullivan jake", 
            "sullivan jake j", 
            "sullivanjj@state.gov", 
            "jake. sullivan", 
            "jake.sullivan", 
            "jake.sullivan@", 
            "sulllivanjj@state.gov", 
            "sullivanil@state.gov", 
            "sullivann@state.gov.", 
            "c:mills cheryl", 
            "cheryl mills", 
            "cheryl mills cos", 
            "mill cheryl", 
            "mills cherlyl d", 
            "mills chery d", 
            "mills cheryl", 
            "mills cheryl d", 
            "millscheryl d", 
            "mills. cherl d", 
            "mills. cheryl d", 
            "millscd@state.gov", 
            "cheryl.mills", 
            "cheryl.mills@", 
              "jilloty lauren c", 
            "jiloty cheryl d", 
            "jiloty lauren", 
            "jiloty lauren c", 
            "jiloty lauren cd", 
            "jiloty. lauren c", 
            "jilotylc@state.gov", 
            "jjiloty lauren c", 
            "jjilotylc@state.gov", 
            "lauren jiloty" )


###Sentiment on Cheryl###---------
setwd("~/TextMining/output")
Cheryl= read.csv("Cheryl.csv")

CherylVector<- as.vector.factor(Cheryl$MetadataSubject)
d.cheryl<-get_nrc_sentiment(CherylVector)
td.cheryl<-data.frame(t(d.cheryl))

td_new.cheryl <- data.frame(rowSums(td.cheryl[2:1318]))

#Transformation and cleaning
names(td_new.cheryl)[1] <- "count"
td_new.cheryl <- cbind("sentiment" = rownames(td_new.cheryl), td_new.cheryl)
rownames(td_new.cheryl) <- NULL
td_new2.cheryl<-td_new.cheryl[1:8,]

#Vizualization
#install.packages("Rcpp")
#library(Rcpp)
#library(ggplot2)
#install.packages('ggplot2', dep = TRUE) 
#update.packages("ggplot2")
qplot(sentiment, data=td_new2.cheryl, weight=count, xlab= "Sentiment", ylab= "Count",geom="histogram",fill=I("blue"), col=I("red"), alpha=I(.2) )+ggtitle("Sentiment of Emails To Cheryl Mills")


###Sentiment on Huma###----------------
setwd("~/TextMining/output")
Huma= read.csv("Huma.csv")

HumaVector<- as.vector.factor(Huma$MetadataSubject)
d.huma<-get_nrc_sentiment(HumaVector)
td.huma<-data.frame(t(d.huma))

td_new.huma <- data.frame(rowSums(td.huma[2:1437]))

#Transformation and cleaning
names(td_new.huma)[1] <- "count"
td_new.huma <- cbind("sentiment" = rownames(td_new.huma), td_new.huma)
rownames(td_new.huma) <- NULL
td_new2.huma<-td_new.huma[1:8,]

qplot(sentiment, data=td_new2.huma, weight=count, xlab= "Sentiment", ylab= "Count",geom="histogram",fill=I("blue"), col=I("red"), alpha=I(.2) )+ggtitle("Sentiment of Emails To Abendin Huma")


###Sentiment on Jacob Sullivan###------------
Jacob= read.csv("Jacob.csv")

JacobVector<- as.vector.factor(Jacob$MetadataSubject)
d.jacob<-get_nrc_sentiment(JacobVector)
td.jacob<-data.frame(t(d.jacob))

td_new.jacob <- data.frame(rowSums(td.jacob[2:873]))

#Transformation and cleaning
names(td_new.jacob)[1] <- "count"
td_new.jacob <- cbind("sentiment" = rownames(td_new.jacob), td_new.jacob)
rownames(td_new.jacob) <- NULL
td_new2.jacob<-td_new.jacob[1:8,]

qplot(sentiment, data=td_new2.jacob, weight=count, xlab= "Sentiment", ylab= "Count",geom="histogram",fill=I("blue"), col=I("red"), alpha=I(.2) )+ggtitle("Sentiment of Emails To Jacob Sullivan")


###Sentiment on Lauren###---------------
Lauren= read.csv("Lauren.csv")

LaurenVector<- as.vector.factor(Lauren$MetadataSubject)
d.lauren<-get_nrc_sentiment(LaurenVector)
td.lauren<-data.frame(t(d.lauren))

td_new.lauren <- data.frame(rowSums(td.lauren[2:341]))

#Transformation and cleaning
names(td_new.lauren)[1] <- "count"
td_new.lauren <- cbind("sentiment" = rownames(td_new.lauren), td_new.lauren)
rownames(td_new.lauren) <- NULL
td_new2.lauren<-td_new.lauren[1:8,]

qplot(sentiment, data=td_new2.lauren, weight=count, xlab= "Sentiment", ylab= "Count",geom="histogram",fill=I("blue"), col=I("red"), alpha=I(.2) )+ggtitle("Sentiment of Emails To Lauren Jiloty")
