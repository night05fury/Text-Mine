#Text Mining / Text Analytics is the process of deriving meaningful information from natural language text.
  #Auto complete
  #Spam Detection
  #Predictive Typing
  #Spell Checker

#We generate an unbelievably large amount of data daily through social media and mails.
#Around 80% of that data is unstructured.
#The only way to add value to a business is by extracting useful
#information from all the data that you receive. This data can be from customers, 
#through social media, likes, comments, etc. 
#Analyzing texts and data, adds a great value to a business.

#Natural Language Processing is a part of computer science and artificial intelligence
#which deals with human languages.

#NLP is used for Sentimental Analysis, Chatbot, Speech Recognition,
#Machine Translation, Spell Checking, Keyword Search ETC.


#installing tm Packages
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud2")
install.packages("ggplot2")
library(tm)

#Loading required package: NLP

#create Corpus 
#loading a text file from local computer
newdata<- readLines("https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/tweets_julia.csv")
#Load data as corpus
#VectorSource() creates character vectors
docs <- Corpus(VectorSource(newdata))
inspect(docs)

writeLines(as.character(docs[[30]]))

#start preprocessing
toSpace <- content_transformer(function(x,pattern){return(gsub(pattern," ",x))})

docs<-tm_map(docs, toSpace,"_")
docs<-tm_map(docs,toSpace,":")
docs<-tm_map(docs,toSpace,";")
docs<-tm_map(docs,toSpace,"'")
docs<-tm_map(docs,toSpace,"-")
docs<-tm_map(docs,toSpace," '' ")

#Remove Punctuations
docs <- tm_map(docs,removePunctuation)
#transform to lower case
docs<-tm_map(docs,content_transformer(tolower))

#strip digits
docs<-tm_map(docs,removeNumbers)

#Remove stopwords from standard stopwords list
docs <-tm_map(docs,removeWords,stopwords("english"))
#strip whitespaces
docs<-tm_map(docs, stripWhitespace)
#inspect Output
inspect(docs)

#need snowballC library for stemming
#Stemming is a part of data pre-processing which chop off all 
#the words to their root words. E.g we stemmed a word ‘making’ it will stem down to word ‘make’.

library(SnowballC)

#stem document
docs<- tm_map(docs, stemDocument)
#some Clean Up using gsub. The gsub () function in R can be used to replace all occurrences 
#of certain text within a string in R. 
docs<-tm_map(docs, content_transformer(gsub), pattern="oraganiz",replacement="organ")
docs<-tm_map(docs, content_transformer(gsub), pattern="oraganis",replacement="organ")
docs<-tm_map(docs, content_transformer(gsub), pattern="andovern",replacement="govern")
docs<-tm_map(docs, content_transformer(gsub), pattern="inenteriors",replacement="enterprize")
docs<-tm_map(docs, content_transformer(gsub), pattern="team-",replacement="team")

#inspect output
inspect(docs)

#create a document matrix 

#Term document matrix is also a method for representing the text data. 
#In this method, the text data is represented in the form of a matrix.
#The rows of the matrix represent the sentences from the data which needs 
#to be analyzed and the columns of the matrix represent the word.


dtm<-DocumentTermMatrix(docs)

#inspect
inspect(dtm[1:6,1001:1007])

#collapse matrix by summing over columns- this gets total counts (over all docs )for each term
freq <-colSums(as.matrix(dtm))

#length should be total numbers of terms
length(freq)
#creating a sorting order
ord<-order(freq,decreasing = TRUE)
#inspect most frequently occurring terms 
freq[head(ord)]
#inspect least frequently occurring terms 
freq[tail(ord)]

#remove very frequent and very rare words

dtmr<-DocumentTermMatrix(docs, control = list(wordLengths=c(4,20),bounds=list(global=c(3,27))))
freqr<-colSums(as.matrix(dtmr))

#length should be total number of terms
length(freqr)

#now creating a sorting order
ordr<-order(freqr,decreasing = TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]
#inspect most least occurring terms
freqr[tail(ordr)]

#listing most frequents terms , lower bound specified as second argument
findFreqTerms(dtmr,lowfreq = 15)

#correlations 
findAssocs(dtmr,"throw",0.1)
findAssocs(dtmr,"form",0.1)
findAssocs(dtmr,"taken",0.1)
findAssocs(dtmr,"spoke",0.1)

#histogram
wf=data.frame(term=names(freqr),occurrences=freqr)

library(ggplot2)
p<-ggplot(subset(wf,freqr>26),aes(term,occurrences))
p<-p+geom_bar(stat="identity")
p<-p+theme(axis.text.x=element_text(angle=45,hjust=1))
p

#wordcloud

library(wordcloud2)
#setting the same seeds for consistent look
set.seed(30)
#limit words by specifying the min frequency
wordcloud(names(freqr),freqr,min.freq=27)
#..now adding color
wordcloud(names(freqr),freqr,min.freq = 27,colors=brewer.pal(6,"Dark2"))

