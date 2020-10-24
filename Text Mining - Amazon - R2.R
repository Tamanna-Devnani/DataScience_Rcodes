install.packages(c("rvest","XML","magrittr"))

library(rvest)
library(XML)
library(magrittr)

# Extracting Amazon Reviews for book Tower of Nero

aurl <- "https://www.amazon.in/Tower-Nero-Trials-Apollo-Book/product-reviews/0141364084/ref=cm_cr_getr_d_paging_btm_prev_2?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
# Loop to read and club the reviews from pages
for (i in 1:2){
 murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>% html_nodes(".review-text") %>% html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}


length(amazon_reviews)
amazon_reviews

#creating the .txt file 
write.table(amazon_reviews,"towerofnero.txt",row.names = F)
# Install package for pre-processiong the data
install.packages("tm")  # for text mining
install.packages(c("SnowballC","textstem")) # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes



library('tm')
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library('textstem')
# Importing book reviews data
x <- as.character(amazon_reviews)
x <- iconv(x, "UTF-8") #Unicode Transformation Format. The '8' means it uses 8-bit blocks to represent a character
# Load the data as a corpus
x <- Corpus(VectorSource(x))
inspect(x[1:3])

# Removing unnecessary symbols like -,;,: etc

toSpace <- content_transformer(function(y,pattern) { return (gsub(pattern, " ",y))})

x1 <- tm_map(x, toSpace, "-")
x1 <- tm_map(x1, toSpace, ";")
x1 <- tm_map(x1, toSpace, ":")
x1 <- tm_map(x1, toSpace, ".")

inspect(x[1])


# Convert the text to lower case
x1 <- tm_map(x, tolower)
inspect(x1[1])
# Remove numbers
x1 <- tm_map(x1, removeNumbers)
# Remove punctuations
x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])
# Remove english common stopwords
x1 <- tm_map(x1, removeWords, stopwords('english'))
# Remove your own stop word
# specify your stopwords as a character vector
inspect(x1[1])
x1 <- tm_map(x1, removeWords, c("the","will","im")) 
inspect(x1[1])
#striping white spaces
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])
# Text stemming
x1<-lemmatize_words(x1)
inspect(x1[1])


# Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
tdm
#Frequency of words
v <- sort(rowSums(tdm),decreasing=TRUE)
v
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Bar plot
w <- rowSums(tdm)
w_sub <- subset(w, w >= 10)
barplot(w_sub, las=3, col = rainbow(30))
# Term watch repeats in all most all documents
x1 <- tm_map(x1, removeWords, c('bit','just'))
x1 <- tm_map(x1, stripWhitespace)
tdm <- TermDocumentMatrix(x1)
tdm <- as.matrix(tdm)
tdm
w1 <- rowSums(tdm)
# Word cloud
#with all the words

wordcloud(words = names(w1), freq = w1, random.order = F, colors = rainbow(20), scale=c(2,.2), rot.per = 0.3)
pos.words = scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words = scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words = c(pos.words,"wow", "kudos", "hurray") # including own positive words to the existing list

# Positive wordcloud
pos.matches = match(names(w), c(pos.words))
pos.matches = !is.na(pos.matches)
freq_pos <- w[pos.matches]
p_names <- names(freq_pos)

wordcloud(p_names,freq_pos,scale=c(3.5,.2),colors = rainbow(20))

# Negative wordcloud
neg.matches = match(names(w), c(neg.words))
neg.matches = !is.na(neg.matches)
freq_neg <- w[neg.matches]
n_names <- names(freq_neg)

wordcloud(n_names,freq_neg,scale=c(3.5,.2),colors = brewer.pal(8,"Dark2"))

# Sentiment Alanysis
tower <- readLines("C:/Users/HP/Desktop/RCodes/Assignments/Text Mining/towerofnero.txt")
library(syuzhet)
library(plotly)
library(tm)
s_v <- get_sentences(tower)

syuzhet <- get_sentiment(s_v, method="syuzhet")
bing <- get_sentiment(s_v, method="bing")
afinn <- get_sentiment(s_v, method="afinn")
nrc <- get_sentiment(s_v, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc)

#anger", "anticipation", "disgust", "fear", "joy", "sadness", 
#"surprise", "trust", "negative", "positive."
sentiments
emotions <- get_nrc_sentiment(tower)
head(emotions)
emo_bar = colSums(emotions)
barplot(emo_bar)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))


# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(syuzhet)]
negative

# and to extract the most positive sentence
positive <- s_v[which.max(syuzhet)]
positive
