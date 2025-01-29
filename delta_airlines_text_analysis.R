# import delta airlines customer service tweets

options(stringsAsFactors = F)
Sys.setlocale('LC_ALL','C')

install.packages(c("stringi", "wordcloud2", "igraph", "cluster", "NbClust", "factoextra", "openxl", "tm", "stopwords", "stringr", "SnowballC", "textstem", "tm"))

library(tm)
library(SnowballC)
library(textstem)
library(stringi)
library(stringr)
library(qdap)
library(tm)
library(stopwords)
library(igraph)
library(wordcloud2)

delta = read.csv("https://raw.githubusercontent.com/kwartler/text_mining/master/oct_delta.csv", sep=",", header=T)

# look at the columns of data
colnames(delta)

# look at the first 10 tweets
delta[1:10,5]

# some basic string manipulations and calculations
# how many characters is each tweet?
nchar(delta$text)

# calculate the average number of characters in each tweet
mean(nchar(delta$text))

# for now let's just work with the tweets and ignore the other data
text.only = delta$text
text.only

# let's strip out anything that isn't important words
text.only <- gsub(pattern="@\\S+", replacement="", x=text.only)
text.only <- gsub(pattern="https?://\\S+", replacement="", x=text.only)

# first examine the first 5 tweets
first.5.texts.only = text.only[1:5]
first.5.texts.only

# let's do some text cleanup.
# notice the tweets always start with the twitter account of the customer.
# Before doing analysis, we might want to remove these, as not really relevant and it reveals personal cust. info.
# first extract all twitter IDs
unlist(str_extract_all(text.only, "@\\S+"))

# remove twitter IDs from first 5 tweets
text.only <- gsub(pattern="\\S*\\d+\\S*", replacement="", x=text.only) # Remove numbers
text.only <- str_replace_all(text.only,"&amp","") # Remove &amp
text.only <- str_replace_all(text.only, "\\*[a-zA-Z]{2}", "") # Remove initials
text.only <- iconv(text.only,"UTF-8", "ASCII", sub = "") # Remove special characters

# compare before and after: first five tweets
lapply(1:5, function(i){
  rbind(first.5.texts.only[i], (gsub(pattern="@\\S+", replacement="", x=first.5.texts.only))[i]) })

# remove the twitter IDs for all customers
text.only = gsub(pattern="@\\S+", replacement="", x=text.only)
text.only

# it looks like some of the tweets also have websites. let's flag and remove those
# let's use regular expressions to clean this up (regex), which is a powerful language for text manipulation
# google "regular expression" for more info.
has.websites = str_extract_all(text.only, "https?://(\\w*:\\w*@)?[-\\w.]+(:\\d+)?(/([\\w/_.]*(\\?\\S+)?)?)?")
these.have.urls = lengths(has.websites)>0
text.only[these.have.urls]  #show the tweets with web links

table(these.have.urls) # how many of the tweets had websites listed vs. not

# let's check to see if we can strip these out.
text.only = gsub(pattern="https?://(\\S+)", replacement="", x=text.only)

#let's see how successful it was
text.only[these.have.urls]

# notice also that there are quite a few phone numbers in the data and other numbers e.g. 100%
# let's flag any tweets that have at least three numbers listed together e.g. "342"
at.least.three = lengths(str_extract_all(text.only, "[0-9]{3,}"))>0
text.only[at.least.three==TRUE] # view the tweets with numbers

# look at the first 20 tweets involving numbers.
# notice that you get a lot more "stuff" than just phone numbers.
# notice also that some phone numbers are in very non-standard formats, like international numbers
# notice also some phone numbers have spaces while others have "-" "()"
text.only[at.least.three][1:20]

# one option is to strip out any words that have any numbers in them, but you will remove more than phone numbers
all.numbers = str_extract_all(text.only,"\\S*\\d+\\S*")
all.numbers = all.numbers[lengths(all.numbers)>0]
all.numbers

# so let's strip out all numbers from our text
text.only = gsub(pattern="\\S*\\d+\\S*", replacement="", x=text.only)
text.only

# notice there are lots of &amp should remove these too
text.only[str_detect(text.only,"&amp")==TRUE]

# so let's strip out the "&amp" values
text.only = str_replace_all(text.only,"&amp","")

# notice also that each sentence ends with a "*XX" which are the initials of the customer service rep
text.only[1:10]

# we should remove this also as not really important
text.only = str_replace_all(text.only, "\\*[a-zA-Z]{2}", "")
text.only[1:10]

# notice also that some text has weird characters
weird.stuff = text.only[Encoding(text.only)=="UTF-8"]
weird.stuff

# test if we can remove very special characters
iconv(weird.stuff,"UTF-8", "ASCII", sub = "")

# remove for the entire file
text.only = iconv(text.only,"UTF-8", "ASCII", sub = "")

# notice the carriage return special character "\n"
carriage.returns = text.only[str_detect(text.only,"[\n]")==TRUE]
carriage.returns
str_replace_all(carriage.returns,"[\n]","")

# remove for the entire text
text.only = str_replace_all(text.only,"[\n]","")

# notice also that some words are abbreviated
# pls = please, flt = flight, thx = thanks etc.
# let's replace the abbreviations with the full words
has.abbrev = str_detect(tolower(text.only),"pls")|str_detect(tolower(text.only),"flt")|
  str_detect(tolower(text.only),"thx")

text.only[has.abbrev]

# let's replace "pls" with "please", "flt" with "flight", and "thx" with thanks 
mgsub(c("pls","flt","thx"), c("please","flight","thanks"), tolower(text.only[has.abbrev]))
text.only = mgsub(c("pls","flt","thx"), c("please","flight","thanks"), tolower(text.only))

text.only[has.abbrev] # abbreviations are removed


# let's convert all text to lower case so "Hello" is the same as "hello"
text.only = tolower(text.only)
text.only

# let's also get rid of words that might not be really important
# these unimportant words are called "stop words" https://en.wikipedia.org/wiki/Stop_words
# we may want to exclude commonly used words like "the", "and", "a" as they don't really improve the analysis
# you can use pre-built lists or you can make your own custom list of words to exclude
stopwords.list <- c(stopwords("english"), "delta", "dm", "tsa", "atl", "dl", "eta", "lol", "smh")
text.only <- removeWords(text.only, words=stopwords.list)

# but let's say we wanted to add more stopwords
more.words = c("delta","dm","tsa","atl","dl","eta","lol","smh")
more.words

custom.list = c(stopwords('english'),more.words)
custom.list

# an example with only one tweet
text.only[1]
removeWords(text.only[1],words=custom.list)

# now remove all of these words from the entire document
# remove all stop words
text.only = removeWords(text.only, words=custom.list)
text.only

# now let's now remove all punctuation
# show example with first 5 tweets
text.only[1:5]
removePunctuation(text.only[1:5])

# remove for entire file
text.only = removePunctuation(text.only)
text.only

# now let's remove any extra whitespaces
text.only = stripWhitespace(text.only)
text.only = trimws(stripWhitespace(text.only))
text.only

# notice however that some words are the same but with different endings
test = c("appreciate","appreciates","appreciative","appreciating",
         "support","supports","supportive","supporting",
         "apology","apologize","apologizes","apologizing","apologetic")
test

# we can "stem" the words so that we "cut off" the ends of the words, leaving only the roots of the words
# stemming helps to treat all words that are basically the same, as similar

stemmed = stemDocument(test)
stemmed

# notice that when we stem sometimes the root of the words are not real words.
# we can use stem completion to transform the words back to full english words
completed = stemCompletion(stemmed, DICTIONARY[,1], type=c("longest"))
completed

# look at words before stemming, after stemming, and after completed
cat("\n")
results = rbind(test,stemmed,completed)
colnames(results) = paste("Word",1:length(test),sep="")
t(results)

# let's stem the first 10 tweets to make sure we're doing it correctly
# it is always best to work with a small sample when coding to make sure it's correct first
test1 = text.only[1:10]
test1.stemmed = stemDocument(test1)
test1.stemmed

# in order to use the  completion functions, it is best to split each word into a separate string
# split strings for each word into a separate substring 
# no2 split each tweet into separate substrings
test2 = str_split(test1.stemmed,pattern=" ")
test2

# Let's complete the stems for the first 10 tweets and look at the results
completed = lapply(1:10, function(i) {
  stemCompletion( test2[[i]], DICTIONARY[,1], type=c("shortest") ) } )

# notice that there are some words that could not get converted back. These have NA in them.
# most are mispelled words. For now let's exclude them from future analysis.
# a more throrough analysis would try to correct these
completed

# notice also that the sometimes the algorithm converts back to the wrong word
as.vector(test1[6])
as.vector(completed[[6]])          

# notice that in some cases, the stemmed word is the same as the original word.
# for example, "hi" is still "hi" when stemmed
# in these cases, it probably doesn't make sense to complete the word because it's already "complete"
cat(as.vector(test1[[10]]), "\n\n",
as.vector(test1.stemmed[[10]]), "\n\n",
as.vector(completed[[10]]) )

# so one option is to do a simple test:
# if the stemmed word is the same as the original word, don't complete
# otherwise if the stemmed word is NOT the same as the original word, then you can complete

# first let's string split all words
test3 = text.only[1:10]
test3

# first split each tweet into separate words
test3.split = str_split(test3,pattern=" ")
test3[1]
test3.split[1]

# now stem each one of the separate words
test3.stemmed = lapply(1:10,function(i){stemDocument(test3.split[[i]])})
test3.stemmed

# we can look at the before and after
# notice in many cases the stemmed word is the same as the original word
lapply(1:10,function(i){ rbind(test3.split[[i]],test3.stemmed[[i]])})

# so let's only do stem completion if the stemmed word is different from the original word

as.vector(test3.split[[10]])

as.vector(test3.stemmed[[10]])

as.vector(stemCompletion(test3.stemmed[[10]], DICTIONARY[,1], type=c("shortest"))) 

# only complete stem if different from the original word
ifelse(test3.stemmed[[10]]==test3.split[[10]], test3.stemmed[[10]],
       stemCompletion(test3.stemmed[[10]], DICTIONARY[,1], type=c("shortest")))

# let's look at the results all together
as.vector(test3.split[[10]])

as.vector(test3.stemmed[[10]])

ifelse(test3.stemmed[[10]]==test3.split[[10]], test3.split[[10]],
       stemCompletion(test3.stemmed[[10]], DICTIONARY[,1], type=c("shortest")))

# let's run on the entire sample
text.only.split = str_split(text.only,pattern=" ")
text.only.split

# number of tweets in sample
J = length(text.only)

# now stem each one of the separate words for all tweets
text.only.stemmed = lapply(1:J,function(i){stemDocument(text.only.split[[i]])})
text.only.stemmed

# now conditionally complete the stems ONLY IF the stemmed word is different from the original word
text.only.completed = lapply(1:J,function(i){
  ifelse(nchar(text.only.stemmed[[i]]) == nchar(text.only.split[[i]]),  text.only.split[[i]],
    stemCompletion(text.only.stemmed[[i]], DICTIONARY[,1], type=c("longest")) )
} )

# note that some tweets have NA values because some stemmed words were unable 
#to be recovered into completed words 
has.nas = sapply(1:J,function(j){any(is.na(text.only.completed[[j]]))})
table(has.nas)
text.only.completed[has.nas==TRUE]

# we can remove all the NA values using na.omit
# remove words that we can't complete back
text.only.completed.no.nas = lapply(1:J,function(i){as.vector(na.omit(text.only.completed[[i]]))})
text.only.completed.no.nas

# double check no nas
has.nas = sapply(1:J,function(j){any(is.na(text.only.completed.no.nas[[j]]))})
table(has.nas)

# we can remove the NA values which are the words that we can't convert back
# this highlights how time consuming good text analytics and cleaning can take
as.vector(na.omit(completed[[6]]))

# put the individual words for each tweet back together into one string 
final.text = sapply(1:J, function(i){paste(unlist(text.only.completed.no.nas[[i]]),collapse=" ")})
final.text

# let's do some analysis
# let's see which words seem to come up most often in the tweets
all.words = sort(unlist(text.only.completed.no.nas))
freq = table(all.words)
word=names(freq)

# let's do a word cloud

# Extract word frequencies
all.words <- sort(unlist(strsplit(text.only, " ")))
word.freq <- table(all.words)
wordcounts <- data.frame(word=names(word.freq), freq=as.numeric(word.freq))

# Generate word cloud
wordcloud2(wordcounts, size=1.5, shape='square')

# Sentiment Analysis: Identifying Negative Feedback
negative.words <- c("\\bdelay\\b", "\\blong\\b", "\\bslow\\b", "\\bhold up\\b", "\\bwait\\b")
negative.pattern <- paste(negative.words, collapse="|")
has.negativity <- grepl(negative.pattern, x=text.only, ignore.case=FALSE)
table(has.negativity)
text.only[has.negativity==TRUE]
# let's say we wanted to know how many tweets involved slow service
# we could search for terms like "delay", "slow", "hold up" etc.
# let's do this
slow.service = c("\\bdelay\\b","\\blong\\b","\\bslow\\b","\\bhold up\\b","\\bwait\\b")
slow.service.pasted= paste(slow.service,collapse="|")
has.delays = grepl(slow.service.pasted,x=text.only,ignore.case=FALSE)
table(has.delays)
text.only[has.delays==TRUE]


### TDM for Most Frequent Words
library(tm)

ID <- 1:length(text.only)
tweets.df <- data.frame(doc_id=ID, text=text.only)
corpus <- VCorpus(DataframeSource(tweets.df))

# Create Term-Document Matrix
tdm <- TermDocumentMatrix(corpus, control=list(weighting=weightTf))
tdm.matrix <- as.matrix(tdm)

# Extract and visualize frequent words
term.freq <- rowSums(tdm.matrix)
freq.df <- data.frame(word=names(term.freq), frequency=term.freq)
freq.df <- freq.df[order(freq.df$frequency, decreasing=TRUE),]

# Barplot of top 20 frequent words
library(ggplot2)
ggplot(freq.df[1:20,], aes(x=reorder(word, -frequency), y=frequency)) +
  geom_bar(stat="identity", fill="darkred") +
  coord_flip() +
  theme_minimal() +
  labs(title="Top 20 Frequent Words", x="Word", y="Frequency")


## Refund Word Network
refund.tweets <- text.only[grepl("refund", text.only, ignore.case=TRUE)]
refund.corpus <- VCorpus(VectorSource(refund.tweets))
refund.tdm <- TermDocumentMatrix(refund.corpus, control=list(weighting=weightTf))

refund.matrix <- as.matrix(refund.tdm)
refund.adj <- refund.matrix %*% t(refund.matrix)

# Convert to graph
refund.graph <- graph.adjacency(refund.adj, weighted=T, mode="undirected", diag=F)
refund.graph <- simplify(refund.graph)

# Plot Word Network
plot.igraph(refund.graph, vertex.shape="none",
            vertex.label.font=2, vertex.label.color="darkred",
            vertex.label.cex=.7, edge.color="gray85")

title(main="@DeltaAssist Refund Word Network")
