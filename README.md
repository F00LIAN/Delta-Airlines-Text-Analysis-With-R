<p align="center">
<img src="https://resilienteducator.com/wp-content/uploads/2015/09/Revising-With-Pictures-How-Word-Clouds-Help-Students-Become-Better-Writers.jpg" height="60%" width="70%" alt="Delta Airlines Text Analysis"/>
</p>

<h1>Delta Airlines Customer Service Tweet Analysis</h1>
This project performs text mining and natural language processing (NLP) on Delta Airlines customer service tweets using R. The goal is to clean, preprocess, and analyze customer feedback to extract meaningful insights.<br />

<h2>Video Demonstration</h2>

- ### [YouTube: Text Mining in R](https://www.youtube.com/watch?v=wNsKf7wSqhQ)

<h2>Environments and Technologies Used</h2>

- R Programming
- RStudio
- Natural Language Processing (NLP)
- Text Mining Libraries

<h2>Operating Systems Used </h2>

- Windows 11

<h2>List of Prerequisites</h2>

Before running this project, ensure you have:
- R and RStudio installed.
- Required R libraries: `stringi`, `stringr`, `qdap`, `tm`, `wordcloud2`, `ggplot2`, `ggthemes`, `igraph`.
- Access to the dataset (`oct_delta.csv` from GitHub). (We import explicitly in code)
- Downloaded the dataset [Delta Tweets](https://raw.githubusercontent.com/kwartler/text_mining/master/oct_delta.csv)

<h2>Installation Steps</h2>

### 1. Install Required Libraries
```r
install.packages(c("stringi", "stringr", "qdap", "tm", "wordcloud2", "ggplot2", "ggthemes", "igraph"))
```
### 2. Load and Preprocess Data
```r
delta <- read.csv("https://raw.githubusercontent.com/kwartler/text_mining/master/oct_delta.csv", sep=",", header=T)

# Select only tweet text
text.only <- delta$text
```

### 3. Load and Preprocess Data
```r
mean(nchar(text.only))

text.only <- gsub(pattern="@\\S+", replacement="", x=text.only)
text.only <- gsub(pattern="https?://\\S+", replacement="", x=text.only)
```
<p> <img src="https://github.com/user-attachments/assets/string_cleanup.png" height="80%" width="80%" alt="String Cleanup"/> </p>

### 4. Remove Unwanted Elements
```r
text.only <- gsub(pattern="\\S*\\d+\\S*", replacement="", x=text.only) # Remove numbers
text.only <- str_replace_all(text.only,"&amp","") # Remove &amp
text.only <- str_replace_all(text.only, "\\*[a-zA-Z]{2}", "") # Remove initials
text.only <- iconv(text.only,"UTF-8", "ASCII", sub = "") # Remove special characters
```

### 5. Stopword Removal
```r
stopwords.list <- c(stopwords("english"), "delta", "dm", "tsa", "atl", "dl", "eta", "lol", "smh")
text.only <- removeWords(text.only, words=stopwords.list)
```

### 6. Tokenization and Stemming
```r
text.only <- removePunctuation(text.only)
text.only <- stripWhitespace(text.only)
text.only <- tolower(text.only)
text.only.stemmed <- stemDocument(text.only)
```

### 7. Generate a Word Cloud
```r
library(wordcloud2)

# Extract word frequencies
all.words <- sort(unlist(strsplit(text.only, " ")))
word.freq <- table(all.words)
wordcounts <- data.frame(word=names(word.freq), freq=as.numeric(word.freq))

# Generate word cloud
wordcloud2(wordcounts, size=1.5, shape='square')
```
<p> <img src="https://github.com/user-attachments/assets/wordcloud.png" height="80%" width="80%" alt="Word Cloud"/> </p>

### 8. Sentiment Analysis: Identifying Negatice Feedback
```r
negative.words <- c("\\bdelay\\b", "\\blong\\b", "\\bslow\\b", "\\bhold up\\b", "\\bwait\\b")
negative.pattern <- paste(negative.words, collapse="|")
has.negativity <- grepl(negative.pattern, x=text.only, ignore.case=FALSE)
table(has.negativity)
text.only[has.negativity==TRUE]
```
<p> <img src="https://github.com/user-attachments/assets/negative_feedback.png" height="80%" width="80%" alt="Negative Feedback"/> </p>

### 9. Term Document Matrix (TDM) Analysis
```r
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
```
<p> <img src="https://github.com/user-attachments/assets/bar_chart.png" height="80%" width="80%" alt="Bar Chart of Word Frequency"/> </p>

### 10. Word Network for "Refund" Complaints
```r
library(igraph)

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
```
<p> <img src="https://github.com/user-attachments/assets/word_network.png" height="80%" width="80%" alt="Word Network"/> </p>

### 11. Conclusion

- The dataset was cleaned by removing unnecessary characters, Twitter handles, URLs, and stopwords.
- Frequent words were identified using a word cloud and term-document matrix.
- Sentiment analysis was performed to detect complaints about delays, slow service, and refunds.
- A word network was built to analyze associations with refund complaints.

<h2>Future Improvements</h2>

- Implement topic modeling (LDA) for deeper insights.
- Use sentiment scoring (e.g., syuzhet package) to classify tweets.
- Apply machine learning to predict customer satisfaction trends.
