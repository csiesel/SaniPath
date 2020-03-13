library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

text <- readLines('H:/SaniPath/Workshop_Notes.txt')
docs <- Corpus(VectorSource(text))
inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
#docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Making word cloud
set.seed(1234)
png("wordcloud_packages.png", width=12,height=8, units='in', res=1000)
#par(bg="#eeeeee")
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

cloud <- wordcloud2(d, gridSize=0, size=0.8, minSize=.1, color='random-dark', backgroundColor='#eeeeee', fontFamily='calibri', shuffle=TRUE,minRotation = -pi/2, maxRotation = pi/6,
                    rotateRatio = .9)

cloud

# #install webshot
# library(webshot)
# webshot::install_phantomjs()
# # Make the graph
# my_graph=wordcloud2(demoFreq, size=1.5)
# # save it in html
# library("htmlwidgets")
# saveWidget(cloud,"tmp.html",selfcontained = F)
# # and in png
# webshot("tmp.html","fig_1.png", delay =5, vwidth = 800, vheight=800) # changed to png.
# 
