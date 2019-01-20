# Loading in the libraries
library(NLP)
library(RWeka)
library(stringi)
library(tm)
library(ggplot2)
library(data.table)
library(SnowballC)

# Loading in the data
blogs_txt <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news_txt <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter_txt <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

# remove non-ASCII
blogs_txt <- iconv(blogs, "latin1", "ASCII", sub="")
news_txt <- iconv(news, "latin1", "ASCII", sub="")
twitter_txt <- iconv(twitter, "latin1", "ASCII", sub="")

# sampling the data
set.seed(1)
sample_data <- c(sample(blogs_txt, length(blogs_txt) * 0.04),
                 sample(news_txt, length(news_txt) * 0.04),
                 sample(twitter_txt, length(twitter_txt) * 0.04)
)

# Building the corpus, convert to lower case, remove all numbers, punctuations, white stripes without function and create plain text
corpus <- VCorpus(VectorSource(sample_data))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

# Tokenization for uniqrams, bigrams and trigrams
uni_token <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bi_token <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tri_token <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tetra_token <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

# Building the document matrices of uniqrams, bigrams and trigrams.
uni_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = uni_token))
bi_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = bi_token))
tri_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = tri_token))
tetra_matrix <- TermDocumentMatrix(corpus, control = list(tokenize = tetra_token))

# Calculating the frequencies of the NGrams
uni_corpus <- findFreqTerms(uni_matrix,lowfreq = 65)
bi_corpus <- findFreqTerms(bi_matrix,lowfreq=65)
tri_corpus <- findFreqTerms(tri_matrix,lowfreq=65)
tetra_corpus <- findFreqTerms(tetra_matrix,lowfreq=65)

uni_corpus_freq <- sort(rowSums(as.matrix(uni_matrix[uni_corpus,])), decreasing = TRUE)
bi_corpus_freq <- sort(rowSums(as.matrix(bi_matrix[bi_corpus,])), decreasing = TRUE)
bi_corpus_freq <- data.table(word=names(bi_corpus_freq), frequency=bi_corpus_freq)
tri_corpus_freq <- sort(rowSums(as.matrix(tri_matrix[tri_corpus,])), decreasing = TRUE)
tri_corpus_freq <- data.table(word=names(tri_corpus_freq), frequency=tri_corpus_freq)
tetra_corpus_freq <- sort(rowSums(as.matrix(tetra_matrix[tetra_corpus,])), decreasing = TRUE)
tetra_corpus_freq <- data.table(word=names(tetra_corpus_freq), frequency=tetra_corpus_freq)

save(uni_corpus_freq, file = "txt_uni_word.RData")
save(bi_corpus_freq , file = "txt_bi_words.RData")
save(tri_corpus_freq , file = "txt_tri_words.RData")
save(tetra_corpus_freq , file = "txt_tetra_words.RData")