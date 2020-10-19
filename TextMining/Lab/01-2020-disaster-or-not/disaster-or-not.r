# Przed uruchomieniem na �rodowisku lab09011 proponuj� restart sesji R

# https://www.kaggle.com/c/nlp-getting-started/data

# Zbi�r danych to tweety w tematyce kataklizm�w, nieszcz�� czy katastrof (disasters)
# Celem konkursu jest nauczenie rozpoznawania czy dany tweet faktycznie opisuje kataklizm czy nie.
# Do tego celu dostarczony zosta� zbi�r oznaczonych danych, na podstawie kt�rych wytrenowany zostanie model
# Model zostanie wykorzystany do analizy tweet�w dostarczonych jako dane testowe.

# Wczytuj� zbi�r treningowy i testowy
train_data <- read.csv("./data/train.csv", na.strings = c(""))
test_data <- read.csv("./data/test.csv", na.strings = c(""))

# Eksploracja danych treningowych

# Rzut okiem na 6 pierwszych wierszy
head(train_data)

# Do dyspozycji s� nast�puj�ce kolumny: id, keyword, location, text, target.
# Ju� pierwsze wiersze pokazuj�, �e niekt�re kolumny zawieraj� puste wiersze
# Oczywi�cie kolumna target zostaje jako zmienna celu, 
# Kolumna id jest niepotrzebna

train_data$id <- NULL
test_data$id <- NULL

# Warto przeanalizowa� kolumny:
# keyword, location i text pod k�tem brakuj�cych danych

missing_train_data <- colSums(sapply(train_data, is.na))
missing_test_data <- colSums(sapply(test_data, is.na))


# Brakuj�ce dane treningowe:
#keyword location     text   target 
#61     2533        0        0 

# Brakuj�ce dane testowe:
#keyword location     text 
#26     1105        0 

# Jak to si� ma procentowo do ca�o�ci
missing_train_data_prop <- missing_train_data / nrow(train_data)

#keyword   location       text     target 
#0.00801261 0.33272035 0.00000000 0.00000000 

missing_test_data_prop <- missing_test_data / nrow(test_data)
#    keyword    location        text 
# 0.007968127 0.338645418 0.000000000

# Zar�wno w danych testowych jak i treningowych obserwuje brak >30% danych w kolumnie location.
# Odrzucam t� kolumn�

# Kolumna keyword jest w zdecydowanej wi�kszo�ci przypadk�w dost�pna (99+%), zatem zostawiam do analizy.

train_data$location <- NULL
test_data$location <- NULL

# Zobaczmy jakiego r�ne s�owa kluczowe (keywords) wyst�puj� w danych treningowych

dist_keyword<-unique(train_data$keyword)
dist_keyword

# Zobaczmy czy keyword w jaki� spos�b b�dzie mia� zwi�zek ze zmienn� celu
library(dplyr)
train_data$keyword <- sapply(train_data$keyword, as.character)
train_data$keyword[is.na(train_data$keyword)] <- "no category"

by_target <- train_data %>% group_by(keyword) %>% summarize(target_mean = sum(target)/n()) %>% arrange(desc(target_mean))
View(by_target)

# Mo�na zauwa�y�, �e istniej� s�owa kluczowe, kt�re znacz�co zwi�zane s� ze zmienn� celu:
# 12 s��w kluczowych z tabeli keyword w 90% jest dodatnio skorelowane ze zmienn� celu
# debris (gruzy), derailment (wykolejenie), wreckage, 
# outbreak (wybuch), oil%20spill, typhoon (tajfun), suicide bombing, suicide bomber, bombing, rescuers
# suicide bomb, nuclear disaster

# Z drugiej strony s� s�owa kluczowe kt�re wskazuj� na warto�� zmiennej celu zero:
# smoke, bloody,epicentre,panic,explode,wrecked, blight (zaraza),blew up,panicking,traumatised,screaming
# electrocute, body bag, blazing, ruin, body bags,aftershock


# Wizualizacje

# Analiza dystrybucji tweet�w wzgl�dem zmiennej celu
library(ggplot2)

tweets_target_dist = train_data %>% group_by(target) %>% summarise(n())

barplot(tweets_target_dist$`n()`, main="Dystrybucja zmiennej celu w zbiorze treningowym",
        names.arg=c("Disaster", "Not disaster"), ylim =c(0, 5000))

# Chmura s��w przed procesowaniem 
library(tm)
library(wordcloud)
#install.packages('corpus')
library(corpus)


print_word_cloud <- function(tweetCorpus){
  train_tdm <- TermDocumentMatrix(tweetCorpus[1:7613], control= list(wordLengths= c(1, Inf)))
  (freq.terms <- findFreqTerms(train_tdm, lowfreq = 50))
  train.term.freq <- rowSums(as.matrix(train_tdm))
  train.term.freq <- subset(train.term.freq, train.term.freq > 50)
  train_df <- data.frame(term = names(train.term.freq), freq= train.term.freq)
  
  word.freq <-sort(rowSums(as.matrix((train_tdm))), decreasing= F)
  pal<- brewer.pal(8, "Dark2")
  wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, random.order = F, colors = pal, max.words = 100)
}

tweet_corpus<- Corpus(VectorSource(train_data$text))
print_word_cloud(tweet_corpus)
# Wyszukiwanie bigram�w i ich wizualizacja

#install.packages("tidytext")

library(tidytext)
library(stringi)
# ?unnest_tokens

train_data %>% unnest_tokens(bigrams_in_tweets, text, token = "ngrams", n = 2) %>% count(bigrams_in_tweets, sort = TRUE) %>% slice(1:10) %>% 
  ggplot() + geom_bar(aes(bigrams_in_tweets, n), stat = "identity", fill = "#de5678") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Najwi�cej bigram�w w tweetach",
       subtitle = "biblioteka: tidytext")



# Procesowanie tekstu
# ��cz� zbiory treningowy i testowy w jeden - te same operacje czyszczenia:

all_tweets_df <- bind_rows(train_data, test_data)
all_data_corpus <- Corpus(VectorSource(all_tweets_df$text))

writeLines(strwrap(all_data_corpus[[1325]]$content,60))

# tre�� tweet�w do ma�ych liter
all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    stri_trans_tolower))

# Ususni�cie link�w (ulr-�w)
remove_urls <- function(x) {
  gsub("http[^[:space:]]*", "", x)  
}

all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    remove_urls))

# Usuni�cie nazw u�yttkownik�w
remove_usernames <- function(x){
  gsub("@[^[:space:]]*", "", x)  
}

all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    remove_usernames))

# usuni�cie wszystkiego co nie jest tekstem
remove_anything_but_text <- function(x){
  gsub("[^[:alpha:][:space:]]*", "", x)   
} 
all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    remove_anything_but_text))

# stemming s��w z text�w tweet�w
all_data_corpus <- tm_map(
  all_data_corpus, stemDocument, "english") 


# Usuni�cie s��w jedno i dwu wyrazowych
remove_one_char <- function(x){
  gsub(" . ", " ", x)   
} 
remove_two_chars <- function(x){
  gsub(" .. ", " ", x)   
}

all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(remove_one_char))
all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(remove_two_chars))

#install.packages('qdap')
library(qdap)
# Usuni�cie angielskich stop-words
all_data_corpus <- tm_map(
  all_data_corpus, removeWords, c(Top200Words,"amp", "via", "im")) 

all_data_corpus<- tm_map(all_data_corpus, stripWhitespace)

writeLines(strwrap(all_data_corpus[[1325]]$content,60))

print_word_cloud(all_data_corpus)


### Budowanie modelu
# Powr��my do podzia�u na zbi�r treningowy i testowy

train_data_corpus = all_data_corpus[0:7613]
test_data_corpus = all_data_corpus[7614:10876]

DTM <- DocumentTermMatrix(train_data_corpus)
inspect(DTM[1:10, 50:60])

sparse_DTM <- removeSparseTerms(DTM, 0.995)
tweetsSparse <- as.data.frame(as.matrix(sparse_DTM))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$target <- train_data$target

inspect(sparse_DTM[1:5, 50:60])

# nauka - podzia� na 70% danych treningowych i 30 testowych
smp_size <- floor(0.7 * nrow(tweetsSparse))
set.seed(123)
train_ind <- sample(seq_len(nrow(tweetsSparse)), size = smp_size)
train <- tweetsSparse[train_ind, ]
test <- tweetsSparse[-train_ind, ]

#install.packages('rpart')

# ternowanie modelu - drzewo decyzyjne
library('rpart')

tweetCART <- rpart(target ~ . , data = train, method = "class")

predictCART <- predict(tweetCART, newdata = test, type = "class")
cmat_CART <- table(test$target, predictCART)
cmat_CART 
accu_CART <- (cmat_CART[1,1] + cmat_CART[2,2])/sum(cmat_CART)
accu_CART


# trenowanie modelu - SVM

model_svm <- e1071::svm(target ~ ., 
                        data = train, type = "nu-classification", kernal = "gaussian")


predictions_svm <- predict(model_svm, test)
truth_table <- table(test$target, predictions_svm)
truth_table

accu_svm <- (truth_table[1,1] + truth_table[2,2])/sum(truth_table)
accu_svm


