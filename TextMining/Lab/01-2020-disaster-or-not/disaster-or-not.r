# Przed uruchomieniem na œrodowisku lab09011 proponujê restart sesji R

# https://www.kaggle.com/c/nlp-getting-started/data

# Zbiór danych to tweety w tematyce kataklizmów, nieszczêœæ czy katastrof (disasters)
# Celem konkursu jest nauczenie rozpoznawania czy dany tweet faktycznie opisuje kataklizm czy nie.
# Do tego celu dostarczony zosta³ zbiór oznaczonych danych, na podstawie których wytrenowany zostanie model
# Model zostanie wykorzystany do analizy tweetów dostarczonych jako dane testowe.

# Wczytujê zbiór treningowy i testowy
train_data <- read.csv("./data/train.csv", na.strings = c(""))
test_data <- read.csv("./data/test.csv", na.strings = c(""))

# Eksploracja danych treningowych

# Rzut okiem na 6 pierwszych wierszy
head(train_data)

# Do dyspozycji s¹ nastêpuj¹ce kolumny: id, keyword, location, text, target.
# Ju¿ pierwsze wiersze pokazuj¹, ¿e niektóre kolumny zawieraj¹ puste wiersze
# Oczywiœcie kolumna target zostaje jako zmienna celu, 
# Kolumna id jest niepotrzebna

train_data$id <- NULL
test_data$id <- NULL

# Warto przeanalizowaæ kolumny:
# keyword, location i text pod k¹tem brakuj¹cych danych

missing_train_data <- colSums(sapply(train_data, is.na))
missing_test_data <- colSums(sapply(test_data, is.na))


# Brakuj¹ce dane treningowe:
#keyword location     text   target 
#61     2533        0        0 

# Brakuj¹ce dane testowe:
#keyword location     text 
#26     1105        0 

# Jak to siê ma procentowo do ca³oœci
missing_train_data_prop <- missing_train_data / nrow(train_data)

#keyword   location       text     target 
#0.00801261 0.33272035 0.00000000 0.00000000 

missing_test_data_prop <- missing_test_data / nrow(test_data)
#    keyword    location        text 
# 0.007968127 0.338645418 0.000000000

# Zarówno w danych testowych jak i treningowych obserwuje brak >30% danych w kolumnie location.
# Odrzucam t¹ kolumnê

# Kolumna keyword jest w zdecydowanej wiêkszoœci przypadków dostêpna (99+%), zatem zostawiam do analizy.

train_data$location <- NULL
test_data$location <- NULL

# Zobaczmy jakiego ró¿ne s³owa kluczowe (keywords) wystêpuj¹ w danych treningowych

dist_keyword<-unique(train_data$keyword)
dist_keyword

# Zobaczmy czy keyword w jakiœ sposób bêdzie mia³ zwi¹zek ze zmienn¹ celu
library(dplyr)
train_data$keyword <- sapply(train_data$keyword, as.character)
train_data$keyword[is.na(train_data$keyword)] <- "no category"

by_target <- train_data %>% group_by(keyword) %>% summarize(target_mean = sum(target)/n()) %>% arrange(desc(target_mean))
View(by_target)

# Mo¿na zauwa¿yæ, ¿e istniej¹ s³owa kluczowe, które znacz¹co zwi¹zane s¹ ze zmienn¹ celu:
# 12 s³ów kluczowych z tabeli keyword w 90% jest dodatnio skorelowane ze zmienn¹ celu
# debris (gruzy), derailment (wykolejenie), wreckage, 
# outbreak (wybuch), oil%20spill, typhoon (tajfun), suicide bombing, suicide bomber, bombing, rescuers
# suicide bomb, nuclear disaster

# Z drugiej strony s¹ s³owa kluczowe które wskazuj¹ na wartoœæ zmiennej celu zero:
# smoke, bloody,epicentre,panic,explode,wrecked, blight (zaraza),blew up,panicking,traumatised,screaming
# electrocute, body bag, blazing, ruin, body bags,aftershock


# Wizualizacje

# Analiza dystrybucji tweetów wzglêdem zmiennej celu
library(ggplot2)

tweets_target_dist = train_data %>% group_by(target) %>% summarise(n())

barplot(tweets_target_dist$`n()`, main="Dystrybucja zmiennej celu w zbiorze treningowym",
        names.arg=c("Disaster", "Not disaster"), ylim =c(0, 5000))

# Chmura s³ów przed procesowaniem 
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
# Wyszukiwanie bigramów i ich wizualizacja

#install.packages("tidytext")

library(tidytext)
library(stringi)
# ?unnest_tokens

train_data %>% unnest_tokens(bigrams_in_tweets, text, token = "ngrams", n = 2) %>% count(bigrams_in_tweets, sort = TRUE) %>% slice(1:10) %>% 
  ggplot() + geom_bar(aes(bigrams_in_tweets, n), stat = "identity", fill = "#de5678") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Najwiêcej bigramów w tweetach",
       subtitle = "biblioteka: tidytext")



# Procesowanie tekstu
# ³¹czê zbiory treningowy i testowy w jeden - te same operacje czyszczenia:

all_tweets_df <- bind_rows(train_data, test_data)
all_data_corpus <- Corpus(VectorSource(all_tweets_df$text))

writeLines(strwrap(all_data_corpus[[1325]]$content,60))

# treœæ tweetów do ma³ych liter
all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    stri_trans_tolower))

# Ususniêcie linków (ulr-ów)
remove_urls <- function(x) {
  gsub("http[^[:space:]]*", "", x)  
}

all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    remove_urls))

# Usuniêcie nazw u¿yttkowników
remove_usernames <- function(x){
  gsub("@[^[:space:]]*", "", x)  
}

all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    remove_usernames))

# usuniêcie wszystkiego co nie jest tekstem
remove_anything_but_text <- function(x){
  gsub("[^[:alpha:][:space:]]*", "", x)   
} 
all_data_corpus <- tm_map(
  all_data_corpus, content_transformer(
    remove_anything_but_text))

# stemming s³ów z textów tweetów
all_data_corpus <- tm_map(
  all_data_corpus, stemDocument, "english") 


# Usuniêcie s³ów jedno i dwu wyrazowych
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
# Usuniêcie angielskich stop-words
all_data_corpus <- tm_map(
  all_data_corpus, removeWords, c(Top200Words,"amp", "via", "im")) 

all_data_corpus<- tm_map(all_data_corpus, stripWhitespace)

writeLines(strwrap(all_data_corpus[[1325]]$content,60))

print_word_cloud(all_data_corpus)


### Budowanie modelu
# Powróæmy do podzia³u na zbiór treningowy i testowy

train_data_corpus = all_data_corpus[0:7613]
test_data_corpus = all_data_corpus[7614:10876]

DTM <- DocumentTermMatrix(train_data_corpus)
inspect(DTM[1:10, 50:60])

sparse_DTM <- removeSparseTerms(DTM, 0.995)
tweetsSparse <- as.data.frame(as.matrix(sparse_DTM))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$target <- train_data$target

inspect(sparse_DTM[1:5, 50:60])

# nauka - podzia³ na 70% danych treningowych i 30 testowych
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


