# Zadanie dotyczące klasyfikacji jakości wina.

# W ćwiczeniu podjąłem próbę klasyfikacji jakości wina, na podstawie zbioru wine_red.csv
# Na początku zapoznałem sie ze zbiorem. 
# Upatrując jako zmienną celu jakość wina, dodałem nowa kolumnę 'likeable' (binarną)
# Następnie dokonałem podziału zbioru na treningowy i testowy w proporcji 70:30
# Do klasyfikacji użyte zostały następujące algorytmy:
# C5.0, C5.0 (rule-based), rpart, randomForest, SVM
# Dla przypadku lasów losowych dokonana została eksperymentalna próba optymalizacji parametrów modelu.
# Każdy model został zweryfikowany z użyciem zbioru testowego.
# Na końcu widać rezultaty - porównie dokładności wykorzystanych algorytmów.

# Biblioteki

library(gmodels) #bib. do analizy wyników
library(Hmisc) #bib. do analizy wyników
library(party) #bib. zawierającca ctree()
library(caret) #bib. do reprezentacji graficznej
library(rpart) #bib. zawierająca rpart()
library(rpart.plot) 
library(e1071) #bib. zawierająca klasyfik. Bayesa
library(C50) 
library(randomForest)
library(e1071)

#Utworzenie klasyfikatora dla zbioru danych: wines ? dane i opis dost?pne pod adresem: http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');

wineRed_ds = read.csv("wine_red.csv", header = TRUE, sep=";", na.strings= "*")


# Zbiór zawiera nastepujące kolumny
#fixed.acidity volatile.acidity citric.acid residual.sugar chlorides free.sulfur.dioxide total.sulfur.dioxide density pH sulphates alcohol quality

head(wineRed_ds)
summary(wineRed_ds)

# Zobaczmy dystrybucję jakości wina dostępnego w zbiorze
hist(wineRed_ds$quality)

wineRed_ds$likeable<-ifelse(wineRed_ds$quality>=7,1,0)


wineRed_ds$quality <- NULL # Usuwam kolumnę quality, żeby nie została wykorzystana do klasyfikacji

#podział na zbiór treningowy i testowy
trainData <- unlist(createDataPartition(wineRed_ds$likeable,p=0.7))# dzielimy zbiory proporcjonpnie.

wine_train <-wineRed_ds[trainData,]
wine_test <-wineRed_ds[-trainData,]
dim(wine_train)
dim(wine_test)

# Klasyfikator C50

wine_C50 <- C5.0(as.factor(wine_train$likeable)~., data = wine_train ) 
summary(wine_C50)
plot(wine_C50)

# wygenerowane drzewo dcyzyjne ma 24 liście. 
# Do klasyfikacji użyte zostały nastepujące atrybuty:
#100.00%	alcohol
#85.18%	volatile.acidity
#19.91%	sulphates
#14.91%	total.sulfur.dioxide
#9.91%	free.sulfur.dioxide
#7.68%	pH
#5.36%	fixed.acidity
#2.95%	residual.sugar
#2.50%	density


#zbadajmy jakość klasyfikacji używając danych treningowych
wine_c50_trainPred <- predict(wine_C50, wine_train[,-12])


CrossTable(wine_c50_trainPred, wine_train$likeable, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))


confusionMatrix(table(wine_c50_trainPred, wine_train$likeable))

# Tablica prawdy dla danych treningowych wynosi 94%. Oznacza to, że drzewo wygenerowane przez klasyfikator C5.0 
# nie dokonało właściwie predykcji dla prawie 6% danych. Zobaczmy jednak na ile dobrze problem klasyfikacji
# został uogólniony z wykorzystaniem danych treningowych, badając jakość klasyfikacji z danymi testowymi.


wine_c50_testPred <- predict(wine_C50, wine_test[-12])

CrossTable(wine_c50_testPred, wine_test$likeable, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(table(wine_c50_testPred, wine_test$likeable))

# Drzewo całkiem dobrze uogólniło problem, dla danych testowych, otrzymana dokładność wynosiła 89,5%


# Do kolejnej klasyfikacji wykorzystany będzie algorytm C 5.0 z wykorzystaniem reguł (rules=True)

wine_C50R <- C5.0(as.factor(wine_train$likeable)~., data = wine_train,  rules = TRUE) 
summary(wine_C50R)

#jakość klasyfikacji -  dane testowe
wine_c50_testPred <- predict(wine_C50R, wine_test[-12])
CrossTable(wine_c50_testPred, wine_test$likeable, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(table(wine_c50_testPred, wine_test$likeable), mode="everything")

# Wykorzystanie algorytmu C5.0 z regułami poprawiło minimalnie jakość klasyfikacji - 
# Dokładność w tym przypadku wyniosła 0.8998  


# Spróbujmy dokonać predykcji wykorzystując rodzinę drzew

wine_C50B <- C5.0(as.factor(wine_train$likeable)~., data = wine_train, trials = 10) 

wine_C50B_testPred = predict(wine_C50B, wine_test[-12])
confusionMatrix(table(wine_C50B_testPred, wine_test$likeable), mode="everything")
summary(wine_C50B)


# W rodzinie drzew wykorzystanych zostało 10 drzew. Użycie cech wykorzystanych w klasyfikacji 
# rozkładało się nastepująco:

#100.00%	volatile.acidity
#100.00%	sulphates
#100.00%	alcohol
#99.46%	pH
#99.02%	total.sulfur.dioxide
#83.12%	residual.sugar
#76.07%	chlorides
#75.98%	fixed.acidity
#74.73%	free.sulfur.dioxide
#66.79%	density
#55.09%	citric.acid

# drzewa mialy rozmiar od 14 do 37 lisci. 

# Dokonując oceny klasyfikacji na zbiorze testowym osiągnięto dokładność na poziomie 89.9 procenta.
# Jest to bardzo zbliżona wartość dokładności, w zestawieniu z c5.0 z regułami.

# Kolejnym klasyfikatorem do analizy jakości wina będzie las losowy
wine_forest = randomForest(factor(likeable)~., data = wine_train, importance = TRUE, nodesize = 10, mtry = 12, ntree = 100 )

# Opis argumentow metody ze skryptu dostarczonego do laboratorium:
# nodesize = minimalna liczba obiektów w węzłach liściach
# mtry - licza  losowo  wybieranych atrybutów, które są brane pod uwagę w szukaniu najlepszego testu do podziału danych w węzłach
# ntree - liczba drzew w lesie
# importance - wyznaczenie wskaźnikóW ważności atrybutów w klasyfikacji


print(wine_forest)
plot(wine_forest)

# Z wykresu można zauważyć że błąd zatrzymał się na mniej więcej stałym poziomie przy lesie z 30 drzewami.

round(importance(wine_forest, type = 1),2)

# Analiza ważności cech w zbiorze win czerwonych wskazuje, że najistotniejszym czytnnikiem jest alkohol.
# Ważność pozostałych parametrów rozkłada się w nastepujący sposób:

#fixed.acidity          15.54
#volatile.acidity       22.96
#citric.acid            14.21
#residual.sugar         12.96
#chlorides              12.97
#free.sulfur.dioxide    15.68
#total.sulfur.dioxide   16.50
#density                21.98
#pH                     14.04
#sulphates              30.38
#alcohol                45.07


# Ewaluacja modelu lasów losowych
wine_forest_testPred = predict(wine_forest, newdata = wine_test[-12])
confusionMatrix(table(wine_forest_testPred, wine_test$likeable))

# Tak sparametryzowany model lasu losowego pozwolił na uzyskanie dokładności równej 88,3%

# Przy użyciu k-krotnej walidacji podjemuję próbę optymalizacji parametrów

trControl <- trainControl(method = "cv", number = 10, search = "grid")

tuneGrid <- expand.grid(mtry = c(1:12))

wine_forest_with_cv <- train(factor(likeable)~.,  data = wine_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,    
                            nodesize = 10,        
                            ntree = 250)          
print(wine_forest_with_cv)

# Osiagnięta maksymalna dokładność wynosi 90,89% dla mtry=12


# Próba optymializacji liczbności lasu
treesModels <- list()
for (nbTree in c(5,10,25, 50, 100, 250, 500)) 
{
  wine_F_maxtrees <- train(factor(likeable)~.,  data = wine_train,
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tuneGrid,
                          trControl = trControl,
                          importance = TRUE,
                          nodesize = 10,
                          ntree = nbTree)
  key <- toString(nbTree)
  treesModels[[key]] <- wine_F_maxtrees
}

results_tree <- resamples(treesModels)
summary(results_tree)
# Najlepszy wynik uzyskano dla nbTrees = 500
#5   0.8558559 0.8683036 0.8973214 0.8919146 0.9084821 0.9375000    0
#10  0.8660714 0.8772321 0.9017857 0.8964431 0.9148529 0.9196429    0
#25  0.8571429 0.8794643 0.8973214 0.8991071 0.9174107 0.9375000    0
#50  0.8839286 0.9017857 0.9017857 0.9035780 0.9107143 0.9189189    0
#100 0.8761062 0.8861607 0.8928571 0.8964668 0.9062500 0.9196429    0
#250 0.8660714 0.8794643 0.9062500 0.9000000 0.9174107 0.9375000    0
#500 0.8928571 0.9017857 0.9062824 0.9098279 0.9174107 0.9375000    0

#końcowy model
wine_forest_final = randomForest(factor(likeable)~., data = wine_train, importance = TRUE, mtry = 12, ntree = 500, nodesize = 10)

print(wine_forest_final)
plot(wine_forest_final)

wine_forest_final_testPred = predict(wine_forest_final, newdata = wine_test[-12])
confusionMatrix(table(wine_forest_final_testPred, wine_test$likeable), mode = "everything")
# Las losowy w zoptymalizowanej formie osiągnął dokladność na poziomie 88,1%.
# Optymalizacja ilości drzew jak i losowych cech branych do drzewa nie poprawiła wyraźnie wyników.
# Pierwsza próba okazała się być całkiem niezłym intucyjnym doborem parametrów do trenowania.

varImpPlot(wine_forest_final)


wine_rpart <- rpart(factor(likeable)~., data=wine_train)
wine_rpart_testPred = predict(wine_rpart, wine_test[-12], type = "class")
confusionMatrix(table(wine_rpart_testPred, wine_test$likeable), mode = "everything")
# rpart: dokładność: 88,31%


# Jako dodatkową klasyfikację, użyję algorytmu SVM z jądrem gaussowskim

modelSvm <- svm(factor(likeable)~., data = wine_train, kernel='radial', cost=50)
svm_wine_predict = predict(modelSvm, wine_test[-12])
tab = table(svm_wine_predict, wine_test$likeable)
svmConfusionMatrix = confusionMatrix(tab)
svmConfusionMatrix

# klasyfikacja z wykorzystaniem algorytmu SVM. Uzyskana dokladność: 88,1%

# Porównianie klasyfikatorów 

classifier = c('C50', 'C50_rules' ,'rpart',  'random_forest', 'svm')
acc = c( mean(wine_c50_testPred == wine_test$likeable),
                mean(wine_C50B_testPred == wine_test$likeable),
                mean(wine_rpart_testPred == wine_test$likeable),
                mean(wine_forest_final_testPred == wine_test$likeable),
                mean(svm_wine_predict == wine_test$likeable))

classification_results <- data.frame(classifier, acc)
classification_results

# Na podstawie powyższych wyników, najlepszym klasyfikatorem okazał się 
# klasyfikator C5.0.
# W tej dwuklasowej klasyfikacji, w której zmienną celu była jakość wina
# uproszczona do poziomu 0 - wino o poziomie jakości <7, i 1 >= 7
# klasyfikatory z rodziny drzew i lasów losowych okazały się być skutecznymi klasyfikatorami.


