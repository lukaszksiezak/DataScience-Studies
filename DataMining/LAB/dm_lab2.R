#DataScience  - data mining 2019z Lab2
#Wykrywanie wzorców i reguł sekwencyjnych
#autor: Grzegorz Protaziuk, Robert Bembenik 

#ustawienie katalogu roboczego
setwd('/home/elektron/d/70/gprotazi/DS2019z');
setwd("~/DM")

#ładowanie bibiliotek
library(arules)
library(arulesSequences)

#wczytanie sekwencji - etykiet przypisanych przez użykownika .
download.file("http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/tags.data", "tags.data")

?read_baskets
dataSeq <- read_baskets(con = file("tags.data", "r"), info = c("sequenceID","eventID","SIZE"))
str(dataSeq)
View(dataSeq)

#statystyki
summary(dataSeq)

#transformacja danych do postaci tabelarycznej (data.frame)
frameS =   as(dataSeq,"data.frame")
View(frameS)

#informacje związane z czasem 
?timeFrequency
timeSeq  = as(dataSeq,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
freqT

#odstęp miedzy zdarzeniami
spanT = timeFrequency(timeSeq, c("span"))
spanT

#obliczenie częstości elementów
#liczba różnych elementów
?nitems
nitems(dataSeq)
str(dataSeq)

#obliczenie częstości elementów
?itemFrequency
freqItem = itemFrequency(dataSeq)
#str(freqItem)
freqItem = sort(freqItem, decreasing = TRUE )
head(freqItem,20)

#ids <- unique(dataSeq@itemsetInfo$sequenceID)
#transakcje dla zadanej sekwencji
trans109 <- dataSeq[dataSeq@itemsetInfo$sequenceID == 109]
#trans109 <- dataSeq[dataSeq@transactionInfo$sequenceID == 109]
inspect(trans109)

#Odkrywanie częstych sekwencji
#Parametry algorymtu SPADE
?'SPparameter-class'
seqParam = new ("SPparameter",support = 0.003, maxsize = 5, mingap=1, maxgap =3, maxlen = 5)
print(seqParam)

#Wykonanie algorytmu SPADE przy użyciu funkcji cspade
?cspade
patSeq= cspade(dataSeq,seqParam, control = list(verbose = TRUE, tidLists = TRUE, summary= TRUE))

#informacje dotyczące wygenerowanych sekwencji
summary(patSeq)
inspect(patSeq)
#length(patSeq)
#str(patSeq)
size(patSeq)
inspect(patSeq[324:354])
#sekwencje  przynajmniej z dwoma składowymi

seq2elem <- patSeq[size(patSeq)>1]
length(seq2elem)
inspect(seq2elem)

#wzorce wspierana przez daną sekwencje (bez ograniczeń związanych z czasem)
inspect(patSeq[which(support(patSeq,trans109,type='absolute') >0)])

#wyszukiwanie wzorców z zadanymi elementami
?subset
?match

seqI = subset(patSeq, x %ein% c('design','art'))
inspect(seqI)
View(as(seqI,"data.frame"))

#wykonanie algorytmu z innym zestawem paramatrów
seqParam1 = new ("SPparameter",support = 0.001, maxsize = 5, mingap=1, maxgap =5, maxlen = 5 )
patSeq1= cspade(dataSeq,seqParam1, control = list(verbose = TRUE, tidLists = TRUE, summary= TRUE))

summary(patSeq1)

#wybór nowo odkrytych wzorców
seqdiff = patSeq1[which(!(patSeq1 %in% patSeq))]

length(patSeq1)
length(seqdiff)

#generowanie reguł na podstawie sekwencji częstych
?ruleInduction
seqRules = ruleInduction(patSeq1,confidence = 0.8)

#statystyki dotyczące wykrytych reguł
summary(seqRules)
str(seqRules)
#length(seqRules)


#pokazanie reguł
inspect(seqRules)
arulesSequences::size(seqRules)
size(lhs(seqRules))
inspect(lhs(seqRules))
inspect(rhs(seqRules))

#lista elementów występujących w sekwencjach
seqRules@elements@items@itemInfo

#wszystkie sekwencje
allSeq <- c(rhs(seqRules),lhs(seqRules))
allSeq <- unique(allSeq)
inspect(allSeq)
str(allSeq)

#wyszukiwanie reguł
rulesI = subset(seqRules, lhs(seqRules) %in% c('as3','design','webdesign') | rhs(seqRules) %in% c('as3','design','webdesign'))
inspect(rulesI)
View(as(rulesI,"data.frame"))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign'))
inspect(rulesI)

##########################################################
#Przykład wykrycia reguł sekwencyjnych ze zbioru   diab_trans.

#https://archive.ics.uci.edu/ml/datasets/Diabetes
#pobranie danych
download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#wczytanie danych do ramki danych
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)

#Znalezd zależnoci jeżeli co tam i co tam w odpowiednim przedziale czasowym to mamy hiperglikemie

#Trzeba zamienic kody na nazwy 
#osobno dyskretyzujemy dawki insuliny i pomiary poziomu cukru
#podzielic wartosci na 3 kategorie - normalny, ponizej normy, powyzej normy 
#sklejenie zazwy pomiaru i wartosci, np: pomiar cukry przed sniadaniem : niski (code:value)
#do dyskretyzacji warto zobaczyć histogramy

library('plyr')

#przykladowe kody
diab.df[diab.df['code']=='id_33',]
diab.df[diab.df['code']=='id_67',]
diab.df[diab.df['code']=='id_66',]

id_list <- c('id_33',
'id_34',
'id_35',
'id_48' ,
'id_57' ,
'id_58' ,
'id_59' ,
'id_60' ,
'id_61' ,
'id_62' ,
'id_63' ,
'id_64' ,
'id_65',
'id_66' ,
'id_67' ,
'id_68' ,
'id_69' ,
'id_70' ,
'id_71' ,
'id_72' )

map_id <- c('insulin dose',
'insulin dose',
'insulin dose',
'Unspecified blood glucose measurement',
'Unspecified blood glucose measurement',
'Pre-meal blood glucose measurement',
'Post-meal blood glucose measurement',
'Pre-meal blood glucose measurement',
'Post-meal blood glucose measurement',
'Pre-meal blood glucose measurement',
'Post-meal blood glucose measurement',
'Pre-meal blood glucose measurement',
'Hypoglycemic symptoms',
'Typical meal ingestion',
'More-than-usual meal ingestion',
'Less-than-usual meal ingestion',
'Typical exercise activity',
'More-than-usual exercise activity',
'Less-than-usual exercise activity',
'Unspecified special event')

#mapowanie kodow
diab.df['code_map'] <- mapvalues(x=as.vector(diab.df[['code']]), from = id_list, to=map_id , warn_missing = TRUE)

#histogramy
hist(as.integer(unlist(diab.df[diab.df['code_map']=='Pre-meal blood glucose measurement',]['value']))) #50-200
hist(as.integer(unlist(diab.df[diab.df['code_map']=='Post-meal blood glucose measurement',]['value'])))#50-300
hist(as.integer(unlist(diab.df[diab.df['code_map']=='Unspecified blood glucose measurement',]['value']))) #50-200
hist(as.integer(unlist(diab.df[diab.df['code_map']=='insulin dose',]['value'])),xlim=c(0,50),breaks=c(0,10,20,30,40,500)) #10

diab.df <- diab.df[complete.cases(diab.df),] #usuniecie NA

#dyskretyzacja
diab.df = transform(diab.df, value = as.numeric(value))
diab.df['text_value'] <- ' '
diab.df[diab.df['code_map']=='Pre-meal blood glucose measurement' & diab.df['value']<50,]['text_value'] <- '_LOW'
diab.df[diab.df['code_map']=='Pre-meal blood glucose measurement' & diab.df['value']>=50 & diab.df['value']<=200,]['text_value'] <- '_NORMAL'
diab.df[diab.df['code_map']=='Pre-meal blood glucose measurement' & diab.df['value']>200,]['text_value'] <- '_HIGH'
diab.df[diab.df['code_map']=='Post-meal blood glucose measurement' & diab.df['value']<50,]['text_value'] <- '_LOW'
diab.df[diab.df['code_map']=='Post-meal blood glucose measurement' & diab.df['value']>=50 & diab.df['value']<=300,]['text_value'] <- '_NORMAL'
diab.df[diab.df['code_map']=='Post-meal blood glucose measurement' & diab.df['value']>300,]['text_value'] <- '_HIGH'
diab.df[diab.df['code_map']=='insulin dose' & diab.df['value']<=10,]['text_value'] <- '_NORMAL'
diab.df[diab.df['code_map']=='insulin dose' & diab.df['value']>10,]['text_value'] <- '_HIGH'
diab.df[diab.df['code_map']=='Unspecified blood glucose measurement' & diab.df['value']<50,]['text_value'] <- '_LOW'
diab.df[diab.df['code_map']=='Unspecified blood glucose measurement' & diab.df['value']>=50 & diab.df['value']<=200,]['text_value'] <- '_NORMAL'
diab.df[diab.df['code_map']=='Unspecified blood glucose measurement' & diab.df['value']>200,]['text_value'] <- '_HIGH'

#polaczenie kodu i wartosci
diab.df['code_final'] <- apply( diab.df[ , c('code_map','text_value') ] , 1 , paste , collapse = "" )

#finalny zbior
diab.df_final = diab.df[c('patient_id','time_sek','code_final')]


#ANALIZA

#przykład zapisu danych do pliku
write.table(diab.df_final, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#wczytanie danych w postaci transkacji 
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))

#ustawienie parametrów
seqParam = new ("SPparameter",support = 0.2, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#odkrycie reguł
seqRules = ruleInduction(patSeq,confidence = 0.8)

length(seqRules)
#podsumowanie 
summary(seqRules)
#prezentacja przykładowych reguł
inspect(head(seqRules,100))


inspect(rhs(seqRules))
rulesI = subset(seqRules, rhs(seqRules) %in% c('Hypoglycemic symptoms'))
rulesI = subset(seqRules, lhs(seqRules) %in% c('Hypoglycemic symptoms'))
inspect(rulesI)
View(as(rulesI,"data.frame"))