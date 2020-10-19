###
# Łukasz Księzak DS 2019/2020 gr.3 

# Eksperyment ma na celu analizę zbioru danych Groceries, dostepnego w pakiecie aRules,
# pod kątem reguł asocjacyjnych. 

#ładowanie bibiliotek
library(arules) # reguły asocjacyjne
library(arulesViz) # wizualizacja reguł

data("Groceries") # wczytanie danych dotyczacych trasakcji sklepu
summary(Groceries)

# Wstępna obserwacja zbioru
freqGroceries  = itemFrequency(Groceries, type = "relative")
summary(freqGroceries)
print(freqGroceries)

freqGroceries = sort(freqGroceries, decreasing= TRUE)
print(freqGroceries[freqGroceries>0.1])

# Uzywam wizualizacji, żeby lepiej zapoznać się ze zbiorem danych:
itemFrequencyPlot(Groceries,topN=25) # wykres przedstawia top25 kupowanych przedmiotów.

# Wykonuję analizę reguł asocjacyjnych Apriori dla danych

# Przyjmuję nastepujace parametry:
aParam  = new("APparameter", "confidence" = 0.5, "support" =0.01, "minlen"= 2, maxtime = 20) 
print(aParam)

rulesApprioriBasic <- apriori(Groceries, parameter=aParam)
summary(rulesApprioriBasic)

# Podjemuję próbę analizy wygenerowanych regułach.
# Wyświetlam top 10 reguł, sortując po współczynniku podniesienia
inspect(head(rulesApprioriBasic, n = 10, by ="lift"))

# Na bazie zaprezentowanych wyników, dostrzegam dosyć proste:
# Klient kupujący cytrusy i warzywa korzeniowe kupi również inne warzywa, (lift 3.02, confidence 0.58)
# Klienci kupujący twaróg i jogurt, sięgną również po pelne mleko (lift 2.27, confidence 0.58)
# itd.
# Istotnym jednak jest tutaj fakt, że zastosowany został współczynnik wsparcia na poziomie 1%.
# Oznacza to, ze oczywistość reguł wynika z faktu, że zakupy warzyw czy nabiału występują najczęściej
# (co mozna zaobserwować na wizualizacji itemFrequencyPlot)

# Celem wyszukania bardziej interesujących reguł z perspektywy biznesowej, zastosuję algorytm apriori 
# z nizszą wartością parametru wsparcia

lessFrequentItemsRules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5, minlen=2))
summary(lessFrequentItemsRules)
inspect(head(lessFrequentItemsRules, n = 10, by ="lift"))

# Powyższy eksperyment pozwala na obserwacje ciekawszych zależności.
# Zaobserowować można, że najwyższym współczynnikiem wsparcia ciesza się transakcje:
# {Instant food products,soda} => {hamburger meat} sup: 0.001220132 conf: 0.6315789 lift: 18.99565
# {soda,popcorn} => {salty snack} sup: 0.001220132  conf: 0.6315789 lift: 16.69779  
# {flour,baking powder} => {sugar} sup: 0.001016777 conf: 0.5555556 lift: 16.40807 

# Już taka analiza pozwala na pierwsze pomysły biznesowe,
# które możnaby wdrożyć celem rozszerzenia listy zakupow u klientów wykonujacych takie transakcje.
# Przykładowa biznesowa koncepcja:
# Transakcja: soda, popcorn -> salty snack: moze być to zestaw kojarzony z wieczorem przy np. książce czy filmie, 
# a więc oprócz napoju gazowanego, popcornu i czipsów blisko zlokalizowałbym produkty z kategorii Lev.2: games/books/hobby
# Inny przykład: transakcja: flour, baking powder -> sugar: wskazuje ze klienci również pieką ciasta, można zatem przygotować
# regał z innymi słodkimi dodatkami do takich wypieków (kategoria chocolate) ew. do popicia dobra kawa (kat. coffee)

# Postarajmy się jednak wyciągnąć więcej informacji na temat wystepujących reguł w tym zbiorze danych.
# Zbiór transakcji lessFrequentItemRules zawiera 5668 wykrytych reguł. Wykorzystajmy metody wizualizacji
# celem wykrycia interesujących zależności

plot(lessFrequentItemsRules, measure = c("support", "lift"), shading = "confidence", jitter = 0) # engine = 'interactive',

# Powyższa wizualizacja pozwala na łatwą identyfikację transakcji o wysokim współczynniku podniesienia.

plot(lessFrequentItemsRules, method = "grouped", control = list(k = 15))

# Powyższy wykres przedstawia pogrupowane wyniki reguł asocjacyjnych na wykresie bąbelkowym.
# Wielkość bąbelka zależy od wsparcia, natężenie koloru od zagregowanego współczynnika podniesienia.
# Po prawej stronie widzimy najczęsciej pojawiający się obiekt RHS, i powiązane z nim reguły i ich ilość.

# Wysoki współczynnik podniesienia dla transakcji ze słoną przekąską oraz ich liczebność wskazują, 
# że klienci generalnie często skłonni są wrzucić do koszyka dodatkową paczkę słonych orzeszków (jako salty snack)
# Uzasadnia to zatem częste ulokowanie tego typu produktów przy kasach, aby klienci przypadkiem o nich nie zapomnieli.

# Przeanalizujmy powyższy zbiór danych używajac algorytmu Eclat, tym razem skupmy się na zbiorach
# z większą ilością elementów 

ecParam  = new("ECparameter", "confidence" = 0.5, "support" = 0.001, "minlen" = 5) 
print(ecParam)
 
rulesEclatBasic <- eclat(Groceries,ecParam)

summary(rulesEclatBasic)
inspect(head(rulesEclatBasic, n = 50, by="support"))

# Metoda eclat pozwala na wykrywanie zbiorów częstych.
# W tym przypadku skupilem się na analize danych z uwzględnieniem większej ilości elemntów w zbiorze (5+). 
# Przy założonej minimalnej wartości współczynnika wsparcia (0.001) i zaufania (0.5) wykrytych zostało:
# 376 zbiorów o liczebności 5
# 10 zbiorów o liczebności 6

# Przyblizając 10 wyników dla zbiorów o liczebności 6:
#[1]  {citrus fruit,tropical fruit,root vegetables,other vegetables,whole milk,yogurt}       0.001423488 14   
#[2]  {tropical fruit,pip fruit,root vegetables,other vegetables,whole milk,yogurt}          0.001321810 13   
#[3]  {tropical fruit,root vegetables,other vegetables,whole milk,yogurt,rolls/buns}         0.001321810 13   
#[4]  {beef,tropical fruit,root vegetables,other vegetables,whole milk,rolls/buns}           0.001118454 11   
#[5]  {tropical fruit,root vegetables,other vegetables,whole milk,butter,yogurt}             0.001118454 11   
#[6]  {tropical fruit,root vegetables,other vegetables,whole milk,yogurt,whipped/sour cream} 0.001118454 11   
#[7]  {tropical fruit,root vegetables,other vegetables,whole milk,yogurt,bottled water}      0.001118454 11   
#[8]  {tropical fruit,root vegetables,other vegetables,whole milk,yogurt,oil}                0.001016777 10   
#[9]  {tropical fruit,other vegetables,whole milk,butter,yogurt,domestic eggs}               0.001016777 10   
#[10] {citrus fruit,root vegetables,other vegetables,whole milk,yogurt,whipped/sour cream}   0.001016777 10 

# Patrząc tylko na te zbiory, nasuwa się obraz najczęsciej kupowanych towarów wśród klientów
# wykonujących 'większe' zakupy. 

# Celem uogólnienia warto byłoby przeanalizować te zakupy pod kątem kategorii tych towarów,
# aby znaleźć biznesową regułę, która pozwolilaby wygenerować najdłuższą scieżkę w sklepie dla takich klientów 
# aby mijając inne kategorie produktów, być może skusili się na dodatkowy zakup.


aggrRules = aggregate(Groceries, by = "level2")
aggrRules@itemInfo 

rulesEclatBasicAggr <- eclat(aggrRules, ecParam)

summary(rulesEclatBasicAggr)
inspect(head(rulesEclatBasicAggr, n = 10, by="support"))

# Powyższa operacja zwraca następujące wyniki:
#[1]  {bread and backed goods,dairy produce,fruit,non-alc. drinks,sausage,vegetables}      0.008947636 88   
#[2]  {bread and backed goods,cheese,dairy produce,fruit,non-alc. drinks,vegetables}       0.007320793 72   
#[3]  {bread and backed goods,cheese,dairy produce,fruit,sausage,vegetables}               0.006914082 68   
#[4]  {bread and backed goods,cheese,dairy produce,fruit,non-alc. drinks,sausage}          0.006100661 60   
#[5]  {bread and backed goods,dairy produce,frozen foods,fruit,sausage,vegetables}         0.005490595 54   
#[6]  {bread and backed goods,dairy produce,frozen foods,fruit,non-alc. drinks,vegetables} 0.005490595 54   
#[7]  {bread and backed goods,dairy produce,fruit,non-alc. drinks,vegetables,vinegar/oils} 0.005185562 51   
#[8]  {bread and backed goods,cheese,dairy produce,non-alc. drinks,sausage,vegetables}     0.005185562 51   
#[9]  {beef,bread and backed goods,dairy produce,fruit,sausage,vegetables}                 0.004677173 46   
#[10] {cheese,dairy produce,fruit,non-alc. drinks,sausage,vegetables}                      0.004575496 45   

# Spośród 55 kategorii zaobserwować można, że wiele z nich nie pojawia się w top 10 zbiorów częstych zaprezenowanych powyżej.
# Jest to pewnego rodzaju potwierdzenie obserwacji z supermarketów, iż te częste kombinacje kategorii
# zazwyczaj nie są fizycznie zlokalizowane blisko siebie


# Aby potwierdzić intuicyjną obserwację przedstawioną powyżej, podejmuję próbę ekperymentu, 
# który na podstawie analizy danych, pozwoli na decyzję gdzie
# powinien znajdować się dział z pieczywem (bread and backed goods) tak, 
# aby klient wykonujacy podstawowe zakupy przeszedł przez większą część sklepu.

# Aby tego dokonać podejmuję następujące kroki:
# Dodaję agregatę na poziomie L2, celem znalezienia reguł częstych dotyczących wielu poziomów 

grocHier <- addAggregate(Groceries, "level2") 
hLevel2 <- paste0(grocHier@itemInfo$level1, "_l2")
grocHier@itemInfo$myH <- as.factor(hLevel2)

grocTFullHier <- addAggregate(grocHier, "myH") 
View(grocTFullHier@itemInfo)

# Podejmuję eksperyment znalezienia reguł asocjacyjnych dla różnych wartości parametrów 
# dla algorytmu apriori przy danych zagregowanych do wielu poziomów

rulesAgg_5perc <- apriori(grocTFullHier, parameter=list(supp=0.05, conf=0.5, minlen=4)) 
summary(rulesAgg_5perc)
inspect(rulesAgg_5perc, by="lift")

rulesAggFH_10perc <- apriori(grocTFullHier, parameter=list(supp=0.1, conf=0.5, minlen=4)) 
summary(rulesAggFH_10perc)
inspect(rulesAggFH_10perc, by="lift")

rulesAggFH_20perc <- apriori(grocTFullHier, parameter=list(supp=0.2, conf=0.5, minlen=3)) 
summary(rulesAggFH_20perc)
inspect(rulesAggFH_20perc, by="lift")

# Szukając optymalnych parametrów dla algorytmu apriori, z tych przetestowanych wybieram (supp=0.1, conf=0.5, minlen=4)

# Filtruję reguły oparte o częste zbiory maksymalne
maxRulesAgg <- rulesAggFH_10perc[is.maximal(rulesAggFH_10perc) == TRUE]
inspect(maxRulesAgg)

rulesInBread<- subset(maxRulesAgg, subset = rhs %in% "bread and backed goods*" & lift >=1)
inspect(rulesInBread)

# Wizualizacja:
plot(rulesInBread, method="graph", shading="lift")
title(main = "Bread")

# Wynik eksperymentu:
#lhs                                                          rhs                       support   confidence lift     count
#[1] {sausage*,fresh products_l2*,meat and sausage_l2*}        => {bread and backed goods*} 0.1036096 0.6507024  1.883360 1019 
#[2] {fruit*,fresh products_l2*,fruit and vegetables_l2*}      => {bread and backed goods*} 0.1075750 0.5300601  1.534179 1058 
#[3] {non-alc. drinks*,drinks_l2*,fresh products_l2*}          => {bread and backed goods*} 0.1242501 0.5472459  1.583921 1222 
#[4] {vegetables*,fresh products_l2*,fruit and vegetables_l2*} => {bread and backed goods*} 0.1162176 0.5195455  1.503746 1143 

# Patrząc na powyższe wyniki obserwuję, że klient zakupujący pieczywo, często kupuje również produkty z 
# kategorii fresh_products, sausage, fruit and vegetables, non_alc drinks, drinks

# Zatem biznesowo podjąbym decyzję o umieszczeniu dzialu z pieczywem na jednym z rogów sklepu (zakładam wielkopowierzchniowość 
# analizowanego sklepu), dzialy z produktami warzywnymi na przeciwległym krańcu, wcześniej dział mięsny 
# i w krańcu przy kasach dział napojów alkoholowych i bezalkoholowych.


# Wnioski z podjętych eksperymentów:

# Ćwiczenie pozwoliło zapoznać się z metodami wyszukiwania reguł częstych na zbiorze danych. 
# Podjęte eksperymenty, dały możliwość zapoznania się z algorytmami eclat i apriori oraz próbami dobierania 
# właściwych parametrów.
# Na uwagę zasługują również metody wizualizacji wyników i ich przydatność w szukaniu wzorców i zrozumienia danych.

# Zaprezentowane na laboratorium metody przybliżyły w jaki sposób można również dane modyfikować aby lepiej je wykorzystać 
# do szukania rozwiązania biznesowego problemu (co zastosowałem w ekperymencie poszukiwania właściwej lokalizacji dla 
# wybranej kategorii produktów). Jest to niezbędna umiejętność, przy pracy z realnymi danymi, gdzie źródeł danych
# może być kilka i do działania algorytmu trzebaby je złączyć w jeden, ubogacony zbiór.

# Podjęte eksperymenty dotyczące dostrajania parametrów algorymtów również dały ogląd na wpływ poszczegónych wartości 
# na otrzymane wyniki. 
# Im wyższe wsparcie, tym silniejsze reguły asocjacyjne ukazujące transakcje na najczęstszych zbiorach. 
# W kontekście analizowanych danych, pozwalało to na obserwacje transakcji z katetegorii 'ogólnych', kojarzących 
# się z codziennymi zakupami w sklepie wielobranżowym. 
# Obniżając parametr wsparcia, ale podnosząc współczynnik podniesienia (lift) ukazywały się kategorie, które 
# wystepuja rzadziej ale są ze sobą silnie powiązane. 
# Pozwoliło to na znalezienie reguł dotyczących chociażby klientów zainteresowanych pieczeniem, 
# co w kontekście biznesowym również jest istotną informacją, gdyż do takich klientów można skierować pewnego rodzaju 
# promocje lub pakiety.

