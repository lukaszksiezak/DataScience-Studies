#### Zadanie 1:

# a) Im niższe C tym model bardziej pozwalamy na błędy klasyfikacji (mniejsza kara)

# b) Dla m x p zbioru treningowego, gdzie p to liczba przypadków treningowych
# obliczenia dla jadra RBF obliczenia będą wykonane tej samej przestrzeni co zbior, a więc p (Kernel trick)
# (Odpowiedz bazuje na tekście: https://towardsdatascience.com/truly-understanding-the-kernel-trick-1aeb11560769)

# c) Tak SVM jest klasyfikatorem liniowym 

#### Zadanie 2:

library(e1071)

# koty
summary(cats)
dim(cats)
pairs(cats)

# a) zmienne: plec, masa ciala, masa serca.
# plec - binary - M/F
# BWT - float - 2.0 - 4.0
# HWT - float - 6.0 - 21.0
# b) istnieje mozliwsoc klasyfikacji na podstawie masy ciala i masy serca. 
# Mozna zgrubnie przyjac ze dla Bwt>3.0->sex=M & Hwt>12.0->sex=M

#### Zadanie 3:
# regresja logistyczna
mdl = glm(Sex~., data = cats, family = binomial)
mdlP = predict(mdl, cats)
slope <- coef(mdl)[3]/(-coef(mdl)[2])
intercept <- coef(mdl)[1]/(-coef(mdl)[2])
xyplot( Bwt ~ Hwt , data = cats, groups = cats$Sex,
        panel=function(...){
          panel.xyplot(...)
          panel.abline(intercept , slope)
          panel.grid(...)
        })

#### Zadanie 4:
# svm
?svm
m1 <- svm(Sex~., data = cats, kernel="linear", cost=1)
plot(m1, cats)

# Dla zadanego problemu, można uznać, że klasyfikator oparty o regresję logistyczną jak i SVM w rezultacie wykreśliły zbliżone 
# granice decyzyjne. Margines decyzyjny dla SVM jest szerszy, w przypadku klasyfikatora opartego o RL granica jest styczna z granicznymi
# punktami.

#### Zadanie 5
# C = {1,50, 100, 10e6}.

svnWithPlot = function(cost, kernelFunction){
  modelSvm <- svm(Sex~., data = cats, kernel=kernelFunction, cost=cost)
  plot(modelSvm, cats)
  modelSvm
}

m_linear_cost_1 <- svnWithPlot(1, "linear")
m_linear_cost_50 <- svnWithPlot(50, "linear")
m_linear_cost_100 <- svnWithPlot(100, "linear")
m_linear_cost_10e6 <- svnWithPlot(10e6, "linear")

# Radial
m_radial_cost_1 <- svnWithPlot(1, "radial")
m_radial_cost_50 <- svnWithPlot(50, "radial")
m_radial_cost_100 <- svnWithPlot(100, "radial")
m_radial_cost_10e6 <- svnWithPlot(10e6, "radial")

# Obserwacja: im niższe C tym model bardziej pozwalamy na błędy klasyfikacji (mniejsza kara)
# Klasyfikacja z użyciem radialnej funkcji bazowej dokładniej oddziela dwie grupy obserwacji, bardziej jednak dopasowując 
# klasyfikator do danych (overfitting)

# Ocena klasyfikatora binarnego

classifierAssessment <- function(model) {
  m_predict = predict(model, cats)
  tab = table(m_predict, cats$Sex)
  cm = confusionMatrix(tab)
  cm
}

# Ocena klasyfikatora z uzyciem confusinMatrix
classifierAssessment(m_linear_cost_1)
classifierAssessment(m_linear_cost_50)
classifierAssessment(m_linear_cost_100)
classifierAssessment(m_linear_cost_10e6)
classifierAssessment(m_radial_cost_1)
classifierAssessment(m_radial_cost_50)
classifierAssessment(m_radial_cost_100)
classifierAssessment(m_radial_cost_10e6)

# a) Obserwując współczynnik dokładności dla powyższych modeli, mozna zauważyć że dla SVM z liniową funkcją bazową 
# im wyższe C, tym dokładność spada. Biorąc pod uwagę wnioski z poprzedniego ćwiczenia - im niższe C tym bardziej pozwalamy 
# na błędy klasyfikacji, wskazywałoby to że dla analizowanych danych lepiej sprawdza się bardziej liberalny model, mniej zważający 
# na outlinery.
# Dla SVM z radialną funkcją bazową sytuacja jest inna, klasyfikator jest dokładniejszy im wyższe C. Z puntku widzenia optymalizacj
# pod kątem dokładności, nalezałoby wybrać C=10e6. Jendak należy wziąć pod uwagę fakt, że w ramach tego ćwiczenia analizy dokonujemy 
# na zbiorze treningowym, można zatem przyjąć, że będzie to model dostosowany do danego zbioru.

# b) Balanced accuracy należałoby rozumieć jako wartość dokładności, dla zbioru danych, w któych dwie grupy nie są tak samo liczne.
# Liczona jest ona ze wzoru: Balanced Accuracy = (Sensitivity+Specificity)/2

# c) 
# - Sensitivity - czułość, stosunek prawdziwie dodatnich do sumy prawdziwie dodatnich i fałyszwie ujemnych. 
# Okresla zatem zdolnosc do wykrywania '1'
# - Specificity - stosunek prawdziwie ujemnych do sumy prawdziwie ujemnych i falszywie dodatnich wartości.
# Określa zdolność do wykrywania '0'. 
# W praktyce: jak dobry jest model w unikaniu fałszywych 0.
# - Positive prediction value - stosunek wyników prawdziwie dodatnich do sumy wyników prawdziwie dodatnich 
# i fałszywie dodatnich (wszystkich wyników dodatnich) - proporcja prawdziwych '1' do wszystkich wskazanych przez model '1'. 
# W praktyce: prawdopodobieństwo że wskazana '1' to prawdziwa '1'
# - Negative prediction value - Stosunek wyników prawdziwie ujemnych do sumy wyników prawdziwie ujemnych 
# i fałszywie ujemnych (wszystkich wyników ujemnych)
# W praktyce: prawdopodobieństwo że wskazane '0' to prawdziwe '0'

# d) W zależności od decyzji jaką przyjdzie nam podjąć na podstawie klasyfikacji, nalażałoby się skupić na optymalizacji modelu
# pod kątem różnych statystyk.
# Przy wykrywaniu nowotworu, istotnym byłoby:
#  - optymalizować wykrywalność choroby: wysoka czułość (1 - pacjent jest chory)
#  - jak najwyższe prawdopodobieństwo, że wykryta choroba jest faktycznie chorobą - PPV wysokie (unikając zaaplikowanie 
# bolesnego leczenia osobie zdrowej)

# Inną sytuacją byłaby kampania reklamowa, gdzie optymalizowalibyśmy czułość - wskazani przez model zainteresowani, byliby faktycznie 
# zainteresowanymi, natomiast nie miałoby znaczenia aż tak wielkiego PPV - małą szkodą byłoby podanie reklamy osobie niezainteresowanej.

# Przy wykrywaniu niespłacalności kredytów - ('0' - klient nie spłaca) optymalizowalibyśmy specyficzność - chcemy jak dać jak najwięcej
# kredytów, ale tylko klientom, którzy go spłacą.

#### Zadanie 6:
library(plyr)
numberOfF <- count(cats, "Sex")[1,2]
numberOfM <- count(cats, "Sex")[2,2]

# Kotow (samcow) jest ponad 2 razy wiecej niz samic
# class.weights jest parametrem pozwalajacym na nadanie wag podzbiorom, w sytuacji gdy nie są one zbalansowane
# (celem uniknięcia sytuacji gdzie jedna klasa ma dużo większy wpływ na działanie algorytmu).

svnWithWeight = function(cost, kernelFunction, wf, wm){
  modelSvm <- svm(Sex~., data = cats, kernel=kernelFunction, cost=cost, class.weights=c(F=wf, M=wm))
  modelSvm
}

wF=1
wM=numberOfF/numberOfM
wM2 = wM+0.1

m_linear_cost_1_w_0_48 <- svnWithWeight(1, "linear", wF, wM)
m_linear_cost_1_w_0_58 <- svnWithWeight(1, "linear", wF, wM2)

m_linear_cost_50_w_0_48 <- svnWithWeight(50, "linear", wF, wM)
m_linear_cost_50_w_0_58 <- svnWithWeight(50, "linear", wF, wM2)

m_radial_cost_1_w_0_48 <- svnWithWeight(1, "radial", wF, wM)
m_radial_cost_1_w_0_58 <- svnWithWeight(1, "radial", wF, wM2)

m_radial_cost_50_w_0_48 <- svnWithWeight(50, "radial", wF, wM)
m_radial_cost_50_w_0_58 <- svnWithWeight(50, "radial", wF, wM2)

classifierAssessment(m_linear_cost_1) #Accuracy=0,80
classifierAssessment(m_linear_cost_1_w_0_48) #Accuracy=0,73
classifierAssessment(m_linear_cost_1_w_0_58) #Accuracy=0.74

classifierAssessment(m_linear_cost_50) #Accuracy=0,79
classifierAssessment(m_linear_cost_50_w_0_48) #Accuracy=0.7292 
classifierAssessment(m_linear_cost_50_w_0_58) #accuracy=0.7431

classifierAssessment(m_radial_cost_1) #Accuracy=0,8056
classifierAssessment(m_radial_cost_1_w_0_48) #Accuracy=0,75
classifierAssessment(m_radial_cost_1_w_0_58) #Accuracy=0.76

classifierAssessment(m_radial_cost_50) #Accuracy=0,8194
classifierAssessment(m_radial_cost_50_w_0_48) #Accuracy=0.7639
classifierAssessment(m_radial_cost_50_w_0_58) #accuracy=0.7917

# Zblanasowanie populacji samic i samcow w wypadku badanego zbioru danych nie ma pozytywnego wplywu na wartosc Accuracy.
# Wieksza waga dla populacji kocurow wplywa pozytywnie na skutecznosc klasyfikacji algorytmu svm.

#### Zadanie 7


svnRBFWithGamma = function(cost, gamma){
  modelSvm <- svm(Sex~., data = cats, kernel='radial', cost=cost, gamma = gamma)
  modelSvm
}

m_gamma_01 <- svnRBFWithGamma(1, 0.1)
m_gamma_1 <- svnRBFWithGamma(1, 1)
m_gamma_5 <- svnRBFWithGamma(1, 5)
m_gamma_50 <- svnRBFWithGamma(1, 50)
m_gamma_500 <- svnRBFWithGamma(1, 500)

plot(m_gamma_01, cats)
plot(m_gamma_1, cats)
plot(m_gamma_5, cats)
plot(m_gamma_50, cats)
plot(m_gamma_500, cats)

classifierAssessment(m_gamma_01)
#Accuracy : 0.7917 
#Sensitivity : 0.7234          
#Specificity : 0.8247          
#Pos Pred Value : 0.6667          
#Neg Pred Value : 0.8602

classifierAssessment(m_gamma_1)
#Accuracy : 0.7986
#Sensitivity : 0.6809          
#Specificity : 0.8557          
#Pos Pred Value : 0.6957          
#Neg Pred Value : 0.8469 

classifierAssessment(m_gamma_5) 
#Accuracy : 0.8125 
#Sensitivity : 0.6809         
#Specificity : 0.8763         
#Pos Pred Value : 0.7273         
#Neg Pred Value : 0.8500

classifierAssessment(m_gamma_50) 
#Accuracy : 0.9167
#Sensitivity : 0.7872         
#Specificity : 0.9794         
#Pos Pred Value : 0.9487         
#Neg Pred Value : 0.9048 

classifierAssessment(m_gamma_500)
#Accuracy : 0.9722
#Sensitivity : 0.9574          
#Specificity : 0.9794          
#Pos Pred Value : 0.9574          
#Neg Pred Value : 0.9794

# Parameter gamma definiuje jak duzy wplyw pojedyncze probki będą miały na granice klasyfikacji.
# Im wyższa wartość parametru gamma, tym bardziej model będzie dopasowany do zbioru treningowego.
# Gamma o wartości 500 daje bardzo wysokie współczynniki dokładności i poprawności klasyfikacji, jest to 
# jednak zdecydowanie przetrenowany model, dopasowany do treningowych danych (patrząc na wykres, w zasadzie wskazuje dokładnie
# obszary wokół pojedynczych próbek. 
# Nie dzieląc zbioru na treningowy i testowy, obserwując wykreslone granice przy różnych parametrach gamma, 
# uznać można że gamma=1 mogłoby być wartością akceptowalną.

#### Zadanie 9 (zadania 8 brak w pdf-ie :) )

nmbOfCats = dim(cats)[1]
trIndx = sample(nmbOfCats, nmbOfCats*0.7)
catsTr = cats[trIndx, ]
catsTst = cats[-trIndx, ]

# Trenowanie modelu z wykorzystaniem danych treningowych i hipotetycznie najlepszych hiperparametrach (gamma 500)
svnRBFWithGammaModelOnTrainingSet = function(cost, gamma, trainingSet){
  modelSvm <- svm(Sex~., data = trainingSet, kernel='radial', cost=cost, gamma = gamma)
  modelSvm
}

# Ocena z wykorzystaniem zbioru testowego
classifierAssessmentOnTestDataset <- function(model, testData) {
  m_predict = predict(model, testData)
  tab = table(m_predict, testData$Sex)
  cm = confusionMatrix(tab)
  cm
}

hipotheticallyBestModelGamma500 <- svnRBFWithGammaModelOnTrainingSet(1,500,catsTr)

plot(hipotheticallyBestModelGamma500, catsTst)

classifierAssessmentOnTestDataset(hipotheticallyBestModelGamma500, catsTst)

# Zgodnie z intuicją model dopasowany do zbioru treningowego wykazuje bardzo niską czułość.
# Obserwując na wykresie, model zdaje się być bezużyteczny w zadaniu klasyfikacyjnym.

# c) Zaprezentowany wykres jest ilustracją szukanego kompormisu pomiędzy  obciążeniem a wariancją.
# Model nadmiernie dopasowany (overfitting) w pewnym momencie będzie zbytnio odzwierciedlał zbiór treningowy.
# Model nadmiernie uproszczony (underfitting) nie będzie właściwie odzwieciedlał modelowanego zjawiska. 
# Na wykresie wskazany jest punkt optimum, będący optymalnym estymatorem (odpowiednio dostrojone hiperparametry),
# z najlepszymi zdolnościami predykcyjnymi w danym zbiorze.

### Zadanie 10

# Proba znalezienia opytmalnego kompromisu z wykorzystaniem k-walidacji krzyzowej używając metody tune

tuner <- tune(svm, Sex~., data = cats,  
            ranges=list(class.weights=list(c(F=1, M=1), c(F=1, M =numberOfF/numberOfM)), cost = seq(0.01, 2, 0.1),  kernel='radial', gamma = seq(0.01,2,0.1)))

summary(tuner)

# Zgodnie z przewidywaniami, przy próbie doboru właściwych parametrów do modelu i każdorazowej ocenie przy użyciu k-krotnej walidacji,
# zaobserwować można, że parametr błędu do pewnego momentu maleje, 
# a powyżej granicy 'optymalnego kompromisu' model jest zbytnio dopasowany do zbioru treningowego, co zwieksza 
# wartość parametru błedu.

# Przy doborze odpowiednich zakresów udało się ustalić, że model zachowuje najlepsze zdolności do klasyfikacji przy parametrach:
#  class.weights cost kernel gamma
#           1, 1 0.61 radial  0.71

# Do tej granicy parametr błędu malał, powyzej niej zaczął rosnąc (zgodnie z ilustracją z poprzedniego ćwiczenia).

# Wyniki obliczeń:


# - best parameters:
#  class.weights cost kernel gamma
#           1, 1 0.61 radial  0.71

# - best performance: 0.2090476 

# - Detailed performance results:
#            class.weights cost kernel gamma     error dispersion
# 1                   1, 1 0.01 radial  0.01 0.3261905 0.06322364
# 2   1.0000000, 0.4845361 0.01 radial  0.01 0.5195238 0.19271763
# 3                   1, 1 0.11 radial  0.01 0.3261905 0.06322364
# 4   1.0000000, 0.4845361 0.11 radial  0.01 0.5195238 0.19271763
# 5                   1, 1 0.21 radial  0.01 0.3261905 0.06322364
# 6   1.0000000, 0.4845361 0.21 radial  0.01 0.4509524 0.16943357
# 7                   1, 1 0.31 radial  0.01 0.3261905 0.06322364
# 8   1.0000000, 0.4845361 0.31 radial  0.01 0.3576190 0.16554346
# 9                   1, 1 0.41 radial  0.01 0.3261905 0.06322364
# 10  1.0000000, 0.4845361 0.41 radial  0.01 0.3500000 0.12149339
# 11                  1, 1 0.51 radial  0.01 0.3261905 0.06322364
# 12  1.0000000, 0.4845361 0.51 radial  0.01 0.3361905 0.12547115
# 13                  1, 1 0.61 radial  0.01 0.3261905 0.06322364
# 14  1.0000000, 0.4845361 0.61 radial  0.01 0.3223810 0.12594916
# 15                  1, 1 0.71 radial  0.01 0.3261905 0.06322364
# 16  1.0000000, 0.4845361 0.71 radial  0.01 0.3366667 0.12608911
# 17                  1, 1 0.81 radial  0.01 0.3261905 0.06322364
# 18  1.0000000, 0.4845361 0.81 radial  0.01 0.3366667 0.12608911
# 19                  1, 1 0.91 radial  0.01 0.3261905 0.06322364
# 20  1.0000000, 0.4845361 0.91 radial  0.01 0.3295238 0.11754180
# 21                  1, 1 1.01 radial  0.01 0.3261905 0.06322364
# 22  1.0000000, 0.4845361 1.01 radial  0.01 0.3223810 0.10779753
# 23                  1, 1 1.11 radial  0.01 0.3261905 0.06322364
# 24  1.0000000, 0.4845361 1.11 radial  0.01 0.3080952 0.10143962
# 25                  1, 1 1.21 radial  0.01 0.3261905 0.06322364
# 26  1.0000000, 0.4845361 1.21 radial  0.01 0.3009524 0.09427556
# 27                  1, 1 1.31 radial  0.01 0.3261905 0.06322364
# 28  1.0000000, 0.4845361 1.31 radial  0.01 0.3009524 0.09427556
# 29                  1, 1 1.41 radial  0.01 0.3190476 0.07926978
# 30  1.0000000, 0.4845361 1.41 radial  0.01 0.3009524 0.09427556
# 31                  1, 1 1.51 radial  0.01 0.3261905 0.08601505
# 32  1.0000000, 0.4845361 1.51 radial  0.01 0.3009524 0.09427556
# 33                  1, 1 1.61 radial  0.01 0.3119048 0.10889514
# 34  1.0000000, 0.4845361 1.61 radial  0.01 0.3009524 0.09427556
# 35                  1, 1 1.71 radial  0.01 0.3047619 0.11797399
# 36  1.0000000, 0.4845361 1.71 radial  0.01 0.3076190 0.08275917
# 37                  1, 1 1.81 radial  0.01 0.2976190 0.11660456
# 38  1.0000000, 0.4845361 1.81 radial  0.01 0.2871429 0.09065841
# 39                  1, 1 1.91 radial  0.01 0.3042857 0.11654405
# 40  1.0000000, 0.4845361 1.91 radial  0.01 0.2871429 0.09065841
# 41                  1, 1 0.01 radial  0.11 0.3261905 0.06322364
# 42  1.0000000, 0.4845361 0.01 radial  0.11 0.5195238 0.19271763
# 43                  1, 1 0.11 radial  0.11 0.3261905 0.06322364
# 44  1.0000000, 0.4845361 0.11 radial  0.11 0.3223810 0.11293403
# 45                  1, 1 0.21 radial  0.11 0.3042857 0.11654405
# 46  1.0000000, 0.4845361 0.21 radial  0.11 0.3009524 0.09427556
# 47                  1, 1 0.31 radial  0.11 0.2980952 0.09903032
# 48  1.0000000, 0.4845361 0.31 radial  0.11 0.2938095 0.08586260
# 49                  1, 1 0.41 radial  0.11 0.2776190 0.10505131
# 50  1.0000000, 0.4845361 0.41 radial  0.11 0.3004762 0.07375232
# 51                  1, 1 0.51 radial  0.11 0.2309524 0.09094144
# 52  1.0000000, 0.4845361 0.51 radial  0.11 0.2938095 0.08586260
# 53                  1, 1 0.61 radial  0.11 0.2176190 0.09481520
# 54  1.0000000, 0.4845361 0.61 radial  0.11 0.2871429 0.09065841
# 55                  1, 1 0.71 radial  0.11 0.2242857 0.09047480
# 56  1.0000000, 0.4845361 0.71 radial  0.11 0.2942857 0.09919809
# 57                  1, 1 0.81 radial  0.11 0.2304762 0.09027268
# 58  1.0000000, 0.4845361 0.81 radial  0.11 0.2942857 0.09919809
# 59                  1, 1 0.91 radial  0.11 0.2171429 0.09409901
# 60  1.0000000, 0.4845361 0.91 radial  0.11 0.2871429 0.09065841
# 61                  1, 1 1.01 radial  0.11 0.2171429 0.09409901
# 62  1.0000000, 0.4845361 1.01 radial  0.11 0.2942857 0.09919809
# 63                  1, 1 1.11 radial  0.11 0.2171429 0.09409901
# 64  1.0000000, 0.4845361 1.11 radial  0.11 0.2938095 0.08670939
# 65                  1, 1 1.21 radial  0.11 0.2171429 0.09409901
# 66  1.0000000, 0.4845361 1.21 radial  0.11 0.2938095 0.08670939
# 67                  1, 1 1.31 radial  0.11 0.2171429 0.09409901
# 68  1.0000000, 0.4845361 1.31 radial  0.11 0.2938095 0.08670939
# 69                  1, 1 1.41 radial  0.11 0.2304762 0.09027268
# 70  1.0000000, 0.4845361 1.41 radial  0.11 0.2866667 0.07674402
# 71                  1, 1 1.51 radial  0.11 0.2304762 0.09027268
# 72  1.0000000, 0.4845361 1.51 radial  0.11 0.2866667 0.07674402
# 73                  1, 1 1.61 radial  0.11 0.2304762 0.09027268
# 74  1.0000000, 0.4845361 1.61 radial  0.11 0.2866667 0.07674402
# 75                  1, 1 1.71 radial  0.11 0.2304762 0.09027268
# 76  1.0000000, 0.4845361 1.71 radial  0.11 0.2795238 0.06440025
# 77                  1, 1 1.81 radial  0.11 0.2304762 0.09027268
# 78  1.0000000, 0.4845361 1.81 radial  0.11 0.2795238 0.06440025
# 79                  1, 1 1.91 radial  0.11 0.2233333 0.09442644
# 80  1.0000000, 0.4845361 1.91 radial  0.11 0.2795238 0.06440025
# 81                  1, 1 0.01 radial  0.21 0.3261905 0.06322364
# 82  1.0000000, 0.4845361 0.01 radial  0.21 0.5195238 0.19271763
# 83                  1, 1 0.11 radial  0.21 0.3261905 0.06322364
# 84  1.0000000, 0.4845361 0.11 radial  0.21 0.2938095 0.08586260
# 85                  1, 1 0.21 radial  0.21 0.2771429 0.09988151
# 86  1.0000000, 0.4845361 0.21 radial  0.21 0.2938095 0.07990452
# 87                  1, 1 0.31 radial  0.21 0.2376190 0.08525598
# 88  1.0000000, 0.4845361 0.31 radial  0.21 0.2866667 0.07578596
# 89                  1, 1 0.41 radial  0.21 0.2171429 0.09409901
# 90  1.0000000, 0.4845361 0.41 radial  0.21 0.2933333 0.06258076
# 91                  1, 1 0.51 radial  0.21 0.2242857 0.09047480
# 92  1.0000000, 0.4845361 0.51 radial  0.21 0.2933333 0.06258076
# 93                  1, 1 0.61 radial  0.21 0.2171429 0.09409901
# 94  1.0000000, 0.4845361 0.61 radial  0.21 0.2795238 0.06440025
# 95                  1, 1 0.71 radial  0.21 0.2304762 0.09027268
# 96  1.0000000, 0.4845361 0.71 radial  0.21 0.2795238 0.06440025
# 97                  1, 1 0.81 radial  0.21 0.2304762 0.09027268
# 98  1.0000000, 0.4845361 0.81 radial  0.21 0.2795238 0.06440025
# 99                  1, 1 0.91 radial  0.21 0.2233333 0.09442644
# 100 1.0000000, 0.4845361 0.91 radial  0.21 0.2657143 0.07030151
# 101                 1, 1 1.01 radial  0.21 0.2161905 0.08545522
# 102 1.0000000, 0.4845361 1.01 radial  0.21 0.2657143 0.07030151
# 103                 1, 1 1.11 radial  0.21 0.2161905 0.08545522
# 104 1.0000000, 0.4845361 1.11 radial  0.21 0.2585714 0.07165924
# 105                 1, 1 1.21 radial  0.21 0.2228571 0.08664399
# 106 1.0000000, 0.4845361 1.21 radial  0.21 0.2585714 0.07165924
# 107                 1, 1 1.31 radial  0.21 0.2228571 0.08664399
# 108 1.0000000, 0.4845361 1.31 radial  0.21 0.2585714 0.07165924
# 109                 1, 1 1.41 radial  0.21 0.2228571 0.08664399
# 110 1.0000000, 0.4845361 1.41 radial  0.21 0.2585714 0.07165924
# 111                 1, 1 1.51 radial  0.21 0.2295238 0.08725251
# 112 1.0000000, 0.4845361 1.51 radial  0.21 0.2585714 0.07165924
# 113                 1, 1 1.61 radial  0.21 0.2228571 0.08664399
# 114 1.0000000, 0.4845361 1.61 radial  0.21 0.2585714 0.07165924
# 115                 1, 1 1.71 radial  0.21 0.2228571 0.08664399
# 116 1.0000000, 0.4845361 1.71 radial  0.21 0.2585714 0.07165924
# 117                 1, 1 1.81 radial  0.21 0.2228571 0.08664399
# 118 1.0000000, 0.4845361 1.81 radial  0.21 0.2585714 0.07165924
# 119                 1, 1 1.91 radial  0.21 0.2157143 0.08378337
# 120 1.0000000, 0.4845361 1.91 radial  0.21 0.2585714 0.07165924
# 121                 1, 1 0.01 radial  0.31 0.3261905 0.06322364
# 122 1.0000000, 0.4845361 0.01 radial  0.31 0.5195238 0.19271763
# 123                 1, 1 0.11 radial  0.31 0.3261905 0.06322364
# 124 1.0000000, 0.4845361 0.11 radial  0.31 0.2871429 0.09065841
# 125                 1, 1 0.21 radial  0.31 0.2842857 0.09140570
# 126 1.0000000, 0.4845361 0.21 radial  0.31 0.2800000 0.08052486
# 127                 1, 1 0.31 radial  0.31 0.2442857 0.08459141
# 128 1.0000000, 0.4845361 0.31 radial  0.31 0.2866667 0.06896273
# 129                 1, 1 0.41 radial  0.31 0.2442857 0.08459141
# 130 1.0000000, 0.4845361 0.41 radial  0.31 0.2795238 0.06440025
# 131                 1, 1 0.51 radial  0.31 0.2376190 0.08525598
# 132 1.0000000, 0.4845361 0.51 radial  0.31 0.2795238 0.06440025
# 133                 1, 1 0.61 radial  0.31 0.2233333 0.09442644
# 134 1.0000000, 0.4845361 0.61 radial  0.31 0.2657143 0.07030151
# 135                 1, 1 0.71 radial  0.31 0.2233333 0.09442644
# 136 1.0000000, 0.4845361 0.71 radial  0.31 0.2585714 0.07165924
# 137                 1, 1 0.81 radial  0.31 0.2233333 0.09442644
# 138 1.0000000, 0.4845361 0.81 radial  0.31 0.2585714 0.07165924
# 139                 1, 1 0.91 radial  0.31 0.2161905 0.08545522
# 140 1.0000000, 0.4845361 0.91 radial  0.31 0.2657143 0.07030151
# 141                 1, 1 1.01 radial  0.31 0.2161905 0.08545522
# 142 1.0000000, 0.4845361 1.01 radial  0.31 0.2657143 0.07030151
# 143                 1, 1 1.11 radial  0.31 0.2161905 0.08545522
# 144 1.0000000, 0.4845361 1.11 radial  0.31 0.2657143 0.07030151
# 145                 1, 1 1.21 radial  0.31 0.2228571 0.08664399
# 146 1.0000000, 0.4845361 1.21 radial  0.31 0.2728571 0.08308883
# 147                 1, 1 1.31 radial  0.31 0.2228571 0.08664399
# 148 1.0000000, 0.4845361 1.31 radial  0.31 0.2728571 0.08308883
# 149                 1, 1 1.41 radial  0.31 0.2228571 0.08664399
# 150 1.0000000, 0.4845361 1.41 radial  0.31 0.2728571 0.08308883
# 151                 1, 1 1.51 radial  0.31 0.2228571 0.08664399
# 152 1.0000000, 0.4845361 1.51 radial  0.31 0.2728571 0.08308883
# 153                 1, 1 1.61 radial  0.31 0.2228571 0.08664399
# 154 1.0000000, 0.4845361 1.61 radial  0.31 0.2728571 0.08308883
# 155                 1, 1 1.71 radial  0.31 0.2228571 0.08664399
# 156 1.0000000, 0.4845361 1.71 radial  0.31 0.2800000 0.08728138
# 157                 1, 1 1.81 radial  0.31 0.2223810 0.09065841
# 158 1.0000000, 0.4845361 1.81 radial  0.31 0.2800000 0.08728138
# 159                 1, 1 1.91 radial  0.31 0.2223810 0.09065841
# 160 1.0000000, 0.4845361 1.91 radial  0.31 0.2800000 0.08728138
# 161                 1, 1 0.01 radial  0.41 0.3261905 0.06322364
# 162 1.0000000, 0.4845361 0.01 radial  0.41 0.5195238 0.19271763
# 163                 1, 1 0.11 radial  0.41 0.3328571 0.05966438
# 164 1.0000000, 0.4845361 0.11 radial  0.41 0.2866667 0.06896273
# 165                 1, 1 0.21 radial  0.41 0.2709524 0.09832177
# 166 1.0000000, 0.4845361 0.21 radial  0.41 0.2795238 0.06440025



### Zadanie 11

def = read.csv("defaults.csv", sep = ";", header = TRUE)
smpl = def[sample(nrow(def),2000),] 

smpl<-smpl[,-1] # Usuniecie kolumny ID
smpl$default.payment.next.month <- as.factor(smpl$default.payment.next.month) # def_w[,25] = as.factor(def_w[,25])

# przygotowanie pelnego zbioru danych
def<-def[,-1]
def$default.payment.next.month <- as.factor(def$default.payment.next.month)

# Próba doboru parametrow do klasyfikatora z użyciem metody tune
# (kilka prob celem przyspieszenia obliczen...)

# Proba 1)
# ranges = list(gamma = seq(0.1,2,0.5), cost = seq(0.1,2, 0.2)))
# - best parameters: gamma cost 0.1  1.7
# - best performance: 0.196 

# Proba 2)
# ranges = list(gamma = seq(0.01,0.1,0.05), cost = seq(0.1,2, 0.2))) 
# - best parameters:  gamma cost 0.06  1.9
# - best performance: 0.1955 

# Proba 3)
# ranges = list(gamma = seq(0.01,0.1,0.05), cost = seq(1,10, 1)))
# - best parameters:  gamma cost 0.06    3
# - best performance: 0.1905 

# Proba 4) 
# ranges = list(gamma = seq(0.01,0.1,0.05), cost = seq(2,5, 0.2)))
# - best parameters: gamma cost 0.06  4.4
# - best performance: 0.1915 

# Analizując otrzymane wyniki do modelu wybrane zostaną parametry wyłonione z zakresu użytego w próbie 2.

tuner<-tune(svm, 
            default.payment.next.month~.,
            data=smpl,
            ranges = list(gamma = seq(0.01,0.1,0.05), cost = seq(0.1,2, 0.2)))

summary(tuner) 


# Na podstawie kilkukortnej walidacji krzyżowej (wykorzystując metodę tune) wybrane zostały następujące parametry:
#  gamma = 0.06  cost = 1.9

# install.packages("ROCR") Doinstaluj pakiet ROCR który zawiera metody prediction i performance do wyrysowania krzywej ROC
library ("ROCR")

# Budowa modelu na podstawie pelnego zbioru def (traktujac go jako treningowy)
svm.fit = svm(default.payment.next.month~., data = def, kernel='radial', cost= 1.9, gamma = 0.06)

fitted=attributes(predict(svm.fit,def,decision.values=T))$decision.values
predob = prediction(fitted, def$default.payment.next.month)
perf = performance(predob, 'tpr', 'fpr')
plot(perf)

def_w = read.csv("./defaults_valid.csv", sep = ";", header = TRUE)
def_w[,25] = as.factor(def_w[,25])
defaultsResults = predict(svm.fit, def_w)
ctab = table(defaultsResults, def_w$default.payment.next.month)
cm = confusionMatrix(ctab)
cm

#Confusion Matrix and Statistics

#defaultsResults    0    1
#0 1522  294
#1   53  132

#Accuracy : 0.8266          
#95% CI : (0.8093, 0.8429)
#No Information Rate : 0.7871          
#P-Value [Acc > NIR] : 5.744e-06       

#Kappa : 0.348           

#Mcnemar's Test P-Value : < 2.2e-16       
                                          
#            Sensitivity : 0.9663          
#            Specificity : 0.3099          
#         Pos Pred Value : 0.8381          
#         Neg Pred Value : 0.7135          
#             Prevalence : 0.7871          
#         Detection Rate : 0.7606          
#   Detection Prevalence : 0.9075          
#      Balanced Accuracy : 0.6381          
                                          
#       'Positive' Class : 0  

# Oceniając model używając krzywej ROC, można stwierdzić że posiada on zdolności predykcyjne na danych walidacyjnych.
# Oceniając po parametrze Accuracy (0.82) oraz Balanced Accuracy (0.63) możnaby stwierdzić, że nie jest to jednak najlepszy klasyfikator.
# Jedną z przyczyn jest na pewno dysproporcja defaultów do grupy spłacającej. 
# Być moze dane wymagałyby uzupełnienia, lub analizy przez ekperta domenowego celem odrzucenia/zebrania nowych parametrów.
