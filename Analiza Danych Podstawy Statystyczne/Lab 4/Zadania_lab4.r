# ZADANIE 1 

read_company_data = function(nazwa_archiwum, nazwa_spolki){
  unzip('mstall.zip', nazwa_archiwum)
  df_data = read.csv(nazwa_archiwum)
  Header <- c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
  names(df_data) = Header
  df_data$date = as.Date.character(df_data$date, format = '%Y%m%d')
  
  df_data$close_ch= with(df_data, c(NA, 100*diff(close))/close)
  return(df_data)
} 

dane = read_company_data('CCC.mst', 'CCC')

#### Porównaj średnie dla ostatnich 6 miesięcy

y1 =with(dane, close_ch[format(date,'%Y-%m') == '2019-04'])
y2 =with(dane, close_ch[format(date,'%Y-%m') == '2019-03'])
y3 =with(dane, close_ch[format(date,'%Y-%m') == '2019-02'])
y4 =with(dane, close_ch[format(date,'%Y-%m') == '2019-01'])
y5 =with(dane, close_ch[format(date,'%Y-%m') == '2018-12'])
y6 =with(dane, close_ch[format(date,'%Y-%m') == '2018-11'])

boxplot(y1, y2, y3, y4, y5, y6)

dane_anova_6m = data.frame(
  dane = c(y1, y2, y3, y4, y5, y6),
  proba = rep( 
    c("y1", "y2", "y3", "y4", "y5", "y6"), 
    times = c(length(y1), length(y2), length(y3), length(y4), length(y5), length(y6)) 
  )
)

# ANOVA
aov_res_6m = aov(dane~proba, data = dane_anova_6m)
summary(aov_res_6m )

# Tukey Honest Significant Differences
Tukey_res = TukeyHSD(aov_res_6m)
print(Tukey_res)
plot(Tukey_res)

# ------ p>0.05 wiec nie mamy podstaw dk odrzucenia H0 ze średnie są równe

#### Porównaj średnie dla ostatnich 3 miesięcy:
dane_anova_3m = dane_anova_6m[dane_anova_6m$proba %in% c("y1","y2","y3"),]

# ANOVA
aov_res_3m = aov(dane~proba, data = dane_anova_3m)
# Tukey Honest Significant Differences
Tukey_res = TukeyHSD(aov_res_3m)
print(Tukey_res)
plot(Tukey_res)

# ------ p>0.05 wiec nie ma podstaw do odrzucenia H0

#### ZADANIE 2

# Wyznaczenie zaleznosci indeksu WIG od poszczegolnych spolek (patrzac na p-wartosc)

wczytaj_mst = function(plik_zip, plik_mst) {
  unzip(plik_zip, plik_mst)
  dane = read.csv(plik_mst)
  names(dane) = c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
  dane$date =  as.Date.character(dane$date, '%Y%m%d')
  return(dane)
}
wczytaj_kurs <- function(TICKER, zip="mstall.zip") {
  dane = wczytaj_mst(zip, paste0(TICKER,".mst"))
  dane_z_roku = subset(dane,
                 format(date, '%Y') == '2018',
                 select = c('date', 'close')
  )
  names(dane_z_roku) = c('date', TICKER)
  return(dane_z_roku)
}

WIG20 = wczytaj_kurs("WIG20")
COMARCH = wczytaj_kurs("COMARCH")
GETIN = wczytaj_kurs("GETIN")
KGHM = wczytaj_kurs("KGHM")
PGNIG = wczytaj_kurs("PGNIG")
PZU = wczytaj_kurs("PZU")
PEKAO = wczytaj_kurs("PEKAO")

ALL_df = merge(WIG20, COMARCH, by = 'date')
ALL_df = merge(ALL_df, GETIN, by = 'date')
ALL_df = merge(ALL_df, KGHM, by = 'date')
ALL_df = merge(ALL_df, PGNIG, by = 'date')
ALL_df = merge(ALL_df, PZU, by = 'date')
ALL_df = merge(ALL_df, PEKAO, by = 'date')

# Liczby model na wszystkich zmiennych 
model_liniowy <- lm(WIG20 ~ COMARCH+GETIN+KGHM+PEKAO+PGNIG+PZU, data=ALL_df)
summary(model_liniowy)

#Zmienne w modelu wydają się satatystycznie istotne 
#(poza GETIN która jest istona jedynie do poziomu 0,01). 

model_small <- lm(WIG20 ~ COMARCH+KGHM+PZU, data=ALL_df)
summary(model_small)
#Wszystkie zmienne są statystycznie istotne. 


##### ZADANIE 3

# Wczytanie danych
CHF = wczytaj_kurs("CHF", zip="mstnbp.zip")
EUR = wczytaj_kurs("EUR", zip="mstnbp.zip")
USD = wczytaj_kurs("USD", zip="mstnbp.zip")
GBP = wczytaj_kurs("GBP", zip="mstnbp.zip")
JPY = wczytaj_kurs("JPY", zip="mstnbp.zip")

df_waluty = merge(CHF, EUR, by = 'date')                
df_waluty = merge(df_waluty, USD, by = 'date')
df_waluty = merge(df_waluty, GBP, by = 'date')
df_waluty = merge(df_waluty, JPY, by = 'date')

DAX = wczytaj_kurs("DAX", zip="mstzgr.zip")
DJIA = wczytaj_kurs("DJIA", zip="mstzgr.zip")
NIKKEI = wczytaj_kurs("NIKKEI", zip="mstzgr.zip")
FTSE100 = wczytaj_kurs("FT-SE100", zip="mstzgr.zip")

df_indeksy = merge(DAX, DJIA, by = 'date')  
df_indeksy = merge(df_indeksy, NIKKEI, by = 'date')
df_indeksy = merge(df_indeksy, FTSE100, by = 'date')

#CHF
model_chf <- lm(CHF ~ EUR+USD+GBP+JPY, data=df_waluty)
summary(model_chf)

# p-wartosc dla JPY p>0.05 

#WIG20

# Łacze df z zadania 2 z indeksami
df_model <- merge(ALL_df, df_indeksy, by="date")

# Model
model_wig20 <- lm(WIG20 ~ DAX+DJIA+NIKKEI+`FT-SE100`, data=df_model)
summary(model_wig20)

#USD
df_model2 <- merge(df_waluty, df_indeksy, by="date")

# Model
model_usd <- lm(USD ~ DAX+DJIA+NIKKEI+`FT-SE100`, data=df_model2)
summary(model_usd)

# NIKKEI p-wartosc>0.05 (0.466) zatem nie mozna odrzucić H0 że wartości współczynników są zerowe.

######Zadanie 4

# Wczytanie danych
sprzedaz <- read.delim("sprzedaz.txt", sep=",")

# Model
model <- lm(Income ~ Advert, data=sprzedaz)
summary(model)

# Wykres
plot(Income ~ Advert, data=sprzedaz)
abline(model, col="red")

# Prognozowane wartosci(fit), oraz przedziały prognozy
# fit: the predicted sale values for the three new advertising budget
# lwr and upr: the lower and the upper confidence limits for the expected values, respectively. 
# By default the function produces the 95% confidence limits.

y <- sprzedaz$Income
x <- sprzedaz$Advert
n <- length(x)

y_est <- coef(model)[2]*x + coef(model)[1]
lines(x, y_est, col = 'blue')

x_new <- c(300,500, 700)
y_est_new <- coef(model)[2]*x_new + coef(model)[1]

lines(x_new, y_est_new, type = 'o', col = 'green', pch = 5)

s2 <- 1/(n - 2)*sum((y - y_est)^2)
std_y_est <- sqrt(s2*(1 + 1/n + (mean(x)-x_new)^2 / (n*(mean(x^2)-mean(x)^2)) ))

 predict(model, data.frame(Advert=c(300,500,700)),  se.fit=TRUE, interval="p")

####Zadanie 5

#I() prevents the formula-interface from interpreting the argument, so it gets passed along instead to the expression-parsing part.
#In the formula interface -x means 'remove x from the predictors'. So I can do y~.-x to mean 'fit y against everything but x'.
#You don't want it to do that - you actually want to make a variable that is the difference of two variables and regress on that, so you don't want the formula interface to parse that expression.
#I() achieves that for you.
#Terms with squaring in them (x^2) also need the same treatment. The formula interface does something special with powers, and if you actually want a variable squared you have to I() it.
#I() has some other uses in other contexts as well. See ?I

# https://stackoverflow.com/questions/24198013/significance-of-i-keyword-in-lm-model-in-r

qmodel <- lm(Income ~ I(Advert^2), data=sprzedaz)
summary(qmodel)

plot(Income ~ Advert, data=sprzedaz)
abline(model, col="red", lwd=2)
curve(predict(qmodel,data.frame(Advert=x)), col="yellow", lwd=2, add=TRUE)
