# Zadanie 1

library(dplyr)
dane = read.csv("tempciala.txt", header = T)

dane_mezczyzni <- select(filter(dane, dane[2]==1), c('temperatura'))
dane_kobiety <- select(filter(dane, dane[2]==2), c('temperatura'))

mean_mezczyzni = mean(dane_mezczyzni$temperatura)                      
sd_mezczyzni = sd(dane_mezczyzni$temperatura)

mean_kobiety = mean(dane_kobiety$temperatura)                      
sd_kobiety = sd(dane_kobiety$temperatura)

mi_0 = 36.6
alfa = 0.05

n_m = length(dane_mezczyzni$temperatura)
n_k = length(dane_kobiety$temperatura)

# test że średnia==36,6 H0=36,6; H1!=36,6

# Mezczyzni
T_m = abs(mean_mezczyzni - mi_0)*sqrt(n_m)/sd_mezczyzni
c_m = qt(1-alfa/2,df = n_m-1) 
p_val_m = 2*(1 - pt(T_m, df = n_m-1)) 

# z parkietu:
t.test(dane_mezczyzni, mu = mi_0, alternative = "two.sided")
# p_val < 0.05

# Kobiety
T_k = abs(mean_kobiety - mi_0)*sqrt(n_k)/sd_kobiety
c_k = qt(1-alfa/2,df = n_k-1) 
p_val_k = 2*(1 - pt(T_k, df = n_k-1)) 

# z parkietu:
t.test(dane_kobiety, mu = mi_0, alternative = "two.sided")
# p_val < 0.05

# test normalnosci dla zarejestrowanych temperatur
shapiro.test(dane_mezczyzni$temperatura) # H0 -> p=0.4818

shapiro.test(dane_kobiety$temperatura) # H1 -> p=0.033



