lozyska = read.csv("lozyska.txt")

#Wyliczenia z definicji:

x_1 = lozyska[,1]
x_2 = lozyska[,2]
n = length(x_1)

mean_1 = mean(x_1)
mean_2 = mean(x_2)
s2_1 = var(x_1)
s2_2 = var(x_2);
s2 = (s2_1 + s2_2)/n;

T = (mean_1 - mean_2)/sqrt(s2)
d = s2^2/((s2_1^2 + s2_2^2)/((n-1)*n^2))

p_val = (1 - pt(abs(T), d))*2


alpha = 0.05
c = qt(1 - alpha/2, d)

# Z pakietu:

t.test(lozyska$X.Typ.I., lozyska$X.Typ.II.)

#data:  lozyska$X.Typ.I. and lozyska$X.Typ.II.
# t = 2.0723, df = 16.665, p-value = 0.05408
# wynik testu wskazuje na H1 - wystepuje roznica pomiedzy lozyskami

# bez zakladania normalnosci rozkladow:

wilcox.test(lozyska$X.Typ.I., lozyska$X.Typ.II.)

# W = 75, p-value = 0.06301 
# p-wartosc niska ale o 0.01 wyzsza niz zakladany prog istotnosci, a zatem mozna przyjac H0 

# Szacownie 
N = 1000
x = sample(x_1, N, replace = T)
y = sample(x_2, N, replace = T)
z = x > y
prawd_boot = sum(z)/N
