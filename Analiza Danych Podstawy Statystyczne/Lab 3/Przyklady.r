# Test wartości średniej
# Wytłumaczenie wzorów: https://cyrkiel.info/statystyka/test-dla-jednej-sredniej/

n = 30; 
mi = 1; 
sigma = 2;
x = rnorm(n, mean = mi, sd = sigma) 
mi_0 = 1;
alfa = 0.05;

# znana wariancja:
Z = abs(mean(x) - mi_0)*sqrt(n)/sigma;
c = qnorm(1-alfa/2);
p_val = 2*(1 - pnorm(Z))

z.test(x, mu = mi_0, sigma, alternative = "two.sided")

# nieznana wariancja:
T = abs(mean(x) - mi_0)*sqrt(n)/sd(x) 
c = qt(1-alfa/2,df = n-1) 
p_val = 2*(1 - pt(T, df = n-1)) 

t.test(x, mu = mi_0, alternative = "two.sided")

# Test zgodnosci Pearsona

n = 50;
x = sample(1:6, n, replace = T)
ni_i = as.data.frame(table(factor(x, levels = 1:6)))$Freq 
p_i = rep(1/6, 6)
T = sum((ni_i - n*p_i)^2 / (n*p_i)) 
r = 6 
alfa = 0.05 
c = qchisq(1 - alfa, r - 1) 
p_val = (1 - pchisq(T, r - 1)) 

chisq.test(ni_i, p=p_i)

#c<T więc H0 true -> kostka jest symetryczna


# test Kolomogorowa-Smirnova ## JAKIE PRZYJAC C?

n = 50; 
a = 2;
b = 4; 
x = runif(n, min = a, max = b)

#Przeprowadź test Kołmogorowa-Smirnowa dla różnych założeń dotyczących krańców przedziałów: 
ks.test(x, 'punif', min = a, max = b) 
ks.test(x, 'punif', a - 0.5, b - 0.5) 
ks.test(x, 'punif', a - 0.5, b + 0.5)

# Zweryfikuj hipotezę, że wygenerowane dane pochodzą z rozkładu normalnego
# o parametrach będących wartością średnia i wariancją rozkładu jednostajnego
ks.test(x, 'pnorm', mean = (a + b)/2, sd = sqrt((b - a)^2/12)) 

# Sprawdź wynik po zwiększeniu liczby próbek:
x = runif(500, min = a, max = b) 
ks.test(x, 'pnorm', mean = (a + b)/2, sd = sqrt((b - a)^2/12)) 

# Zweryfikuj czy dane z rozkładu normalnego dopasowują się do rozkładu jednostajnego
n = 100; 
mi= 1; 
sigma = 2;
x = rnorm(n, mean = mi, sd = sigma) 
ks.test(x, 'punif', min = mi-sigma, max = mi+sigma)


#### PRZANALIZWOAC: OD 8 slajdu
# Test Normalności

# DLA ROZKLADU NORMALNEGO
n = 100; mi = 1; sigma = 2;
x = rnorm(n, mean = mi, sd = sigma)

# Korzystając z testu Kołmogorowa-Smirnowa zweryfikuj hipotezę, że dane pochodzą z rozkładu N(1,4) lub N(0,9): 
ks.test(x, 'pnorm', mean = 1, sd = 2)
ks.test(x, 'pnorm', mean = 0, sd = 3) 

# Za pomocą testu Shapiro-Wilka zweryfikuj hipotezę, że dane pochodzą z rozkładu normalnego
shapiro.test(x)

# DLA ROZKLADU JEDNOSTAJNEGO
n = 100; a = 2; b = 4;
x = runif(n, min = a, max = b) 

# Zweryfikuj hipotezę, że dane pochodzą z rozkładu normalnego korzystając z testu Shapiro-Wilka
shapiro.test(x) 

# Powtórz weryfikację tej hipotezy korzystając z testu Kołmogorowa-Smirnowa:
ks.test(x, 'pnorm', mean = (a + b)/2, sd = sqrt((b - a)^2/12))


# POROWNANIE SREDNICH

#Wygeneruj dwie próby z rozkładów normalnych N(0,1) oraz N(1,1) o licznościach 9 i 12:
n_1 = 9; 
mi_1 = 0;
sigma_1 = 1
n_2 = 12; 
mi_2 = 1; 
sigma_2 = 1 
x_1 = rnorm(n_1, mean = mi_1, sd = sigma_1) 
x_2 = rnorm(n_2, mean = mi_2, sd = sigma_2)

#Przy poziomie istotności α = 0.05 zweryfikuj hipotezę o równości średnich przy założeniu tej samej wariancji
mean_1 = mean(x_1);
mean_2 = mean(x_2) 
s2_1 = var(x_1); 
s2_2 = var(x_2) 
s2 = ((n_1 - 1)*s2_1 + (n_2 - 1)*s2_2)/(n_1 + n_2 - 2) 
alfa = 0.05; 
c = qt(1 - alfa/2, n_1 + n_2 - 2) 
T = abs(mean_1 - mean_2) / ( sqrt(s2*( n_1^(-1) + n_2^(-1) ) ) )
p_value = 2*(1 - pt(T, n_1 + n_2 - 2))

# w pakiecie r to samo co wyzej:
t.test(x_1, x_2, var.equal = T)

# Powtórz testy dla innych wartości mi_2, np. mi_2 = 0, 0.5, 2....


# Przeprowadź analogiczne testy bez zakładania równości wariancji 
s2 = s2_1/n_1 + s2_2/n_2 
d = (s2^2)/(((s2_1/n_1)^2)/(n_1-1) + ((s2_2/n_2)^2)/((n_2-1))) 
c = qt(1 - alfa/2,  d) 
T = abs(mean_1 - mean_2)/sqrt(s2) 
p_value = 2*(1 - pt(T, d)) 

# Lub z pakietu R:
t.test(x_1, x_2)


# Przeprowadź analogiczne testy bez zakładania normalności rozkładów
wilcox.test(x_1, x_2)

#Funkcja gęstości rozkładu Wilcoxona
plot(dwilcox(0:100, length(x_1), length(x_2)), type = 'l'); grid()


# TEST NIEZALEZNOSCI
# Przeprowadź testy niezależności dla danych zawartych w tabeli kontyngencji xx

x_1 = c(16, 25, 11)
x_2 = c(13, 32, 15) 
x_3 = c(31, 43, 26) 
xx = cbind(x_1, x_2, x_3) 
I = 3 
J = 3 
n_i = x_1 + x_2 + x_3 
n_j = c(sum(x_1), sum(x_2), sum(x_3))
N = sum(n_j)

#Obliczenie wartości statystyki decyzyjnej, progu i p-wartości: 

T = 0 
for (i in 1:I) 
  { 
  for (j in 1:J) 
    { 
    T = T + (N*xx[i,j] - n_i[i]*n_j[j])^2/(N*n_i[i]*n_j[j]) 
    } 
  } 
alfa = 0.05 
c = qchisq(1 - alfa, df = (I - 1)*(J - 1)) 
p_val = 1 - pchisq(T, df = (I - 1)*(J - 1)) 

# lub z pakietu R:
chisq.test(xx)
