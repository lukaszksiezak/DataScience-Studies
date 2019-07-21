# przyklad 1
# Przedziały ufności 

n = 30;
x = rnorm(n, mean = 1, sd = 2);
x_mean = mean(x);
x_var = var(x);

#plot(x, type='l')
#x_mean
#x_var

lev = 0.95;  # 0.95, 0.99 
S = sd(x);
w = S*qt((1+lev)/2, n-1)/sqrt(n);
ci_mean = c(x_mean - w, x_mean + w);

a = (1 - lev)/2;
b = (1 - lev)/2;
ci_var = c((n-1)*S^2/qchisq(1-b,n-1), (n-1)*S^2/qchisq(a,n-1))

# przyklad 2
# Bootstrap parametryczny

# Metodą bootstrapu parametrycznego oszacuj odchylenia standardowe 
# estymatora wartości średniej i estymatora wariancji.

# Metoda 1:
K = 1000;
boot_res = replicate(K,  {
                          boot_dane = rnorm(n, 
                                            mean = x_mean, 
                                            sd = sqrt(x_var)) 
                          c(mean(boot_dane), var(boot_dane)) 
                          }
                     )
sd_mean = sd(boot_res[1,]) 
sd_var = sd(boot_res[2,])

# Metoda 2:
K = 1000;
boot_mean = c();
boot_var = c();
for (k in 1:K) 
  { 
  boot_dane = rnorm(n, mean = x_mean, sd = sqrt(x_var)) 
  boot_mean[k] = mean(boot_dane) 
  boot_var[k]  = var(boot_dane) 
} 

sd_mean = sd(boot_mean) 
sd_var = sd(boot_var)


# Przyklad 3:
# Bootstrap nieparametryczny

# Metodą bootstrapu nieparametrycznego oszacuj odchylenia standardowe estymatora 
# wartości średniej i estymatora wariancji. 

# Metoda 1:
K = 1000;
boot_res = replicate(K,
                     { 
                       boot_dane = sample(x, n, replace = T);
                       c(mean(boot_dane), var(boot_dane));
                     })
sd_mean = sd(boot_res[1,]);
sd_var = sd(boot_res[2,]);

# Metoda 2:
K = 1000;
boot_mean = c();
boot_var = c();

for (k in 1:K) 
  { 
  boot_dane = sample(x, n, replace = T);
  boot_mean[k] = mean(boot_dane);
  boot_var[k]  = var(boot_dane);
  } 
sd_mean = sd(boot_mean); 
sd_var = sd(boot_var);

# przedzialy ufnosci metoda bootstrapu
lev = 0.95
int_mean = quantile(boot_res[1,], c((1-lev)/2,(1+lev)/2)) 
int_var = quantile(boot_res[2,], c((1-lev)/2,(1+lev)/2))


# Przykład 4:
# Metoda momentów 

# Wygeneruj 1000 liczb będących próbą losową z rozkładu gamma o parametrach: shape = 1.1, scale = 5.
n = 1000;
x_g = rgamma(n, shape = 1.1, scale = 5);

# Korzystając ze wzorów na estymatory parametrów rozkładu gamma dla metody momentów wyznacz ich wartości. 
m1 = mean(x_g);
m2 = mean(x_g^2);
alpha_mom = m1^2/(m2 - m1^2);
beta_mom = (m2 - m1^2)/m1;

# Przyklad 5:
# Metoda najwyższej wiarygodności

# Metoda 1:
# Dla wygenerowanej próby losowej wyznacz wartości estymatorów parametrów 
# uzyskane za pomocą metody największej wiarygodności. 
# W celu uzyskania estymatora parametru kształtu α rozwiązujemy numerycznie równanie podane na wykładzie
fun = function(x) digamma(x) - log(x) - mean(log(x_g)) + log(mean(x_g));
alpha_nw = uniroot(fun, lower = 0.5, upper = 4)$root;
beta_nw = mean(x_g)/alpha_nw

# Metoda 2:
# Inny sposób z wykorzystaniem funkcji fitdistr() z pakietu MASS (należy dodać pakiet MASS). 
est_nw = fitdistr(x_g, 'gamma', list(shape=1, scale=1), lower=0);
alpha_nw = as.numeric(est_nw$estimate[1]);
beta_nw = as.numeric(est_nw$estimate[2])
