# Wczytanie danych z odstępami między chwilami rejestracji kolejnych fotonów
foton_data = scan('fotony.txt');

# Estymacja parametrow używając metody momentów

m1 = mean(foton_data);
m2 = mean(foton_data^2);
alpha_mom = m1^2/(m2 - m1^2);
beta_mom = (m2 - m1^2)/m1;

# Estymacja parametrow używając metody najwyższej wiarygodności

fun = function(x) digamma(x) - log(x) - mean(log(foton_data)) + log(mean(foton_data));
alpha_nw = uniroot(fun, lower = 0.5, upper = 4)$root;
beta_nw = mean(foton_data)/alpha_nw

# Histogram odstepow miedzy chwilami rejestracji kolejnych fotonow wraz z wykreslonymi rozkladami
# gamma z wyestymowanymi parametrami alfa i beta

hist(foton_data, prob=TRUE, xlab="Odstepy rejestracji fotonow");

x = seq(1:length(foton_data))
gamma_mom = dgamma(x, alpha_mom, 1/beta_mom)
lines(gamma_mom, col = 'red');

gamma_nw = dgamma(x, alpha_nw, 1/beta_nw)
lines(gamma_nw, col = 'green');


# Metodą bootstrapu parametrycznego wyznacz odchylenia standardowe estymatorów parametrów 

# odchylenie standardowe estymatorow parametrow wyznaczonych metoda momentow
n = length(foton_data)
K = 1000;

boot_alpha_mom = c();
boot_beta_mom = c();

for (k in 1:K) 
{ 
  boot_dane = rgamma(n, shape = alpha_mom, rate = 1/beta_mom)
  m1 = mean(boot_dane);
  m2 = mean(boot_dane^2);
  alpha_mom_boot = m1^2/(m2 - m1^2);
  beta_mom_boot = (m2 - m1^2)/m1;
  
  boot_alpha_mom[k] = alpha_mom_boot
  boot_beta_mom[k]  = beta_mom_boot
} 

odchylenie_mm_alfa = sd(boot_alpha_mom) 
odchylenie_mm_beta = sd(boot_beta_mom)

# odchylenie standardowe estymatorow parametrow wyznaczonych metoda najwyzszej wiarygodnosci

n = length(foton_data)
K = 1000;

boot_alpha_nw = c();
boot_beta_nw = c();

for (k in 1:K) 
{ 
  boot_dane = rgamma(n, shape = alpha_nw, rate = 1/beta_nw)
  
  fun = function(x) digamma(x) - log(x) - mean(log(boot_dane)) + log(mean(boot_dane));
  alpha_nw_boot = uniroot(fun, lower = 0.5, upper = 4)$root;
  beta_nw_boot = mean(boot_dane)/alpha_nw
  
  boot_alpha_nw[k] = alpha_nw_boot
  boot_beta_nw[k]  = beta_nw_boot
} 

odchylenie_nw_alfa = sd(boot_alpha_nw) 
odchylenie_nw_beta = sd(boot_beta_nw)

# przedziały ufności na poziomie ufności 95% dla estymatorów parametrow wyznaczonych metoda najwyzszej wiarygodności
lev = 0.95
przed_ufn_alfa_nw = quantile(boot_alpha_nw, c((1-lev)/2,(1+lev)/2)) 
przed_ufn_beta_nw = quantile(boot_beta_nw, c((1-lev)/2,(1+lev)/2))

# przedziały ufności na poziomie ufności 95% dla estymatorow parametrów wyznaczonych metodą momentów
przed_ufn_alfa_mm = quantile(boot_alpha_mom, c((1-lev)/2,(1+lev)/2)) 
przed_ufn_beta_mm = quantile(boot_beta_mom, c((1-lev)/2,(1+lev)/2))

# maria.gibinska@gmail.com

