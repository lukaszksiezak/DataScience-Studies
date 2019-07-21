read_company_data = function(nazwa_archiwum, nazwa_spolki){
  unzip('mstall.zip', nazwa_archiwum)
  df_data = read.csv(nazwa_archiwum)
  Header <- c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
  names(df_data) = Header
  df_data$date = as.Date.character(df_data$date, format = '%Y%m%d')
  
  # Dodanie nowych pol obliczajacych procentową zmianę ceny najwyższej i otwarcia  
  df_data$high_ch= with(df_data, c(NA, 100*diff(high)/high[1:length(high) -1]))
  df_data$open_ch= with(df_data, c(NA, 100*diff(open)/open[1:length(open) -1]))
  return(df_data)
} 

# Wczytaj dane dla spolki
company_data = read_company_data('CCC.mst', 'CCC')

hist(company_data$high_ch, main="% zmiany ceny najwyzszej") # zgrubnie mozna ocenic ze procentowe zmiany ceny najwyzszej ma rozklad normalny
hist(company_data$open_ch, main="% zmiany ceny otwarcia") # zgrubnie mozna ocenic ze procentowe zmiany ceny otwarcia również ma rozklad normalny

# estymaty wartości średniej i wariancji zmian najwyzszych cen w dniu
high_mean = mean(company_data$high_ch, na.rm = T)
high_var = var(company_data$high_ch, na.rm = T)


# Zakładając, że zmiany cen otwarcia wartość mają rozkład normalny wyznacz 90%, 95% i 99% 
# przedziały ufności dla wartości średniej i wariancji procentowych 
#zmian najwyższych cen w dniu dla wybranej spółki.

# Dla nieznane µ i σ2 (tak jak przyklad str 3.)

przedzial_ufnosci_dla_spolki = function(lev, x)
{
  n = length(x)
  x_mean = mean(x)
  S = sd(x);
  w = S*qt((1+lev)/2, n-1)/sqrt(n);
  ci_mean = c(x_mean - w, x_mean + w);
  
  a = (1 - lev)/2;
  b = (1 - lev)/2;
  ci_var = c((n-1)*S^2/qchisq(1-b,n-1), (n-1)*S^2/qchisq(a,n-1))
  
  return(c(ci_mean, ci_var))
}

x = company_data$high_ch[2:length(company_data$high_ch)]

# 90% przedział ufności
lev=0.9
ci_mean_90 = c(przedzial_ufnosci_dla_spolki(lev, x)[1], przedzial_ufnosci_dla_spolki(lev, x)[2])
ci_var_90 = c(przedzial_ufnosci_dla_spolki(lev, x)[3], przedzial_ufnosci_dla_spolki(lev, x)[4])

# 95% przedział ufności
lev=0.95
ci_mean_95 = c(przedzial_ufnosci_dla_spolki(lev, x)[1], przedzial_ufnosci_dla_spolki(lev, x)[2])
ci_var_95 = c(przedzial_ufnosci_dla_spolki(lev, x)[3], przedzial_ufnosci_dla_spolki(lev, x)[4])

# 99% przedział ufności
lev=0.99
ci_mean_99 = c(przedzial_ufnosci_dla_spolki(lev, x)[1], przedzial_ufnosci_dla_spolki(lev, x)[2])
ci_var_99 = c(przedzial_ufnosci_dla_spolki(lev, x)[3], przedzial_ufnosci_dla_spolki(lev, x)[4])
