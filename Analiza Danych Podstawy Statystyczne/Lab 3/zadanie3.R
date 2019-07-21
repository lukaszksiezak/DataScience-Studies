read_company_data = function(nazwa_archiwum, nazwa_spolki){
  unzip('mstall.zip', nazwa_archiwum)
  df_data = read.csv(nazwa_archiwum)
  Header <- c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
  names(df_data) = Header
  df_data$date = as.Date.character(df_data$date, format = '%Y%m%d')
  
  # Dodanie nowych pol obliczajacych procentową zmianę ceny najnizszej  
  df_data$low_ch= with(df_data, c(NA, 100*diff(low)/low[1:length(low) -1]))
  
  return(df_data)
} 

# Wczytaj dane dla spolki
company_data$date <- as.Date(company_data$date, format="%Y-%m-%d")
company_data = read_company_data('CCC.mst', 'CCC')

# wybierz tylko 2018
dane_2018 <- subset(company_data, date> "2018-01-01" & date < "2018-12-30")

# histogram

hist(dane_2018$low_ch,
     breaks = 50, prob = T, lwd = 1.5)
grid()

sd_dane = sd(dane_2018$low_ch)
mean_dane = mean(dane_2018$low_ch)

curve(dnorm(x, mean = mean_dane, sd=sd_dane), add = TRUE, lwd = 1.5, col = 'red', -10, 10)

#weryfikacja czy dane maja rozklad normalny:
ks.test(dane_2018$low_ch, 'pnorm', mean = mean(dane_2018$low_ch), sd = sd(dane_2018$low_ch))
shapiro.test(dane_2018$low_ch)


