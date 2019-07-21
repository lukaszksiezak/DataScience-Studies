#Przyklad 1
unzip('mstall.zip', 'KGHM.mst')
df_KGHM = read.csv('KGHM.mst')
df_KGHM
names(df_KGHM) = c('ticker', 'date', 'open', 'high', 'low', 'close',
                   'vol')
df_KGHM$date = as.Date.character(df_KGHM$date, format =
                                   '%Y%m%d')

  plot(open ~ date, df_KGHM,
       type = 'l', col = 'blue',
       xlab = 'Data', ylab = 'Kurs otwarcia [zł]',
       main = 'KGHM' )
  grid()
  
  df_KGHM$open_ch= with(df_KGHM,
                        c(NA, 100*diff(open)/open[1:length(open) -1]))
  plot(open_ch ~ date, df_KGHM,
       type = 'l', col = 'blue',
       xlab = 'Data', ylab = 'Procentowa zmiana kursu otwarcia [%]',
       main = 'KGHM' )
  grid()
  
  hist(df_KGHM$open_ch,
       breaks = 50, prob = T,
       xlab = 'Zmiana kursu otwarcia [%] ',
       ylab = 'Częstość występowania',
       main = 'Histogram procentowych zmian kursu KGHM' )
  grid()
  
  m = mean(df_KGHM$open_ch, na.rm = T)
  s = sd(df_KGHM$open_ch, na.rm = T)
  
  curve(dnorm(x, mean = m, sd = s), add = T, col = 'red', -10, 10)

  boxplot(df_KGHM $open_ch,
          col = 'green',
          xlab = 'KGHM', ylab = 'Zmiana kursu otwarcia [%] ',
          main = 'KGHM' )
  grid()  
  
  
  
  # przyklad 2
  
  kat =
    read.csv('Airplane_Crashes_and_Fatalities_Since_1908.csv')
  
  kat
  
  kat$Year = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%Y')

  plot(table(kat$Year),
       type = 'h', col = 'blue',
       xlab = 'Rok', ylab = 'Liczba katastrof',
       main = 'Liczba katastrof w roku' )
  grid()
  
  Ofiary_agr = aggregate(Fatalities ~ Year, kat, FUN = sum)
  
  Ofiary_agr
  
  
  # Przyklad 3
  
  proba = rnorm(1000, mean = 2, sd = 3)
  plot(proba)
  m = mean(proba); s = sd(proba)
  
  hist(proba, breaks = 20, prob = T)
  curve(dnorm(x, mean = 2, sd = 3), add = T, col = 'red', -15, 15)
  grid()
  
  plot(ecdf(proba))
  curve(pnorm(x, mean = 2, sd = 3), add = T, col = 'red', -15, 15)  

  boxplot(proba)
  grid()
  
  qnorm(c(0.25, 0.5, 0.75), mean = 2, sd = 3)
  quantile(proba, c(0.25, 0.5, 0.75))
  
  # Przyklad 4
  
  M = 1000
  proba = rpois(M, lambda = 4); proba
  m = mean(proba); v = var(proba) m; v;
  
  Arg = 0:max(proba)
  Freq = as.numeric(table(factor(proba, levels = Arg))) / M
  Freq

  plot(Freq ~ Arg, type = 'h', xlab = 'x', ylab = 'f(x)')  
  plot(Arg, dpois(Arg, lambda = 4), type = 'h')
  
  plot(cumsum(Freq) ~ Arg, type = 'b', xlab = 'x', ylab = 'F(x)')
  plot(ecdf(proba))  
  plot(Arg, ppois(Arg, 4), type = 'b')  
  