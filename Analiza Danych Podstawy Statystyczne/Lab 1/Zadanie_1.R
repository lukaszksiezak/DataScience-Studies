zamkniecie_spolki = function(nazwa_archiwum, nazwa_spolki){
  unzip('mstall.zip', nazwa_archiwum)
  
  df_data = read.csv(nazwa_archiwum)
  
  Header <- c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')
  names(df_data) = Header
  
  df_data$date = as.Date.character(df_data$date, format = '%Y%m%d')
  
  df_data$close_ch= with(df_data, c(NA, 100*diff(close)/close[1:length(close) -1]))
  return(df_data)
} 

zamkniecie_spolki_procent = function(nazwa_archiwum, nazwa_spolki){

  zamkniecie_spolki(nazwa_archiwum, nazwa_spolki) -> df_data
  
  plot(close_ch ~ date, df_data,
       type = 'l', col = 'blue',
       xlab = 'Data', ylab = 'Procentowa zmiana kursu zamkniecia [%]',
       main = nazwa_spolki )
  grid()
}
zamkniecie_spolki_histogram = function(nazwa_archiwum, nazwa_spolki){
  unzip('mstall.zip', nazwa_archiwum)
  
  zamkniecie_spolki(nazwa_archiwum, nazwa_spolki) -> df_data
  
  hist(df_data$close_ch,
       breaks = 50, prob = T,
       xlab = 'Zmiana kursu otwarcia [%] ',
       ylab = 'Częstość występowania',
       main = sprintf("Histogram procentowych zmian kursu spolki %s", nazwa_spolki))
  grid()
}

wykresy_pudelkowe = function(spolka_1_arch, spolka_1_name, spolka_2_arch, spolka_2_name){
  zamkniecie_spolki(spolka_1_arch, spolka_1_name) -> spolka_1_df
  zamkniecie_spolki(spolka_2_arch, spolka_2_name) -> spolka_2_df
  
  boxplot(spolka_1_df$close_ch, spolka_2_df$close_ch,
          col = c("orange","red"),
          names = c(spolka_1_name, spolka_2_name),
          main = 'Procent zamkniecia')
  
  grid()
}

zamkniecie_spolki_procent('CDPROJEKT.mst', 'CD PROJEKT') -> data_cdprojekt
zamkniecie_spolki_procent('CCC.mst', 'CCC') -> data_ccc

zamkniecie_spolki_histogram('CDPROJEKT.mst', 'CD PROJEKT') -> data_cdprojekt
zamkniecie_spolki_histogram('CCC.mst', 'CCC') -> data_ccc

wykresy_pudelkowe ('CDPROJEKT.mst', 'CCC' , 'CDPROJEKT.mst', 'CD PROJEKT')
