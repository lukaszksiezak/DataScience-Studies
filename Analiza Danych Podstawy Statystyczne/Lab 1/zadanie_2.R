kat =
  read.csv('Airplane_Crashes_and_Fatalities_Since_1908.csv')

# w miesiacu
kat$Month = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%m')
plot(table(kat$Month),
     type = 'h', col = 'blue',
     xlab = 'Rok', ylab = 'Liczba katastrof',
     main = 'Liczba katastrof w miesiacu' )
grid()
Ofiary_agr = aggregate(Fatalities ~ Month, kat, FUN = sum)
Ofiary_agr

# w dniu
kat$day = strftime(as.Date(kat$Date, '%m/%d/%Y'), '%d')
plot(table(kat$day),
     type = 'h', col = 'blue',
     xlab = 'Rok', ylab = 'Liczba katastrof',
     main = 'Liczba katastrof w miesiacu' )
grid()
Ofiary_agr = aggregate(Fatalities ~ day, kat, FUN = sum)
Ofiary_agr

# w dniu tygdnia
kat$weekday = weekdays((as.Date(kat$Date, '%m/%d/%Y')), TRUE)
plot(table(kat$weekday),
     type = 'h', col = 'blue',
     xlab = 'Rok', ylab = 'Liczba katastrof',
     main = 'Liczba katastrof w miesiacu' )
grid()
Ofiary_agr = aggregate(Fatalities ~ weekday, kat, FUN = sum)
Ofiary_agr

