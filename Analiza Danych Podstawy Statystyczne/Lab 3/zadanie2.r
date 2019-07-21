dane = data.frame(
  c(1:12), 
  c(1867, 1789, 1944, 2094, 2097, 1981, 1887, 2024, 1928, 2032, 1978, 1859),
  c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

names(dane) <- c('miesiac', 'suicides', 'dni')

p_i = dane$dni/sum(dane$dni)
p_i

chisq.test(dane$suicides, p = p_i)


# p-value = 1.852e-06 H0 odrzucamy, wskazuje to na sezonowosc