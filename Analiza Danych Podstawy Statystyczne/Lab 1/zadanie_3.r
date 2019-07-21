empiryczna_funkcja_prawdopodobienstwa = function(v_rozkladu){
  Arg = 0:max(v_rozkladu)
  Freq = as.numeric(table(factor(v_rozkladu, levels = Arg))) / M
  plot(Freq, type ='h')
  return (Freq)
}

empiryczna_dystrybuanta = function(v_rozkladu){
  Arg = 0:max(v_rozkladu)
  Freq = as.numeric(table(factor(v_rozkladu, levels = Arg))) / M
  plot(cumsum(Freq) ~ Arg, type = 'b', xlab = 'x', ylab = 'F(x)')
  return (Freq)
}

empir_prawdopodobienstwo_08 = function(M)
{
  v_binom_08 = rbinom(M, 20,0.8) 
  empiryczna_funkcja_prawdopodobienstwa(v_binom_08) -> freq
  plot(freq, type='h');
  return(freq)
}

empir_dystrybuanta_08 = function(M)
{
  v_binom_08 = rbinom(M, 20,0.8) 
  empiryczna_dystrybuanta(v_binom_08) -> freq
  plot(freq, type='h');
  return(freq)
}

prawd_i_war = function(M){
  v_binom_08 = rbinom(M, 20,0.8) 
  m_teor = mean(v_binom_08); 
  v_teor = var(v_binom_08);
}

M = 1000
v_binom_02 = rbinom(M, 20,0.2) 
v_binom_08 = rbinom(M, 20,0.8) 

empiryczna_funkcja_prawdopodobienstwa(v_binom_02)
empiryczna_funkcja_prawdopodobienstwa(v_binom_08)
empiryczna_dystrybuanta(v_binom_02)
empiryczna_dystrybuanta(v_binom_08)

empir_prawdopodobienstwo_08(100)
empir_prawdopodobienstwo_08(1000)
empir_prawdopodobienstwo_08(10000)

empir_dystrybuanta_08(100)
empir_dystrybuanta_08(1000)
empir_dystrybuanta_08(10000)

prawd_i_war(100)
prawd_i_war(1000)
prawd_i_war(10000)
