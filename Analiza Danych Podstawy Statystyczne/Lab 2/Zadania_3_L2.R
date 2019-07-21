alfa_apriori = 10
beta_apriori = 10

E_priori = alfa_apriori / (alfa_apriori + beta_apriori) # Wartosc oczekiwana dla rozkladu beta

p = seq(0,1, length=100)
plot(p, dbeta(p, alfa_apriori, beta_apriori), type ="l", col='red', main='apriori')


# 20 rzutów pinezki
n = 20
sim_throw = c(0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0)
sum(sim_throw)

alfa_posteriori = alfa_apriori + sum(sim_throw);
beta_posteriori = beta_apriori + n - sum(sim_throw)

# estymator:
E_posteriori = alfa_posteriori / (alfa_posteriori + beta_posteriori) 

lines(p, dbeta(p, alfa_posteriori, beta_posteriori), type ="l", col='green', main='aposteriori 20 rzutow')

# Dodatkowe 20 rzutów
n_additional = 20
additional_throws = c(0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0)

alfa_posteriori_additional = alfa_apriori + (sum(sim_throw) + sum(additional_throws));
beta_posteriori_additional = beta_apriori + (n + n_additional) - (sum(sim_throw) + sum(additional_throws))

# estymator:
E_posteriori_additional = alfa_posteriori_additional / (alfa_posteriori_additional + beta_posteriori_additional) 

plot(p, dbeta(p, alfa_posteriori_additional, beta_posteriori_additional), type ="l", col='red', main='aposteriori 40 rzutow')

var_beta = function(alfa, beta){
  return((alfa*beta)/((alfa + beta)^2 *(alfa + beta + 1)))
}

var_apriori = var_beta(alfa_apriori, beta_apriori)
var_aposteriori_20 = var_beta(alfa_posteriori, beta_posteriori)
var_aposteriori_40 = var_beta(alfa_posteriori_additional, beta_posteriori_additional)