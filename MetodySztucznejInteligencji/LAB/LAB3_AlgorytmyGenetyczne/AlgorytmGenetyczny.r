cities = read.csv("./cities.csv", header = TRUE, sep=",")
View(cities)
summary(cities)

bestResult <- 99999


totalDistance = function(visitedCities, distances=cities) {
  visitedCities = c(visitedCities, visitedCities[1])
  route = embed(visitedCities, 2)[, 2:1]
  distancesSum = sum(distances[route])
  return(distancesSum)
}

permu <- function(perm,fun,current=NULL){
  for(i in 1: length(perm)){
    fix <- c(current,perm[i]) # calculated elements; fix at this point
    
    rest <- perm[-i] # elements yet to permutate
    #Call callback.
    if(!length(rest)){
      result <- fun(fix)
      if(result<bestResult){
        assign("bestResult", result, envir = .GlobalEnv)
        print(bestResult)
      }
    }
    if(length(rest)){
      result <- permu(rest, fun, fix)
    }
  }
}

install.packages("GA")
library(GA)
permu(c("Barcelona", "Belgrade", "Berlin", "Brussels",  "Budapest", "Copenhagen"), totalDistance)


costFunction <- function(cfVisitedCities, distance=cities){
  -totalDistance(cfVisitedCities)
  }

?ga

pm = 0.2
pco = 0.8
elitism =0.05
popSize = 50
GA.fit = ga(type = "permutation", fitness = costFunction, min = 1, max =
       length(cities), pmutation = pm, pcrossover = pco, elitism =
       elitism, popSize = popSize)


GA.fit@solution
plot(GA.fit)
summary(GA.fit)
totalDistance(GA.fit@solution)

# Zadanie 9

results = replicate(30, ga(type = "permutation", fitness = costFunction, min = 1, max =
                             length(cities), pmutation = pm, pcrossover = pco, elitism =
                             elitism, popSize = popSize))

res1fitnesses = sapply(results, function(x){x@fitnessValue})
hist(res1fitnesses)
summary(res1fitnesses)


# Zadanie 10

dCostFunction = function(x){
  ga(type = "permutation",
     fitness = costFunction, min = 1, max =
       length(cities),
     pmutation = x[1],
     pcrossover = x[2],
     elitism = x[3],
     popSize = x[4], maxiter = 100)@fitnessValue
}


final = ga(type="real-valued", fitness=dCostFunction,
           min = c(0.01, 0.7, 0.01, 80), max = c(0.2, 0.9, 0.10, 90),
           popSize=5, maxiter = 10)

final@population
final@fitness
params = final@solution
ga(type = "permutation",
   fitness = costFunction, min = 1, max = length(cities),
   pmutation = params[1],
   pcrossover = params[2],
   elitism = params[3],
   popSize = params[4], maxiter = 1500)

plot(GA.fit)
summary(GA.fit)
totalDistance(GA.fit@solution)

params[1] #pmutation
params[2] #pcossover
params[3] #elitism
params[4] #popSize
 