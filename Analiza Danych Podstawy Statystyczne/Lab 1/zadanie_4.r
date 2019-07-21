srednie = function(mm, M){
  srednia = mean(rbinom(M, 20,0.8))
  return (srednia)
  }
  

mm = replicate(500, mean(rbinom(M, 20, 0.8)))
hist(mm, breaks = 20, prob = T)


srednie(mm, 1000)
srednie(mm, 10000)