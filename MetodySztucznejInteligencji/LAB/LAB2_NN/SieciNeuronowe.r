### Łukasz Księżak MSI, gr. 3 DS


#Zad1.
#sigmoid
sigmoid = function(x) {
  1 / (1 + exp(-x))
}

relu = function(x){
  pmax(0, x)  
}


x <- seq(-10, 10, 0.01)

plot(sigmoid(x), x=x) # wykres sigmiod
plot(relu(x), x=x) # wykres relu
plot(tanh(x), x=x) # tangens hiperboliczny

# Wyraz wolny, graficznie patrząc na funkcje aktywacji 
# przesuwa je w lewo, bądź w prawo. 
# W praktyce, wyraz wolny dodawnay jest po to aby lepiej 
# dopasować model do danych.

perceptron <- function(w1,w2,w3,x1,x2){
  y <- w1*x1+w2*x2 + w3  
  if(y>0){
    1
  } else {
    0
  }
}

# Or
y <- perceptron(0.1, 0.1, -0.05, 1,0)

# And
y <- perceptron(0.05, 0.05, -0.09, 1,1)

#Xor
# y <- perceptron(0, 0, 0, 1,1) Not doable

# Zadanie 2,3,4

library(e1071)

computeFeedForward <- function(x, w1, w2, actFunc = sigmoid){
  x = cbind(x, rep(1,dim(x)[1]))
  h = actFunc(x%*%w1)
  
  h2 = cbind(h, rep(1,dim(h)[1]))
  list(ou= actFunc(h2 %*% w2), oh = h)
}

computeBackpropagation <- function(x, y, ou, oh, w1, w2, eta){
  
  delta_u = ou*(1-ou) * (y-ou)
  ohB = cbind(oh, rep(1, dim(oh)[1])) #oh + bias
  
  delta_h = (oh*(1-oh)) * (delta_u %*% t(w2[-length(w2),])) # nie interesuje nas wyraz wolny w2
  xB = cbind(x, rep(1,dim(x)[1])) #x + bias
  
  w2 <- w2 + t(ohB) %*% delta_u * eta
  w1 <- w1 + t(xB) %*% delta_h * eta
  
  list(w1 = w1, w2 = w2)
}


trainANN <- function(x, y, hidden = 10, eta = 0.1, itNmb=10000){
  nmbOfInputsInW1 = dim(x)[2]+1 # +1 bo bias
  nmbOfInputsInW2 = (hidden+1)
  nmbOfOutputs = dim(y)[2]

  w1 <- matrix(rnorm(nmbOfInputsInW1*hidden), nmbOfInputsInW1, hidden)
  w2 <- matrix(rnorm(nmbOfInputsInW2*nmbOfOutputs), nmbOfInputsInW2, nmbOfOutputs)
  
  for(i in 1:itNmb){
    outputs = computeFeedForward(x,w1,w2)
    newW = computeBackpropagation(x,y,outputs$ou,outputs$oh, w1,w2,eta)
    w1 = newW$w1
    w2 = newW$w2
  }
  list(w1=w1, w2=w2, hidden=10)
}

# Zadanie 6
predict <- function(x, model, actFunc = sigmoid){
  x = cbind(x, rep(1,dim(x)[1]))
  h = actFunc( x%*%model$w1 )
  h2 = cbind(h, rep(1,dim(h)[1]))
  
  out = actFunc(h2 %*% model$w2)
  out
}

# Zadanie 7 xor
x = matrix(c(0, 0, 1, 1, 0, 1, 0, 1), 4)
y = matrix(c(0, 1, 1, 0))

model = trainANN(x,y)
predict(x, model)


# Zadanie 8

data(cats, package = "MASS")

# transfromacja danych (F <-1, M <- 0)
x = data.matrix(cats[,-1],rownames.force = NA)
y = data.matrix(cats[,1], rownames.force = NA)
y[y[,1] == "F", ] <- 1
y[y[,1] == "M", ] <- 0

y = data.matrix(as.numeric(y), rownames.force = NA)
y

x
# Przed skalowaniem masa ciała jest znacząco większa,
# niż masy serca, a zatem jedno z wejść bedzie dominowalo nad drugim
# Wynika to z budowy pojedynczego neuronu gdzie suma masy ciala 
# na sumatorze będzie miała większy wpływ na wejściu i masa serce nie 
# będzie wystarczająco brana pod uwagę

model = trainANN(x,y)
res_przed <- predict(x, model)

plec = res_przed
plec[res_przed<0.5] = 0
plec[res_przed>=0.5] = 1
plec = as.factor(plec)

confusionMatrix(plec,as.factor(y))



# Po skalowaniu

# Accuracy zncząco wzrosło, do wartości 79,8%
# Skalownie poprawiło jakoąść klasyfikacji ponieważ obie cechy
# (masa serca i masa ciała) były zrównoważone (z tego samego zakresu)


x = apply(x, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

model = trainANN(x,y)
res_po <- predict(x, model)

plec = res_po
plec[res_po<0.5] = 0
plec[res_po>=0.5] = 1
plec = as.factor(plec)

confusionMatrix(plec,as.factor(y))

#Prediction  0  1
#0 84 18
#1 13 29

#Accuracy : 0.7847  

#SVM <- wykorzystując parametry zoptymalizowane 
#w poprzednim ćwiczeniu.

m_svn <- svm(Sex~., data = cats, kernel="radial", cost=0.61, gamma=0.71)
m_svn
m_predict <- predict(m_svn, cats)

as_num <- as.numeric(m_predict)

plec_predicted_svm = as_num
plec_predicted_svm[as_num==1] = 1
plec_predicted_svm[as_num==2] = 0

as.factor(plec_predicted_svm)
as.factor(y)
confusionMatrix(as.factor(plec_predicted_svm), as.factor(y))

#Prediction  0  1
#0 83 15
#1 14 32

#Accuracy : 0.7986  

# Sieć neuronowa poradziła sobie z problemem klasyfikacji
# na podobnym poziomie co SVM (0.784 vs 0,798)

# k.zbikowski@ii.pw.edu.pl
#[MSI]_imie_naz
# Do 02.02.2020
