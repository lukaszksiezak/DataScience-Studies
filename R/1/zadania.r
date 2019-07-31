#################
### ZADANIE 1 ###
#################

r <- 0.03
K <- 10000
n <- 120

q <- 1 + r/12
R <- K * q^n * (q-1)/((q^n)-1)
F <- R * n

cat('Rata kreytu: ', R)

#################
### ZADANIE 2 ###
#################

K <- 500000
r <- 0.007
n <- 360

R <- K/n
i <- 1:n
R1 <- ((K-(i-1)*R)*r)/12
R2 <- R1 + R
F <- sum(R2)
Fsummary <- summary(R2)
Fsummary

R2[1]
R2[length(R2)] # R2[n]

head(R2)
tail(R2, n = 3)

#################
### ZADANIE 3 ###
#################

Dane<-readRDS('wig_changes.rds')

# PODEJŚCIE 1

Wyniki <- matrix(0,2,2)
colnames(Wyniki)<-c('+','-')
rownames(Wyniki)<-c('+','-')

Wyniki
j <- Dane[1]
for (i in Dane[-1]) {
    Wyniki[j,i] <- Wyniki[j,i] +1
    j <- i
}
Wyniki

Wyniki %*% Wyniki %*% Wyniki

# PODEJŚCIE 2

Wyniki <- matrix(0,2,2)
colnames(Wyniki)<-c('+','-')
rownames(Wyniki)<-c('+','-')

Wyniki
j <- Dane[1]
for (i in 2:length(Dane)) {
    Wyniki[Dane[i-1],Dane[i]] <- Wyniki[Dane[i-1],Dane[i]] +1
    j <- i
}
Wyniki

# PODEJŚĆIE 3 (ZARYS)

plusy <- which(Dane == '+')
nextId <- plusy + 1
minusy <- which(Dane[nextId] == '-')
cat('minusy po plusach: ',length(minusy))

# PODEJŚĆIE 1 (PRAWDOPODOBIEŃSTWA)

# for(i in 1:nrow(Wyniki)) {
#     Wyniki[i, ] / sum(Wyniki[i, ]) -> Wyniki[i, ]
# }
# Wyniki

# PODEJŚĆIE 2 (PRAWDOPODOBIEŃSTWA)

Wyniki / rowSums(Wyniki)

#################
### ZADANIE 4 ###
#################

my_sim <- function(K = 100, N = 1000, F = 1000000, T = 12) {
    rep(NA, T) -> S
    for(t in 1:12) {
        K * N + ifelse(t == 1, 0, S[t - 1]) -> S[t]
        
        sum(rt(N, df = 2) > qt(0.9999, df = 2)) -> a
        
        S[t] - a * F -> S[t]
        if(S[t] < 0) {
            NA -> S[t]
            break
        }
        
        sample(0:100, 1) -> n
        sample(0:90, 1) -> o
        N + n - o - a -> N
    }
    
    S
}

1000 -> M
12 -> T
matrix(NA, M, T) -> SIM
for(i in 1:M) {
    my_sim(T = T) -> SIM[i, ]
}

# Jakie jest prawdopodobieństwo tego, że spółka nie zbankrutuje do
# chwili t = 1, 2, …, T?
colMeans(is.na(SIM))

# Jaki średni poziom rezerw będzie miała spółka pod warunkiem, że nie
# zbankrutuje do chwili t = 1, 2, …, T?

colMeans(SIM, na.rm = TRUE)

# Jaki jest oczekiwany okres życia spółki przy założeniu, że maksymalny
# czas jej życia wynosi T?

mean(rowSums(!is.na(SIM)))

#################
### ZADANIE 8 ###
#################

setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R/')

readRDS('./zadania/grupa_3/1/bank_register.rds') -> data

data.frame(
    client_id = rep(NA, nrow(data)),
    argument_id = rep(NA, nrow(data)),
    date = rep(NA, nrow(data)),
    income = rep(NA, nrow(data)),
    sex = rep(NA, nrow(data)),
    age = rep(NA, nrow(data)),
    child = rep(NA, nrow(data)),
    dep = rep(NA, nrow(data)),
    cre = rep(NA, nrow(data)),
    mor = rep(NA, nrow(data))
) -> clean_data

strsplit(data$id, split = '_', fixed = TRUE) -> X
for(i in 1:length(X)) {
    as.numeric(X[[i]][1]) -> clean_data[i, 'client_id']
    as.numeric(X[[i]][2]) -> clean_data[i, 'argument_id']
}
clean_data
# do.call(rbind, X)

Sys.setlocale("LC_TIME", "C")
data$date <- as.Date(data$date, format = '%b %d, %Y')
data

clean_data$income <- gsub("$", "", data$income, fixed = TRUE)
clean_data$income <- gsub(".", "", clean_data$income, fixed = TRUE)
clean_data$income <- as.numeric(gsub(",", ".", clean_data$income), fixed = TRUE)

clean_data
str(clean_data)
