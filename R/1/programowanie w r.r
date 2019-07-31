#################
### ZADANIE 1 ###
#################

### OPERATORY ARYTMETYCZNE ###

9.99 + 0.01
9.99 - 0.99
9.99 * 100
9.99 / 10

5^3
5**3

5 %/% 3
5 %% 3
3 * (5 %/% 3) + (5 %% 3)

10000 * (1 + 0.03)**5

### OPERATORY PRZYPISANIA ###

x <- 1; x # DOBRZE
2 -> y; y # DOBRZE
z = 3; z # ŹLE

10000 -> K
0.03 -> r
5 -> n

K * (1 + r)**n

### UŻYWANIE FUNKCJI ###

# POMOC

# nazwa_funkcji()
# nazwa_funkcji(argument_1, argument_2)
# sum(2, 3) -> z; z

?format
??format

# ŹLE
format(1023323.123, FALSE, 2, 2, 'right', 11, TRUE, FALSE, ' ', 3, '', 5, ',')

# MNIEJ ŹLE
format(
    1023323.123,
    trim = FALSE, digits = 2, nsmall = 2, justify = 'right', width = 11,
    na.encode = TRUE, scientific = FALSE, big.mark = ' ', big.interval = 3,
    small.mark = '', small.interval = 5, decimal.mark = ','
)

# JESZCZE MNIEJ ŹLE
format(
    1023323.123,
    scientific = FALSE, digits = 2, nsmall = 2,
    big.mark = ' ', big.interval = 3,
    decimal.mark = ',', na.encode = TRUE,
    small.mark = '', small.interval = 5,
    width = 11, trim = FALSE, justify = 'right'
)

# DOBRZE
format(
    1023323.123,
    scientific = FALSE, nsmall = 2,
    big.mark = ' ', decimal.mark = ',',
    width = 11, justify = 'right'
)

#################
### ZADANIE 2 ###
#################

### WEKTORY ###

# KONSTRUKCJA

# logical   : TRUE    FALSE
# numeric   : 3   2.1  3.21
# character : 'ala ma kota'

logical(10)
numeric(10)
character(10)

c(1, 2, 1, 3, 2)

rep(4, 10)

2:9
seq(2, 9)
seq(1, 10, 2)
seq(1, 10, length.out = 5)

# WEKTOR TO NAJPROSTSZA STRUKTURA W R
is.vector(2:9)
is.vector(2)

c(1, 2, 9) -> x
is.logical(x)
is.numeric(x)
is.character(x)

# WEKTOR ZAWIERA ELEMENTY TYLKO JEDNEGO TYPU
c(TRUE, 3, 'R Language')
c(TRUE, 3)
c(TRUE)

# OPERACJE ZWEKTORYZOWANE
c(2, 3, 5, 4, 2, 2, 3, 4) -> x; x
c(3, 4, 5, 2, 3, 2, 1, 2) -> y; y
x + y

# REGUŁA ZAWIJANIA
c(2, 3, 5, 4, 2, 2, 3, 4) -> x; x
c(3, 4, 5, 2) -> y; y
x + y

c(2, 3, 5, 4, 2, 2, 3, 4) -> x; x
2 -> y; y
x + y

c(2, 3, 5, 4, 2, 2, 3, 4, 2) -> x; x
c(3, 4, 5, 2) -> y; y
x + y

# INDEKSOWANIE PRZEZ POZYCJĘ
c('jeden', 'dwa', 'trzy', 'cztery', 'pięć', 'sześć') -> x
x[1]
x[4]
x[-5]

# INDEKSOWANIE PRZEZ WEKTOR POZYCJI
c('jeden', 'dwa', 'trzy', 'cztery', 'pięć', 'sześć') -> x
x[c(1, 3)]
x[2:4]
x[-(2:4)]
x[c(1, 1, 1, 2, 3)]

# INDEKSOWANIE PRZEZ WEKTOR LOGICZNY
c('jeden', 'dwa', 'trzy', 'cztery', 'pięć', 'sześć') -> x
x[c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)]
x[c(TRUE, FALSE, TRUE)]

# INDEKSOWANIE PRZEZ NAZWĘ
c(val_1 = 'jeden', val_2 = 'dwa', val_3 = 'trzy') -> x
x['val_2'] # x[2]
x[c('val_2', 'val_3')] # x[c(2, 3)]

# WŁAŚCIWOŚCI WEKTORA
c(x_1 = 'jeden', x_2 = 'dwa', x_3 = 'trzy', x_4 = 'cztery', x_5 = 'pięć') -> x
length(x)
names(x)

c('y_1', 'y_2', 'y_3', 'y_4', 'y_5') -> names(x); x
NULL -> names(x); x

# PORZĄDKOWANIE WEKTORA
c(1, 3, 2, 5, 4) -> x
sort(x)
order(x)
rev(x)
sample(x)
sample(seq(1, 10, 2)) -> x; x
rank(x)

# WEKTOR JAKO ZBIÓR
c('low', 'mid', 'high') -> edu_1
c('mid', 'high', 'very_high') -> edu_2
c('low', 'low', 'mid', 'high', 'mid', 'mid', 'low', 'high') -> edu_3

union(edu_1, edu_2)
intersect(edu_1, edu_2)

setdiff(edu_1, edu_2)
setdiff(edu_2, edu_1)

setequal(edu_1, edu_2)
setequal(edu_1, edu_3)

unique(edu_3)

'low' %in% edu_1 # is.element('low', edu_1)
'very_high' %in% edu_1 # is.element('very_high', edu_1)

### WEKTOR NUMERYCZNY ###

# RZUTOWANIE NUMERIC NA INNE TYPY
as.logical(0) # = FALSE
as.logical(1) # = TRUE
as.logical(4.12) # = TRUE
as.logical(-2.12) # = TRUE

as.character(0) # = "0"
as.character(4.23) # = "4.23"

# PODSTAWOWE STATYSTYKI
c(2.6, 4.3, 3.7, 0.9, 0.0, -0.9, -0.6, 2.0, 1.6) -> pol_infl

min(pol_infl)
max(pol_infl)

mean(pol_infl)
median(pol_infl)

quantile(pol_infl, probs = c(0.25, 0.75))

summary(pol_infl)

var(pol_infl)
sd(pol_infl)

# TABELE LICZNOŚCI
sample(2:5, size = 100, replace = TRUE, prob = c(0.1, 0.4, 0.3, 0.2)) -> marks
table(marks)

# PRZEKSZTAŁCENIA MATEMATYCZNE
c(3.12, 3.2144, -2, 23.43) -> x; x

sign(x)

round(x, 1)
floor(x)
ceiling(x)

abs(x)
sqrt(x)
exp(x)
log(x)

sin(x)
cos(x)
tan(x)

# FUNKCJE DZIAŁAJĄCE NA PODZBIORZE ELEMENTÓW
sample(seq(-1000, 3000, 1000), size = 5, replace = TRUE) -> income; income

cumsum(income) # cumprod cummin cummax
diff(income)

# WARTOŚCI SPECJALNE
# NA - Not Available
is.na(c(1, 2, NA, 5))

# Inf - Infinity
2 / 0
is.na(c(1, 3, Inf, 4))
is.infinite(c(1, 3, Inf, 4))

# NaN - Not a Number
0 / 0
Inf - Inf
log(-2)

is.na(c(1, 4, NaN, 9))
is.nan(c(1, 4, NaN, 9))

#################
### ZADANIE 3 ###
#################

### PRZYDATNE FUNKCJE - ZARZĄDZANIE ŚRODOWISKIEM R ###

# PORZĄDKOWANIE ZMIENNYCH
123 -> new_variable_1
'Ala' -> new_variable_2
'kot' -> new_variable_3
TRUE -> new_variable_4
FALSE -> new_variable_5
1e10 -> new_variable_6
-1 -> new_variable_7

ls()
rm(new_variable_1)
ls()
rm('new_variable_2')
ls()
rm(list = c('new_variable_3', 'new_variable_4'))
ls()
rm(list = ls())
ls()

# KATALOG BAZOWY
getwd()
list.files()
setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R')
getwd()
list.files()

# ŁADOWANIE DANYCH RDS
readRDS('./data/age.rds')

### PRZYDATNE FUNKCJE - ROZKŁADY PRAWDOPODOBIEŃSTWA ###

# d* - gęstość rozkładu
# p* - dystrybuanta rozkładu
# q* - funkcja kwantylowa (funkcja odwrotna do dysyrybuanty)
# r* - funkcja generująca obserwacje z danego rozkład

# ROZKŁAD NORMALNY
dnorm(7, mean = 2, sd = 3)    # P(X = 7), X ~ N(2, 3)
pnorm(7, mean = 2, sd = 3)    # P(X < 7), X ~ N(2, 3)
qnorm(0.95, mean = 2, sd = 3) # Dla jakiego x mamy P(X < x) = 0.95?
rnorm(1000, mean = 2, sd = 3) # Wylosuj 1000 obserwacji z rozkładu N(2, 3).

# INNE ROZKŁADY
# *t - rozkład t-Studenta
# *exp - rozkład wykładniczy
# *beta - rozkład beta
# *gamma - rozkład gamma
# itd.

### OPERATORY PORÓWNANIA ###

5 < 3
5 > 3

5 <= 3
5 >= 3

5 != 3
5 == 3

0 == FALSE
5 < NA
10000000000000000000000000 < Inf

102 * (1 + 0.03)**3 -> future; future
future == 111.46
abs(future - 111.46) < 0.01

# ANALIZA ZMIENNEJ
c(32, 43, 11, 53, 11) -> points; points

points >= 40
points[points >= 40]

any(points >= 40)
all(points >= 40)

which(points >= 40)

which.min(points)
which.max(points)

which.min(points)
which(points == min(points))

### OPERATORY LOGICZNE ###

TRUE && TRUE
TRUE && FALSE
FALSE && TRUE
FALSE && FALSE

TRUE || TRUE
TRUE || FALSE
FALSE || TRUE
FALSE || FALSE

c(TRUE, FALSE) || c(FALSE, TRUE)

c(TRUE, FALSE) | c(FALSE, TRUE)
c(TRUE, FALSE) & c(FALSE, TRUE)
c(TRUE, TRUE, TRUE, TRUE) & c(FALSE, TRUE)

!TRUE
!c(TRUE, FALSE)

c(1, 2, 3, NA, 4) -> x
x[!is.na(x)] -> y; y

### PĘTLE ###

for(i in 1:10) {
    2 * i + 1 -> j
    cat(i, ' : ', j, '\n', sep = '')
}

c('Ala', 'ma', 'kota', '.') -> txt
for(e in txt) {
    cat(e, '\n', sep = '')
}

for(i in 1:100) {
    print(i)
    next
    print('next...')
}

for(i in 1:100) {
    print(i)
    break
    print('break...')
}

1 -> i
while(i <= 10) {
    print(i)
    i + 1 -> i
}

# PĘTLE A OPERACJE ZWEKTORYZOWANE
sample(16:80, size = 1e8, replace = TRUE) -> age

Sys.time() -> time
rep(NA, length(age)) -> new_age
for(i in seq_along(age)) {
    age[i] + 1 -> new_age[i]
}
Sys.time() - time

Sys.time() -> time
age + 1 -> new_age
Sys.time() - time

### MACIERZE ###

matrix(1:12, 3, 4)
matrix(1:12, 3, 4, byrow = TRUE)

matrix(1:9, 3, 3) -> A
is.matrix(A)

# INDEKSOWANIE ZGODNIE Z REGUŁAMI DLA WEKTORÓW
matrix(1:12, 3, 4) -> A

A[2, 3]
A[2:3, 3:4]
A[c(TRUE, FALSE, TRUE), c(TRUE, TRUE, FALSE, FALSE)]

A[2, ]
is.matrix(A[2, ]); is.vector(A[2, ])
A[, 3]

A[2, , drop = FALSE]
A[, 3, drop = FALSE]

# ROZSZERZANIE MACIERZY
rbind(A, c(7, 7, 7, 7))
cbind(A, c(7, 7, 7))

# WŁAŚCIWOŚCI MACIERZY
matrix(1:12, 3, 4) -> A

dim(A)
nrow(A)
ncol(A)

# c(2, 6) -> dim(A)
# A

c('engine_1', 'engine_2', 'engine_3') -> rownames(A)
c('test_1', 'test_2', 'test_3', 'test_4') -> colnames(A)
A
rownames(A)
colnames(A)

# OPERACJE MACIERZOWE
matrix(1:12, 3, 4) -> A

A
t(A)

matrix(c(2, 5, 7, 6, 3, 4, 5, -2, -3), 3, 3, byrow = TRUE) -> A
matrix(1:3, 3, 3) -> B

A
B
A * B
A %*% B

A
A**2
A %*% A

A
A**(-1)
solve(A)

1:10 -> a -> b
a %o% b

# OPERACJE NA WIERSZACH I KOLUMNACH
matrix(1:12, 3, 4) -> A

A
rowSums(A)
colSums(A)

A
rowMeans(A)
colMeans(A)

#################
### ZADANIE 4 ###
#################

### WEKTOR LOGICZNY ###

# RZUTOWANIE LOGICAL NA INNE TYPY
as.numeric(TRUE) # = 1
as.numeric(FALSE) # = 0

as.character(TRUE) # = "TRUE"
as.character(FALSE) # = "FALSE"

# TYP LOGICAL INTERPRETOWANY W KATEGORIACH LICZB 0 i 1
TRUE + TRUE # = 2
TRUE + FALSE # = 1
FALSE + FALSE # = 0

c(TRUE, TRUE, FALSE, FALSE, TRUE) -> is_employed
sum(is_employed) # = 1 + 1 + 0 + 0 + 1
mean(is_employed) # = (1 + 1 + 0 + 0 + 1) / 5

sum(is.na(c(1, 2, 3, NA, 5)))
mean(is.na(c(1, 2, 3, NA, 5)))

### INSTRUKCJA WARUNKOWA ###

# 3.5 -> number
'3.5' -> number
if(mode(number) == 'character') {
    print('konwertuję...')
    as.numeric(number) -> number
}
number

1000 -> amount
'depozyt' -> product
# 'lokata 3M' -> product
if(product == 'lokata 3M') {
    round(amount * 0.03 * 90 / 360, 2) -> interests
} else {
    0 -> interests
}
amount + interests

1000 -> amount
# 'depozyt' -> product
'lokata 1M' -> product
# 'lokata 3M' -> product
if(product == 'lokata 3M') {
    round(amount * 0.03 * 90 / 360, 2) -> interests
} else if(product == 'lokata 1M') {
    round(amount * 0.02 * 30 / 360, 2) -> interests
} else {
    0 -> interests
}
amount + interests

### OPERATOR TRÓJARGUMENTOWY ###

1000 -> amount
# 'depozyt' -> product
'lokata 3M' -> product
ifelse(
    product == 'lokata 3M',
    round(amount * 0.03 * 90 / 360, 2),
    0
) -> interests
amount + interests

c(1000, 2000, 1500, 1000) -> amount
c('depozyt', 'lokata 3M', 'depozyt', 'lokata 3M') -> product
ifelse(
    product == 'lokata 3M',
    round(amount * 0.03 * 90 / 360, 2),
    0
) -> interests
amount + interests

### TWORZENIE FUNKCJI ###

my_sum <- function(x, y) {
    x + y -> z
    
    return(z)
}
my_sum(5, 3)

# WYNIK OSTATNIEJ INSTRUCJI ZWRACANY JAKO WYNIK FUNKCJI
my_sum <- function(x, y) {
    x + y
}
my_sum(5, 3)

# FUNKCJA JEDNOLINIJKOWA BEZ NAWIASÓW KLAMROWYCH
my_sum <- function(x, y) x + y
my_sum(5, 3)

# WARTOŚCI DOMYŚLNE PARAMETRÓW
my_sum <- function(x = 0, y = 0) x + y
my_sum()
my_sum(5)
my_sum(y = 5)
my_sum(5, 3)

# NAZWY LOKALNE I GLOBALNE
print_welcome_for <- function(name) {
    name -> fname
    
    cat('Witaj ', fname, '!\n', sep = '')
}
'Paweł' -> fname
print_welcome_for('Piotr')
fname

print_welcome_for <- function(name) {
    cat('Witaj ', fname, '!\n', sep = '')
}
'Paweł' -> fname
print_welcome_for('Piotr')
fname

print_welcome_for <- function(name) {
    name ->> fname
    
    cat('Witaj ', fname, '!\n', sep = '')
}
'Paweł' -> fname
print_welcome_for('Piotr')
fname

# FUNKCJA JAKO ARGUMENT
my_operation <- function(x, y, operation) {
    operation(x, y)
}
my_operation(2, 3, sum)
my_operation(2, 3, min)
my_operation(2, 3, max)
my_operation(2, 3, paste0)

# Argument ...
show_args <- function(...) {
    list(...) -> args
    
    print(args)
}
show_args(2, 3, 1, 2)
show_args(2, a = 3, c = 1, 2)

my_paste <- function(...) {
    paste(..., sep = ' ^_^ ')
}
my_paste('Ala', 'ma', 'kota')

# Leniwa ewaluacja
get_random_number <- function() {
    cat(' >>> rzucam kostką <<< ', sep = '')
    
    4
}

my_sum <- function(x, y) {
    cat('Dodaję ', sep = '')
    cat(x, sep = '')
    cat(' i ', sep = '')
    cat(y, sep = '')
    cat('.\n', sep = '')
    
    x + y
}

my_sum(get_random_number(), get_random_number())

#################
### ZADANIE 5 ###
#################

### PRZYDATNE FUNKCJE - DYSKRETYZACJA ###

sample(1000:3000, size = 100, replace = TRUE) -> income

cut(
    income, breaks = seq(1000, 3000, 500),
    include.lowest = TRUE, dig.lab = 4
) -> income

table(income, useNA = 'ifany')

#################
### ZADANIE 6 ###
#################

### LISTA ###

list()
list(1:3, seq(1, 9), 'R Language', TRUE)

# LISTA JAKO SPOSÓB PRZECHOWYWANIA ZŁOŻONYCH WYNIKÓW ANALIZ
list(
    title = 'Jakość działania maszyn',
    description = 'Pomiar jakości działania maszyn na skali 5-cio punktowej.',
    
    test_dates = c('2017-06-01', '2017-12-15', '2018-05-30'),
    
    test_1 = c(1, 2, 4, 2, 5, 3, 2, 1, 5),
    test_2 = c(4, 2, 1, 5, 3, 4, 2, 1, 5),
    test_3 = c(5, 3, 2, 4, 1, 3, 4, 3, 2),
    
    deterioration_probability = matrix(
        c(
            1.00, 0.00, 0.00, 0.00, 0.00,
            0.01, 0.99, 0.00, 0.00, 0.00,
            0.00, 0.01, 0.99, 0.00, 0.00,
            0.00, 0.00, 0.01, 0.99, 0.00,
            0.00, 0.00, 0.00, 0.01, 0.99
        ), nrow = 5, ncol = 5, byrow = TRUE
    )
) -> machine_tests; machine_tests

is.list(machine_tests)

# STRUKTURA LISTY
str(machine_tests)

# INDEKSOWANIE PRZEZ WARTOŚĆ
machine_tests[[4]]
machine_tests[4]

# INDEKSOWANIE PRZEZ WEKTOR WARTOŚCI
# machine_tests[[4:6]]
machine_tests[4:6]

# INDEKSOWANIE PRZEZ NAZWĘ
machine_tests[['test_1']]
machine_tests$test_1
machine_tests[c('test_1', 'test_2', 'test_3')]

# INDEKSOWANIE PRZEZ WEKTOR LOGICZNY
machine_tests[c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)]

# WŁAŚCIWOŚCI LISTY
length(machine_tests)
names(machine_tests)

#################
### ZADANIE 7 ###
#################

### FAKTOR ###

sample(c(0, 1), 10000, replace = TRUE) -> sex_num; sex_num
factor(
    sex_num,
    levels = c(0, 1),
    labels = c('male', 'female')
) -> sex_ftr; sex_ftr

is.factor(sex_ftr)

# RÓŻNICA POMIĘDZY CHARACTER I FACTOR
sample(c('male', 'female'), 10000, replace = TRUE) -> sex_chr
head(sex_ftr); head(sex_chr)
object.size(sex_ftr); object.size(sex_chr)

# KONWERSJA CHARACTER NA FACTOR
as.factor(sex_chr)

# KONWERSJA FACTOR NA INNE TYPY
as.numeric(sex_ftr)
as.character(sex_ftr)

# WŁAŚCIWOŚCI
levels(sex_ftr)

### RAMKA DANYCH ###

data.frame()

data.frame(
    name = c('Bella', 'Smokey', 'Tigger', 'Lucy'),
    sex = c('f', 'm', 'm', 'f'),
    age = c(11, 2, 16, 7)
) -> cats; cats
str(cats)

data.frame(
    name = c('Bella', 'Smokey', 'Tigger', 'Lucy'),
    sex = as.factor(c('f', 'm', 'm', 'f')),
    age = c(11, 2, 16, 7),
    
    stringsAsFactors = FALSE 
) -> cats; cats
str(cats)

is.data.frame(cats)

# RAMKA DANYCH JEST LISTĄ
typeof(cats)
class(cats)

# KONWERSJA POMIĘDZY LISTĄ I RAMKĄ DANYCH
as.list(cats) -> cats_lst; cats_lst
as.data.frame(cats_lst)

# c(cats_lst$name, 'Misty') -> cats_lst$name; cats_lst
# as.data.frame(cats_lst)

# INDEKSOWANIE JAK W LIŚCIE
cats$sex
cats$age

cats[['sex']]
cats[['age']]

# INDEKSOWANIE JAK W MACIERZY
cats[2, 1]

class(cats[2, ])

class(cats[, 1])
class(cats[, 2])
class(cats[, 3])

class(cats[, 1, drop = FALSE])
class(cats[, 2, drop = FALSE])
class(cats[, 3, drop = FALSE])

# WŁAŚCIWOŚCI
nrow(cats)
ncol(cats)
dim(cats)

names(cats) # colnames(cats)
rownames(cats)

### PRZYDATNE FUNKCJE - ŁADOWANIE DANYCH ###

setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R')

# CZYTANIE PLIKU CSV
read.table(
    './data/diamonds.csv',
    header = TRUE, sep = ',', dec = '.',
    stringsAsFactors = FALSE, row.names = 1
) -> diamonds

class(diamonds)
str(diamonds)

# INNE ODMIANY FUNKCJI CZYTAJĄCYCH PLIK CSV
# read.csv read.csv2 read.delim read.delim2

# ZAPIS PLIKU CSV
write.table(
    diamonds, file = './diamonds.csv',
    quote = TRUE, sep = '!#@#!', eol = '\n',
    na = '[NA]', dec = '_', row.names = FALSE,
    col.names = TRUE, qmethod = 'escape'
)

# INNE ODMIANY FUNKCJI ZAPISUJĄCYCH PLIK CSV
# write.csv write.csv2

# ODCZYT I ZAPIS PLIKÓW RDS (BINARNY ZAPIS OBIEKTU R)
readRDS('./diamonds.rds') -> diamonds_test
saveRDS(diamonds, file = './diamonds.rds')

# ODCZYT I ZAPIS PLIKÓW Z TEKSTOWĄ REPREZENTACJĄ OBIEKTÓW R
dput(diamonds, file = './diamonds.txt')
dget('./diamonds.txt') -> diamonds_test

#################
### ZADANIE 8 ###
#################

### WEKTORY TEKSTOWE ###

# RZUTOWANIE CHARACTER NA INNE TYPY
as.logical('TRUE') # = TRUE
as.logical('FALSE') # = FALSE
as.logical('') # = NA
as.logical('kot') # = NA

as.numeric('1.94') # = 1.94
as.numeric('1,94') # = NA
as.numeric('1,234,234.43') # = NA
as.numeric('1 234 234.43') # = NA

# DŁUGOŚĆ
'Ala ma kota.' -> txt
nchar(txt)

# KONKATENACJA
paste('jeden', 'dwa', 'trzy')
paste('jeden', 'dwa', 'trzy', sep = ' -> ')

c('1', '2', '3') -> x; x
c('jeden', 'dwa', 'trzy') -> y; y
paste(x, y)
paste(x, y, collapse = ' / ')

# MODYFIKOWANIE ŁAŃCUCHA
'Ala ma kota.' -> txt
substr(txt, 5, 6)
strsplit(txt, ' ', fixed = TRUE)
gsub('kota', 'stado kotów', txt, fixed = TRUE)

### WEKTORY DAT ###

as.Date('2019-01-01') -> date

# DATA JEST ODPOWIEDNIO SFORMATOWANĄ LICZBĄ
mode(date)
class(date)

# KONWERSJA DATE NA INNE TYPY
as.numeric(date)
as.character(date)

as.numeric(as.Date('1970-01-01'))
as.numeric(as.Date('1969-01-01'))

# DATA JAKO ŁAŃCUCH TEKSTOWY CZY DATA JAKO OBIEKT
'2018-01-01' < '2018-01-10'
as.Date('2018-01-01') < as.Date('2018-01-10')

'2018-01-01' + 1
date + 1
date - 1

as.Date('2018-01-10') - as.Date('2018-01-01')
as.numeric(as.Date('2018-01-10') - as.Date('2018-01-01'))

# SEKWENCJE DAT
seq(as.Date('2018-01-01'), as.Date('2018-01-10'), 'day')
seq(as.Date('2018-01-01'), as.Date('2019-01-10'), 'month')

#################
### ZADANIE 9 ###
#################

### FUNKCJE APPLY ###

matrix(c(0.8, 0.1, 0.1, 0.1, 0.7, 0.2, 0.0, 0.1, 0.9), 3, 3, byrow = TRUE) -> M
apply(M, 1, sum)
apply(M, 2, sum)

setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R/')
read.table(
    './data/suicides.csv',
    header = TRUE, sep = ',', dec = '.', quote = '"', na.strings = '-',
    stringsAsFactors = FALSE, comment.char = ''
) -> data
'country' -> names(data)[1]
str(data)

lapply(data, class)

lapply(data[, c('suicides_no', 'population')], mean)
lapply(data[, c('suicides_no', 'population')], summary)
lapply(data[, c('suicides_no', 'population')], sd)

sapply(data[, c('suicides_no', 'population')], mean)
sapply(data[, c('suicides_no', 'population')], summary)
sapply(data[, c('suicides_no', 'population')], sd)

lapply(
    data[, c('suicides_no', 'population')],
    quantile, probs = c(0.95, 0.99, 0.999)
)

lapply(
    data[, c('suicides_no', 'population')],
    function(x) {
        data.frame(
            var = var(x), 
            sd = sd(x),
            iqr = quantile(x, probs = 0.75) - quantile(x, probs = 0.25),
            range = max(x) - min(x)
        )
    }
)

tapply(data$suicides_no, data$year, sum)
tapply(data$suicides_no, data[, c('year', 'sex')], sum)
tapply(data$suicides_no, data[, c('age', 'sex')], sum)

split(data, data$country.year) -> data_1; head(data_1)
lapply(
    data_1,
    function(df) {
        data.frame(
            country = unique(df$country),
            year = unique(df$year),
            suicides_no = sum(df$suicides_no),
            population = sum(df$population),
            suicides_per_1M = 1000000 * sum(df$suicides_no) / sum(df$population)
        )
    }
) -> data_2; head(data_2)

do.call(rbind, data_2) -> data_3; head(data_3)
