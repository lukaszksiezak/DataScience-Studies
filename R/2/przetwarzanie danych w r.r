#################
### ZADANIE 1 ###
#################

### PAKIETY W R ###

# INSTALOWANIE
# install.packages('dplyr')

# ŁADOWANIE
library(dplyr)

# ZARZĄDZANIE
search()
# detach('package:dplyr')

### DPLYR - TIBBLE ###

setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R')

readRDS('./diamonds.rds') -> data

data
str(data)

as_tibble(data) -> data

class(data)

data
glimpse(data)

c('x1', 'x2', 'x3') -> names(data)[8:10]; data
c('x', 'y', 'z') -> colnames(data)[8:10]; data

paste0('diament_', 1:nrow(data)) -> rownames(data)
data
rownames(data)
data['diament_2', ]

### DPLYR - PIPELINE ###

# WIELOKROTNIE ZAGNIEŻDŻONA FUNKCJA
round(
    prop.table(
        table(
            cut(
                data$carat,
                breaks = seq(min(data$carat), max(data$carat), length.out = 10)
            ),
            useNA = 'ifany'
        )
    ),
    3
)

# PRZECHOWYWANIE WYNIKÓW POŚREDNICH
data$carat ->  res
cut(res, breaks = seq(min(data$carat), max(data$carat), length.out = 10)) -> res
table(res, useNA = 'ifany') -> res
prop.table(res) -> res
round(res, 5) -> res
res

# PIPELINE
data$carat %>%
    cut(breaks = seq(min(data$carat), max(data$carat), length.out = 10)) %>%
    table(useNA = 'ifany') %>%
    prop.table() %>%
    round(5)

### DPLYR - FILTROWANIE ###

# FILTROWANIE Z WYKORZYSTANIEM WARUNKÓW LOGICZNYCH
data %>% filter(cut == 'Ideal')

data %>% filter(cut == 'Ideal', clarity == 'IF')
# data %>% filter(cut == 'Ideal' & clarity == 'IF')

data %>% filter(cut == 'Ideal' | price > quantile(price, probs = 0.9))

# FILTROWANIE Z WYKORZYSTANIEM NUMERÓW WIERSZY
data %>% slice(10:15)

data %>% slice(10)
data %>% slice(10) %>% class()

data %>% slice(n())

data %>% slice(-(4:n())) # data %>% slice(1:3)

### DPLYR - SELEKCJA ###

data %>% select(cut, carat, price)

data %>% select(-x, -y, -z)

# WYBIERANIE ZMIENNYCH W OPARCIU O REGUŁĘ
data %>% select(starts_with('c'))

# WYBIERANIE ZMIENNYCH W OPARCIU O ZAKRES
data %>% select(cut:clarity)

# ZWRACANIE TIBBLE I ZWRACANIE WEKTORA
data %>% select(price)
data %>% pull(price)

### DPLYR - MUTACJA ###

# MODYFIKOWANIE ZMIENNEJ
data %>% mutate(price = price + 0.2 * price)

# TWORZENIE ZMIENNEJ
data %>% mutate(ppc = price / carat)
data %>% mutate(ppc = price / carat, size = x * y * z)
data %>% mutate(ppc = price / carat, size = x * y * z, pps = price / size)

# TWORZENIE NOWYCH ZMIENNYCH Z JEDNOCZESNYM USUNIĘCIEM STARYCH ZMIENNYCH
data %>% transmute(size = x * y * z)
data %>% transmute(x, y, z, size = x * y * z)

### DPLYR - SORTOWANIE ###

data %>% select(cut, color, clarity) %>% distinct() -> tmp; tmp

# SORTOWANIE W PORZĄDKU ROSNĄCYM
tmp %>% arrange(cut)
tmp %>% arrange(cut, color, clarity)

# SORTOWANIE W PORZĄDKU MALEJĄCYM
tmp %>% arrange(desc(cut), desc(color), desc(clarity))

# WYKORZYSTANIE JEDNOCZEŚNIE PORZĄDKU ROSNĄCEGO I MALEJĄCEGO
tmp %>% arrange(desc(cut), color, clarity)

#####################
### ZADANIE 2 i 3 ###
#####################

setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R')
readRDS('./diamonds.rds') %>% as_tibble() -> data; data

### DPLYR - SUMMARISE ###

data %>% summarise(mean(carat))
data %>% summarise(carat_avg = mean(carat))

# WIELE INFORMACJI O JEDNEJ ZMIENNEJ
data %>%
    summarise(
        carat_min = min(carat),
        carat_avg = mean(carat),
        carat_max = max(carat)
    )

# JEDNA INFORMACJA O WIELU ZMIENNYCH
data %>%
    summarise(
        carat_avg = mean(carat),
        depth_avg = mean(depth),
        table_avg = mean(table),
        price_avg = mean(price)
    )

# PODSUMOWANIE W GRUPACH OBSERWACJI
data %>%
    group_by(clarity) %>%
    summarise(price = mean(price))

# JEDNO SUMMARISE ZDEJMUJE JEDEN WYMIAR GRUPOWANIA
data %>%
    group_by(clarity, color) %>%
    summarise(price = mean(price)) %>%
    ungroup()

# ZLICZANIE OBSERWACJI W GRUPACH
data %>% group_by(clarity) %>% summarise(n = n())
data %>% count(clarity)

#################
### ZADANIE 4 ###
#################

setwd('C:/Users/pawel/Dysk Google/sages/studia/Data Science/Programowanie w R')
readRDS('./diamonds.rds') %>% as_tibble() -> data; data

### DPLYR - SCALANIE TABEL ###

# SCALANIE WIERSZY
data %>% slice(1:5) -> data_part1; data_part1
data %>% slice(6:10) -> data_part2; data_part2

bind_rows(data_part1, data_part2)

# KOLUMNY WYSTĘPUJĄCE W TYLKO JEDNEJ Z TABEL SĄ UWZGLĘDNIANE
data %>% slice(1:5) %>% select(-table) -> data_part1; data_part1
data %>% slice(6:10) %>% select(-x, -y, -z) -> data_part2; data_part2

bind_rows(data_part1, data_part2)

# SCALANIE PO NAZWACH, NIE PO KOLEJNOŚCI
data %>% slice(1:5) %>% select(carat, cut, color) -> data_part1; data_part1
data %>% slice(6:10) %>% select(cut, color, carat) -> data_part2; data_part2

bind_rows(data_part1, data_part2)

# SCALANIE KOLUMN
data %>% slice(1:10) %>% select(carat:color) -> data_part1; data_part1
data %>% slice(1:10) %>% select(clarity:price) -> data_part2; data_part2

bind_cols(data_part1, data_part2)

# POWTARZAJĄCE SIĘ KOLUMNY Z SUFIKSEM
data %>% slice(1:10) %>% select(carat:depth) -> data_part1; data_part1
data %>% slice(1:10) %>% select(clarity:price) -> data_part2; data_part2

bind_cols(data_part1, data_part2)

# OPERACJE TEORIOMNOGOŚCIOWE NA TABELACH
data %>% slice(1:6) -> data_part1; data_part1
data %>% slice(5:10) -> data_part2; data_part2

bind_rows(data_part1, data_part2)

union(data_part1, data_part2)
intersect(data_part1, data_part2)
setdiff(data_part1, data_part2)
setdiff(data_part2, data_part1)

#################
### ZADANIE 5 ###
#################

### DPLR - JOIN ###

# GENEROWANIE DANYCH
data %>% mutate(id = 1:n()) %>% slice(1:10) -> tmp
tmp %>% select(id, carat:table) %>% slice(1:4, 6:9) -> diam_descr; diam_descr
tmp %>% select(id, price) %>% slice(2:6, 8:10) -> diam_price; diam_price

# ŹLE: ZGADYWANIE KOLUMN KLUCZY PRZEZ DPLYR
diam_descr %>% inner_join(diam_price)

# DOBRZE: JAWNA DEKLARACJA KOLUMN KLUCZY
diam_descr %>% inner_join(diam_price, by = 'id')

# TYLKO REKORDY WYSTĘPUJĄCE W OBU TABELACH
diam_descr %>% inner_join(diam_price, by = 'id')

# TYLKO REKORDY WYSTĘPUJĄCE W LEWEJ TABELI
diam_descr %>% left_join(diam_price, by = 'id')

# TYLKO REKORDY WYSTĘPUJĄCE W PRAWEJ TABELI
diam_descr %>% right_join(diam_price, by = 'id')

# WSZYSTKIE REKORDY Z OBU TABEL
diam_descr %>% full_join(diam_price, by = 'id')

# GENEROWANIE DANYCH
tmp %>% select(ident = id, carat, price) %>% slice(2:6, 8:10) -> diam_price
diam_price

# ŹLE ODGADNIĘTY IDENTYFIKATOR
diam_descr %>% inner_join(diam_price)

# DEKLARACJA IDENTYFIKATORÓW W SYTUACJI RÓŻNIC W NAZEWNICTWIE
diam_descr %>% inner_join(diam_price, by = c('id' = 'ident'))

# DEKLARACJA SUFIKSÓW DLA POWTARZAJĄCYCH SIĘ KOLUMN
diam_descr %>%
    inner_join(diam_price, by = c('id' = 'ident'), suffix = c('', '_copy'))
