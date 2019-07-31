library(dplyr)

#####################
### 1. R GRAPHICS ###
#####################

##########################
### 1.1. JEDNA ZMIENNA ###
##########################

readRDS('diamonds.rds') -> data; data
as_tibble(data) -> data; data

########################
### 1.1.1. BAR CHART ###
########################

data

table(data$cut) -> tab; tab
barplot(tab)
barplot(tab, horiz = TRUE)
barplot(tab, col = 'orange')
barplot(tab, col = 'orange', border = 'blue')
barplot(tab, col = 'orange', border = 'blue', main = 'CUT')
barplot(tab, col = c('orange', 'red'), border = 'blue')

prop.table(table(data$cut)) -> tab; tab
barplot(tab)

########################
### 1.1.2. PIE CHART ###
########################

data

table(data$cut) -> tab; tab
pie(tab)
pie(tab, labels = c('F', 'G', 'I', 'P', 'VG'))
pie(tab, radius = 0.9)
pie(tab, clockwise = TRUE)
pie(tab, clockwise = TRUE, col = c('red', 'green', 'blue'))
pie(tab, clockwise = TRUE, col = 'white', border = 'blue')
pie(tab, clockwise = TRUE, col = 'white', border = 'blue', lty = 3)

########################
### 1.1.3. HISTOGRAM ###
########################

data$depth -> depth

hist(depth)
hist(depth, breaks = 100)
hist(depth, breaks = seq(min(depth), max(depth), 0.5))
hist(depth, breaks = 100, col = 'orange')
hist(depth, breaks = 100, col = 'orange', border = 'blue')
hist(depth, breaks = 100, main = 'Diamonds Depth')
hist(depth, breaks = 100, xlab = 'depth', ylab = 'freq')
hist(depth, breaks = 100, xlim = c(55, 70))
hist(depth, breaks = 100, xlim = c(55, 70), freq = FALSE)
hist(depth, breaks = 20, xlim = c(55, 70), labels = TRUE)

hist(depth, plot = FALSE)
str(hist(depth, plot = FALSE))

######################
### 1.1.4. BOXPLOT ###
######################

data$price -> price
boxplot(price)
boxplot(price, outline = FALSE)
boxplot(price, outline = FALSE, log = 'y')
boxplot(price, outline = FALSE, log = 'x', horizontal = TRUE)

boxplot(data$price ~ data$cut, outline = FALSE, log = 'y')
boxplot(data$price ~ data$cut + data$color, outline = FALSE, log = 'y', las = 2)

#########################
### 1.2. DWIE ZMIENNE ###
#########################

###################
### 1.2.1. PLOT ###
###################

readRDS('./suicides.rds') -> data; data
as_tibble(data) -> data; data

data %>%
    filter(country == 'Poland') %>%
    group_by(year) %>%
    summarise(suicides = 1000000 * sum(suicides_no) / sum(population)) %>%
    arrange(year) -> tmp; tmp
plot(tmp$suicides)
plot(tmp$suicides, type = 'l')
plot(tmp$suicides, type = 'b')
plot(tmp$year, tmp$suicides, type = 'b')
plot(tmp$year, tmp$suicides, type = 'b', pch = 16)
plot(tmp$year, tmp$suicides, type = 'b', pch = 16, lwd = 2)
plot(tmp$year, tmp$suicides, type = 'b', pch = 16, lwd = 2, lty = 3)
plot(tmp$year, tmp$suicides, type = 'b', pch = 16, lwd = 2, lty = 3, cex = 2)

plot(suicides ~ year, data = tmp, type = 'b', pch = 16)

data %>%
    filter(year == 2014) %>%
    group_by(country) %>%
    summarise(
        suicides = 1000000 * sum(suicides_no) / sum(population),
        gdp = unique(gdp_per_capita)
    ) -> tmp; tmp
plot(tmp$gdp, tmp$suicides, xlab = 'gdp', ylab = 'suicides', pch = 16)
plot(tmp$gdp, tmp$suicides, log = 'x', pch = 16)

plot(tmp$gdp, tmp$suicides, log = 'x', pch = 16, xaxt = 'n')
axis(
    1, at = c(1e3, 2e3, 5e3, 1e4, 2e4, 5e4, 1e5),
    labels = paste0(c(1, 2, 5, 10, 20, 50, 100), ' tys')
)

##########################
### 1.2.2. MOSAIC PLOT ###
##########################

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data

mosaicplot(table(data$clarity))
mosaicplot(table(data$clarity, data$color))
mosaicplot(table(data$clarity, data$color), col = 'orange')

###############################
### 1.3. FUNKCJE POMOCNICZE ###
###############################

###############################
### 1.3.1. ELEMENTY WYKRESU ###
###############################

### POINTS ###

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data

plot(data$carat, data$price, pch = 3, col = 'lightgray')

data %>% filter(color == 'D') -> tmp; tmp
points(tmp$carat, tmp$price, pch = 3, col = 'darkgreen')

data %>% filter(color == 'J') -> tmp; tmp
points(tmp$carat, tmp$price, pch = 3, col = 'red')

### LINES ###

readRDS('./suicides.rds') -> data; data
as_tibble(data) -> data; data

data %>%
    filter(country %in% c('Poland', 'Germany', 'United States')) %>%
    group_by(country, year) %>%
    summarise(suicides = 1000000 * sum(suicides_no) / sum(population)) %>%
    ungroup() -> tmp; tmp

range(tmp$year) -> x_lim
range(tmp$suicides) -> y_lim

tmp %>% filter(country == 'Poland') -> tmp1
plot(
    tmp1$year, tmp1$suicides,
    type = 'b', col = 'red', pch = 16,
    xlim = x_lim, ylim = y_lim
)

tmp %>% filter(country == 'Germany') -> tmp1
lines(tmp1$year, tmp1$suicides, type = 'b', col = 'darkgreen', pch = 16)

tmp %>% filter(country == 'United States') -> tmp1
lines(tmp1$year, tmp1$suicides, type = 'b', col = 'blue', pch = 16)

legend(
    'bottomleft', col = c('red', 'darkgreen', 'blue'), lwd = 2,
    legend = c('Poland', 'Germany', 'United States')
)

### ABLINE ###

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data

plot(data$x, data$z, ylim = c(0, 10), xlab = 'x', ylab = 'z', col = 'darkgrey')
abline(h = mean(data$z), lwd = 2, col = 'darkgreen')
abline(v = mean(data$x), lwd = 2, col = 'darkgreen')
abline(lm(z ~ x, data = data), col = 'red', lwd = 2)

#####################
### 1.3.2. KOLORY ###
#####################

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data

rainbow(7) -> rainbow_col
boxplot(data$price ~ data$color, outline = FALSE)
boxplot(data$price ~ data$color, outline = FALSE, col = rainbow_col)
# heat.colors, colors, palette, ...

boxplot(data$price ~ data$color, outline = FALSE, col = '#FF00FF')
boxplot(data$price ~ data$color, outline = FALSE, col = rgb(0.5, 0.9, 0.5))

################################
### 1.3.3. MACIERZE WYKRESÓW ###
################################

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data
data %>% split(data$clarity) -> tmp; tmp

range(data$carat) -> x_lim
range(data$price) -> y_lim
par(mfrow = c(2, 4)) -> settings
for(nme in names(tmp)) {
    plot(
        tmp[[nme]]$carat, tmp[[nme]]$price,
        xlim = x_lim, ylim = y_lim,
        main = nme, xlab = 'carat', ylab = 'price'
    )
}
par(settings)

############################
### 1.3.4. ZAPIS WYKRESU ###
############################

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data

png('./plot1.png', 1024, 768)
rainbow(7) -> col
boxplot(data$price ~ data$color, outline = FALSE, col = col)
dev.off()

pdf('./plot1.pdf')
rainbow(7) -> col
for(color in unique(data$color)) {
    boxplot(
        data$price[data$color == color],
        outline = FALSE, col = 'orange', main = color
    )
}
dev.off()

##################
### 2. GGPLOT2 ###
##################

library(ggplot2)

readRDS('./diamonds.rds') -> data; data
as_tibble(data) -> data; data

set.seed(123456)
data %>% sample_n(1000) -> data

######################
### 2.1. FILOZOFIA ###
######################

### WARSTWY ###

# DATA LAYER -> JAKIE DANE BĘDĄ WYKORZYSTANE DO NARYSOWANIA WYKRESU?
ggplot(data)

# AESTHETIC LAYER -> JAK DANE BĘDĄ MAPOWANE NA ELEMENTY GRAFICZNE?
ggplot(data, aes(x = carat, y = price, col = clarity))

# GEOMETRIC LAYER -> W JAKIEJ FORMIE GRAFICZNEJ ZAPREZENTOWAĆ DANE?
ggplot(data, aes(x = carat, y = price, col = clarity)) + geom_point()

# col -> ELEMENT WARSTWY AESTHETIC.
# alpha, size, shape -> PARAMETR GRAFICZNY.
# col, alpha, size, shape -> MOGĄ BYĆ PARAMETRAMI LUB ELEMENTAMI AESTHETIC.
ggplot(data, aes(x = carat, y = price, col = clarity)) +
    geom_point(alpha = 0.5, size = 2, shape = 16)

# scale_... -> MODYFIKWANIE AESTHETIC (aesthetic -> scale -> geometry).
ggplot(data, aes(x = carat, y = price, col = clarity)) +
    geom_point(alpha = 0.5, size = 2, shape = 16) + 
    scale_x_log10() + scale_y_log10()

#######################
### 2.2. DATA LAYER ###
#######################

ggplot(data, aes(x = carat, y = price, col = clarity)) -> g

# DATA + AESTHETIC TAKIE SAME, INNE GEOMETRIC.
g + geom_point()
g + geom_boxplot()

############################
### 2.3. AESTHETIC LAYER ###
############################

# PARAMETR GRAFICZNY VS ELEMENT WARSTWY AESTHETIC
ggplot(data, aes(x = carat, y = price)) + geom_point(color = 'darkgreen')
ggplot(data, aes(x = carat, y = price, color = clarity)) + geom_point()

# ELEMENTY AESTHETIC: x, y, color, fill, shape, size, alpha, label, ...
ggplot(data, aes(x = carat, y = price, color = clarity, fill = cut)) +
    geom_point(shape = 21, size = 3, alpha = 0.3)
ggplot(data, aes(x = carat, y = price, label = clarity)) + geom_text()

# PRZYGOTOWANIE DANYCH NA POTRZEBY WYKRESU
data %>%
    group_by(carat = floor(carat), cut) %>%
    summarise(price = median(price)) %>%
    ungroup() -> tmp
ggplot(tmp, aes(x = carat, y = price, linetype = cut)) + geom_line()

# AESTHETIC -> (SCALE) -> GEOMETRIC
ggplot(data, aes(x = carat, y = price, color = clarity)) +
    geom_point(size = 3, alpha = 0.3) -> g
g + scale_color_brewer(palette = 'Blues')
g + scale_x_log10() + scale_y_log10()
g + labs(x = 'CARAT', y = 'PRICE', col = 'CLARITY')

###########################
### 2.4. GEOMETIC LAYER ###
###########################

#####################
### 2.4.1. POINTS ###
#####################

ggplot(data, aes(x = carat, y = price)) + geom_point()

# GEOMETRIC -> MOŻLIWOŚĆ MODYFIKOWANIA DATA LAYER ORAZ AESTHETIC LAYER.
data %>%
    filter(clarity %in% c('IF', 'I1')) %>%
    group_by(clarity) %>%
    summarise(carat = mean(carat), price = mean(price)) -> tmp; tmp
ggplot(data, aes(x = carat, y = price, color = clarity)) +
    geom_point() +
    geom_point(data = tmp, aes(color = clarity), alpha = 0.6, size = 5) +
    geom_vline(data = tmp, aes(xintercept = carat, col = clarity)) +
    geom_hline(data = tmp, aes(yintercept = price, col = clarity))

########################
### 2.4.2. HISTOGRAM ###
########################

# LICZNOŚĆ
ggplot(data, aes(x = depth)) + geom_histogram()
ggplot(data, aes(x = depth)) + geom_histogram(binwidth = 0.5)

# GĘSTOŚĆ
ggplot(data, aes(x = depth)) + geom_histogram(aes(y = ..density..))

# PODZIAŁ NA GRUPY
ggplot(data, aes(x = depth, fill = cut)) +
    geom_histogram(binwidth = 2)
ggplot(data, aes(x = depth, fill = cut)) +
    geom_histogram(binwidth = 2, position = 'dodge')
ggplot(data, aes(x = depth, fill = cut)) +
    geom_histogram(binwidth = 2, position = 'fill')

######################
### 2.4.3. BARPLOT ###
######################

# DOMYŚLNIE ZLICZANIE OBSERWACJI W KATEGORII
ggplot(data, aes(x = cut)) + geom_bar()

# ZAMIANA ZLICZANIA OBSERWACJI PRZEZ INNĄ INFORMACJĘ
data %>% group_by(cut) %>% summarise(price = mean(price)) -> tmp; tmp
ggplot(data, aes(x = cut, y = price)) + geom_bar(stat = 'identity')

####################
### 2.4.4. LINES ###
####################

readRDS('./suicides.rds') -> data; data
as_tibble(data) -> data; data

data %>%
    filter(country == 'Poland') %>%
    group_by(year) %>%
    summarise(suicides = 1000000 * sum(suicides_no) / sum(population)) %>%
    arrange(year) -> tmp; tmp
ggplot(tmp, aes(x = year, y = suicides)) + geom_line()

# RÓŻNY SPOSÓB PRZEDSTAWIENIA INFORMACJI
data %>%
    filter(country == 'Poland') %>%
    group_by(year, age) %>%
    summarise(suicides = 1000000 * sum(suicides_no) / sum(population)) %>%
    arrange(year) -> tmp; tmp
ggplot(tmp, aes(x = year, y = suicides, color = age)) + geom_line(size = 1.5)
ggplot(tmp, aes(x = year, y = suicides, fill = age)) + geom_area()
ggplot(tmp, aes(x = year, y = suicides, fill = age)) +
    geom_area(position = 'fill')
ggplot(tmp, aes(x = year, y = suicides, fill = age)) +
    geom_ribbon(alpha = 0.3, aes(ymin = 0, ymax = suicides))

# ZAZNACZENIE OKRESU CZASU
data %>%
    filter(country == 'Poland') %>%
    group_by(year) %>%
    summarise(suicides = 1000000 * sum(suicides_no) / sum(population)) %>%
    arrange(year) -> tmp; tmp
tibble(begin = c(1991, 2000, 2008), end = c(1994, 2006, 2013)) -> dates
ggplot(tmp, aes(x = year, y = suicides)) +
    geom_rect(
        data = dates,
        aes(xmin = begin, xmax = end, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE, fill = 'red', alpha = 0.5
    ) +
    geom_line(size = 1.5)
