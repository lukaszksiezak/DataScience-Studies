# �ukasz Ksi�ak DS, R, gr3, lab2

library(dplyr);

#######
# Zad1#
#######

# 1. Wczytaj dane 

read.csv('crypto.csv') -> crypto_data;

as_tibble(crypto_data) -> crypto_data; crypto_data

# 2. Filtrowanie: dane tylko o Bitcoinie
unique(crypto_data$Currency)
crypto_data %>% filter(Currency == 'bitcoin') -> bitcoin_data; bitcoin_data

#3. Pozostaw tylko kolumny Date i Close
bitcoin_data %>% select(Date, Close) -> bitcoin_data

#4. Zmien typ kolumny Date na Date

# w wypadku srodowiska z jezykiem innym niz angielski:
Sys.setlocale("LC_TIME", "C")
bitcoin_data %>% mutate(Date = as.Date(Date, format = "%b %d, %Y")) -> bitcoin_data

#5. Dodaj kolumn� rate (metoda z liczeniem rate z adps (zmiana procentowa))
bitcoin_data %>% mutate(Rate = c(NA, (bitcoin_data$Close[2:length(bitcoin_data$Close)]-bitcoin_data$Close[length(bitcoin_data$Close)-1])/bitcoin_data$Close[2:length(bitcoin_data$Close)-1])) -> bitcoin_data
# Rownoznaczne z:
bitcoin_data %>% mutate(Rate = (Close-lag(Close))/lag(Close))

#6. Posortuj wg Rate malejaco
bitcoin_data %>% arrange(desc(bitcoin_data$Rate))


######
#Zad2#
######

read.csv('albums.csv') -> albums_data

# Gatunki muzyczne w zbiorze
albums_data %>% distinct(genre)

# Z ktorego gatunku muzycznego, dla zadanego zbioru, jest najwiecej albumow
albums_data %>% group_by(genre) %>% count() %>% arrange(desc(n)) %>% head(1)

# Jak wyglada zestawienie gatunkow muzycznych wg najwiekszej sprzedawalnosci plyt
albums_data %>% group_by(genre) %>% summarise(sum(as.numeric(num_of_sales))) %>% arrange(desc(`sum(as.numeric(num_of_sales))`))

# Ktore gatunki muzyczne sa oceniane najczesciej na 4.5 i wyzej przez The Rolling Stone
albums_data %>% filter(rolling_stone_critic >=4.5) %>% group_by(genre) %>% count() %>% arrange(desc(n))

# Jak wiele razy roznica miedzy ocena music maniac a the roling stone byla wieksza niz 4 
albums_data %>% 
  mutate(difference_maniac_stone = music_maniac_critic - rolling_stone_critic) %>%
     filter(difference_maniac_stone>4) %>%
      count()

# Ile razy (kazdego roku w danym zbiorze) mtv dalo ocene 5
albums_data %>% filter(mtv_critic == 5) %>% group_by(year_of_pub) %>% count()

# Jaka jest srednia ocene z 3 danych recenzji 
albums_data %>% mutate(avg_note = (mtv_critic + rolling_stone_critic + music_maniac_critic)/3) -> albums_data

#Ile albumow dostalo note srednia wieksza niz 4 od wszystkich recenzentow
albums_data %>% filter(avg_note > 4) %>% count


#######
# Zad3#
#######
#Plik suicides.rds zawiera informacje na temat liczby pope�nionych samob�jstw w 101 krajach �wiata na przestrzeni 
#lat 1985 � 2016 z uwzgl�dnieniem podzia�u na p�e� oraz grup� wiekow�.

readRDS('suicides.rds') -> suicides_data;

##Wska� pi�� kraj�w w kt�rych na przestrzeni lat 1985 � 2016 pope�niono najwi�cej / najmniej 
##samob�jstw na 100k mieszka�c�w.
suicides_data %>% group_by(country) %>% summarise(sum(suicides.100k.pop)) -> suicides_per_country
c('Country', 'Sum_per_100k_ppl') -> colnames(suicides_per_country)

# Top 5
suicides_per_country %>% 
  arrange(desc(Sum_per_100k_ppl)) %>% 
    slice(1:5)

# Bottom 5
suicides_per_country %>% 
  arrange(Sum_per_100k_ppl) %>% 
    slice(1:5) 

##Dla ka�dego roku badania wyznacz ��czn� liczb� samob�jstw pope�nionych na 100k mieszka�c�w na �wiecie.
suicides_data %>% 
  group_by(year) %>% 
    summarise(
      sum(suicides_no),
      sum(population)) -> suicides_by_year
c('Year', 'suicides_no', 'world_population') -> colnames(suicides_by_year)

suicides_by_year %>% 
  mutate(suicides_by_year_per_100k_ppl = (suicides_no/world_population)*100000) %>% 
    arrange(desc(suicides_by_year_per_100k_ppl))


##Ustal ��czn� liczb� samob�jstw pope�nionych na 100k mieszka�c�w na przestrzeni ca�ej pr�by w 
##podziale na p�e� oraz wiek.

suicides_data %>% 
  group_by(sex, age) %>% 
    summarise(sum(suicides.100k.pop)) -> suicides_by_sex_age
c('sex', 'age', 's_no_100_k') -> colnames(suicides_by_sex_age)


##Dla ka�dego roku badania wska� trzy kraje, w kt�rych odnotowano najwi�ksz� liczb� samob�jstw.
suicides_data %>%  
  group_by(year, country) %>% 
    summarise(sum(suicides_no)) -> suicides_each_year_in_countries


suicides_each_year_in_countries %>% 
  group_by(year) %>% 
    arrange(year, desc(`sum(suicides_no)`)) %>% 
      group_by(year) %>% 
        slice(1:3) -> three_worst_countries_yearly

#tabelka z krajami dla ktorych odnotowano najwieksza ilosc samobojstw
three_worst_countries_yearly %>% arrange(desc(year))


##Znajd� kraj w kt�rym nast�pi�a najwi�ksza / najmniejsza zmiana pomi�dzy liczb� samob�jstw na 100k mieszka�c�w
##w najgorszym roku (najwi�cej samob�jstw) i najlepszym roku (najmniej samob�jstw).

#Koncept:
#1.Groupby country i year a potem suma samobojstw na 100k 
#2. Dla kazdego kraju wybrac najgorszy i najlepszy rok 
#3. Dla kazdego kraju policz roznice w ilosci samobojcow na 100k wg formuly najgorszy_rok-najlepszy_rok
#4. Posotruj 3 desc.
#5. Wybierz pierwszy i ostatni wiersz - najwieksza zmiana/najmniejsza zmiana

suicides_data %>% 
  group_by(year, country) %>%
    summarise(sum(suicides.100k.pop)) %>%
      arrange(country) %>% group_by(country) %>% summarise(max(`sum(suicides.100k.pop)`), min(`sum(suicides.100k.pop)`)) -> worst_and_best_for_exach_country

c("Country", "WorstYear", "BestYear") -> colnames(worst_and_best_for_exach_country)

worst_and_best_for_exach_country %>% mutate(difference = WorstYear - BestYear) -> worst_and_best_for_exach_country
worst_and_best_for_exach_country %>% arrange(desc(difference)) -> worst_and_best_for_exach_country

#Kraj z najwieksza roznica:
head(worst_and_best_for_exach_country, 1)

#Kraj z najmniejsza roznica
tail(worst_and_best_for_exach_country, 1)
