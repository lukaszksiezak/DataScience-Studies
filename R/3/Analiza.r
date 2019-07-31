# WYKRES 1: Jak wiek wplywa na zycie spoleczne 
library(dplyr)
library(data.table)
readRDS('ind2015_data.rds') -> data; 

agebreaks <- c(0,18,24,34,49,64,110)
agelabels <- c("0-18","19-24","25-34",
               "35-49","50-64","65+")
data <- data%>%filter(!is.na(age))

setDT(data)[ , agegroups := cut(age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

data %>% mutate(id = 1:n()) -> data

data$isSocial = ifelse((data$numberOfGetTogether<2 & data$numberOfDinnerMeetings<1), 'limitedSocialLife', "intenseSocialLife")

table(data$isSocial)

data %>% filter(!is.na(isSocial)) %>% group_by(agegroups) %>% count(isSocial)

counts_age_social_life <- table( data$isSocial, data$agegroups)

barplot(counts_age_social_life, main="Zycie towarzyskie vs wiek",
        xlab="Zycie towarzyskie limited/intense", col=c("darkblue","red"),
        legend = rownames(counts_age_social_life), beside=TRUE)


# WYKRES 2: Czy osoby niepelnosprawne spotykają się ze znajomymi

counts_disabled_social_life <- table(data$isSocial, data$isDisabled)

barplot(counts_disabled_social_life, main="Zycie towarzyskie porownujac osoby bez i z niepelnosprawnoscia",
        xlab="Zycie towarzyskie porownujac osoby bez i z niepelnosprawnoscia", col=c("green","gray"),
        legend = rownames(counts_disabled_social_life), beside=TRUE)

# WYKRES 3: Czy stopień niepełnosprawnosci wplywa na czestosc spotkan

counts_disabled_level_social_life <- table(data$isSocial, data$disabilityLevel)

barplot(counts_disabled_level_social_life[3:8], main="Zycie towarzyskie wsrod osob z niepelnosprawnosciami z podziałem na stopień",
        xlab="Zycie towarzyskie u osob niepelnosprawnych (niski, średni, wysoki poziom niepelnosprawnosci", col=c("orange","black"),
        legend = rownames(counts_disabled_level_social_life), beside=TRUE)


# WYKRES 4: Jak wyglada życie towarzyskie u osób o różnym stanie cywilnym

counts_martial_status <- table(data$isSocial, data$maritalStatus)

barplot(counts_martial_status, main="Zycie towarzyskie wsrod osob z o róznym stanie cywilnym",
        xlab="Zycie towarzyskie u osob o roznym stanie cywilnym", col=c("yellow","red"),
        legend = rownames(counts_martial_status), beside=TRUE)

# WYKRES 5: Jak duża część osób żyjących aktywnie towarzysko czuje się samotnych

active_social <- data %>% filter(isSocial=='intenseSocialLife')
length(data$isFeelingLonely[data$isFeelingLonely==TRUE]) -> lonelyPeople
perc_lonely = as.integer(lonelyPeople / count(data) * 100)
perc_not_lonely = as.integer(100 - perc_lonely)

perc = c(perc_lonely, perc_not_lonely)
pie(perc, labels = c('lonely' , 'not_lonely'), main="Jak dużo osób aktywnych towarzysko czuje sie samotnie")


# WYKRES 6: Ilosc przyjaciół a wiek
average_number_in_group <- data %>% group_by(agegroups) %>% summarize(Mean = mean(numberOfFriends + numberOfCloseFriends, na.rm=TRUE))

plot(average_number_in_group, type = 'o', main="Srednia ilosc ilosc przyjaciol i bliskich przyjaciol w grupach wiekowych")

