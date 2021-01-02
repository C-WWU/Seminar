#load and install packages
library(plyr)
library(dplyr)

######Personality scale TIPI

#prep, evtl löschen oder davor schon
data$`Extrovertiert/enthusiastisch`[is.na(data$`Extrovertiert/enthusiastisch`)] <- 0
data$`Kritisch/konfliktfreudig`[is.na(data$`Kritisch/konfliktfreudig`)] <- 0
data$`Zuverlaessig/selbstdiszipliniert`[is.na(data$`Zuverlaessig/selbstdiszipliniert`)] <- 0
data$`Aengstlich/leicht reizbar`[is.na(data$`Aengstlich/leicht reizbar`)] <- 0
data$`Offen fuer neue Erfahrungen/vielseitig`[is.na(data$`Offen fuer neue Erfahrungen/vielseitig`)] <- 0
data$`Kontrollfrage Persoenlichkeit`[is.na(data$`Kontrollfrage Persoenlichkeit`)] <- 0
data$`Zurueckhaltend/ruhig`[is.na(data$`Zurueckhaltend/ruhig`)] <- 0
data$`Sympathisch/warmherzig`[is.na(data$`Sympathisch/warmherzig`)] <- 0
data$`Unorganisiert/nachlaessig`[is.na(data$`Unorganisiert/nachlaessig`)] <- 0
data$`Ruhig/emotional stabil`[is.na(data$`Ruhig/emotional stabil`)] <- 0
data$`Konventionell/unkreativ`[is.na(data$`Konventionell/unkreativ`)] <- 0

#transform character into numeric values

cols_personality <- c("Extrovertiert/enthusiastisch", "Kritisch/konfliktfreudig", "Zuverlaessig/selbstdiszipliniert", "Aengstlich/leicht reizbar", "Offen fuer neue Erfahrungen/vielseitig", "Zurueckhaltend/ruhig", "Sympathisch/warmherzig", "Unorganisiert/nachlaessig", "Ruhig/emotional stabil", "Konventionell/unkreativ") #exclude control question

data[cols_personality] <- sapply(data[cols_personality], as.numeric)
sapply(data[cols_personality], class)


#transform reserve-coded items: Q8_2, Q8_4, Q8_7, Q8_9, Q8_11

'Kritisch/konfliktfreudig_nichtR' <- transmute(data, `Kritisch/konfliktfreudig` = 8 - (`Kritisch/konfliktfreudig`))
'Aengstlich/leicht reizbar_nichtR' <- transmute(data, `Aengstlich/leicht reizbar` = 8 - (`Aengstlich/leicht reizbar`))
'Zurueckhaltend/ruhig_nichtR' <- transmute(data, `Zurueckhaltend/ruhig` = 8 - (`Zurueckhaltend/ruhig`))
'Unorganisiert/nachlaessig_nichtR' <- transmute(data, `Unorganisiert/nachlaessig` = 8 - (`Unorganisiert/nachlaessig`))
'Konventionell/unkreativ_nichtR' <- transmute(data, `Konventionell/unkreativ` = 8 - (`Konventionell/unkreativ`))

nichtR <- bind_cols(`Kritisch/konfliktfreudig_nichtR`, `Aengstlich/leicht reizbar_nichtR`, `Zurueckhaltend/ruhig_nichtR`, `Unorganisiert/nachlaessig_nichtR`, `Konventionell/unkreativ_nichtR`)
colnames(nichtR) <- c("Kritisch/konfliktfreudig_nichtR", "Aengstlich/leicht reizbar_nichtR", "Zurueckhaltend/ruhig_nichtR", "Unorganisiert/nachlaessig_nichtR", "Konventionell/unkreativ_nichtR")
data <- bind_cols(data, nichtR)

#extraversion: Q8_1 and Q8_7

data$`Extrovertiert/enthusiastisch`[is.na(data$`Extrovertiert/enthusiastisch`)] <- 0
data$`Zurueckhaltend/ruhig_nichtR`[is.na(data$`Zurueckhaltend/ruhig_nichtR`)] <- 0

data <- data %>%
  rowwise() %>%
  mutate(Extraversion = mean(c(`Extrovertiert/enthusiastisch`, `Zurueckhaltend/ruhig_nichtR`)))


#Agreeableness: 2R + 8

data$`Kritisch/konfliktfreudig_nichtR`[is.na(data$`Kritisch/konfliktfreudig_nichtR`)] <- 0
data$`Sympathisch/warmherzig`[is.na(data$`Sympathisch/warmherzig`)] <- 0

data <- data %>%
  rowwise() %>%
  mutate(Agreeableness = mean(c(`Kritisch/konfliktfreudig_nichtR`, `Sympathisch/warmherzig`)))


#Conscientiousness: 3 + 9R

data$`Zuverlaessig/selbstdiszipliniert`[is.na(data$`Zuverlaessig/selbstdiszipliniert`)] <- 0
data$`Unorganisiert/nachlaessig_nichtR`[is.na(data$`Unorganisiert/nachlaessig_nichtR`)] <- 0

data <- data %>%
  rowwise() %>%
  mutate(Conscientiousness = mean(c(`Zuverlaessig/selbstdiszipliniert`, `Unorganisiert/nachlaessig_nichtR`)))


#Emotional stability: 4R + 10

data$`Aengstlich/leicht reizbar_nichtR`[is.na(data$`Aengstlich/leicht reizbar_nichtR`)] <- 0
data$`Ruhig/emotional stabil`[is.na(data$`Ruhig/emotional stabil`)] <- 0

data <- data %>%
  rowwise() %>%
  mutate(Emotional_stablity = mean(c(`Aengstlich/leicht reizbar_nichtR`, `Ruhig/emotional stabil`)))


#Openness to Experiences: 5R + 11

data$`Offen fuer neue Erfahrungen/vielseitig`[is.na(data$`Offen fuer neue Erfahrungen/vielseitig`)] <- 0
data$`Konventionell/unkreativ_nichtR`[is.na(data$`Konventionell/unkreativ_nichtR`)] <- 0

data <- data %>%
  rowwise() %>%
  mutate(Openness_to_Experiences = mean(c(`Offen fuer neue Erfahrungen/vielseitig`, `Konventionell/unkreativ_nichtR`)))



#####GREEN scale (Q16)

#replace NAs with 0 in all Q16 items
#später unnötig? sonst nochmal schönere Lösung finden

    
data$Q16_1[is.na(data$Q16_1)] <- 0
data$Q16_2[is.na(data$Q16_2)] <- 0
data$Q16_3[is.na(data$Q16_3)] <- 0
data$Q16_4[is.na(data$Q16_4)] <- 0
data$Q16_5[is.na(data$Q16_5)] <- 0
data$Q16_6[is.na(data$Q16_6)] <- 0
data$Q16_7[is.na(data$Q16_7)] <- 0

#transform character into numeric values

cols_green <- c("Verwendete Produkte Umwelt nicht belasten", "Auswirkungen meiner Handlungen auf Umwelt", "Kaufgewohnheiten, Sorge um Umwelt", "Verschwendung Ressourcen", "Umweltverantwortlich", "Unannehmlichkeiten fuer Umwelt") #exclude Q16_5 as it is the control question

data[cols_green] <- sapply(data[cols_green], as.numeric)
sapply(data[cols_green], class)

#create green_values variable which summarizes green consumer values
data <- data %>%
  rowwise() %>%
  mutate(green_values = mean(c(`Verwendete Produkte Umwelt nicht belasten`, `Auswirkungen meiner Handlungen auf Umwelt`, `Kaufgewohnheiten, Sorge um Umwelt`, `Verschwendung Ressourcen`, Umweltverantwortlich, `Unannehmlichkeiten fuer Umwelt`)))
#muss man runden?
