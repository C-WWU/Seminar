#Descriptives of the Respondents

#Instagram usage: how often and how many accounts
  #Frequency
table(data$`Instagram Nutzungshaeufigkeit`) #exclude 9 people with "Once Per Week" usage?
usage <- as.data.frame(table(data$`Instagram Nutzungshaeufigkeit`))
usage_order <- c("Täglich", "4- bis 6-mal pro Woche", "2- bis 3-mal pro Woche", "Einmal pro Woche")

ggplot(usage, aes(factor(Var1, levels = usage_order), Freq))+
  geom_col()+
  geom_text(aes(label = Freq), vjust = -1)+
  labs(x = "Usage", y = "", title = "Count")+
  ylim(0,1700)

  #how many accounts?
table(data$Accounts_followed) #exclude people with too many or too little accounts followed?
ggplot(data, aes(Accounts_followed))+
  geom_histogram(binwidth = 1)

  #Accounts per Frequency --> is there a connection?
Nutzungshaeufigkeit <- data$`Instagram Nutzungshaeufigkeit`
ggplot(data, aes(x = Accounts_followed, y = factor(Nutzungshaeufigkeit, levels = usage_order)))+
  geom_boxplot()

#is there a significant difference between usage groups and how many accounts are followed?
anova_usage <- aov(Accounts_followed ~ `Instagram Nutzungshaeufigkeit`, data = data)
summary(anova_usage)
  #yes, there seems to be!


#gender #1=female, 2=male, 3=diverse

table(data$Geschlecht) 
round(table(data$Geschlecht)/sum(table(data$Geschlecht)),2) #relative spread: 61% female, 39% male, diverse almost 0


#age

summary(data$Alter)
ggplot(data, aes(x = Alter))+
  geom_density()
ggplot(data, aes(x = Alter))+
  geom_histogram()

#Age range

table(data$Age_Range)
Ordered_ranges <- c('niedriges Alter', 'mittleres Alter', 'hohes Alter')

ggplot(data, aes(x = factor(Age_Range, levels = Ordered_ranges)))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "Age Ranges", y = "Count", title = "Abs. Count per Age Range")+
  ylim(0,1200)
  
  


#PLZ --> Münster Region dominiert
table(data$PLZ)
ggplot(data, aes(x = PLZ))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "PLZ", y = "Count", title = "Abs. Count per PLZ")+
  ylim(0,200)

#PLZ zusammengefasst in Ost/West
table(data$Ost_West) #passt zum Deutschlandweiten Verhältnis von ca. 1:5

#Beziehungsstatus
table(data$Beziehungsstatus)
#zusammengefasst:
table(data$Allein_vs_Beziehung)


#sexuelle Orientierung
table(data$`Sexuelle Orientierung`)
#sonstiges ist nicht weiter relevant für uns (nur n = 6)

#Children
table(data$`Anzahl Kinder`) #most people without children
#wir wissen, dass einer unserer Privatkontakte sich verklickt hat und statt 20 Kindern 0 eingeben wollte --> Korrektur hierfür, um Datensatz nicht aussortieren zu müssen:
data$`Anzahl Kinder`[data$`Anzahl Kinder` == 20] <- 0
###necessary to clean 3 respondents with >10 children?
many_children <- data %>% subset(data$`Anzahl Kinder` > 10) #prüfen ob sonstige Antworten Sinn ergeben --> keine Auffälligkeiten, daher kein Ausschluss notwendig

table(data$`Anzahl Kinder`) #weiter zusammenfassen für Analyse notwendig, z.B. mehr als 3 Kinder als eine Gruppe?


#Education
table(data$Bildungsabschluss)
#zusammengefasst:
table(data$Bildungsgruppe)



#Beschäftigung
table(data$Beschaeftigung)


#Migrationshintergrund
table(data$Migrationshintergrund)
round(table(data$Migrationshintergrund)/sum(table(data$Migrationshintergrund)), 2) #17% mit Migrationshintergrund

#woher Migration
table(data$`Woher Vorfahren`) #Hintergrund vorwiegend aus Europa und Asien --> vermutlich viele Europäer und Türken


#Religion
table(data$Religion)

#auch offene Nennungen beachten?
table(data$`Religion Sonstiges`) #keine Religion oft genug erwähnt, um sie nachträglich mit aufzunehmen (max. 3x)

#Personality
  #Extraversion
summary(data$Extraversion)

ggplot(data, aes(Extraversion))+
  geom_density()
ggplot(data, aes(Extraversion))+
  geom_histogram(binwidth = 0.5)

  #Agreeableness
summary(data$Agreeableness)

ggplot(data, aes(Agreeableness))+
  geom_density()
ggplot(data, aes(Agreeableness))+
  geom_histogram(binwidth = 0.5)

  #Conscientiousness
summary(data$Conscientiousness)

ggplot(data, aes(Conscientiousness))+
  geom_density()
ggplot(data, aes(Conscientiousness))+
  geom_histogram(binwidth = 0.5)

  #Emotional stability
summary(data$Emotional_stablity)

ggplot(data, aes(Emotional_stablity))+
  geom_density()
ggplot(data, aes(Emotional_stablity))+
  geom_histogram(binwidth = 0.5)

  #Openness to Experiences
summary(data$Openness_to_Experiences)

ggplot(data, aes(Openness_to_Experiences))+
  geom_density()
ggplot(data, aes(Openness_to_Experiences))+
  geom_histogram(binwidth = 0.5)

##gute Verteilung, auch wenn jeweils öfter eher hohe Werte (>4) angegeben sind
#weiter zusammenfassen notwendig; nur Extremwerte beachten?


#Green Values
table(data$Green_Values)
summary(data$Green_Values)

ggplot(data, aes(Green_Values))+
  geom_density()
ggplot(data, aes(Green_Values))+
  geom_histogram(binwidth = 0.33)

##most respondents do care about the environment (rating between 5 and 6), but also some people below and 51 people with highest score of 7
#necessary to set thresholds and make pro and con green value division?

#General Goals in Life
table(data$`Gefuehl der Zugehoerigkeit`)
table(data$Spannung)
table(data$`Herzliche Beziehung zu anderen Menschen`)
table(data$Selbstverwirklichung)
table(data$`Respekt vor Anderen`)
table(data$`Spass und Freude am Leben`)
table(data$Sicherheit)
table(data$Selbstachtung)
table(data$`Gefuehl von Erfolg`)
#important for most people: Spaß, Herzliche Beziehungen
#no controverse goals, usually majority >5


#Parteien
round(table(data$`Wahl Partei`)/sum(table(data$`Wahl Partei`)), 2) #relative shares of voters in our dataset
#differs a bit from actual German voting data/ forecasts; mainly more Grüne and less CDU and SPD

#Parteien Sonstige - was tun?
table(data$`Wahl Partei Sonstiges`)
#Offene Nennungen Parteien: Aufnahme von "Die Partei"
data$`Wahl Partei Sonstiges` <- tolower(data$`Wahl Partei Sonstiges`)
data$`Wahl Partei` <- ifelse(data$`Wahl Partei Sonstiges` %in% "die partei", "Die Partei", data$`Wahl Partei`)

Partei <- as.data.frame(table(data$`Wahl Partei`)/sum(table(data$`Wahl Partei`)))

Partei_Order <- c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "AfD", "Die Linke", "FDP", "Die Partei", "Sonstige:", "Ich würde nicht wählen gehen")
ggplot(Partei, aes(factor(Var1, levels = Partei_Order), Freq))+
  geom_col()+
  geom_text(aes(label = percent(Freq)), vjust = -1)+
  labs(x = "Parties", y = "", title = "Voters per Party")+
  ylim(0,0.3)



#Corona: 4 Gruppen eingeteilt: Hardliner, Softliner, Skeptiker, Leugner
table(data$Corona_Hardliner) #582 Hardliner: Wollen härtere Maßnahmen
table(data$Corona_Softliner) #246 Softliner: Wollen softere Maßnahmen
table(data$Corona_Skeptiker) #279 Skeptiker: Bezweifeln Gefährlichkeit des Virus
table(data$Corona_Leugner) #118 Leugner: Glauben nicht an Virus



#Alkohol, Zigaretten, Drogen
table(data$`Alkohol Konsum`)
table(data$`Zigaretten Konsum`)
table(data$`Drogen Konsum`)
#zusammengefasst:
table(data$Alkoholgruppe)
table(data$Zigarettengruppe) #1250 Nichtraucher
table(data$Drogengruppe) #"nur" 64 mit hohem Konsum



#prüfen: gibt es Zusammenhänge zwischen den variablen?
#Alter <-> Accounts_followed


#Green Values zu Partei
ggplot(data, aes(Green_Values, fill = `Wahl Partei`))+
  geom_dotplot(binwidth = 0.1)

ggplot(data, aes(Green_Values, y = factor(data$'Wahl Partei')))+
  geom_boxplot()


#Korrelationen zwischen Accounts?
cor_accounts <- as.data.frame(cor(Accounts))
ggplot(cor_accounts_df, aes(Tagesschau))+
  geom_density()
