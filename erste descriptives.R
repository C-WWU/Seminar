#Descriptives of the Respondents

#Instagram usage: how often and how many accounts
  #Frequency
table(data$`Instagram Nutzungshaeufigkeit`) #exclude 14 people with "Once Per Week" usage?
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
round(table(data$Geschlecht)/sum(table(data$Geschlecht)),2) #relative spread: 61% female, 39% male


#age

summary(data$Alter)
ggplot(data, aes(x = Alter))+
  geom_density()
ggplot(data, aes(x = Alter))+
  geom_histogram()

#Age range wie anpassen?

table(data$Age_Range)
Ordered_ranges <- c('unter20', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60plus')

ggplot(data, aes(x = factor(Age_Range, levels = Ordered_ranges)))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "Age Ranges", y = "Count", title = "Abs. Count per Age Range")+
  ylim(0,500)
  
  


#PLZ





#Beziehungsstatus
table(data$Beziehungsstatus)



#sexuelle Orientierung
table(data$`Sexuelle Orientierung`)


#Children
table(data$`Anzahl Kinder`) #most people without children
###necessary to clean 3 respondents with >10 children?

#Education
table(data$Bildungsabschluss)

  #summarized into: low, medium, high education





#Beschäftigung
table(data$Beschaeftigung)


#Migrationshintergrund
table(data$Migrationshintergrund)
round(table(data$Migrationshintergrund)/sum(table(data$Migrationshintergrund)), 2) #only 17% with migration background

#woher Migration
table(data$`Woher Vorfahren`) #most migration background from Asia and Europe


#Religion
table(data$Religion)

#also pay attention to open entries:
table(data$`Religion Sonstiges`) #how to deal with those?

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

##we can observe a good spread of personality types,  though majority scores > 4 for all personality attributes


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
#no controversial goals, usually majority >5

##necessary to test whether people answered in patterns or always in same number?




#Parteien
round(table(data$`Wahl Partei`)/sum(table(data$`Wahl Partei`)), 2) #relative shares of voters in our dataset
#differs a bit from actual German voting data/ forecasts; mainly more Grüne and less CDU and SPD

Partei <- as.data.frame(table(data$`Wahl Partei`)/sum(table(data$`Wahl Partei`)))
names(Partei)[names(Partei) == 'Ich würde nicht wählen gehen'] <- "Nichtwähler"
Partei_ohne_kA <- Partei[-7,]

Partei_Order <- c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "AfD", "Die Linke", "FDP", "Sonstige:", "Nichtwähler")
ggplot(Partei_ohne_kA, aes(factor(Var1, levels = Partei_Order), Freq))+
  geom_col()+
  geom_text(aes(label = percent(Freq)), vjust = -1)+
  labs(x = "Parties", y = "", title = "Voters per Party")+
  ylim(0,0.3)


#Parteien Sonstige - was tun?
table(data$`Wahl Partei Sonstiges`)



#Corona






#Alkohol, Zigaretten, Drogen
table(data$`Alkohol Konsum`)
table(data$`Zigaretten Konsum`)
table(data$`Drogen Konsum`)
