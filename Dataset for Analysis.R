#Socially (Ir)Responsible Algorithms

#####
#load and install packages
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)

#####
#preparations
#load datasets and delete unnecessary first rows using readr
  #USE data <- data[-1, ]

#gapfish data: load and then remove last column (id) and first four rows (test ids)
gapfish_text <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Gapfish Text.csv")
gapfish_text <- gapfish_text[-(1:4), -303]
gapfish_num <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Gapfish Numeric.csv")
gapfish_num <- gapfish_num[-(1:4), -303]

#private contacts data: load and then remove first two rows
private_text <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Privat Text.csv")
private_text <- private_text[-(1:2), ]
private_num <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Privat Numeric.csv")
private_num<- private_num[-(1:2), ]

#surveycircle data
surveycircle_text <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Surveycircle Text.csv")
surveycircle_text <- surveycircle_text[-(1:2), ]
surveycircle_num <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Surveycircle Numeric.csv")
surveycircle_num <- surveycircle_num[-(1:2), ]

  
#create two datasets: text full and num full

text_full <- rbind(gapfish_text, private_text, surveycircle_text)
num_full <- rbind(gapfish_num, private_num, surveycircle_num)
  
######
#name columns
#for text_full
data <- text_full
names(data)[names(data) == 'StartDate'] <- 'Startdatum'
names(data)[names(data) == 'Q29'] <- 'DSGVO'
names(data)[names(data) == 'Q3'] <- 'Alter'
names(data)[names(data) == 'Q4'] <- 'Geschlecht'
names(data)[names(data) == 'Q5'] <- 'PLZ'
names(data)[names(data) == 'Q6'] <- 'Instagram Nutzer'
names(data)[names(data) == 'Q7'] <- 'Instagram Nutzungshaeufigkeit'
names(data)[names(data) == 'Q30_1'] <- 'Alman Memes'
names(data)[names(data) == 'Q30_2'] <- 'Barbara Schoeneberger'
names(data)[names(data) == 'Q30_3'] <- 'Berlin Tag und Nacht'
names(data)[names(data) == 'Q30_4'] <- 'Brigitte Magazin'
names(data)[names(data) == 'Q30_5'] <- 'Michael Bully Herbig'
names(data)[names(data) == 'Q30_6'] <- 'Dein Beichtstuhl'
names(data)[names(data) == 'Q30_7'] <- 'Dieter Nuhr'
names(data)[names(data) == 'Q30_8'] <- 'Disney Deutschland'
names(data)[names(data) == 'Q30_9'] <- 'Elyas M Barek'
names(data)[names(data) == 'Q30_10'] <- 'Faktastisch'
names(data)[names(data) == 'Q30_11'] <- 'Felix Lobrecht'
names(data)[names(data) == 'Q30_12'] <- 'Germanys next Topmodel'
names(data)[names(data) == 'Q30_13'] <- 'heute show'
names(data)[names(data) == 'Q30_14'] <- 'Jan Josef Liefers'
names(data)[names(data) == 'Q30_15'] <- 'Julien Bam'
names(data)[names(data) == 'Q30_16'] <- 'Jens Knossalla'
names(data)[names(data) == 'Q41_1'] <- 'Laser Luca'
names(data)[names(data) == 'Q41_2'] <- 'Love Island'
names(data)[names(data) == 'Q41_3'] <- 'Mario Barth'
names(data)[names(data) == 'Q41_4'] <- 'Made My Day'
names(data)[names(data) == 'Q41_5'] <- 'Netflix DE'
names(data)[names(data) == 'Q41_6'] <- 'Nicholas Puschmann'
names(data)[names(data) == 'Q41_7'] <- 'Joko Winterscheidt'
names(data)[names(data) == 'Q41_8'] <- 'Oliver Pocher'
names(data)[names(data) == 'Q41_9'] <- 'Palina Rojinski'
names(data)[names(data) == 'Q41_10'] <- 'Playboy Germany'
names(data)[names(data) == 'Q41_11'] <- 'Promiflash'
names(data)[names(data) == 'Q41_12'] <- 'Sebastian Fitzek'
names(data)[names(data) == 'Q41_13'] <- 'Sophia Thomalla'
names(data)[names(data) == 'Q41_14'] <- 'The Voice of Germany'
names(data)[names(data) == 'Q41_15'] <- 'Wer weiss denn sowas'
names(data)[names(data) == 'Q31_1'] <- 'Food Stories'
names(data)[names(data) == 'Q31_2'] <- 'Aldi Nord'
names(data)[names(data) == 'Q31_3'] <- 'Aldi Sued'
names(data)[names(data) == 'Q31_4'] <- 'Astra'
names(data)[names(data) == 'Q31_5'] <- 'Backen.de'
names(data)[names(data) == 'Q31_6'] <- 'BakeClub'
names(data)[names(data) == 'Q31_7'] <- 'Chefkoch'
names(data)[names(data) == 'Q31_8'] <- 'Edeka'
names(data)[names(data) == 'Q31_9'] <- 'Einfach Tasty'
names(data)[names(data) == 'Q31_10'] <- 'Etepetete'
names(data)[names(data) == 'Q31_11'] <- 'Foodist'
names(data)[names(data) == 'Q31_12'] <- 'Fritz Kola'
names(data)[names(data) == 'Q31_13'] <- 'Fruehlingszwiebel'
names(data)[names(data) == 'Q31_14'] <- 'Haribo'
names(data)[names(data) == 'Q31_15'] <- 'Hello Fresh'
names(data)[names(data) == 'Q31_16'] <- 'Junk Food Guru'
names(data)[names(data) == 'Q42_1'] <- 'Just Spices'
names(data)[names(data) == 'Q42_2'] <- 'Kaufland'
names(data)[names(data) == 'Q42_3'] <- 'Leckerschmecker'
names(data)[names(data) == 'Q42_4'] <- 'McDonalds Deutschland'
names(data)[names(data) == 'Q42_5'] <- 'Pam Goes Nuts'
names(data)[names(data) == 'Q42_6'] <- 'Pflanzlich stark'
names(data)[names(data) == 'Q42_7'] <- 'Plantbased Food and Travel'
names(data)[names(data) == 'Q42_8'] <- 'Redbull Germany'
names(data)[names(data) == 'Q42_9'] <- 'Sallys Welt'
names(data)[names(data) == 'Q42_10'] <- 'SimplyV'
names(data)[names(data) == 'Q42_11'] <- 'Starbucks Deutschland'
names(data)[names(data) == 'Q42_12'] <- 'Steffen Henssler'
names(data)[names(data) == 'Q42_13'] <- 'Tim Maelzer'
names(data)[names(data) == 'Q42_14'] <- 'True fruits'
names(data)[names(data) == 'Q42_15'] <- 'Vapiano'
names(data)[names(data) == 'Q42_16'] <- 'Weber Grill'
names(data)[names(data) == 'Q32_1'] <- 'Animal Crossing'
names(data)[names(data) == 'Q32_2'] <- 'Call of Duty'
names(data)[names(data) == 'Q32_3'] <- 'EA Sports FIFA'
names(data)[names(data) == 'Q32_4'] <- 'Felix von der Laden'
names(data)[names(data) == 'Q32_5'] <- 'Fortnite'
names(data)[names(data) == 'Q32_6'] <- 'Gamingzelle'
names(data)[names(data) == 'Q32_7'] <- 'Go Pro Deutschland'
names(data)[names(data) == 'Q32_8'] <- 'Huawei Deutschland'
names(data)[names(data) == 'Q32_9'] <- 'Microsoft Deutschland'
names(data)[names(data) == 'Q32_10'] <- 'Mohammed Harkous'
names(data)[names(data) == 'Q32_11'] <- 'Montana Black'
names(data)[names(data) == 'Q32_12'] <- 'Nintendo'
names(data)[names(data) == 'Q32_13'] <- 'PlayStation DACH'
names(data)[names(data) == 'Q32_14'] <- 'Rewinside'
names(data)[names(data) == 'Q32_15'] <- 'Reyst'
names(data)[names(data) == 'Q32_16'] <- 'Rezo'
names(data)[names(data) == 'Q32_17'] <- 'Ungespielt/Simon Unge'
names(data)[names(data) == 'Q32_18'] <- 'Xbox DACH'
names(data)[names(data) == 'Q33_1'] <- 'Apotheken Umschau'
names(data)[names(data) == 'Q33_2'] <- 'ARTE'
names(data)[names(data) == 'Q33_3'] <- 'BILD Zeitung'
names(data)[names(data) == 'Q33_4'] <- 'Frankfurter Allgemeine Zeitung'
names(data)[names(data) == 'Q33_5'] <- 'GEO Magazin'
names(data)[names(data) == 'Q33_6'] <- 'Handelsblatt'
names(data)[names(data) == 'Q33_7'] <- 'Quarks & Co.'
names(data)[names(data) == 'Q33_8'] <- 'RTL Aktuell'
names(data)[names(data) == 'Q33_9'] <- 'Der Spiegel'
names(data)[names(data) == 'Q33_10'] <- 'Tagesschau'
names(data)[names(data) == 'Q33_11'] <- 'taz'
names(data)[names(data) == 'Q33_12'] <- 'ZEIT'
names(data)[names(data) == 'Q34_1'] <- 'Andre Schiebler'
names(data)[names(data) == 'Q34_2'] <- 'Anna Maria Damm'
names(data)[names(data) == 'Q34_3'] <- 'bebe'
names(data)[names(data) == 'Q34_4'] <- 'Bibis Beauty Palace'
names(data)[names(data) == 'Q34_5'] <- 'Bonnie Strange'
names(data)[names(data) == 'Q34_6'] <- 'Carmen Kroll'
names(data)[names(data) == 'Q34_7'] <- 'Caro Daur'
names(data)[names(data) == 'Q34_8'] <- 'DagiBee'
names(data)[names(data) == 'Q34_9'] <- 'Daniele Katzenberger'
names(data)[names(data) == 'Q34_10'] <- 'Dilara'
names(data)[names(data) == 'Q34_11'] <- 'dm'
names(data)[names(data) == 'Q34_12'] <- 'Melike'
names(data)[names(data) == 'Q34_13'] <- 'Die groesste Community fuer Muetter'
names(data)[names(data) == 'Q34_14'] <- 'Guido Maria Kretschmer'
names(data)[names(data) == 'Q34_15'] <- 'Ischtar Isik'
names(data)[names(data) == 'Q34_16'] <- 'Julia Beautx'
names(data)[names(data) == 'Q34_17'] <- 'Julien Co'
names(data)[names(data) == 'Q34_18'] <- 'Kelly Misses Vlog'
names(data)[names(data) == 'Q34_19'] <- 'Lena Gercke'
names(data)[names(data) == 'Q34_21'] <- 'Leon Content'
names(data)[names(data) == 'Q34_20'] <- 'Leonie Hanne'
names(data)[names(data) == 'Q43_1'] <- 'Lillydoo'
names(data)[names(data) == 'Q43_2'] <- 'Lisa Marie Schiffner'
names(data)[names(data) == 'Q43_3'] <- 'Alex Koch'
names(data)[names(data) == 'Q43_4'] <- 'MAC Cosmetics'
names(data)[names(data) == 'Q43_5'] <- 'Melina Sophie'
names(data)[names(data) == 'Q43_6'] <- 'Balea'
names(data)[names(data) == 'Q43_7'] <- 'Naturkosmetik Muenchen'
names(data)[names(data) == 'Q43_8'] <- 'NYX Professional Makeup'
names(data)[names(data) == 'Q43_9'] <- 'Paola Maria'
names(data)[names(data) == 'Q43_10'] <- 'Riccardo Simonetti'
names(data)[names(data) == 'Q43_11'] <- 'Roman Lochmann'
names(data)[names(data) == 'Q43_12'] <- 'Sarah Harrison'
names(data)[names(data) == 'Q43_13'] <- 'Simon Desue'
names(data)[names(data) == 'Q43_14'] <- 'Takko Fashion'
names(data)[names(data) == 'Q43_15'] <- 'Team Harrison'
names(data)[names(data) == 'Q43_16'] <- 'Vogue Germany'
names(data)[names(data) == 'Q43_17'] <- 'Alverde'
names(data)[names(data) == 'Q43_18'] <- 'Leon Skincare'
names(data)[names(data) == 'Q43_19'] <- 'Westwing'
names(data)[names(data) == 'Q43_20'] <- 'IKEA'
names(data)[names(data) == 'Q35_1'] <- 'Andrea Berg'
names(data)[names(data) == 'Q35_2'] <- 'Annenmaykantereit'
names(data)[names(data) == 'Q35_3'] <- 'Berliner Philharmoniker'
names(data)[names(data) == 'Q35_4'] <- 'Boehse Onkelz'
names(data)[names(data) == 'Q35_5'] <- 'Bushido'
names(data)[names(data) == 'Q35_6'] <- 'Capital Bra'
names(data)[names(data) == 'Q35_7'] <- 'Die Toten Hosen'
names(data)[names(data) == 'Q35_8'] <- 'Eurovision Song Contest'
names(data)[names(data) == 'Q35_9'] <- 'Helene Fischer'
names(data)[names(data) == 'Q35_10'] <- 'Lena Meyer-Landrut'
names(data)[names(data) == 'Q35_11'] <- 'LionTTV'
names(data)[names(data) == 'Q35_12'] <- 'Mero'
names(data)[names(data) == 'Q35_13'] <- 'Parookaville'
names(data)[names(data) == 'Q35_14'] <- 'Pietro Lombardi'
names(data)[names(data) == 'Q35_15'] <- 'Shirin David'
names(data)[names(data) == 'Q35_16'] <- 'Silbermond'
names(data)[names(data) == 'Q35_17'] <- 'The BossHoss'
names(data)[names(data) == 'Q35_18'] <- 'Wacken Open Air'
names(data)[names(data) == 'Q35_19'] <- 'Michael Wendler'
names(data)[names(data) == 'Q36_1'] <- 'AfD'
names(data)[names(data) == 'Q36_2'] <- 'Alice Weidel'
names(data)[names(data) == 'Q36_3'] <- 'Bundesgesundheitsministerium'
names(data)[names(data) == 'Q36_4'] <- 'Angela Merkel'
names(data)[names(data) == 'Q36_5'] <- 'Bundeswehr'
names(data)[names(data) == 'Q36_6'] <- 'CDU'
names(data)[names(data) == 'Q36_7'] <- 'Christian Lindner'
names(data)[names(data) == 'Q36_8'] <- 'Buendnis 90/Die Gruenen'
names(data)[names(data) == 'Q36_9'] <- 'Die Linke'
names(data)[names(data) == 'Q36_10'] <- 'Die Partei'
names(data)[names(data) == 'Q36_11'] <- 'Evangelisch.de'
names(data)[names(data) == 'Q36_12'] <- 'FDP'
names(data)[names(data) == 'Q36_13'] <- 'Fridays for Future'
names(data)[names(data) == 'Q44_1'] <- 'Islamfakten'
names(data)[names(data) == 'Q44_2'] <- 'Jens Spahn'
names(data)[names(data) == 'Q44_3'] <- 'katholisch.de'
names(data)[names(data) == 'Q44_5'] <- 'Louisa Dellert'
names(data)[names(data) == 'Q44_6'] <- 'Luisa Neubauer'
names(data)[names(data) == 'Q44_7'] <- 'Heiko Maas'
names(data)[names(data) == 'Q44_8'] <- 'PETA Deutschland'
names(data)[names(data) == 'Q44_9'] <- 'Querdenken711'
names(data)[names(data) == 'Q44_10'] <- 'Robert Habeck'
names(data)[names(data) == 'Q44_11'] <- 'Sahra Wagenknecht'
names(data)[names(data) == 'Q44_12'] <- 'SPD'
names(data)[names(data) == 'Q37_1'] <- 'adidas Deutschland'
names(data)[names(data) == 'Q37_2'] <- 'Alica Schmidt'
names(data)[names(data) == 'Q37_3'] <- 'Borussia Dortmund'
names(data)[names(data) == 'Q37_4'] <- 'DFB'
names(data)[names(data) == 'Q37_5'] <- 'RB Leipzig'
names(data)[names(data) == 'Q37_6'] <- 'FC Bayern Muenchen'
names(data)[names(data) == 'Q37_7'] <- 'Felix Neureuther'
names(data)[names(data) == 'Q37_8'] <- 'Felix Sturm'
names(data)[names(data) == 'Q37_9'] <- 'Gina Lueckenkemper'
names(data)[names(data) == 'Q37_10'] <- 'Christoph Icke Dommisch'
names(data)[names(data) == 'Q37_11'] <- 'Inscope21'
names(data)[names(data) == 'Q37_12'] <- 'kicker'
names(data)[names(data) == 'Q37_13'] <- 'Lisa Mueller'
names(data)[names(data) == 'Q45_1'] <- 'Mady Morrison'
names(data)[names(data) == 'Q45_2'] <- 'Manuel Neuer'
names(data)[names(data) == 'Q45_3'] <- 'Marco Reus'
names(data)[names(data) == 'Q45_4'] <- 'McFit'
names(data)[names(data) == 'Q45_5'] <- 'Oceans Apart'
names(data)[names(data) == 'Q45_6'] <- 'Pamela Reif'
names(data)[names(data) == 'Q45_7'] <- 'Philipp Lahm'
names(data)[names(data) == 'Q45_8'] <- 'Sophia Thiel'
names(data)[names(data) == 'Q45_9'] <- 'FC Schalke 04'
names(data)[names(data) == 'Q45_10'] <- 'Sky Sport'
names(data)[names(data) == 'Q45_11'] <- 'Sport1'
names(data)[names(data) == 'Q45_12'] <- 'Uwe Gensheimer'
names(data)[names(data) == 'Q38_1'] <- 'Canon Deutschland'
names(data)[names(data) == 'Q38_2'] <- 'Create! By Obi'
names(data)[names(data) == 'Q38_3'] <- 'Deutsche Bahn'
names(data)[names(data) == 'Q38_4'] <- 'Easy Alex'
names(data)[names(data) == 'Q38_5'] <- 'Flixbus'
names(data)[names(data) == 'Q38_6'] <- 'Ford Deutschland'
names(data)[names(data) == 'Q38_7'] <- 'Germanroamers'
names(data)[names(data) == 'Q38_8'] <- 'Hannes Becker'
names(data)[names(data) == 'Q38_9'] <- 'Linda DIY'
names(data)[names(data) == 'Q38_10'] <- 'Martin Ruetter'
names(data)[names(data) == 'Q38_11'] <- 'Mercedes-Benz Deutschland'
names(data)[names(data) == 'Q38_12'] <- 'Tiere suchen ein Zuhause'
names(data)[names(data) == 'Q38_13'] <- 'Urlaubsguru'
names(data)[names(data) == 'Q38_14'] <- 'Urlaubspiraten'
names(data)[names(data) == 'Q38_15'] <- 'Xlaeta'
names(data)[names(data) == 'Q38_16'] <- 'Yamaha Motor Deutschland'
names(data)[names(data) == 'Q38_17'] <- 'Yvonne Pfeffer'
names(data)[names(data) == 'Q47_1'] <- 'Ariana Grande'
names(data)[names(data) == 'Q47_2'] <- 'Beyonce'
names(data)[names(data) == 'Q47_3'] <- 'Cristiano Ronaldo'
names(data)[names(data) == 'Q47_9'] <- 'Dwayne Johnson'
names(data)[names(data) == 'Q47_4'] <- 'Justin Bieber'
names(data)[names(data) == 'Q47_5'] <- 'Kim Kardashian West'
names(data)[names(data) == 'Q47_6'] <- 'Kylie Jenner'
names(data)[names(data) == 'Q47_7'] <- 'Lionel Messi'
names(data)[names(data) == 'Q47_12'] <- 'National Geographic'
names(data)[names(data) == 'Q47_8'] <- 'Selena Gomez'
names(data)[names(data) == 'Q8_1'] <- 'Extrovertiert/enthusiastisch'
names(data)[names(data) == 'Q8_2'] <- 'Kritisch/konfliktfreudig'
names(data)[names(data) == 'Q8_3'] <- 'Zuverlaessig/selbstdiszipliniert'
names(data)[names(data) == 'Q8_4'] <- 'Aengstlich/leicht reizbar'
names(data)[names(data) == 'Q8_5'] <- 'Offen fuer neue Erfahrungen/vielseitig'
names(data)[names(data) == 'Q8_6'] <- 'Kontrollfrage Persoenlichkeit'
names(data)[names(data) == 'Q8_7'] <- 'Zurueckhaltend/ruhig'
names(data)[names(data) == 'Q8_8'] <- 'Sympathisch/warmherzig'
names(data)[names(data) == 'Q8_9'] <- 'Unorganisiert/nachlaessig'
names(data)[names(data) == 'Q8_10'] <- 'Ruhig/emotional stabil'
names(data)[names(data) == 'Q8_11'] <- 'Konventionell/unkreativ'
names(data)[names(data) == 'Q9'] <- 'Alkohol Konsum'
names(data)[names(data) == 'Q10'] <- 'Zigaretten Konsum'
names(data)[names(data) == 'Q11'] <- 'Drogen Konsum'
names(data)[names(data) == 'Q12_1'] <- 'Gefuehl der Zugehoerigkeit'
names(data)[names(data) == 'Q12_2'] <- 'Spannung'
names(data)[names(data) == 'Q12_3'] <- 'Kontrollfrage Ziele im Leben'
names(data)[names(data) == 'Q12_4'] <- 'Herzliche Beziehung zu anderen Menschen'
names(data)[names(data) == 'Q12_5'] <- 'Selbstverwirklichung'
names(data)[names(data) == 'Q12_6'] <- 'Respekt vor Anderen'
names(data)[names(data) == 'Q12_7'] <- 'Spass und Freude am Leben'
names(data)[names(data) == 'Q12_8'] <- 'Sicherheit'
names(data)[names(data) == 'Q12_9'] <- 'Selbstachtung'
names(data)[names(data) == 'Q12_10'] <- 'Gefuehl von Erfolg'
names(data)[names(data) == 'Q13'] <- 'Wahl Partei'
names(data)[names(data) == 'Q13_8'] <- 'Wahl Partei Sonstiges'
names(data)[names(data) == 'Q14_1'] <- 'Corona-Massnahmen uebertrieben'
names(data)[names(data) == 'Q14_2'] <- 'Corona-Massnahmen muessten haerter sein'
names(data)[names(data) == 'Q14_3'] <- 'Corona ist harmlos, gleich Grippe'
names(data)[names(data) == 'Q14_4'] <- 'Glaube nicht an Corona'
names(data)[names(data) == 'Q15'] <- 'Nettoeinkommen'
names(data)[names(data) == 'Q16_1'] <- 'Verwendete Produkte Umwelt nicht belasten'
names(data)[names(data) == 'Q16_2'] <- 'Auswirkungen meiner Handlungen auf Umwelt'
names(data)[names(data) == 'Q16_3'] <- 'Kaufgewohnheiten, Sorge um Umwelt'
names(data)[names(data) == 'Q16_4'] <- 'Verschwendung Ressourcen'
names(data)[names(data) == 'Q16_5'] <- 'Kontrollfrage Umwelt'
names(data)[names(data) == 'Q16_6'] <- 'Umweltverantwortlich'
names(data)[names(data) == 'Q16_7'] <- 'Unannehmlichkeiten fuer Umwelt'
names(data)[names(data) == 'Q17'] <- 'Beschaeftigung'
names(data)[names(data) == 'Q18'] <- 'Bildungsabschluss'
names(data)[names(data) == 'Q19'] <- 'Religion'
names(data)[names(data) == 'Q19_5'] <- 'Religion Sonstiges'
names(data)[names(data) == 'Q20'] <- 'Migrationshintergrund'
names(data)[names(data) == 'Q21'] <- 'Woher Vorfahren'
names(data)[names(data) == 'Q22'] <- 'Sexuelle Orientierung'
names(data)[names(data) == 'Q22_4'] <- 'Sexuelle Orientierung Sonstiges'
names(data)[names(data) == 'Q23'] <- 'Beziehungsstatus'
names(data)[names(data) == 'Q24'] <- 'Kinder'
names(data)[names(data) == 'Q26'] <- 'Anzahl Kinder'
names(data)[names(data) == 'Q40'] <- 'Instagram Name'

text_full <- data

#for num_full
data <- num_full
names(data)[names(data) == 'StartDate'] <- 'Startdatum'
names(data)[names(data) == 'Q29'] <- 'DSGVO'
names(data)[names(data) == 'Q3'] <- 'Alter'
names(data)[names(data) == 'Q4'] <- 'Geschlecht'
names(data)[names(data) == 'Q5'] <- 'PLZ'
names(data)[names(data) == 'Q6'] <- 'Instagram Nutzer'
names(data)[names(data) == 'Q7'] <- 'Instagram Nutzungshaeufigkeit'
names(data)[names(data) == 'Q30_1'] <- 'Alman Memes'
names(data)[names(data) == 'Q30_2'] <- 'Barbara Schoeneberger'
names(data)[names(data) == 'Q30_3'] <- 'Berlin Tag und Nacht'
names(data)[names(data) == 'Q30_4'] <- 'Brigitte Magazin'
names(data)[names(data) == 'Q30_5'] <- 'Michael Bully Herbig'
names(data)[names(data) == 'Q30_6'] <- 'Dein Beichtstuhl'
names(data)[names(data) == 'Q30_7'] <- 'Dieter Nuhr'
names(data)[names(data) == 'Q30_8'] <- 'Disney Deutschland'
names(data)[names(data) == 'Q30_9'] <- 'Elyas M Barek'
names(data)[names(data) == 'Q30_10'] <- 'Faktastisch'
names(data)[names(data) == 'Q30_11'] <- 'Felix Lobrecht'
names(data)[names(data) == 'Q30_12'] <- 'Germanys next Topmodel'
names(data)[names(data) == 'Q30_13'] <- 'heute show'
names(data)[names(data) == 'Q30_14'] <- 'Jan Josef Liefers'
names(data)[names(data) == 'Q30_15'] <- 'Julien Bam'
names(data)[names(data) == 'Q30_16'] <- 'Jens Knossalla'
names(data)[names(data) == 'Q41_1'] <- 'Laser Luca'
names(data)[names(data) == 'Q41_2'] <- 'Love Island'
names(data)[names(data) == 'Q41_3'] <- 'Mario Barth'
names(data)[names(data) == 'Q41_4'] <- 'Made My Day'
names(data)[names(data) == 'Q41_5'] <- 'Netflix DE'
names(data)[names(data) == 'Q41_6'] <- 'Nicholas Puschmann'
names(data)[names(data) == 'Q41_7'] <- 'Joko Winterscheidt'
names(data)[names(data) == 'Q41_8'] <- 'Oliver Pocher'
names(data)[names(data) == 'Q41_9'] <- 'Palina Rojinski'
names(data)[names(data) == 'Q41_10'] <- 'Playboy Germany'
names(data)[names(data) == 'Q41_11'] <- 'Promiflash'
names(data)[names(data) == 'Q41_12'] <- 'Sebastian Fitzek'
names(data)[names(data) == 'Q41_13'] <- 'Sophia Thomalla'
names(data)[names(data) == 'Q41_14'] <- 'The Voice of Germany'
names(data)[names(data) == 'Q41_15'] <- 'Wer weiss denn sowas'
names(data)[names(data) == 'Q31_1'] <- 'Food Stories'
names(data)[names(data) == 'Q31_2'] <- 'Aldi Nord'
names(data)[names(data) == 'Q31_3'] <- 'Aldi Sued'
names(data)[names(data) == 'Q31_4'] <- 'Astra'
names(data)[names(data) == 'Q31_5'] <- 'Backen.de'
names(data)[names(data) == 'Q31_6'] <- 'BakeClub'
names(data)[names(data) == 'Q31_7'] <- 'Chefkoch'
names(data)[names(data) == 'Q31_8'] <- 'Edeka'
names(data)[names(data) == 'Q31_9'] <- 'Einfach Tasty'
names(data)[names(data) == 'Q31_10'] <- 'Etepetete'
names(data)[names(data) == 'Q31_11'] <- 'Foodist'
names(data)[names(data) == 'Q31_12'] <- 'Fritz Kola'
names(data)[names(data) == 'Q31_13'] <- 'Fruehlingszwiebel'
names(data)[names(data) == 'Q31_14'] <- 'Haribo'
names(data)[names(data) == 'Q31_15'] <- 'Hello Fresh'
names(data)[names(data) == 'Q31_16'] <- 'Junk Food Guru'
names(data)[names(data) == 'Q42_1'] <- 'Just Spices'
names(data)[names(data) == 'Q42_2'] <- 'Kaufland'
names(data)[names(data) == 'Q42_3'] <- 'Leckerschmecker'
names(data)[names(data) == 'Q42_4'] <- 'McDonalds Deutschland'
names(data)[names(data) == 'Q42_5'] <- 'Pam Goes Nuts'
names(data)[names(data) == 'Q42_6'] <- 'Pflanzlich stark'
names(data)[names(data) == 'Q42_7'] <- 'Plantbased Food and Travel'
names(data)[names(data) == 'Q42_8'] <- 'Redbull Germany'
names(data)[names(data) == 'Q42_9'] <- 'Sallys Welt'
names(data)[names(data) == 'Q42_10'] <- 'SimplyV'
names(data)[names(data) == 'Q42_11'] <- 'Starbucks Deutschland'
names(data)[names(data) == 'Q42_12'] <- 'Steffen Henssler'
names(data)[names(data) == 'Q42_13'] <- 'Tim Maelzer'
names(data)[names(data) == 'Q42_14'] <- 'True fruits'
names(data)[names(data) == 'Q42_15'] <- 'Vapiano'
names(data)[names(data) == 'Q42_16'] <- 'Weber Grill'
names(data)[names(data) == 'Q32_1'] <- 'Animal Crossing'
names(data)[names(data) == 'Q32_2'] <- 'Call of Duty'
names(data)[names(data) == 'Q32_3'] <- 'EA Sports FIFA'
names(data)[names(data) == 'Q32_4'] <- 'Felix von der Laden'
names(data)[names(data) == 'Q32_5'] <- 'Fortnite'
names(data)[names(data) == 'Q32_6'] <- 'Gamingzelle'
names(data)[names(data) == 'Q32_7'] <- 'Go Pro Deutschland'
names(data)[names(data) == 'Q32_8'] <- 'Huawei Deutschland'
names(data)[names(data) == 'Q32_9'] <- 'Microsoft Deutschland'
names(data)[names(data) == 'Q32_10'] <- 'Mohammed Harkous'
names(data)[names(data) == 'Q32_11'] <- 'Montana Black'
names(data)[names(data) == 'Q32_12'] <- 'Nintendo'
names(data)[names(data) == 'Q32_13'] <- 'PlayStation DACH'
names(data)[names(data) == 'Q32_14'] <- 'Rewinside'
names(data)[names(data) == 'Q32_15'] <- 'Reyst'
names(data)[names(data) == 'Q32_16'] <- 'Rezo'
names(data)[names(data) == 'Q32_17'] <- 'Ungespielt/Simon Unge'
names(data)[names(data) == 'Q32_18'] <- 'Xbox DACH'
names(data)[names(data) == 'Q33_1'] <- 'Apotheken Umschau'
names(data)[names(data) == 'Q33_2'] <- 'ARTE'
names(data)[names(data) == 'Q33_3'] <- 'BILD Zeitung'
names(data)[names(data) == 'Q33_4'] <- 'Frankfurter Allgemeine Zeitung'
names(data)[names(data) == 'Q33_5'] <- 'GEO Magazin'
names(data)[names(data) == 'Q33_6'] <- 'Handelsblatt'
names(data)[names(data) == 'Q33_7'] <- 'Quarks & Co.'
names(data)[names(data) == 'Q33_8'] <- 'RTL Aktuell'
names(data)[names(data) == 'Q33_9'] <- 'Der Spiegel'
names(data)[names(data) == 'Q33_10'] <- 'Tagesschau'
names(data)[names(data) == 'Q33_11'] <- 'taz'
names(data)[names(data) == 'Q33_12'] <- 'ZEIT'
names(data)[names(data) == 'Q34_1'] <- 'Andre Schiebler'
names(data)[names(data) == 'Q34_2'] <- 'Anna Maria Damm'
names(data)[names(data) == 'Q34_3'] <- 'bebe'
names(data)[names(data) == 'Q34_4'] <- 'Bibis Beauty Palace'
names(data)[names(data) == 'Q34_5'] <- 'Bonnie Strange'
names(data)[names(data) == 'Q34_6'] <- 'Carmen Kroll'
names(data)[names(data) == 'Q34_7'] <- 'Caro Daur'
names(data)[names(data) == 'Q34_8'] <- 'DagiBee'
names(data)[names(data) == 'Q34_9'] <- 'Daniele Katzenberger'
names(data)[names(data) == 'Q34_10'] <- 'Dilara'
names(data)[names(data) == 'Q34_11'] <- 'dm'
names(data)[names(data) == 'Q34_12'] <- 'Melike'
names(data)[names(data) == 'Q34_13'] <- 'Die groesste Community fuer Muetter'
names(data)[names(data) == 'Q34_14'] <- 'Guido Maria Kretschmer'
names(data)[names(data) == 'Q34_15'] <- 'Ischtar Isik'
names(data)[names(data) == 'Q34_16'] <- 'Julia Beautx'
names(data)[names(data) == 'Q34_17'] <- 'Julien Co'
names(data)[names(data) == 'Q34_18'] <- 'Kelly Misses Vlog'
names(data)[names(data) == 'Q34_19'] <- 'Lena Gercke'
names(data)[names(data) == 'Q34_21'] <- 'Leon Content'
names(data)[names(data) == 'Q34_20'] <- 'Leonie Hanne'
names(data)[names(data) == 'Q43_1'] <- 'Lillydoo'
names(data)[names(data) == 'Q43_2'] <- 'Lisa Marie Schiffner'
names(data)[names(data) == 'Q43_3'] <- 'Alex Koch'
names(data)[names(data) == 'Q43_4'] <- 'MAC Cosmetics'
names(data)[names(data) == 'Q43_5'] <- 'Melina Sophie'
names(data)[names(data) == 'Q43_6'] <- 'Balea'
names(data)[names(data) == 'Q43_7'] <- 'Naturkosmetik Muenchen'
names(data)[names(data) == 'Q43_8'] <- 'NYX Professional Makeup'
names(data)[names(data) == 'Q43_9'] <- 'Paola Maria'
names(data)[names(data) == 'Q43_10'] <- 'Riccardo Simonetti'
names(data)[names(data) == 'Q43_11'] <- 'Roman Lochmann'
names(data)[names(data) == 'Q43_12'] <- 'Sarah Harrison'
names(data)[names(data) == 'Q43_13'] <- 'Simon Desue'
names(data)[names(data) == 'Q43_14'] <- 'Takko Fashion'
names(data)[names(data) == 'Q43_15'] <- 'Team Harrison'
names(data)[names(data) == 'Q43_16'] <- 'Vogue Germany'
names(data)[names(data) == 'Q43_17'] <- 'Alverde'
names(data)[names(data) == 'Q43_18'] <- 'Leon Skincare'
names(data)[names(data) == 'Q43_19'] <- 'Westwing'
names(data)[names(data) == 'Q43_20'] <- 'IKEA'
names(data)[names(data) == 'Q35_1'] <- 'Andrea Berg'
names(data)[names(data) == 'Q35_2'] <- 'Annenmaykantereit'
names(data)[names(data) == 'Q35_3'] <- 'Berliner Philharmoniker'
names(data)[names(data) == 'Q35_4'] <- 'Boehse Onkelz'
names(data)[names(data) == 'Q35_5'] <- 'Bushido'
names(data)[names(data) == 'Q35_6'] <- 'Capital Bra'
names(data)[names(data) == 'Q35_7'] <- 'Die Toten Hosen'
names(data)[names(data) == 'Q35_8'] <- 'Eurovision Song Contest'
names(data)[names(data) == 'Q35_9'] <- 'Helene Fischer'
names(data)[names(data) == 'Q35_10'] <- 'Lena Meyer-Landrut'
names(data)[names(data) == 'Q35_11'] <- 'LionTTV'
names(data)[names(data) == 'Q35_12'] <- 'Mero'
names(data)[names(data) == 'Q35_13'] <- 'Parookaville'
names(data)[names(data) == 'Q35_14'] <- 'Pietro Lombardi'
names(data)[names(data) == 'Q35_15'] <- 'Shirin David'
names(data)[names(data) == 'Q35_16'] <- 'Silbermond'
names(data)[names(data) == 'Q35_17'] <- 'The BossHoss'
names(data)[names(data) == 'Q35_18'] <- 'Wacken Open Air'
names(data)[names(data) == 'Q35_19'] <- 'Michael Wendler'
names(data)[names(data) == 'Q36_1'] <- 'AfD'
names(data)[names(data) == 'Q36_2'] <- 'Alice Weidel'
names(data)[names(data) == 'Q36_3'] <- 'Bundesgesundheitsministerium'
names(data)[names(data) == 'Q36_4'] <- 'Angela Merkel'
names(data)[names(data) == 'Q36_5'] <- 'Bundeswehr'
names(data)[names(data) == 'Q36_6'] <- 'CDU'
names(data)[names(data) == 'Q36_7'] <- 'Christian Lindner'
names(data)[names(data) == 'Q36_8'] <- 'Buendnis 90/Die Gruenen'
names(data)[names(data) == 'Q36_9'] <- 'Die Linke'
names(data)[names(data) == 'Q36_10'] <- 'Die Partei'
names(data)[names(data) == 'Q36_11'] <- 'Evangelisch.de'
names(data)[names(data) == 'Q36_12'] <- 'FDP'
names(data)[names(data) == 'Q36_13'] <- 'Fridays for Future'
names(data)[names(data) == 'Q44_1'] <- 'Islamfakten'
names(data)[names(data) == 'Q44_2'] <- 'Jens Spahn'
names(data)[names(data) == 'Q44_3'] <- 'katholisch.de'
names(data)[names(data) == 'Q44_5'] <- 'Louisa Dellert'
names(data)[names(data) == 'Q44_6'] <- 'Luisa Neubauer'
names(data)[names(data) == 'Q44_7'] <- 'Heiko Maas'
names(data)[names(data) == 'Q44_8'] <- 'PETA Deutschland'
names(data)[names(data) == 'Q44_9'] <- 'Querdenken711'
names(data)[names(data) == 'Q44_10'] <- 'Robert Habeck'
names(data)[names(data) == 'Q44_11'] <- 'Sahra Wagenknecht'
names(data)[names(data) == 'Q44_12'] <- 'SPD'
names(data)[names(data) == 'Q37_1'] <- 'adidas Deutschland'
names(data)[names(data) == 'Q37_2'] <- 'Alica Schmidt'
names(data)[names(data) == 'Q37_3'] <- 'Borussia Dortmund'
names(data)[names(data) == 'Q37_4'] <- 'DFB'
names(data)[names(data) == 'Q37_5'] <- 'RB Leipzig'
names(data)[names(data) == 'Q37_6'] <- 'FC Bayern Muenchen'
names(data)[names(data) == 'Q37_7'] <- 'Felix Neureuther'
names(data)[names(data) == 'Q37_8'] <- 'Felix Sturm'
names(data)[names(data) == 'Q37_9'] <- 'Gina Lueckenkemper'
names(data)[names(data) == 'Q37_10'] <- 'Christoph Icke Dommisch'
names(data)[names(data) == 'Q37_11'] <- 'Inscope21'
names(data)[names(data) == 'Q37_12'] <- 'kicker'
names(data)[names(data) == 'Q37_13'] <- 'Lisa Mueller'
names(data)[names(data) == 'Q45_1'] <- 'Mady Morrison'
names(data)[names(data) == 'Q45_2'] <- 'Manuel Neuer'
names(data)[names(data) == 'Q45_3'] <- 'Marco Reus'
names(data)[names(data) == 'Q45_4'] <- 'McFit'
names(data)[names(data) == 'Q45_5'] <- 'Oceans Apart'
names(data)[names(data) == 'Q45_6'] <- 'Pamela Reif'
names(data)[names(data) == 'Q45_7'] <- 'Philipp Lahm'
names(data)[names(data) == 'Q45_8'] <- 'Sophia Thiel'
names(data)[names(data) == 'Q45_9'] <- 'FC Schalke 04'
names(data)[names(data) == 'Q45_10'] <- 'Sky Sport'
names(data)[names(data) == 'Q45_11'] <- 'Sport1'
names(data)[names(data) == 'Q45_12'] <- 'Uwe Gensheimer'
names(data)[names(data) == 'Q38_1'] <- 'Canon Deutschland'
names(data)[names(data) == 'Q38_2'] <- 'Create! By Obi'
names(data)[names(data) == 'Q38_3'] <- 'Deutsche Bahn'
names(data)[names(data) == 'Q38_4'] <- 'Easy Alex'
names(data)[names(data) == 'Q38_5'] <- 'Flixbus'
names(data)[names(data) == 'Q38_6'] <- 'Ford Deutschland'
names(data)[names(data) == 'Q38_7'] <- 'Germanroamers'
names(data)[names(data) == 'Q38_8'] <- 'Hannes Becker'
names(data)[names(data) == 'Q38_9'] <- 'Linda DIY'
names(data)[names(data) == 'Q38_10'] <- 'Martin Ruetter'
names(data)[names(data) == 'Q38_11'] <- 'Mercedes-Benz Deutschland'
names(data)[names(data) == 'Q38_12'] <- 'Tiere suchen ein Zuhause'
names(data)[names(data) == 'Q38_13'] <- 'Urlaubsguru'
names(data)[names(data) == 'Q38_14'] <- 'Urlaubspiraten'
names(data)[names(data) == 'Q38_15'] <- 'Xlaeta'
names(data)[names(data) == 'Q38_16'] <- 'Yamaha Motor Deutschland'
names(data)[names(data) == 'Q38_17'] <- 'Yvonne Pfeffer'
names(data)[names(data) == 'Q47_1'] <- 'Ariana Grande'
names(data)[names(data) == 'Q47_2'] <- 'Beyonce'
names(data)[names(data) == 'Q47_3'] <- 'Cristiano Ronaldo'
names(data)[names(data) == 'Q47_9'] <- 'Dwayne Johnson'
names(data)[names(data) == 'Q47_4'] <- 'Justin Bieber'
names(data)[names(data) == 'Q47_5'] <- 'Kim Kardashian West'
names(data)[names(data) == 'Q47_6'] <- 'Kylie Jenner'
names(data)[names(data) == 'Q47_7'] <- 'Lionel Messi'
names(data)[names(data) == 'Q47_12'] <- 'National Geographic'
names(data)[names(data) == 'Q47_8'] <- 'Selena Gomez'
names(data)[names(data) == 'Q8_1'] <- 'Extrovertiert/enthusiastisch'
names(data)[names(data) == 'Q8_2'] <- 'Kritisch/konfliktfreudig'
names(data)[names(data) == 'Q8_3'] <- 'Zuverlaessig/selbstdiszipliniert'
names(data)[names(data) == 'Q8_4'] <- 'Aengstlich/leicht reizbar'
names(data)[names(data) == 'Q8_5'] <- 'Offen fuer neue Erfahrungen/vielseitig'
names(data)[names(data) == 'Q8_6'] <- 'Kontrollfrage Persoenlichkeit'
names(data)[names(data) == 'Q8_7'] <- 'Zurueckhaltend/ruhig'
names(data)[names(data) == 'Q8_8'] <- 'Sympathisch/warmherzig'
names(data)[names(data) == 'Q8_9'] <- 'Unorganisiert/nachlaessig'
names(data)[names(data) == 'Q8_10'] <- 'Ruhig/emotional stabil'
names(data)[names(data) == 'Q8_11'] <- 'Konventionell/unkreativ'
names(data)[names(data) == 'Q9'] <- 'Alkohol Konsum'
names(data)[names(data) == 'Q10'] <- 'Zigaretten Konsum'
names(data)[names(data) == 'Q11'] <- 'Drogen Konsum'
names(data)[names(data) == 'Q12_1'] <- 'Gefuehl der Zugehoerigkeit'
names(data)[names(data) == 'Q12_2'] <- 'Spannung'
names(data)[names(data) == 'Q12_3'] <- 'Kontrollfrage Ziele im Leben'
names(data)[names(data) == 'Q12_4'] <- 'Herzliche Beziehung zu anderen Menschen'
names(data)[names(data) == 'Q12_5'] <- 'Selbstverwirklichung'
names(data)[names(data) == 'Q12_6'] <- 'Respekt vor Anderen'
names(data)[names(data) == 'Q12_7'] <- 'Spass und Freude am Leben'
names(data)[names(data) == 'Q12_8'] <- 'Sicherheit'
names(data)[names(data) == 'Q12_9'] <- 'Selbstachtung'
names(data)[names(data) == 'Q12_10'] <- 'Gefuehl von Erfolg'
names(data)[names(data) == 'Q13'] <- 'Wahl Partei'
names(data)[names(data) == 'Q13_8'] <- 'Wahl Partei Sonstiges'
names(data)[names(data) == 'Q14_1'] <- 'Corona-Massnahmen uebertrieben'
names(data)[names(data) == 'Q14_2'] <- 'Corona-Massnahmen muessten haerter sein'
names(data)[names(data) == 'Q14_3'] <- 'Corona ist harmlos, gleich Grippe'
names(data)[names(data) == 'Q14_4'] <- 'Glaube nicht an Corona'
names(data)[names(data) == 'Q15'] <- 'Nettoeinkommen'
names(data)[names(data) == 'Q16_1'] <- 'Verwendete Produkte Umwelt nicht belasten'
names(data)[names(data) == 'Q16_2'] <- 'Auswirkungen meiner Handlungen auf Umwelt'
names(data)[names(data) == 'Q16_3'] <- 'Kaufgewohnheiten, Sorge um Umwelt'
names(data)[names(data) == 'Q16_4'] <- 'Verschwendung Ressourcen'
names(data)[names(data) == 'Q16_5'] <- 'Kontrollfrage Umwelt'
names(data)[names(data) == 'Q16_6'] <- 'Umweltverantwortlich'
names(data)[names(data) == 'Q16_7'] <- 'Unannehmlichkeiten fuer Umwelt'
names(data)[names(data) == 'Q17'] <- 'Beschaeftigung'
names(data)[names(data) == 'Q18'] <- 'Bildungsabschluss'
names(data)[names(data) == 'Q19'] <- 'Religion'
names(data)[names(data) == 'Q19_5'] <- 'Religion Sonstiges'
names(data)[names(data) == 'Q20'] <- 'Migrationshintergrund'
names(data)[names(data) == 'Q21'] <- 'Woher Vorfahren'
names(data)[names(data) == 'Q22'] <- 'Sexuelle Orientierung'
names(data)[names(data) == 'Q22_4'] <- 'Sexuelle Orientierung Sonstiges'
names(data)[names(data) == 'Q23'] <- 'Beziehungsstatus'
names(data)[names(data) == 'Q24'] <- 'Kinder'
names(data)[names(data) == 'Q26'] <- 'Anzahl Kinder'
names(data)[names(data) == 'Q40'] <- 'Instagram Name'

num_full <- data


#select relevant columns

cols_names <- names(num_full)  
cols_text <- cols_names[c(9, 22, 23, 264:266, 277, 278, 283, 291, 292, 293, 294, 295, 296, 297, 298, 299, 302)]
cols_num <- cols_names[c(3, 6, 7, 19, 20, 21, 24:252, 253:263, 267:276, 279:282, 284:290, 300, 301)]
  
#combine relevant colums into final dataset
data <- cbind(text_full[, cols_text], num_full[, cols_num])

#make sure all columns have the right specifications: as of now, everything is character
data[c(cols_num)] <- sapply(data[c(cols_num)], as.numeric)
  
#change NAs to 0 when applicable: for Accounts, no of children
change_to_zero <- cols_names[c(24:252, 301)]
data[ ,change_to_zero][is.na(data[ ,change_to_zero])] <- 0


#reorder columns if needed?






#clear respondents who did not finish survey
data <- subset(data, Kinder != "NA")

#make sure that control questions were answered correctly

data <- data[(data$`Kontrollfrage Persoenlichkeit`== 6),] #xx respondents remain
data <- data[(data$`Kontrollfrage Ziele im Leben`== 7),] #xx respondents remain
data <- data[(data$`Kontrollfrage Umwelt`== 6),] #xx respondents remain

#check that all bad respondents were eliminated
table(data$Finished) #xx respondents still are not marked as finished; however those are respondents who answered all questions but then did not press "continue" to end the questionnaire; we can still include them in the analysis
table(data$`Kontrollfrage Persoenlichkeit`)
table(data$`Kontrollfrage Ziele im Leben`)
table(data$`Kontrollfrage Umwelt`) #all okay

#check how long the respondents needed: are there speeders left?
table(data$`Duration (in seconds)`) #exclude all who finished in under 5 min (under 300 seconds)?



#save combined datasets as CSV and R Document
write.csv(data, "/Users/Miriam/Documents/Uni/Master/3. Semester/Seminar SRA/datasets/neu/datasets_combined.csv")
save(data, file = 'datasets_combined.RData')


#####
#add or redefine variables: scales, re-coding, new info

#####
#scales
#####
#TIPI Personality

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
data <- data %>%
  rowwise() %>%
  mutate(Extraversion = mean(c(`Extrovertiert/enthusiastisch`, `Zurueckhaltend/ruhig_nichtR`)))


#Agreeableness: 2R + 8
data <- data %>%
  rowwise() %>%
  mutate(Agreeableness = mean(c(`Kritisch/konfliktfreudig_nichtR`, `Sympathisch/warmherzig`)))


#Conscientiousness: 3 + 9R
data <- data %>%
  rowwise() %>%
  mutate(Conscientiousness = mean(c(`Zuverlaessig/selbstdiszipliniert`, `Unorganisiert/nachlaessig_nichtR`)))


#Emotional stability: 4R + 10
data <- data %>%
  rowwise() %>%
  mutate(Emotional_stablity = mean(c(`Aengstlich/leicht reizbar_nichtR`, `Ruhig/emotional stabil`)))


#Openness to Experiences: 5R + 11
data <- data %>%
  rowwise() %>%
  mutate(Openness_to_Experiences = mean(c(`Offen fuer neue Erfahrungen/vielseitig`, `Konventionell/unkreativ_nichtR`)))



#####
#Green Values
data <- data %>%
  rowwise() %>%
  mutate(Green_Values = mean(c(`Verwendete Produkte Umwelt nicht belasten`, `Auswirkungen meiner Handlungen auf Umwelt`, `Kaufgewohnheiten, Sorge um Umwelt`, `Verschwendung Ressourcen`, Umweltverantwortlich, `Unannehmlichkeiten fuer Umwelt`)))


#####
#Corona
#checking: how to define?
table(data$`Corona-Massnahmen uebertrieben`)
table(data$`Corona-Massnahmen muessten haerter sein`)
table(data$`Corona ist harmlos, gleich Grippe`)
table(data$`Glaube nicht an Corona`)
table(data$`Corona ist harmlos, gleich Grippe`, data$`Glaube nicht an Corona`)

#define new variable Corona_Attitude: Accept vs. Reject
Corona <- data %>% transmute(Corona_Attitude = ifelse((`Corona ist harmlos, gleich Grippe` == 6 | `Corona ist harmlos, gleich Grippe` == 7 | `Glaube nicht an Corona` == 6 | `Glaube nicht an Corona` == 7), yes = "Reject", no = "Accept"))
data <- bind_cols(data, Corona)


#####
#recoding for analysis

#age: ranges? young, medium, old?
#Germany/ PLZ: East/West? Regions?
#Alcohol usage: often vs. not often
#Smoking usage: often vs. not often
#Drug usage: often vs. not often
#Education level: low vs. high
#Religiosity: Religious vs not???
#Relationship status: single or taken


#####
#new info

#how many accounts does each respondent follow?
Accounts <- data %>% select(`Alman Memes` : `Selena Gomez`)
Account_names <- names(Accounts)
#Account_names <- names(data %>% select(`Alman Memes` : `Selena Gomez`))
data[c(Account_names)] <- sapply(data[c(Account_names)], as.numeric)
data$Accounts_followed <- rowSums(data[ ,c(Account_names)], na.rm = TRUE)

table(data$Accounts_followed)
#necessary to exclude respondents who follow too many or too little accounts?

#more new info??




#save full dataset as CSV and R Document
write.csv(data, "/Users/Miriam/Documents/Uni/Master/3. Semester/Seminar SRA/datasets/neu/data_all_var.csv")
save(data, file = 'data_all_var.RData')



#####
#check descriptives and get to know the data

#####
#Descriptives for Respondents



#decide: are there respondents (outliers) to get rid of? 


#####
#Descriptives for Variables





#decide: are there variables which have too little variety?


#####
#Descriptives Accounts

#Followers per Account
Accounts <- data %>% select(`Alman Memes` : `Selena Gomez`)
Followers_Accounts <- as.data.frame(colSums(Accounts, na.rm = TRUE))
colnames(Followers_Accounts) <- "Followers"

#Are there accounts which have too little followers? Exclusion necessary?
#list of less than "a" followers:
a <- 50
delete <- Followers_Accounts %>% filter(Followers < a) #delete
keep <-Followers_Accounts %>% filter(Followers > a) #keep and use for analysis

#next steps: save accounts with <a and delete them from dataset




#####
#Analysis
#run models


#test assumptions and robustness
