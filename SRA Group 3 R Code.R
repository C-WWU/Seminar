#Socially (Ir)Responsible Algorithms

#####
#load and install packages
install.packages("readr")
install.packages("naniar")
install.packages("cowplot")
install.packages("randomForest")
install.packages("pROC")
install.packages("caret")
install.packages("e1071")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("stepPlr")
install.packages("mlbench")
install.packages("readxl")
install.packages("DMwR")
install.packages("ROSE")
install.packages("ranger")
install.packages("MASS")
install.packages("pdp")
install.packages("elasticnet")
install.packages("glmnet")
install.packages("Matrix")
install.packages("Hmisc")


library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(naniar)
library(cowplot)
library(randomForest)
library(pROC)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(stepPlr)
library(mlbench)
library(readxl)
library(DMwR)
library(ROSE)
library(ranger)
library(MASS)
library(pdp)
library(elasticnet)
library(glmnet)
library(Matrix)
library(Hmisc)


options(max.print = 100000)



#####
#preparations
#load datasets and delete unnecessary first rows using readr

#gapfish data: load and then remove last column (id) and first four rows (test ids), add source information
gapfish_text <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Gapfish Text.csv")
gapfish_text <- gapfish_text[-(1:4), -303]
gapfish_text$Quelle <- "Gapfish"
gapfish_num <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Gapfish Numeric.csv")
gapfish_num <- gapfish_num[-(1:4), -303]
gapfish_num$Quelle <- "Gapfish"

#private contacts data: load and then remove first two rows, add source information
private_text <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Privat Text.csv")
private_text <- private_text[-(1:2), ]
private_text$Quelle <- "Privat"
private_num <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Privat Numeric.csv")
private_num<- private_num[-(1:2), ]
private_num$Quelle <- "Privat"

#surveycircle data: load and then remove first two rows, add source information
surveycircle_text <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Surveycircle Text.csv")
surveycircle_text <- surveycircle_text[-(1:2), ]
surveycircle_text$Quelle <- "Surveycircle"
surveycircle_num <- read_csv("~/Uni/Master/3. Semester/Seminar SRA/datasets/Surveycircle Numeric.csv")
surveycircle_num <- surveycircle_num[-(1:2), ]
surveycircle_num$Quelle <- "Surveycircle"


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
names(data)[names(data) == 'Q6'] <- 'Instagram_Nutzer'
names(data)[names(data) == 'Q7'] <- 'Instagram_Nutzungshaeufigkeit'
names(data)[names(data) == 'Q30_1'] <- 'Alman_Memes'
names(data)[names(data) == 'Q30_2'] <- 'Barbara_Schoeneberger'
names(data)[names(data) == 'Q30_3'] <- 'Berlin_Tag_und_Nacht'
names(data)[names(data) == 'Q30_4'] <- 'Brigitte_Magazin'
names(data)[names(data) == 'Q30_5'] <- 'Michael_Bully_Herbig'
names(data)[names(data) == 'Q30_6'] <- 'Dein_Beichtstuhl'
names(data)[names(data) == 'Q30_7'] <- 'Dieter_Nuhr'
names(data)[names(data) == 'Q30_8'] <- 'Disney_Deutschland'
names(data)[names(data) == 'Q30_9'] <- 'Elyas_M_Barek'
names(data)[names(data) == 'Q30_10'] <- 'Faktastisch'
names(data)[names(data) == 'Q30_11'] <- 'Felix_Lobrecht'
names(data)[names(data) == 'Q30_12'] <- 'Germanys_next_Topmodel'
names(data)[names(data) == 'Q30_13'] <- 'heute_show'
names(data)[names(data) == 'Q30_14'] <- 'Jan_Josef_Liefers'
names(data)[names(data) == 'Q30_15'] <- 'Julien_Bam'
names(data)[names(data) == 'Q30_16'] <- 'Jens_Knossalla'
names(data)[names(data) == 'Q41_1'] <- 'Laser_Luca'
names(data)[names(data) == 'Q41_2'] <- 'Love_Island'
names(data)[names(data) == 'Q41_3'] <- 'Mario_Barth'
names(data)[names(data) == 'Q41_4'] <- 'Made_My_Day'
names(data)[names(data) == 'Q41_5'] <- 'Netflix_DE'
names(data)[names(data) == 'Q41_6'] <- 'Nicholas_Puschmann'
names(data)[names(data) == 'Q41_7'] <- 'Joko_Winterscheidt'
names(data)[names(data) == 'Q41_8'] <- 'Oliver_Pocher'
names(data)[names(data) == 'Q41_9'] <- 'Palina_Rojinski'
names(data)[names(data) == 'Q41_10'] <- 'Playboy_Germany'
names(data)[names(data) == 'Q41_11'] <- 'Promiflash'
names(data)[names(data) == 'Q41_12'] <- 'Sebastian_Fitzek'
names(data)[names(data) == 'Q41_13'] <- 'Sophia_Thomalla'
names(data)[names(data) == 'Q41_14'] <- 'The_Voice_of_Germany'
names(data)[names(data) == 'Q41_15'] <- 'Wer_weiss_denn_sowas'
names(data)[names(data) == 'Q31_1'] <- 'Food_Stories'
names(data)[names(data) == 'Q31_2'] <- 'Aldi_Nord'
names(data)[names(data) == 'Q31_3'] <- 'Aldi_Sued'
names(data)[names(data) == 'Q31_4'] <- 'Astra'
names(data)[names(data) == 'Q31_5'] <- 'Backen_de'
names(data)[names(data) == 'Q31_6'] <- 'BakeClub'
names(data)[names(data) == 'Q31_7'] <- 'Chefkoch'
names(data)[names(data) == 'Q31_8'] <- 'Edeka'
names(data)[names(data) == 'Q31_9'] <- 'Einfach_Tasty'
names(data)[names(data) == 'Q31_10'] <- 'Etepetete'
names(data)[names(data) == 'Q31_11'] <- 'Foodist'
names(data)[names(data) == 'Q31_12'] <- 'Fritz_Kola'
names(data)[names(data) == 'Q31_13'] <- 'Fruehlingszwiebel'
names(data)[names(data) == 'Q31_14'] <- 'Haribo'
names(data)[names(data) == 'Q31_15'] <- 'Hello_Fresh'
names(data)[names(data) == 'Q31_16'] <- 'Junk_Food_Guru'
names(data)[names(data) == 'Q42_1'] <- 'Just_Spices'
names(data)[names(data) == 'Q42_2'] <- 'Kaufland'
names(data)[names(data) == 'Q42_3'] <- 'Leckerschmecker'
names(data)[names(data) == 'Q42_4'] <- 'McDonalds_Deutschland'
names(data)[names(data) == 'Q42_5'] <- 'Pam_Goes_Nuts'
names(data)[names(data) == 'Q42_6'] <- 'Pflanzlich_stark'
names(data)[names(data) == 'Q42_7'] <- 'Plantbased_Food_and_Travel'
names(data)[names(data) == 'Q42_8'] <- 'Redbull_Germany'
names(data)[names(data) == 'Q42_9'] <- 'Sallys_Welt'
names(data)[names(data) == 'Q42_10'] <- 'SimplyV'
names(data)[names(data) == 'Q42_11'] <- 'Starbucks_Deutschland'
names(data)[names(data) == 'Q42_12'] <- 'Steffen_Henssler'
names(data)[names(data) == 'Q42_13'] <- 'Tim_Maelzer'
names(data)[names(data) == 'Q42_14'] <- 'True_fruits'
names(data)[names(data) == 'Q42_15'] <- 'Vapiano'
names(data)[names(data) == 'Q42_16'] <- 'Weber_Grill'
names(data)[names(data) == 'Q32_1'] <- 'Animal_Crossing'
names(data)[names(data) == 'Q32_2'] <- 'Call_of_Duty'
names(data)[names(data) == 'Q32_3'] <- 'EA_Sports_FIFA'
names(data)[names(data) == 'Q32_4'] <- 'Felix_von_der_Laden'
names(data)[names(data) == 'Q32_5'] <- 'Fortnite'
names(data)[names(data) == 'Q32_6'] <- 'Gamingzelle'
names(data)[names(data) == 'Q32_7'] <- 'Go_Pro_Deutschland'
names(data)[names(data) == 'Q32_8'] <- 'Huawei_Deutschland'
names(data)[names(data) == 'Q32_9'] <- 'Microsoft_Deutschland'
names(data)[names(data) == 'Q32_10'] <- 'Mohammed_Harkous'
names(data)[names(data) == 'Q32_11'] <- 'Montana_Black'
names(data)[names(data) == 'Q32_12'] <- 'Nintendo'
names(data)[names(data) == 'Q32_13'] <- 'PlayStation_DACH'
names(data)[names(data) == 'Q32_14'] <- 'Rewinside'
names(data)[names(data) == 'Q32_15'] <- 'Reyst'
names(data)[names(data) == 'Q32_16'] <- 'Rezo'
names(data)[names(data) == 'Q32_17'] <- 'Ungespielt_Simon_Unge'
names(data)[names(data) == 'Q32_18'] <- 'Xbox_DACH'
names(data)[names(data) == 'Q33_1'] <- 'Apotheken_Umschau'
names(data)[names(data) == 'Q33_2'] <- 'ARTE'
names(data)[names(data) == 'Q33_3'] <- 'BILD_Zeitung'
names(data)[names(data) == 'Q33_4'] <- 'Frankfurter_Allgemeine_Zeitung'
names(data)[names(data) == 'Q33_5'] <- 'GEO_Magazin'
names(data)[names(data) == 'Q33_6'] <- 'Handelsblatt'
names(data)[names(data) == 'Q33_7'] <- 'Quarks_Co'
names(data)[names(data) == 'Q33_8'] <- 'RTL_Aktuell'
names(data)[names(data) == 'Q33_9'] <- 'Der_Spiegel'
names(data)[names(data) == 'Q33_10'] <- 'Tagesschau'
names(data)[names(data) == 'Q33_11'] <- 'taz'
names(data)[names(data) == 'Q33_12'] <- 'ZEIT'
names(data)[names(data) == 'Q34_1'] <- 'Andre_Schiebler'
names(data)[names(data) == 'Q34_2'] <- 'Anna_Maria_Damm'
names(data)[names(data) == 'Q34_3'] <- 'bebe'
names(data)[names(data) == 'Q34_4'] <- 'Bibis_Beauty_Palace'
names(data)[names(data) == 'Q34_5'] <- 'Bonnie_Strange'
names(data)[names(data) == 'Q34_6'] <- 'Carmen_Kroll'
names(data)[names(data) == 'Q34_7'] <- 'Caro_Daur'
names(data)[names(data) == 'Q34_8'] <- 'DagiBee'
names(data)[names(data) == 'Q34_9'] <- 'Daniela_Katzenberger'
names(data)[names(data) == 'Q34_10'] <- 'Dilara'
names(data)[names(data) == 'Q34_11'] <- 'dm'
names(data)[names(data) == 'Q34_12'] <- 'Melike'
names(data)[names(data) == 'Q34_13'] <- 'Die_groesste_Community_fuer_Muetter'
names(data)[names(data) == 'Q34_14'] <- 'Guido_Maria_Kretschmer'
names(data)[names(data) == 'Q34_15'] <- 'Ischtar_Isik'
names(data)[names(data) == 'Q34_16'] <- 'Julia_Beautx'
names(data)[names(data) == 'Q34_17'] <- 'Julien_Co'
names(data)[names(data) == 'Q34_18'] <- 'Kelly_Misses_Vlog'
names(data)[names(data) == 'Q34_19'] <- 'Lena_Gercke'
names(data)[names(data) == 'Q34_21'] <- 'Leon_Content'
names(data)[names(data) == 'Q34_20'] <- 'Leonie_Hanne'
names(data)[names(data) == 'Q43_1'] <- 'Lillydoo'
names(data)[names(data) == 'Q43_2'] <- 'Lisa_Marie_Schiffner'
names(data)[names(data) == 'Q43_3'] <- 'Alex_Koch'
names(data)[names(data) == 'Q43_4'] <- 'MAC_Cosmetics'
names(data)[names(data) == 'Q43_5'] <- 'Melina_Sophie'
names(data)[names(data) == 'Q43_6'] <- 'Balea'
names(data)[names(data) == 'Q43_7'] <- 'Naturkosmetik_Muenchen'
names(data)[names(data) == 'Q43_8'] <- 'NYX_Professional_Makeup'
names(data)[names(data) == 'Q43_9'] <- 'Paola_Maria'
names(data)[names(data) == 'Q43_10'] <- 'Riccardo_Simonetti'
names(data)[names(data) == 'Q43_11'] <- 'Roman_Lochmann'
names(data)[names(data) == 'Q43_12'] <- 'Sarah_Harrison'
names(data)[names(data) == 'Q43_13'] <- 'Simon_Desue'
names(data)[names(data) == 'Q43_14'] <- 'Takko_Fashion'
names(data)[names(data) == 'Q43_15'] <- 'Team_Harrison'
names(data)[names(data) == 'Q43_16'] <- 'Vogue_Germany'
names(data)[names(data) == 'Q43_17'] <- 'Alverde'
names(data)[names(data) == 'Q43_18'] <- 'Leon_Skincare'
names(data)[names(data) == 'Q43_19'] <- 'Westwing'
names(data)[names(data) == 'Q43_20'] <- 'IKEA'
names(data)[names(data) == 'Q35_1'] <- 'Andrea_Berg'
names(data)[names(data) == 'Q35_2'] <- 'Annenmaykantereit'
names(data)[names(data) == 'Q35_3'] <- 'Berliner_Philharmoniker'
names(data)[names(data) == 'Q35_4'] <- 'Boehse_Onkelz'
names(data)[names(data) == 'Q35_5'] <- 'Bushido'
names(data)[names(data) == 'Q35_6'] <- 'Capital_Bra'
names(data)[names(data) == 'Q35_7'] <- 'Die_Toten_Hosen'
names(data)[names(data) == 'Q35_8'] <- 'Eurovision_Song_Contest'
names(data)[names(data) == 'Q35_9'] <- 'Helene_Fischer'
names(data)[names(data) == 'Q35_10'] <- 'Lena_Meyer_Landrut'
names(data)[names(data) == 'Q35_11'] <- 'LionTTV'
names(data)[names(data) == 'Q35_12'] <- 'Mero'
names(data)[names(data) == 'Q35_13'] <- 'Parookaville'
names(data)[names(data) == 'Q35_14'] <- 'Pietro_Lombardi'
names(data)[names(data) == 'Q35_15'] <- 'Shirin_David'
names(data)[names(data) == 'Q35_16'] <- 'Silbermond'
names(data)[names(data) == 'Q35_17'] <- 'The_BossHoss'
names(data)[names(data) == 'Q35_18'] <- 'Wacken_Open_Air'
names(data)[names(data) == 'Q35_19'] <- 'Michael_Wendler'
names(data)[names(data) == 'Q36_1'] <- 'AfD'
names(data)[names(data) == 'Q36_2'] <- 'Alice_Weidel'
names(data)[names(data) == 'Q36_3'] <- 'Bundesgesundheitsministerium'
names(data)[names(data) == 'Q36_4'] <- 'Angela_Merkel'
names(data)[names(data) == 'Q36_5'] <- 'Bundeswehr'
names(data)[names(data) == 'Q36_6'] <- 'CDU'
names(data)[names(data) == 'Q36_7'] <- 'Christian_Lindner'
names(data)[names(data) == 'Q36_8'] <- 'Buendnis_90_Die_Gruenen'
names(data)[names(data) == 'Q36_9'] <- 'Die_Linke'
names(data)[names(data) == 'Q36_10'] <- 'Die_Partei'
names(data)[names(data) == 'Q36_11'] <- 'Evangelisch_de'
names(data)[names(data) == 'Q36_12'] <- 'FDP'
names(data)[names(data) == 'Q36_13'] <- 'Fridays_for_Future'
names(data)[names(data) == 'Q44_1'] <- 'Islamfakten'
names(data)[names(data) == 'Q44_2'] <- 'Jens_Spahn'
names(data)[names(data) == 'Q44_3'] <- 'katholisch_de'
names(data)[names(data) == 'Q44_5'] <- 'Louisa_Dellert'
names(data)[names(data) == 'Q44_6'] <- 'Luisa_Neubauer'
names(data)[names(data) == 'Q44_7'] <- 'Heiko_Maas'
names(data)[names(data) == 'Q44_8'] <- 'PETA_Deutschland'
names(data)[names(data) == 'Q44_9'] <- 'Querdenken711'
names(data)[names(data) == 'Q44_10'] <- 'Robert_Habeck'
names(data)[names(data) == 'Q44_11'] <- 'Sahra_Wagenknecht'
names(data)[names(data) == 'Q44_12'] <- 'SPD'
names(data)[names(data) == 'Q37_1'] <- 'adidas_Deutschland'
names(data)[names(data) == 'Q37_2'] <- 'Alica_Schmidt'
names(data)[names(data) == 'Q37_3'] <- 'Borussia_Dortmund'
names(data)[names(data) == 'Q37_4'] <- 'DFB'
names(data)[names(data) == 'Q37_5'] <- 'RB_Leipzig'
names(data)[names(data) == 'Q37_6'] <- 'FC_Bayern_Muenchen'
names(data)[names(data) == 'Q37_7'] <- 'Felix_Neureuther'
names(data)[names(data) == 'Q37_8'] <- 'Felix_Sturm'
names(data)[names(data) == 'Q37_9'] <- 'Gina_Lueckenkemper'
names(data)[names(data) == 'Q37_10'] <- 'Christoph_Icke_Dommisch'
names(data)[names(data) == 'Q37_11'] <- 'Inscope21'
names(data)[names(data) == 'Q37_12'] <- 'kicker'
names(data)[names(data) == 'Q37_13'] <- 'Lisa_Mueller'
names(data)[names(data) == 'Q45_1'] <- 'Mady_Morrison'
names(data)[names(data) == 'Q45_2'] <- 'Manuel_Neuer'
names(data)[names(data) == 'Q45_3'] <- 'Marco_Reus'
names(data)[names(data) == 'Q45_4'] <- 'McFit'
names(data)[names(data) == 'Q45_5'] <- 'Oceans_Apart'
names(data)[names(data) == 'Q45_6'] <- 'Pamela_Reif'
names(data)[names(data) == 'Q45_7'] <- 'Philipp_Lahm'
names(data)[names(data) == 'Q45_8'] <- 'Sophia_Thiel'
names(data)[names(data) == 'Q45_9'] <- 'FC_Schalke_04'
names(data)[names(data) == 'Q45_10'] <- 'Sky_Sport'
names(data)[names(data) == 'Q45_11'] <- 'Sport1'
names(data)[names(data) == 'Q45_12'] <- 'Uwe_Gensheimer'
names(data)[names(data) == 'Q38_1'] <- 'Canon_Deutschland'
names(data)[names(data) == 'Q38_2'] <- 'Create_By_Obi'
names(data)[names(data) == 'Q38_3'] <- 'Deutsche_Bahn'
names(data)[names(data) == 'Q38_4'] <- 'Easy_Alex'
names(data)[names(data) == 'Q38_5'] <- 'Flixbus'
names(data)[names(data) == 'Q38_6'] <- 'Ford_Deutschland'
names(data)[names(data) == 'Q38_7'] <- 'Germanroamers'
names(data)[names(data) == 'Q38_8'] <- 'Hannes_Becker'
names(data)[names(data) == 'Q38_9'] <- 'Linda_DIY'
names(data)[names(data) == 'Q38_10'] <- 'Martin_Ruetter'
names(data)[names(data) == 'Q38_11'] <- 'Mercedes_Benz_Deutschland'
names(data)[names(data) == 'Q38_12'] <- 'Tiere_suchen_ein_Zuhause'
names(data)[names(data) == 'Q38_13'] <- 'Urlaubsguru'
names(data)[names(data) == 'Q38_14'] <- 'Urlaubspiraten'
names(data)[names(data) == 'Q38_15'] <- 'Xlaeta'
names(data)[names(data) == 'Q38_16'] <- 'Yamaha_Motor_Deutschland'
names(data)[names(data) == 'Q38_17'] <- 'Yvonne_Pfeffer'
names(data)[names(data) == 'Q47_1'] <- 'Ariana_Grande'
names(data)[names(data) == 'Q47_2'] <- 'Beyonce'
names(data)[names(data) == 'Q47_3'] <- 'Cristiano_Ronaldo'
names(data)[names(data) == 'Q47_9'] <- 'Dwayne_Johnson'
names(data)[names(data) == 'Q47_4'] <- 'Justin_Bieber'
names(data)[names(data) == 'Q47_5'] <- 'Kim_Kardashian_West'
names(data)[names(data) == 'Q47_6'] <- 'Kylie_Jenner'
names(data)[names(data) == 'Q47_7'] <- 'Lionel_Messi'
names(data)[names(data) == 'Q47_12'] <- 'National_Geographic'
names(data)[names(data) == 'Q47_8'] <- 'Selena_Gomez'
names(data)[names(data) == 'Q8_1'] <- 'Extrovertiert_enthusiastisch'
names(data)[names(data) == 'Q8_2'] <- 'Kritisch_konfliktfreudig'
names(data)[names(data) == 'Q8_3'] <- 'Zuverlaessig_selbstdiszipliniert'
names(data)[names(data) == 'Q8_4'] <- 'Aengstlich_leicht_reizbar'
names(data)[names(data) == 'Q8_5'] <- 'Offen_fuer_neue_Erfahrungen_vielseitig'
names(data)[names(data) == 'Q8_6'] <- 'Kontrollfrage_Persoenlichkeit'
names(data)[names(data) == 'Q8_7'] <- 'Zurueckhaltend_ruhig'
names(data)[names(data) == 'Q8_8'] <- 'Sympathisch_warmherzig'
names(data)[names(data) == 'Q8_9'] <- 'Unorganisiert_nachlaessig'
names(data)[names(data) == 'Q8_10'] <- 'Ruhig_emotional_stabil'
names(data)[names(data) == 'Q8_11'] <- 'Konventionell_unkreativ'
names(data)[names(data) == 'Q9'] <- 'Alkohol_Konsum'
names(data)[names(data) == 'Q10'] <- 'Zigaretten_Konsum'
names(data)[names(data) == 'Q11'] <- 'Drogen_Konsum'
names(data)[names(data) == 'Q12_1'] <- 'Gefuehl_der_Zugehoerigkeit'
names(data)[names(data) == 'Q12_2'] <- 'Spannung'
names(data)[names(data) == 'Q12_3'] <- 'Kontrollfrage_Ziele_im_Leben'
names(data)[names(data) == 'Q12_4'] <- 'Herzliche_Beziehung_zu_anderen_Menschen'
names(data)[names(data) == 'Q12_5'] <- 'Selbstverwirklichung'
names(data)[names(data) == 'Q12_6'] <- 'Respekt_vor_Anderen'
names(data)[names(data) == 'Q12_7'] <- 'Spass_und_Freude_am_Leben'
names(data)[names(data) == 'Q12_8'] <- 'Sicherheit'
names(data)[names(data) == 'Q12_9'] <- 'Selbstachtung'
names(data)[names(data) == 'Q12_10'] <- 'Gefuehl_von_Erfolg'
names(data)[names(data) == 'Q13'] <- 'Wahl_Partei'
names(data)[names(data) == 'Q13_8_TEXT'] <- 'Wahl_Partei_Sonstiges'
names(data)[names(data) == 'Q14_1'] <- 'Corona_Massnahmen_uebertrieben'
names(data)[names(data) == 'Q14_2'] <- 'Corona_Massnahmen_muessten_haerter_sein'
names(data)[names(data) == 'Q14_3'] <- 'Corona_ist_harmlos_gleich_Grippe'
names(data)[names(data) == 'Q14_4'] <- 'Glaube_nicht_an_Corona'
names(data)[names(data) == 'Q15'] <- 'Nettoeinkommen'
names(data)[names(data) == 'Q16_1'] <- 'Verwendete_Produkte_Umwelt_nicht_belasten'
names(data)[names(data) == 'Q16_2'] <- 'Auswirkungen_meiner_Handlungen_auf_Umwelt'
names(data)[names(data) == 'Q16_3'] <- 'Kaufgewohnheiten_Sorge_um_Umwelt'
names(data)[names(data) == 'Q16_4'] <- 'Verschwendung_Ressourcen'
names(data)[names(data) == 'Q16_5'] <- 'Kontrollfrage_Umwelt'
names(data)[names(data) == 'Q16_6'] <- 'Umweltverantwortlich'
names(data)[names(data) == 'Q16_7'] <- 'Unannehmlichkeiten_fuer_Umwelt'
names(data)[names(data) == 'Q17'] <- 'Beschaeftigung'
names(data)[names(data) == 'Q18'] <- 'Bildungsabschluss'
names(data)[names(data) == 'Q19'] <- 'Religion'
names(data)[names(data) == 'Q19_5_TEXT'] <- 'Religion_Sonstiges'
names(data)[names(data) == 'Q20'] <- 'Migrationshintergrund'
names(data)[names(data) == 'Q21'] <- 'Woher_Vorfahren'
names(data)[names(data) == 'Q22'] <- 'Sexuelle_Orientierung'
names(data)[names(data) == 'Q22_4_TEXT'] <- 'Sexuelle_Orientierung_Sonstiges'
names(data)[names(data) == 'Q23'] <- 'Beziehungsstatus'
names(data)[names(data) == 'Q24'] <- 'Kinder'
names(data)[names(data) == 'Q26'] <- 'Anzahl_Kinder'
names(data)[names(data) == 'Q40'] <- 'Instagram_Name'

text_full <- data

#for num_full
data <- num_full
names(data)[names(data) == 'StartDate'] <- 'Startdatum'
names(data)[names(data) == 'Q29'] <- 'DSGVO'
names(data)[names(data) == 'Q3'] <- 'Alter'
names(data)[names(data) == 'Q4'] <- 'Geschlecht'
names(data)[names(data) == 'Q5'] <- 'PLZ'
names(data)[names(data) == 'Q6'] <- 'Instagram_Nutzer'
names(data)[names(data) == 'Q7'] <- 'Instagram_Nutzungshaeufigkeit'
names(data)[names(data) == 'Q30_1'] <- 'Alman_Memes'
names(data)[names(data) == 'Q30_2'] <- 'Barbara_Schoeneberger'
names(data)[names(data) == 'Q30_3'] <- 'Berlin_Tag_und_Nacht'
names(data)[names(data) == 'Q30_4'] <- 'Brigitte_Magazin'
names(data)[names(data) == 'Q30_5'] <- 'Michael_Bully_Herbig'
names(data)[names(data) == 'Q30_6'] <- 'Dein_Beichtstuhl'
names(data)[names(data) == 'Q30_7'] <- 'Dieter_Nuhr'
names(data)[names(data) == 'Q30_8'] <- 'Disney_Deutschland'
names(data)[names(data) == 'Q30_9'] <- 'Elyas_M_Barek'
names(data)[names(data) == 'Q30_10'] <- 'Faktastisch'
names(data)[names(data) == 'Q30_11'] <- 'Felix_Lobrecht'
names(data)[names(data) == 'Q30_12'] <- 'Germanys_next_Topmodel'
names(data)[names(data) == 'Q30_13'] <- 'heute_show'
names(data)[names(data) == 'Q30_14'] <- 'Jan_Josef_Liefers'
names(data)[names(data) == 'Q30_15'] <- 'Julien_Bam'
names(data)[names(data) == 'Q30_16'] <- 'Jens_Knossalla'
names(data)[names(data) == 'Q41_1'] <- 'Laser_Luca'
names(data)[names(data) == 'Q41_2'] <- 'Love_Island'
names(data)[names(data) == 'Q41_3'] <- 'Mario_Barth'
names(data)[names(data) == 'Q41_4'] <- 'Made_My_Day'
names(data)[names(data) == 'Q41_5'] <- 'Netflix_DE'
names(data)[names(data) == 'Q41_6'] <- 'Nicholas_Puschmann'
names(data)[names(data) == 'Q41_7'] <- 'Joko_Winterscheidt'
names(data)[names(data) == 'Q41_8'] <- 'Oliver_Pocher'
names(data)[names(data) == 'Q41_9'] <- 'Palina_Rojinski'
names(data)[names(data) == 'Q41_10'] <- 'Playboy_Germany'
names(data)[names(data) == 'Q41_11'] <- 'Promiflash'
names(data)[names(data) == 'Q41_12'] <- 'Sebastian_Fitzek'
names(data)[names(data) == 'Q41_13'] <- 'Sophia_Thomalla'
names(data)[names(data) == 'Q41_14'] <- 'The_Voice_of_Germany'
names(data)[names(data) == 'Q41_15'] <- 'Wer_weiss_denn_sowas'
names(data)[names(data) == 'Q31_1'] <- 'Food_Stories'
names(data)[names(data) == 'Q31_2'] <- 'Aldi_Nord'
names(data)[names(data) == 'Q31_3'] <- 'Aldi_Sued'
names(data)[names(data) == 'Q31_4'] <- 'Astra'
names(data)[names(data) == 'Q31_5'] <- 'Backen_de'
names(data)[names(data) == 'Q31_6'] <- 'BakeClub'
names(data)[names(data) == 'Q31_7'] <- 'Chefkoch'
names(data)[names(data) == 'Q31_8'] <- 'Edeka'
names(data)[names(data) == 'Q31_9'] <- 'Einfach_Tasty'
names(data)[names(data) == 'Q31_10'] <- 'Etepetete'
names(data)[names(data) == 'Q31_11'] <- 'Foodist'
names(data)[names(data) == 'Q31_12'] <- 'Fritz_Kola'
names(data)[names(data) == 'Q31_13'] <- 'Fruehlingszwiebel'
names(data)[names(data) == 'Q31_14'] <- 'Haribo'
names(data)[names(data) == 'Q31_15'] <- 'Hello_Fresh'
names(data)[names(data) == 'Q31_16'] <- 'Junk_Food_Guru'
names(data)[names(data) == 'Q42_1'] <- 'Just_Spices'
names(data)[names(data) == 'Q42_2'] <- 'Kaufland'
names(data)[names(data) == 'Q42_3'] <- 'Leckerschmecker'
names(data)[names(data) == 'Q42_4'] <- 'McDonalds_Deutschland'
names(data)[names(data) == 'Q42_5'] <- 'Pam_Goes_Nuts'
names(data)[names(data) == 'Q42_6'] <- 'Pflanzlich_stark'
names(data)[names(data) == 'Q42_7'] <- 'Plantbased_Food_and_Travel'
names(data)[names(data) == 'Q42_8'] <- 'Redbull_Germany'
names(data)[names(data) == 'Q42_9'] <- 'Sallys_Welt'
names(data)[names(data) == 'Q42_10'] <- 'SimplyV'
names(data)[names(data) == 'Q42_11'] <- 'Starbucks_Deutschland'
names(data)[names(data) == 'Q42_12'] <- 'Steffen_Henssler'
names(data)[names(data) == 'Q42_13'] <- 'Tim_Maelzer'
names(data)[names(data) == 'Q42_14'] <- 'True_fruits'
names(data)[names(data) == 'Q42_15'] <- 'Vapiano'
names(data)[names(data) == 'Q42_16'] <- 'Weber_Grill'
names(data)[names(data) == 'Q32_1'] <- 'Animal_Crossing'
names(data)[names(data) == 'Q32_2'] <- 'Call_of_Duty'
names(data)[names(data) == 'Q32_3'] <- 'EA_Sports_FIFA'
names(data)[names(data) == 'Q32_4'] <- 'Felix_von_der_Laden'
names(data)[names(data) == 'Q32_5'] <- 'Fortnite'
names(data)[names(data) == 'Q32_6'] <- 'Gamingzelle'
names(data)[names(data) == 'Q32_7'] <- 'Go_Pro_Deutschland'
names(data)[names(data) == 'Q32_8'] <- 'Huawei_Deutschland'
names(data)[names(data) == 'Q32_9'] <- 'Microsoft_Deutschland'
names(data)[names(data) == 'Q32_10'] <- 'Mohammed_Harkous'
names(data)[names(data) == 'Q32_11'] <- 'Montana_Black'
names(data)[names(data) == 'Q32_12'] <- 'Nintendo'
names(data)[names(data) == 'Q32_13'] <- 'PlayStation_DACH'
names(data)[names(data) == 'Q32_14'] <- 'Rewinside'
names(data)[names(data) == 'Q32_15'] <- 'Reyst'
names(data)[names(data) == 'Q32_16'] <- 'Rezo'
names(data)[names(data) == 'Q32_17'] <- 'Ungespielt_Simon_Unge'
names(data)[names(data) == 'Q32_18'] <- 'Xbox_DACH'
names(data)[names(data) == 'Q33_1'] <- 'Apotheken_Umschau'
names(data)[names(data) == 'Q33_2'] <- 'ARTE'
names(data)[names(data) == 'Q33_3'] <- 'BILD_Zeitung'
names(data)[names(data) == 'Q33_4'] <- 'Frankfurter_Allgemeine_Zeitung'
names(data)[names(data) == 'Q33_5'] <- 'GEO_Magazin'
names(data)[names(data) == 'Q33_6'] <- 'Handelsblatt'
names(data)[names(data) == 'Q33_7'] <- 'Quarks_Co'
names(data)[names(data) == 'Q33_8'] <- 'RTL_Aktuell'
names(data)[names(data) == 'Q33_9'] <- 'Der_Spiegel'
names(data)[names(data) == 'Q33_10'] <- 'Tagesschau'
names(data)[names(data) == 'Q33_11'] <- 'taz'
names(data)[names(data) == 'Q33_12'] <- 'ZEIT'
names(data)[names(data) == 'Q34_1'] <- 'Andre_Schiebler'
names(data)[names(data) == 'Q34_2'] <- 'Anna_Maria_Damm'
names(data)[names(data) == 'Q34_3'] <- 'bebe'
names(data)[names(data) == 'Q34_4'] <- 'Bibis_Beauty_Palace'
names(data)[names(data) == 'Q34_5'] <- 'Bonnie_Strange'
names(data)[names(data) == 'Q34_6'] <- 'Carmen_Kroll'
names(data)[names(data) == 'Q34_7'] <- 'Caro_Daur'
names(data)[names(data) == 'Q34_8'] <- 'DagiBee'
names(data)[names(data) == 'Q34_9'] <- 'Daniela_Katzenberger'
names(data)[names(data) == 'Q34_10'] <- 'Dilara'
names(data)[names(data) == 'Q34_11'] <- 'dm'
names(data)[names(data) == 'Q34_12'] <- 'Melike'
names(data)[names(data) == 'Q34_13'] <- 'Die_groesste_Community_fuer_Muetter'
names(data)[names(data) == 'Q34_14'] <- 'Guido_Maria_Kretschmer'
names(data)[names(data) == 'Q34_15'] <- 'Ischtar_Isik'
names(data)[names(data) == 'Q34_16'] <- 'Julia_Beautx'
names(data)[names(data) == 'Q34_17'] <- 'Julien_Co'
names(data)[names(data) == 'Q34_18'] <- 'Kelly_Misses_Vlog'
names(data)[names(data) == 'Q34_19'] <- 'Lena_Gercke'
names(data)[names(data) == 'Q34_21'] <- 'Leon_Content'
names(data)[names(data) == 'Q34_20'] <- 'Leonie_Hanne'
names(data)[names(data) == 'Q43_1'] <- 'Lillydoo'
names(data)[names(data) == 'Q43_2'] <- 'Lisa_Marie_Schiffner'
names(data)[names(data) == 'Q43_3'] <- 'Alex_Koch'
names(data)[names(data) == 'Q43_4'] <- 'MAC_Cosmetics'
names(data)[names(data) == 'Q43_5'] <- 'Melina_Sophie'
names(data)[names(data) == 'Q43_6'] <- 'Balea'
names(data)[names(data) == 'Q43_7'] <- 'Naturkosmetik_Muenchen'
names(data)[names(data) == 'Q43_8'] <- 'NYX_Professional_Makeup'
names(data)[names(data) == 'Q43_9'] <- 'Paola_Maria'
names(data)[names(data) == 'Q43_10'] <- 'Riccardo_Simonetti'
names(data)[names(data) == 'Q43_11'] <- 'Roman_Lochmann'
names(data)[names(data) == 'Q43_12'] <- 'Sarah_Harrison'
names(data)[names(data) == 'Q43_13'] <- 'Simon_Desue'
names(data)[names(data) == 'Q43_14'] <- 'Takko_Fashion'
names(data)[names(data) == 'Q43_15'] <- 'Team_Harrison'
names(data)[names(data) == 'Q43_16'] <- 'Vogue_Germany'
names(data)[names(data) == 'Q43_17'] <- 'Alverde'
names(data)[names(data) == 'Q43_18'] <- 'Leon_Skincare'
names(data)[names(data) == 'Q43_19'] <- 'Westwing'
names(data)[names(data) == 'Q43_20'] <- 'IKEA'
names(data)[names(data) == 'Q35_1'] <- 'Andrea_Berg'
names(data)[names(data) == 'Q35_2'] <- 'Annenmaykantereit'
names(data)[names(data) == 'Q35_3'] <- 'Berliner_Philharmoniker'
names(data)[names(data) == 'Q35_4'] <- 'Boehse_Onkelz'
names(data)[names(data) == 'Q35_5'] <- 'Bushido'
names(data)[names(data) == 'Q35_6'] <- 'Capital_Bra'
names(data)[names(data) == 'Q35_7'] <- 'Die_Toten_Hosen'
names(data)[names(data) == 'Q35_8'] <- 'Eurovision_Song_Contest'
names(data)[names(data) == 'Q35_9'] <- 'Helene_Fischer'
names(data)[names(data) == 'Q35_10'] <- 'Lena_Meyer_Landrut'
names(data)[names(data) == 'Q35_11'] <- 'LionTTV'
names(data)[names(data) == 'Q35_12'] <- 'Mero'
names(data)[names(data) == 'Q35_13'] <- 'Parookaville'
names(data)[names(data) == 'Q35_14'] <- 'Pietro_Lombardi'
names(data)[names(data) == 'Q35_15'] <- 'Shirin_David'
names(data)[names(data) == 'Q35_16'] <- 'Silbermond'
names(data)[names(data) == 'Q35_17'] <- 'The_BossHoss'
names(data)[names(data) == 'Q35_18'] <- 'Wacken_Open_Air'
names(data)[names(data) == 'Q35_19'] <- 'Michael_Wendler'
names(data)[names(data) == 'Q36_1'] <- 'AfD'
names(data)[names(data) == 'Q36_2'] <- 'Alice_Weidel'
names(data)[names(data) == 'Q36_3'] <- 'Bundesgesundheitsministerium'
names(data)[names(data) == 'Q36_4'] <- 'Angela_Merkel'
names(data)[names(data) == 'Q36_5'] <- 'Bundeswehr'
names(data)[names(data) == 'Q36_6'] <- 'CDU'
names(data)[names(data) == 'Q36_7'] <- 'Christian_Lindner'
names(data)[names(data) == 'Q36_8'] <- 'Buendnis_90_Die_Gruenen'
names(data)[names(data) == 'Q36_9'] <- 'Die_Linke'
names(data)[names(data) == 'Q36_10'] <- 'Die_Partei'
names(data)[names(data) == 'Q36_11'] <- 'Evangelisch_de'
names(data)[names(data) == 'Q36_12'] <- 'FDP'
names(data)[names(data) == 'Q36_13'] <- 'Fridays_for_Future'
names(data)[names(data) == 'Q44_1'] <- 'Islamfakten'
names(data)[names(data) == 'Q44_2'] <- 'Jens_Spahn'
names(data)[names(data) == 'Q44_3'] <- 'katholisch_de'
names(data)[names(data) == 'Q44_5'] <- 'Louisa_Dellert'
names(data)[names(data) == 'Q44_6'] <- 'Luisa_Neubauer'
names(data)[names(data) == 'Q44_7'] <- 'Heiko_Maas'
names(data)[names(data) == 'Q44_8'] <- 'PETA_Deutschland'
names(data)[names(data) == 'Q44_9'] <- 'Querdenken711'
names(data)[names(data) == 'Q44_10'] <- 'Robert_Habeck'
names(data)[names(data) == 'Q44_11'] <- 'Sahra_Wagenknecht'
names(data)[names(data) == 'Q44_12'] <- 'SPD'
names(data)[names(data) == 'Q37_1'] <- 'adidas_Deutschland'
names(data)[names(data) == 'Q37_2'] <- 'Alica_Schmidt'
names(data)[names(data) == 'Q37_3'] <- 'Borussia_Dortmund'
names(data)[names(data) == 'Q37_4'] <- 'DFB'
names(data)[names(data) == 'Q37_5'] <- 'RB_Leipzig'
names(data)[names(data) == 'Q37_6'] <- 'FC_Bayern_Muenchen'
names(data)[names(data) == 'Q37_7'] <- 'Felix_Neureuther'
names(data)[names(data) == 'Q37_8'] <- 'Felix_Sturm'
names(data)[names(data) == 'Q37_9'] <- 'Gina_Lueckenkemper'
names(data)[names(data) == 'Q37_10'] <- 'Christoph_Icke_Dommisch'
names(data)[names(data) == 'Q37_11'] <- 'Inscope21'
names(data)[names(data) == 'Q37_12'] <- 'kicker'
names(data)[names(data) == 'Q37_13'] <- 'Lisa_Mueller'
names(data)[names(data) == 'Q45_1'] <- 'Mady_Morrison'
names(data)[names(data) == 'Q45_2'] <- 'Manuel_Neuer'
names(data)[names(data) == 'Q45_3'] <- 'Marco_Reus'
names(data)[names(data) == 'Q45_4'] <- 'McFit'
names(data)[names(data) == 'Q45_5'] <- 'Oceans_Apart'
names(data)[names(data) == 'Q45_6'] <- 'Pamela_Reif'
names(data)[names(data) == 'Q45_7'] <- 'Philipp_Lahm'
names(data)[names(data) == 'Q45_8'] <- 'Sophia_Thiel'
names(data)[names(data) == 'Q45_9'] <- 'FC_Schalke_04'
names(data)[names(data) == 'Q45_10'] <- 'Sky_Sport'
names(data)[names(data) == 'Q45_11'] <- 'Sport1'
names(data)[names(data) == 'Q45_12'] <- 'Uwe_Gensheimer'
names(data)[names(data) == 'Q38_1'] <- 'Canon_Deutschland'
names(data)[names(data) == 'Q38_2'] <- 'Create_By_Obi'
names(data)[names(data) == 'Q38_3'] <- 'Deutsche_Bahn'
names(data)[names(data) == 'Q38_4'] <- 'Easy_Alex'
names(data)[names(data) == 'Q38_5'] <- 'Flixbus'
names(data)[names(data) == 'Q38_6'] <- 'Ford_Deutschland'
names(data)[names(data) == 'Q38_7'] <- 'Germanroamers'
names(data)[names(data) == 'Q38_8'] <- 'Hannes_Becker'
names(data)[names(data) == 'Q38_9'] <- 'Linda_DIY'
names(data)[names(data) == 'Q38_10'] <- 'Martin_Ruetter'
names(data)[names(data) == 'Q38_11'] <- 'Mercedes_Benz_Deutschland'
names(data)[names(data) == 'Q38_12'] <- 'Tiere_suchen_ein_Zuhause'
names(data)[names(data) == 'Q38_13'] <- 'Urlaubsguru'
names(data)[names(data) == 'Q38_14'] <- 'Urlaubspiraten'
names(data)[names(data) == 'Q38_15'] <- 'Xlaeta'
names(data)[names(data) == 'Q38_16'] <- 'Yamaha_Motor_Deutschland'
names(data)[names(data) == 'Q38_17'] <- 'Yvonne_Pfeffer'
names(data)[names(data) == 'Q47_1'] <- 'Ariana_Grande'
names(data)[names(data) == 'Q47_2'] <- 'Beyonce'
names(data)[names(data) == 'Q47_3'] <- 'Cristiano_Ronaldo'
names(data)[names(data) == 'Q47_9'] <- 'Dwayne_Johnson'
names(data)[names(data) == 'Q47_4'] <- 'Justin_Bieber'
names(data)[names(data) == 'Q47_5'] <- 'Kim_Kardashian_West'
names(data)[names(data) == 'Q47_6'] <- 'Kylie_Jenner'
names(data)[names(data) == 'Q47_7'] <- 'Lionel_Messi'
names(data)[names(data) == 'Q47_12'] <- 'National_Geographic'
names(data)[names(data) == 'Q47_8'] <- 'Selena_Gomez'
names(data)[names(data) == 'Q8_1'] <- 'Extrovertiert_enthusiastisch'
names(data)[names(data) == 'Q8_2'] <- 'Kritisch_konfliktfreudig'
names(data)[names(data) == 'Q8_3'] <- 'Zuverlaessig_selbstdiszipliniert'
names(data)[names(data) == 'Q8_4'] <- 'Aengstlich_leicht_reizbar'
names(data)[names(data) == 'Q8_5'] <- 'Offen_fuer_neue_Erfahrungen_vielseitig'
names(data)[names(data) == 'Q8_6'] <- 'Kontrollfrage_Persoenlichkeit'
names(data)[names(data) == 'Q8_7'] <- 'Zurueckhaltend_ruhig'
names(data)[names(data) == 'Q8_8'] <- 'Sympathisch_warmherzig'
names(data)[names(data) == 'Q8_9'] <- 'Unorganisiert_nachlaessig'
names(data)[names(data) == 'Q8_10'] <- 'Ruhig_emotional_stabil'
names(data)[names(data) == 'Q8_11'] <- 'Konventionell_unkreativ'
names(data)[names(data) == 'Q9'] <- 'Alkohol_Konsum'
names(data)[names(data) == 'Q10'] <- 'Zigaretten_Konsum'
names(data)[names(data) == 'Q11'] <- 'Drogen_Konsum'
names(data)[names(data) == 'Q12_1'] <- 'Gefuehl_der_Zugehoerigkeit'
names(data)[names(data) == 'Q12_2'] <- 'Spannung'
names(data)[names(data) == 'Q12_3'] <- 'Kontrollfrage_Ziele_im_Leben'
names(data)[names(data) == 'Q12_4'] <- 'Herzliche_Beziehung_zu_anderen_Menschen'
names(data)[names(data) == 'Q12_5'] <- 'Selbstverwirklichung'
names(data)[names(data) == 'Q12_6'] <- 'Respekt_vor_Anderen'
names(data)[names(data) == 'Q12_7'] <- 'Spass_und_Freude_am_Leben'
names(data)[names(data) == 'Q12_8'] <- 'Sicherheit'
names(data)[names(data) == 'Q12_9'] <- 'Selbstachtung'
names(data)[names(data) == 'Q12_10'] <- 'Gefuehl_von_Erfolg'
names(data)[names(data) == 'Q13'] <- 'Wahl_Partei'
names(data)[names(data) == 'Q13_8_TEXT'] <- 'Wahl_Partei_Sonstiges'
names(data)[names(data) == 'Q14_1'] <- 'Corona_Massnahmen_uebertrieben'
names(data)[names(data) == 'Q14_2'] <- 'Corona_Massnahmen_muessten_haerter_sein'
names(data)[names(data) == 'Q14_3'] <- 'Corona_ist_harmlos_gleich_Grippe'
names(data)[names(data) == 'Q14_4'] <- 'Glaube_nicht_an_Corona'
names(data)[names(data) == 'Q15'] <- 'Nettoeinkommen'
names(data)[names(data) == 'Q16_1'] <- 'Verwendete_Produkte_Umwelt_nicht_belasten'
names(data)[names(data) == 'Q16_2'] <- 'Auswirkungen_meiner_Handlungen_auf_Umwelt'
names(data)[names(data) == 'Q16_3'] <- 'Kaufgewohnheiten_Sorge_um_Umwelt'
names(data)[names(data) == 'Q16_4'] <- 'Verschwendung_Ressourcen'
names(data)[names(data) == 'Q16_5'] <- 'Kontrollfrage_Umwelt'
names(data)[names(data) == 'Q16_6'] <- 'Umweltverantwortlich'
names(data)[names(data) == 'Q16_7'] <- 'Unannehmlichkeiten_fuer_Umwelt'
names(data)[names(data) == 'Q17'] <- 'Beschaeftigung'
names(data)[names(data) == 'Q18'] <- 'Bildungsabschluss'
names(data)[names(data) == 'Q19'] <- 'Religion'
names(data)[names(data) == 'Q19_5_TEXT'] <- 'Religion_Sonstiges'
names(data)[names(data) == 'Q20'] <- 'Migrationshintergrund'
names(data)[names(data) == 'Q21'] <- 'Woher_Vorfahren'
names(data)[names(data) == 'Q22'] <- 'Sexuelle_Orientierung'
names(data)[names(data) == 'Q22_4_TEXT'] <- 'Sexuelle_Orientierung_Sonstiges'
names(data)[names(data) == 'Q23'] <- 'Beziehungsstatus'
names(data)[names(data) == 'Q24'] <- 'Kinder'
names(data)[names(data) == 'Q26'] <- 'Anzahl_Kinder'
names(data)[names(data) == 'Q40'] <- 'Instagram_Name'

num_full <- data


#select relevant columns

cols_names <- names(num_full)  
cols_text <- cols_names[c(9, 22, 23, 264:266, 277, 278, 283, 291, 292, 293, 294, 295, 296, 297, 298, 299, 302, 303)]
cols_num <- cols_names[c(3, 6, 7, 19, 20, 21, 24:252, 253:263, 267:276, 279:282, 284:290, 300, 301)]

#combine relevant colums into final dataset
data <- cbind(text_full[, cols_text], num_full[, cols_num])

#make sure all columns have the right specifications: as of now, everything is character
data[c(cols_num)] <- sapply(data[c(cols_num)], as.numeric)

str(data) #all ok

#change NAs to 0 when applicable: for Accounts, no of children
change_to_zero <- cols_names[c(24:252, 301)]
data[ ,change_to_zero][is.na(data[ ,change_to_zero])] <- 0


#clear respondents who did not finish survey
data <- subset(data, Kinder != "NA")

#make sure that control questions were answered correctly

data <- data[(data$Kontrollfrage_Persoenlichkeit == 6),] 
data <- data[(data$Kontrollfrage_Ziele_im_Leben == 7),] 
data <- data[(data$Kontrollfrage_Umwelt == 6),] 

#check that all bad respondents were eliminated
table(data$Finished) #20 respondents still are not marked as finished; however those are respondents who answered all questions but then did not press "continue" to end the questionnaire; we can still include them in the analysis
table(data$Kontrollfrage_Persoenlichkeit)
table(data$Kontrollfrage_Ziele_im_Leben)
table(data$Kontrollfrage_Umwelt) #all okay

#check how long the respondents needed: are there speeders left?
table(data$`Duration (in seconds)`) #exclude all who finished too fast; what is too fast?
summary(data$`Duration (in seconds)`)

duration_smaller_300 <- data %>% subset(data$`Duration (in seconds)` < 300)
duration_smaller_180 <- data %>% subset(data$`Duration (in seconds)` < 180) 



#####
#add or redefine variables: scales, re-coding, new info

#recode "keine Angaben" as NA
data <- data %>% replace_with_na_all(condition = ~.x == "Keine Angabe")

#####
#scales
#####
#TIPI Personality

#transform reserve-coded items: Q8_2, Q8_4, Q8_7, Q8_9, Q8_11
'Kritisch/konfliktfreudig_nichtR' <- transmute(data, Kritisch_konfliktfreudig = 8 - (Kritisch_konfliktfreudig))
'Aengstlich/leicht reizbar_nichtR' <- transmute(data, Aengstlich_leicht_reizbar = 8 - (Aengstlich_leicht_reizbar))
'Zurueckhaltend/ruhig_nichtR' <- transmute(data, Zurueckhaltend_ruhig = 8 - (Zurueckhaltend_ruhig))
'Unorganisiert/nachlaessig_nichtR' <- transmute(data, Unorganisiert_nachlaessig = 8 - (Unorganisiert_nachlaessig))
'Konventionell/unkreativ_nichtR' <- transmute(data, Konventionell_unkreativ = 8 - (Konventionell_unkreativ))

nichtR <- bind_cols(`Kritisch/konfliktfreudig_nichtR`, `Aengstlich/leicht reizbar_nichtR`, `Zurueckhaltend/ruhig_nichtR`, `Unorganisiert/nachlaessig_nichtR`, `Konventionell/unkreativ_nichtR`)
colnames(nichtR) <- c("Kritisch/konfliktfreudig_nichtR", "Aengstlich/leicht reizbar_nichtR", "Zurueckhaltend/ruhig_nichtR", "Unorganisiert/nachlaessig_nichtR", "Konventionell/unkreativ_nichtR")
data <- bind_cols(data, nichtR)

#extraversion: Q8_1 and Q8_7
data <- data %>%
  rowwise() %>%
  mutate(Extraversion = (`Extrovertiert_enthusiastisch` + `Zurueckhaltend/ruhig_nichtR`)/2)


#Agreeableness: 2R + 8
data <- data %>%
  rowwise() %>%
  mutate(Agreeableness = (`Kritisch/konfliktfreudig_nichtR` + `Sympathisch_warmherzig`)/2)


#Conscientiousness: 3 + 9R
data <- data %>%
  rowwise() %>%
  mutate(Conscientiousness = (`Zuverlaessig_selbstdiszipliniert` + `Unorganisiert/nachlaessig_nichtR`)/2)


#Emotional stability: 4R + 10
data <- data %>%
  rowwise() %>%
  mutate(Emotional_stablity = (`Aengstlich/leicht reizbar_nichtR` + `Ruhig_emotional_stabil`)/2)


#Openness to Experiences: 5R + 11
data <- data %>%
  rowwise() %>%
  mutate(Openness_to_Experiences = (`Offen_fuer_neue_Erfahrungen_vielseitig` + `Konventionell/unkreativ_nichtR`)/2)

#Version 2: Persönlichkeit mit je nur 2 Ausgängen coden: <4 oder >4; 4 = NA
data <- data %>% mutate(Extraversion2 = case_when(Extraversion < 4 ~ 'Introvertiert',
                                                  Extraversion > 4 ~ 'Extrovertiert'))
data <- data %>% mutate(Agreeableness2 = case_when(Agreeableness < 4 ~ 'Not_Agreeable',
                                                   Agreeableness > 4 ~ 'Agreeable'))
data <- data %>% mutate(Conscientiousness2 = case_when(Conscientiousness < 4 ~ 'Not_Conscientious',
                                                       Conscientiousness > 4 ~ 'Conscientious'))
data <- data %>% mutate(Emotional_stablity2 = case_when(Emotional_stablity < 4 ~ 'Unstable',
                                                        Emotional_stablity > 4 ~ 'Stable'))
data <- data %>% mutate(Openness_Experiences2 = case_when(Openness_to_Experiences < 4 ~ 'Closed',
                                                          Openness_to_Experiences > 4 ~ 'Open'))



#####
#Green Values
data <- data %>%
  rowwise() %>%
  mutate(Green_Values = (Verwendete_Produkte_Umwelt_nicht_belasten + Auswirkungen_meiner_Handlungen_auf_Umwelt + Kaufgewohnheiten_Sorge_um_Umwelt + Verschwendung_Ressourcen + Umweltverantwortlich + Unannehmlichkeiten_fuer_Umwelt)/6)


#Version 2: with only two classes: <4 or >4; 4 = NA
data <- data %>% mutate(Green2 = case_when(Green_Values < 4 ~ 'Nein',
                                           Green_Values > 4 ~ 'Ja'))


#####
#Corona
#checking: how to define?
table(data$Corona_Massnahmen_uebertrieben)
table(data$Corona_Massnahmen_muessten_haerter_sein)
table(data$Corona_ist_harmlos_gleich_Grippe)
table(data$Glaube_nicht_an_Corona)

#define new variable Corona_Attitude: Accept vs. Reject
data <- data %>% mutate(Corona_Hardliner = ifelse((`Corona_Massnahmen_muessten_haerter_sein` == 5 |`Corona_Massnahmen_muessten_haerter_sein` == 6 | `Corona_Massnahmen_muessten_haerter_sein` == 7), yes = "Ja", no = "Nein"))
data <- data %>% mutate(Corona_Softliner = ifelse((`Corona_Massnahmen_uebertrieben` == 5 |`Corona_Massnahmen_uebertrieben` == 6 | `Corona_Massnahmen_uebertrieben` == 7), yes = "Ja", no = "Nein"))
data <- data %>% mutate(Corona_Skeptiker = ifelse((`Corona_ist_harmlos_gleich_Grippe` == 5 | `Corona_ist_harmlos_gleich_Grippe` == 6 | `Corona_ist_harmlos_gleich_Grippe` == 7), yes = "Ja", no = "Nein"))
data <- data %>% mutate(Corona_Leugner = ifelse((`Glaube_nicht_an_Corona` == 5 | `Glaube_nicht_an_Corona` == 6 | `Glaube_nicht_an_Corona` == 7), yes = "Ja", no = "Nein"))

#überprüfen ob erfolgreich: 
table(data$Corona_Massnahmen_muessten_haerter_sein, data$Corona_Hardliner)
table(data$Corona_Massnahmen_uebertrieben, data$Corona_Softliner)
table(data$Corona_ist_harmlos_gleich_Grippe, data$Corona_Skeptiker)
table(data$Glaube_nicht_an_Corona, data$Corona_Leugner)

#####
#recoding for analysis

#Location within Germany: new vs old federal states
data <- data %>% mutate(Ost_West = case_when(PLZ <= 19 ~ 'Osten',
                                             PLZ >= 20 & PLZ <= 38 ~ 'Westen',
                                             PLZ == 39 ~ 'Osten',
                                             PLZ >= 40 & PLZ <= 97 ~ 'Westen',
                                             PLZ >= 98 ~ 'Osten'))
#Age Group: Yound, medium, old
data <- data %>% mutate(Age_Range = case_when(Alter >= 45 ~ 'hohes.Alter',
                                              Alter >= 30  & Alter <= 44 ~ 'mittleres.Alter',
                                              Alter <= 29 ~ 'niedriges.Alter'))

#Gender: male and female only
data <- data %>% mutate(weiblich_maennlich = case_when(Geschlecht == 1 ~ 'weiblich',
                                                       Geschlecht == 2 ~ 'männlich'))


#Education: low, medium, high
data <- data %>% mutate(Bildungsgruppe = case_when(Bildungsabschluss == "(Noch) kein Abschluss" ~ 'niedrig',
                                                   Bildungsabschluss == "Hauptschulabschluss" ~ 'niedrig',
                                                   Bildungsabschluss == "Realschulabschluss" ~ 'niedrig',
                                                   Bildungsabschluss == "Abitur" ~ 'mittel',
                                                   Bildungsabschluss == "Hochschulabschluss (Bachelor oder Master)" ~ 'hoch',
                                                   Bildungsabschluss == "Promotion" ~ 'hoch'))

#imcome: low, medium, high
data <- data %>% mutate(Einkommensgruppe = case_when(Nettoeinkommen == "0 € - 1000 €" ~ 'niedrig',
                                                     Nettoeinkommen == "1001 € - 2000€" ~ 'niedrig',
                                                     Nettoeinkommen == "2001 € - 3000 €" ~ 'mittel',
                                                     Nettoeinkommen == "3001 € - 4000 €" ~ 'mittel',
                                                     Nettoeinkommen == "4001 € - 5000 €" ~ 'hoch',
                                                     Nettoeinkommen == "Mehr als 5000 €" ~ 'hoch'))

#relationship status: single or in relationship
data <- data %>% mutate(Allein_vs_Beziehung = case_when(Beziehungsstatus == "Single" ~ 'Allein',
                                                        Beziehungsstatus == "Geschieden" ~ 'Allein',
                                                        Beziehungsstatus == "Verwitwet" ~ 'Allein',
                                                        Beziehungsstatus == "In einer Beziehung" ~ 'Beziehung',
                                                        Beziehungsstatus == "Verheiratet" ~ 'Beziehung'))

#occupation: working or not
data <- data %>% mutate(Arbeitend_oder_nicht = case_when(Beschaeftigung == "Arbeitslos/-suchend" ~ 'Nein',
                                                         Beschaeftigung == "Auszubildende/r" ~ 'Ja',
                                                         Beschaeftigung == "Berufstätige/r" ~ 'Ja',
                                                         Beschaeftigung == "Hausfrau/-mann" ~ 'Nein',
                                                         Beschaeftigung == "Rentner/in" ~ 'Nein',
                                                         Beschaeftigung == "Schüler/in" ~ 'Nein',
                                                         Beschaeftigung == "Student/in" ~ 'Nein'))


#Alcohol consumption: 3 (frequency) and 2 (yes/no) classes
data <- data %>% mutate(Alkoholgruppe = case_when(`Alkohol_Konsum` == "Nein" ~ 'kein_Konsum',
                                                  `Alkohol_Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedrig',
                                                  `Alkohol_Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedrig',
                                                  `Alkohol_Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoch',
                                                  `Alkohol_Konsum` == "Ja, mehrmals pro Woche" ~ 'hoch',
                                                  `Alkohol_Konsum` == "Ja, täglich" ~ 'hoch'))

data <- data %>% mutate(Alkohol_ja_nein = ifelse(Alkohol_Konsum == "Nein", "Nein", "Ja"))

#Cigarette consumption: 3 (frequency) and 2 (yes/no) classes
data <- data %>% mutate(Zigarettengruppe = case_when(`Zigaretten_Konsum` == "Nein" ~ 'kein_Konsum',
                                                     `Zigaretten_Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedrig',
                                                     `Zigaretten_Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedrig',
                                                     `Zigaretten_Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoch',
                                                     `Zigaretten_Konsum` == "Ja, mehrmals pro Woche" ~ 'hoch',
                                                     `Zigaretten_Konsum` == "Ja, täglich" ~ 'hoch'))

data <- data %>% mutate(Zigaretten_ja_nein = ifelse(Zigaretten_Konsum == "Nein", "Nein", "Ja"))


#Drug consumption: 3 (frequency) and 2 (yes/no) classes
data <- data %>% mutate(Drogengruppe = case_when(`Drogen_Konsum` == "Nein" ~ 'kein_Konsum',
                                                 `Drogen_Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedrig',
                                                 `Drogen_Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedrig',
                                                 `Drogen_Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoch',
                                                 `Drogen_Konsum` == "Ja, mehrmals pro Woche" ~ 'hoch',
                                                 `Drogen_Konsum` == "Ja, täglich" ~ 'hoch'))

data <- data %>% mutate(Drogen_ja_nein = ifelse(Drogen_Konsum == "Nein", "Nein", "Ja"))


#Values in Life: is the value important or not?
data <- data %>% mutate(Zugehoerigkeit_wichtig = case_when(Gefuehl_der_Zugehoerigkeit <=4 ~ 'Nein',
                                                           Gefuehl_der_Zugehoerigkeit >4 ~ 'Ja'))
data <- data %>% mutate(Spannung_wichtig = case_when(Spannung <=4 ~ 'Nein',
                                                     Spannung >4 ~ 'Ja'))
data <- data %>% mutate(Herzliche_Beziehung_wichtig = case_when(Herzliche_Beziehung_zu_anderen_Menschen <=4 ~ 'Nein',
                                                                Herzliche_Beziehung_zu_anderen_Menschen >4 ~ 'Ja'))
data <- data %>% mutate(Selbstverwirklichung_wichtig = case_when(Selbstverwirklichung <=4 ~ 'Nein',
                                                                 Selbstverwirklichung >4 ~ 'Ja'))
data <- data %>% mutate(Respekt_wichtig = case_when(Respekt_vor_Anderen <=4 ~ 'Nein',
                                                    Respekt_vor_Anderen >4 ~ 'Ja'))
data <- data %>% mutate(Spass_Freude_wichtig = case_when(Spass_und_Freude_am_Leben <=4 ~ 'Nein',
                                                         Spass_und_Freude_am_Leben >4 ~ 'Ja'))
data <- data %>% mutate(Sicherheit_wichtig = case_when(Sicherheit <=4 ~ 'Nein',
                                                       Sicherheit >4 ~ 'Ja'))
data <- data %>% mutate(Selbstachtung_wichtig = case_when(Selbstachtung <=4 ~ 'Nein',
                                                          Selbstachtung >4 ~ 'Ja'))
data <- data %>% mutate(Erfolg_wichtig = case_when(Gefuehl_von_Erfolg <=4 ~ 'Nein',
                                                   Gefuehl_von_Erfolg >4 ~ 'Ja'))

#Religion: Religious yes/no and Christ vs Islam
data <- data %>% mutate(Religioes = case_when(Religion == "Christentum" ~ 'Ja',
                                              Religion == "Islam" ~ 'Ja',
                                              Religion == "Judentum" ~ 'Ja',
                                              Religion == "Ich fühle mich keiner Religion zugehörig" ~ 'Nein'))

data <- data %>% mutate(Islam_oder_Christ = case_when(Religion == "Christentum" ~ 'Christentum',
                                                      Religion == "Islam" ~ 'Islam'))

#sexual orientation: Heterosexual yes or no?
data <- data %>% mutate(Heterosexuell = case_when(Sexuelle_Orientierung == "Heterosexuell" ~ 'Ja',
                                                  Sexuelle_Orientierung == "Homosexuell" ~ 'Nein',
                                                  Sexuelle_Orientierung == "Bisexuell" ~ 'Nein'))

#Number of children
#we know from one of our friends that they typed 20 instead of 0 --> correct for that so we don't accidentally clean the observation:
data$`Anzahl_Kinder`[data$`Anzahl_Kinder` == 20] <- 0

#summarize more: 0, 1, 2 or 3+ children
data <- data %>% mutate(Anzahl_Kinder_grob = case_when(Anzahl_Kinder == 0 ~ "0",
                                                       Anzahl_Kinder == 1 ~ "1",
                                                       Anzahl_Kinder == 2 ~ "2",
                                                       Anzahl_Kinder >2 ~ "3_oder_mehr"))

#Voter for party: code to be binary, so that e.g. Vote for CDU --> yes/no
data <- data %>% mutate(CDU_CSU_Waehler = ifelse(Wahl_Partei == "CDU/CSU", "Ja", "Nein"))
data <- data %>% mutate(SPD_Waehler = ifelse(Wahl_Partei == "SPD", "Ja", "Nein"))
data <- data %>% mutate(Gruene_Waehler = ifelse(Wahl_Partei == "Bündnis 90/Die Grünen", "Ja", "Nein"))
data <- data %>% mutate(FDP_Waehler = ifelse(Wahl_Partei == "FDP", "Ja", "Nein"))
data <- data %>% mutate(AfD_Waehler = ifelse(Wahl_Partei == "AfD", "Ja", "Nein"))
data <- data %>% mutate(Linke_Waehler = ifelse(Wahl_Partei == "Die Linke", "Ja", "Nein"))
data <- data %>% mutate(Nichtwahler = ifelse(Wahl_Partei == "Ich würde nicht wählen gehen", "Ja", "Nein"))

#income: above or below average income?
data <- data %>% mutate(Durchschnittseinkommen = case_when(Nettoeinkommen == "0 € - 1000 €" ~ 'weniger2000',
                                                           Nettoeinkommen == "1001 € - 2000€" ~ 'weniger2000',
                                                           Nettoeinkommen == "2001 € - 3000 €" ~ 'mehr2000',
                                                           Nettoeinkommen == "3001 € - 4000 €" ~ 'mehr2000',
                                                           Nettoeinkommen == "4001 € - 5000 €" ~ 'mehr2000',
                                                           Nettoeinkommen == "Mehr als 5000 €" ~ 'mehr2000'))


#####
#new info

#how many accounts does each respondent follow?
Accounts <- data[,c(27:255)]
Account_names <- names(Accounts)

data[c(Account_names)] <- sapply(data[c(Account_names)], as.numeric)
data$Accounts_followed <- rowSums(data[ ,c(Account_names)], na.rm = TRUE)

summary(data$Accounts_followed)
table(data$Accounts_followed)

#quick visualization to check for outliers

ggplot(data, aes(Accounts_followed))+
  geom_histogram(bins = 229)





#####
#check descriptives and get to know the data and respondents

#Descriptives of the Respondents

#Instagram usage: how often and how many accounts
#Frequency
table(data$`Instagram_Nutzungshaeufigkeit`) #exclude people with "Once Per Week" usage?
usage <- as.data.frame(table(data$`Instagram_Nutzungshaeufigkeit`))
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
Nutzungshaeufigkeit <- data$`Instagram_Nutzungshaeufigkeit`
ggplot(data, aes(x = Accounts_followed, y = factor(Nutzungshaeufigkeit, levels = usage_order)))+
  geom_boxplot()

#is there a significant difference between usage groups and how many accounts are followed?
anova_usage <- aov(Accounts_followed ~ `Instagram_Nutzungshaeufigkeit`, data = data)
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
Ordered_ranges <- c('niedriges.Alter', 'mittleres.Alter', 'hohes.Alter')

ggplot(data, aes(x = factor(Age_Range, levels = Ordered_ranges)))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "Age Ranges", y = "Count", title = "Abs. Count per Age Range")+
  ylim(0,1200)



#Location: Münster region dominates
table(data$PLZ)
ggplot(data, aes(x = PLZ))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "PLZ", y = "Count", title = "Abs. Count per PLZ")+
  ylim(0,200)

#East/West comparison
table(data$Ost_West) #fits with the German distribution

#relationship status
table(data$Beziehungsstatus)
#sumarized into binary:
table(data$Allein_vs_Beziehung)


#sexual orientation: 
table(data$`Sexuelle_Orientierung`)
#"Others" is no longer relevant for us (nur n = 6)

#Children
table(data$`Anzahl_Kinder`) #most people without children
###necessary to clean 3 respondents with >10 children?
many_children <- data %>% subset(data$`Anzahl_Kinder` > 10) #visual inspection: observations otherwise make sense, thus keep in sample!

table(data$`Anzahl_Kinder`) #Alternative 1
table(data$Anzahl_Kinder_grob) #Alternative 2


#Education
table(data$Bildungsabschluss)
#summarized to 3 groups:
table(data$Bildungsgruppe)



#Occupation
table(data$Beschaeftigung)
#summarized to binary:
table(data$Arbeitend_oder_nicht)

#Migration background
table(data$Migrationshintergrund)
round(table(data$Migrationshintergrund)/sum(table(data$Migrationshintergrund)), 2) #17% with migration background

#where from?
table(data$`Woher_Vorfahren`) #mainly Europe and Asia --> probably many Europeans and Turkish people


#Religion
table(data$Religion)

#consider "others"?
table(data$`Religion_Sonstiges`) #no other religion is named often (max. 3 times)

#Religiosity
table(data$Religioes) #2/3 consider themselves religious
#Islam vs. Christianity
table(data$Islam_oder_Christ) #far more Christians

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

#Variablen: Extraversion2, Agreeableness2 etc. split into two groups:
table(data$Extraversion2)/sum(table(data$Extraversion2)) #51% Introvertiert
table(data$Agreeableness2)/sum(table(data$Agreeableness2)) #83% Agreeable
table(data$Conscientiousness2)/sum(table(data$Conscientiousness2)) #92% Conscentious
table(data$Emotional_stablity2)/sum(table(data$Emotional_stablity2)) #73% Emotionally Stable
table(data$Openness_Experiences2)/sum(table(data$Openness_Experiences2)) #89% open to new experiences


#Green Values
table(data$Green_Values)
summary(data$Green_Values)

ggplot(data, aes(Green_Values))+
  geom_density()
ggplot(data, aes(Green_Values))+
  geom_histogram(binwidth = 0.33)

##most respondents do care about the environment (rating between 5 and 6), but also some people below and 51 people with highest score of 7

#summarized Variable: Green Values yes or no
table(data$Green2)/sum(table(data$Green2)) #84% with (rather) green values


#General Goals in Life
table(data$Gefuehl_der_Zugehoerigkeit)
table(data$Spannung)
table(data$Herzliche_Beziehung_zu_anderen_Menschen)
table(data$Selbstverwirklichung)
table(data$Respekt_vor_Anderen)
table(data$Spass_und_Freude_am_Leben)
table(data$Sicherheit)
table(data$Selbstachtung)
table(data$Gefuehl_von_Erfolg)
#important for most people: Spaß, Herzliche Beziehungen
#no controverse goals, usually majority >5

#Summarized variables regarding general values (binary):
table(data$Zugehoerigkeit_wichtig) 
table(data$Spannung_wichtig) 
table(data$Herzliche_Beziehung_wichtig) 
table(data$Selbstverwirklichung_wichtig) 
table(data$Respekt_wichtig) 
table(data$Spass_Freude_wichtig) 
table(data$Sicherheit_wichtig) 
table(data$Selbstachtung_wichtig) 
table(data$Erfolg_wichtig) 

#Parties
round(table(data$`Wahl_Partei`)/sum(table(data$`Wahl_Partei`)), 2) #relative shares of voters in our dataset
#differs a bit from actual German voting data/ forecasts; mainly more Grüne and less CDU and SPD

#What to do with "Others"?
table(data$`Wahl_Partei_Sonstiges`)
#based on "Others": inclusion of "Die Partei"
data$`Wahl_Partei_Sonstiges` <- tolower(data$`Wahl_Partei_Sonstiges`)
data$`Wahl_Partei` <- ifelse(data$`Wahl_Partei_Sonstiges` %in% "die partei", "Die Partei", data$`Wahl_Partei`)

#create variable for "Die Partei" Voters
data <- data %>% mutate(Die_Partei_Waehler = ifelse(Wahl_Partei == "Die Partei", "Ja", "Nein"))


Partei <- as.data.frame(table(data$`Wahl_Partei`)/sum(table(data$`Wahl_Partei`)))

Partei_Order <- c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "AfD", "Die Linke", "FDP", "Die Partei", "Sonstige:", "Ich würde nicht wählen gehen")
ggplot(Partei, aes(factor(Var1, levels = Partei_Order), Freq))+
  geom_col()+
  geom_text(aes(label = percent(Freq)), vjust = -1)+
  labs(x = "Parties", y = "", title = "Voters per Party")+
  ylim(0,0.3)



#Corona: create 4 groups: Hardliner, Softliner, Skeptiker, Leugner
table(data$Corona_Hardliner) #1166 Hardliner: want harder regulations
table(data$Corona_Softliner) #537 Softliner: want softer regulations
table(data$Corona_Skeptiker) #299 Skeptiker: do not believe the virus to be harmful
table(data$Corona_Leugner) #124 Leugner: deny the virus



#Alcohol, Cigarettes, Drugs
table(data$Alkohol_Konsum)
table(data$Zigaretten_Konsum)
table(data$Drogen_Konsum)
#summarized: V1 with 3 classes
table(data$Alkoholgruppe)
table(data$Zigarettengruppe)
table(data$Drogengruppe)
#summarized: V2 with 2 classes
table(data$Alkohol_ja_nein)
table(data$Zigaretten_ja_nein)
table(data$Drogen_ja_nein)



#Correlations between Accounts?
cor_accounts_df <- as.data.frame(cor(Accounts))
ggplot(cor_accounts_df, aes(Tagesschau))+
  geom_density()


#####
#Descriptives Accounts

#Followers per Account
Accounts <- data[,c(27:255)]
Followers_Accounts <- as.data.frame(colSums(Accounts, na.rm = TRUE))
colnames(Followers_Accounts) <- "Followers"
summary(Followers_Accounts)

#final check:
summary(data)
str(data)
head(data)


#save dataset
write.csv(data, "/Users/Miriam/Documents/Uni/Master/3. Semester/Seminar SRA/datasets/neu/data_for_analysis.csv")
save(data, file = 'data_for_analysis.RData')



#handle outliers in the dataset: cut by upper and lower 1%

duration_zu_groß <- quantile(data$`Duration (in seconds)`, 0.99)
duration_zu_klein <- quantile(data$`Duration (in seconds)`, 0.01)

Accounts_zu_viele <- quantile(data$Accounts_followed, 0.99)
Accounts_zu_wenige <- quantile(data$Accounts_followed, 0.01)



######
#define different datasets for analysis: with and without outliers

full_set <- data %>% subset(data$Accounts_followed > 0)
reduced_set <- data %>% subset(data$`Duration (in seconds)` > duration_zu_klein & data$`Duration (in seconds)` < duration_zu_groß & data$Accounts_followed > Accounts_zu_wenige & data$Accounts_followed < Accounts_zu_viele)


#save different datasets as CSV and R Document
#full set
write.csv(full_set, "/Users/Miriam/Documents/Uni/Master/3. Semester/Seminar SRA/datasets/neu/data_full.csv")
save(full_set, file = 'data_full.RData')

#reduced set
write.csv(reduced_set, "/Users/Miriam/Documents/Uni/Master/3. Semester/Seminar SRA/datasets/neu/data_reduced.csv")
save(reduced_set, file = 'data_reduced.RData')


###check descriptives again after cleaning: has something changed? 

#Accounts
#Followers per Account
Accounts2 <- reduced_set[,c(27:255)]
Followers_Accounts2 <- as.data.frame(colSums(Accounts2, na.rm = TRUE))
colnames(Followers_Accounts2) <- "Followers"
Followers_Accounts2 <- Followers_Accounts2 %>% arrange(-Followers)

#Are there accounts which have too little followers? 
#list of less than "a" followers:
a <- 50
delete <- Followers_Accounts2 %>% filter(Followers < a)
keep <-Followers_Accounts2 %>% filter(Followers > a) 
#but: keep in sample nonetheless, just be aware!

mean(Followers_Accounts2$Followers)
median(Followers_Accounts2$Followers)


#how many accounts do respondents follow?
mean(reduced_set$Accounts_followed)
median(reduced_set$Accounts_followed)


#Descriptives of the Respondents

#Instagram usage: how often and how many accounts
#Frequency
table(reduced_set$`Instagram_Nutzungshaeufigkeit`) #exclude people with "Once Per Week" usage?
usage <- as.data.frame(table(reduced_set$`Instagram_Nutzungshaeufigkeit`))
usage_order <- c("Täglich", "4- bis 6-mal pro Woche", "2- bis 3-mal pro Woche", "Einmal pro Woche")

ggplot(usage, aes(factor(Var1, levels = usage_order), Freq))+
  geom_col()+
  geom_text(aes(label = Freq), vjust = -1)+
  labs(x = "Usage", y = "", title = "Count")+
  ylim(0,1700)

#how many accounts?
table(reduced_set$Accounts_followed) #better now!
ggplot(reduced_set, aes(Accounts_followed))+
  geom_histogram(binwidth = 1)


#gender #1=female, 2=male, 3=diverse

table(reduced_set$Geschlecht) 
round(table(reduced_set$Geschlecht)/sum(table(reduced_set$Geschlecht)),2) #relative spread: 62% female, 38% male, diverse almost 0; marginal change from values before cleaning

#age

summary(reduced_set$Alter)
ggplot(reduced_set, aes(x = Alter))+
  geom_density()
ggplot(reduced_set, aes(x = Alter))+
  geom_histogram()

#Age range

table(reduced_set$Age_Range)
Ordered_ranges <- c('niedriges.Alter', 'mittleres.Alter', 'hohes.Alter')

ggplot(reduced_set, aes(x = factor(Age_Range, levels = Ordered_ranges)))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "Age Ranges", y = "Count", title = "Abs. Count per Age Range")+
  ylim(0,1200)



#Location: Münster region still dominates
table(reduced_set$PLZ)
ggplot(reduced_set, aes(x = PLZ))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "PLZ", y = "Count", title = "Abs. Count per PLZ")+
  ylim(0,200)

#East-West-split
table(reduced_set$Ost_West)

#Relationship status
table(reduced_set$Beziehungsstatus)
#summarized:
table(reduced_set$Allein_vs_Beziehung)


#sexual orientation
table(reduced_set$`Sexuelle_Orientierung`)
table(reduced_set$`Sexuelle_Orientierung`)/sum(table(reduced_set$`Sexuelle_Orientierung`))

#Children
table(reduced_set$Kinder)
table(reduced_set$Kinder)/sum(table(reduced_set$Kinder))


#Education
table(reduced_set$Bildungsabschluss)
#summarized:
table(reduced_set$Bildungsgruppe)



#Occupation
table(reduced_set$Beschaeftigung)
table(reduced_set$Beschaeftigung)/sum(table(reduced_set$Beschaeftigung))
#summarized:
table(reduced_set$Arbeitend_oder_nicht)

#Migration background
table(reduced_set$Migrationshintergrund)
round(table(reduced_set$Migrationshintergrund)/sum(table(reduced_set$Migrationshintergrund)), 2) #17% with migration background

#where from
table(reduced_set$`Woher_Vorfahren`) #still similar


#Religion
table(reduced_set$Religion)

#Religiosity
table(reduced_set$Religioes) 
#Islam vs. Christianity
table(reduced_set$Islam_oder_Christ) 

#Personality
#Extraversion
summary(reduced_set$Extraversion)

ggplot(reduced_set, aes(Extraversion))+
  geom_density()
ggplot(reduced_set, aes(Extraversion))+
  geom_histogram(binwidth = 0.5)

#Agreeableness
summary(reduced_set$Agreeableness)

ggplot(reduced_set, aes(Agreeableness))+
  geom_density()
ggplot(reduced_set, aes(Agreeableness))+
  geom_histogram(binwidth = 0.5)

#Conscientiousness
summary(reduced_set$Conscientiousness)

ggplot(reduced_set, aes(Conscientiousness))+
  geom_density()
ggplot(reduced_set, aes(Conscientiousness))+
  geom_histogram(binwidth = 0.5)

#Emotional stability
summary(reduced_set$Emotional_stablity)

ggplot(reduced_set, aes(Emotional_stablity))+
  geom_density()
ggplot(reduced_set, aes(Emotional_stablity))+
  geom_histogram(binwidth = 0.5)

#Openness to Experiences
summary(reduced_set$Openness_to_Experiences)

ggplot(reduced_set, aes(Openness_to_Experiences))+
  geom_density()
ggplot(reduced_set, aes(Openness_to_Experiences))+
  geom_histogram(binwidth = 0.5)

#Variables: Extraversion2, Agreeableness2 etc (binary-codet):
table(reduced_set$Extraversion2)/sum(table(reduced_set$Extraversion2)) #51% Introverted
table(reduced_set$Agreeableness2)/sum(table(reduced_set$Agreeableness2)) #83% Agreeable
table(reduced_set$Conscientiousness2)/sum(table(reduced_set$Conscientiousness2)) #92% Conscentious
table(reduced_set$Emotional_stablity2)/sum(table(reduced_set$Emotional_stablity2)) #73% Emotionally Stable
table(reduced_set$Openness_Experiences2)/sum(table(reduced_set$Openness_Experiences2)) #90% open to new experiences


#Green Values
table(reduced_set$Green_Values)
summary(reduced_set$Green_Values)

ggplot(reduced_set, aes(Green_Values))+
  geom_density()
ggplot(reduced_set, aes(Green_Values))+
  geom_histogram(binwidth = 0.33)

#summarized (binary): Green Values yes or no
table(reduced_set$Green2)/sum(table(reduced_set$Green2)) #84% as before


#General Values in Life
table(reduced_set$Gefuehl_der_Zugehoerigkeit)
table(reduced_set$Spannung)
table(reduced_set$Herzliche_Beziehung_zu_anderen_Menschen)
table(reduced_set$Selbstverwirklichung)
table(reduced_set$Respekt_vor_Anderen)
table(reduced_set$Spass_und_Freude_am_Leben)
table(reduced_set$Sicherheit)
table(reduced_set$Selbstachtung)
table(reduced_set$Gefuehl_von_Erfolg)
#important for most people: Spaß, Herzliche Beziehungen
#no controverse goals, usually majority >5

#General Goals in Life
summary(reduced_set$Gefuehl_der_Zugehoerigkeit)
summary(reduced_set$Spannung)
summary(reduced_set$Herzliche_Beziehung_zu_anderen_Menschen)
summary(reduced_set$Selbstverwirklichung)
summary(reduced_set$Respekt_vor_Anderen)
summary(reduced_set$Spass_und_Freude_am_Leben)
summary(reduced_set$Sicherheit)
summary(reduced_set$Selbstachtung)
summary(reduced_set$Gefuehl_von_Erfolg)
#difficult to analyze, as low deviations


#binary coding for General Values
table(reduced_set$Zugehoerigkeit_wichtig) 
table(reduced_set$Spannung_wichtig) 
table(reduced_set$Herzliche_Beziehung_wichtig) 
table(reduced_set$Selbstverwirklichung_wichtig) 
table(reduced_set$Respekt_wichtig) 
table(reduced_set$Spass_Freude_wichtig) 
table(reduced_set$Sicherheit_wichtig) 
table(reduced_set$Selbstachtung_wichtig) 
table(reduced_set$Erfolg_wichtig) 
#still problematic, also all highly imbalanced

#Parties
round(table(reduced_set$`Wahl_Partei`)/sum(table(reduced_set$`Wahl_Partei`)), 2) #relative shares of voters in our dataset
#almost the same as before cleaning

Partei <- as.data.frame(table(reduced_set$`Wahl_Partei`)/sum(table(reduced_set$`Wahl_Partei`)))

Partei_Order <- c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "AfD", "Die Linke", "FDP", "Die Partei", "Sonstige:", "Ich würde nicht wählen gehen")
ggplot(Partei, aes(factor(Var1, levels = Partei_Order), Freq))+
  geom_col()+
  geom_text(aes(label = percent(Freq)), vjust = -1)+
  labs(x = "Parties", y = "", title = "Voters per Party")+
  ylim(0,0.3)



#Corona: 4 Groups
table(reduced_set$Corona_Hardliner) #1068 Hardliner 
table(reduced_set$Corona_Softliner) #480 Softliner 
table(reduced_set$Corona_Skeptiker) #260 Skeptiker
table(reduced_set$Corona_Leugner) #104 Leugner
#alle but Hardliners are imbalanced

#numeric values for Corona-statements: 
summary(reduced_set$Corona_Massnahmen_muessten_haerter_sein) #somewhat balanced
summary(reduced_set$Corona_Massnahmen_uebertrieben) #median is 2, mean is 2.845
summary(reduced_set$Corona_ist_harmlos_gleich_Grippe) #median is 1
summary(reduced_set$Glaube_nicht_an_Corona) #median is 1


#Alcohol, Cigarettes, Drugs
table(reduced_set$Alkohol_Konsum)
table(reduced_set$Zigaretten_Konsum)
table(reduced_set$Drogen_Konsum)
#summarized: V1 with 3 classes
table(reduced_set$Alkoholgruppe)
table(reduced_set$Zigarettengruppe)
table(reduced_set$Drogengruppe)
#summarized: V2 with 2 classes
table(reduced_set$Alkohol_ja_nein)
table(reduced_set$Zigaretten_ja_nein)
table(reduced_set$Drogen_ja_nein)



#Correlations between accounts? 
cor_accounts_df <- as.data.frame(cor(Accounts))
view(cor_accounts_df)

#test selected correlations
cor.test(reduced_set$Christian_Lindner, reduced_set$FDP)
cor.test(reduced_set$Alice_Weidel, reduced_set$AfD)
cor.test(reduced_set$Jens_Spahn, reduced_set$Bundesgesundheitsministerium)
cor.test(reduced_set$Faktastisch, reduced_set$Made_My_Day)
cor.test(reduced_set$Tagesschau, reduced_set$heute_show)



#----------------------------------------------------
#first insights: growing a single tree for three selected variables: Age Range, Age, Gender

data <- reduced_set

#######################
#Age Ranges (categorical)
######################

#define data for analysis
data_AgeRange <- data[,c(312, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_AgeRange$Age_Range))
data_AgeRange <- data_AgeRange %>% subset(data_AgeRange$Age_Range != "NA")


#is the variable imbalanced?
table(data_AgeRange$Age_Range) 
max(table(data_AgeRange$Age_Range)/sum(table(data_AgeRange$Age_Range)))

#IV als Faktor:
data_AgeRange$Age_Range <- as.factor(data_AgeRange$Age_Range)

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AgeRange$Age_Range, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAgeRange <- data_AgeRange[index,]
test_dfAgeRange <- data_AgeRange[-index,]


#---------------------------------Build decision tree------------------------------------------------

set.seed(1997)
tree_agerange <- rpart(Age_Range ~ . ,
                       data = train_dfAgeRange,
                       method = "class")

rpart.plot(tree_agerange)

importances <- varImp(tree_agerange)
importances %>%
  arrange(desc(Overall))

predictions <- predict(tree_agerange, newdata = test_dfAgeRange, type = "class")
predictions

confusionMatrix(predictions, test_dfAgeRange$Age_Range)



#######################
#Age: numeric
######################

data_Alter<- data[,c(24, 27:255)]

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.numeric(data_Alter$Alter)

#Are there NAs in the DV?
sum(is.na(data_Alter$Alter)) 

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alter$Alter, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlter <- data_Alter[index,]
test_dfAlter <- data_Alter[-index,]


#---------------------------------Build decision tree------------------------------------------------

set.seed(1997)
tree_Alter <- rpart(Alter ~ . ,
                    data = train_dfAlter,
                    method = "anova")

rpart.plot(tree_Alter)

importances <- varImp(tree_Alter)
importances %>%
  arrange(desc(Overall))

predictionsAlter <- predict(tree_Alter, newdata = test_dfAlter, type = "vector")
predictionsAlter




#######################
#Gender
######################

data_GeschlechtMW <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Are there NAs in the DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich))  
data_GeschlechtMW <- data_GeschlechtMW %>% subset(data_GeschlechtMW$weiblich_maennlich != "NA")


#is the variable imbalanced?
table(data_GeschlechtMW$weiblich_maennlich) #Verteilung in Ordnung
max(table(data_GeschlechtMW$weiblich_maennlich)/sum(table(data_GeschlechtMW$weiblich_maennlich)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_GeschlechtMW$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfGeschlechtMW <- data_GeschlechtMW[index,]
test_dfGeschlechtMW <- data_GeschlechtMW[-index,]


#---------------------------------Build decision tree------------------------------------------------

set.seed(1997)
tree_Geschlecht <- rpart(weiblich_maennlich ~ . ,
                         data = train_dfGeschlechtMW,
                         method = "class")

rpart.plot(tree_Geschlecht)

importances <- varImp(tree_Geschlecht)
importances %>%
  arrange(desc(Overall))

predictionsGeschlecht <- predict(tree_Geschlecht, newdata = test_dfGeschlechtMW, type = "class")
predictionsGeschlecht

confusionMatrix(predictionsGeschlecht, test_dfGeschlechtMW$weiblich_maennlich)











#------------------------------------FINAL CODE for RANDOM FORESTS--------------------------------------------

#------------------------Analysis on Reduced Dataset------------------------------

# load data 

load("data_reduced.RData")

data <- reduced_set

cols_names <- names(data)  
cols_names



#define tuning grid
myGrid = expand.grid(mtry = c(10:20),
                     splitrule = "extratrees", 
                     min.node.size = c(5,10,15))

#also test for higher mtry for regression trees 
myGridnum = expand.grid(mtry = c(71:81),
                        splitrule = "extratrees", 
                        min.node.size = c(5,10,15))




#######################
#Age Ranges Categorical (high, medium, low)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_AgeRange <- data[,c(312, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_AgeRange$Age_Range))
data_AgeRange <- data_AgeRange %>% subset(data_AgeRange$Age_Range != "NA")


#is the variable imbalanced?
table(data_AgeRange$Age_Range) 
max(table(data_AgeRange$Age_Range)/sum(table(data_AgeRange$Age_Range)))

#IV als Faktor:
data_AgeRange$Age_Range <- as.factor(data_AgeRange$Age_Range)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AgeRange$Age_Range, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAgeRange <- data_AgeRange[index,]
test_dfAgeRange <- data_AgeRange[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFAgeRange <- train(Age_Range ~ ., 
                    data=train_dfAgeRange,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models to console

RFAgeRange
summary(RFAgeRange)
plot(RFAgeRange)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFAgeRange, newdata=test_dfAgeRange)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfAgeRange$Age_Range))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedriges.Alter"])
  
}

#model auc: 
RFAgeRange %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()


#save model to disk 

tree500_AgeRange <- RFAgeRange
saveRDS(tree500_AgeRange, "./tree_500AgeRange.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFAgeRange1 <- train(Age_Range ~ ., 
                     data=train_dfAgeRange, 
                     method="ranger", metric= "Kappa",
                     tuneGrid = myGrid,
                     na.action = na.omit,
                     num.tree = 1000,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models
RFAgeRange1
summary(RFAgeRange1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFAgeRange1, newdata=test_dfAgeRange)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAgeRange$Age_Range))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedriges.Alter"])
  
}

#model auc: 
RFAgeRange1 %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()


#save model to disk 

tree1000_AgeRange <- RFAgeRange1
saveRDS(tree1000_AgeRange, "./tree1000_AgeRange.rds")



####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFAgeRangeFinal <- RFAgeRange

# Print models
RFAgeRangeFinal 
summary(RFAgeRangeFinal )

#evaluate variable importance 
varImp(RFAgeRangeFinal )
plot(varImp(RFAgeRangeFinal ), 20, main = "Age_Range")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAgeRangeFinal , newdata=test_dfAgeRange)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAgeRange$Age_Range))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAgeRange$Age_Range,
                 predict(model, data, type = "prob")[, "niedriges.Alter"])
  
}

#model auc: 
RFAgeRangeFinal %>%
  test_roc(data = test_dfAgeRange) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFAgeRangeFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFAgeRangeFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "hohes.Alter") %>%plotPartial (main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hohes.Alter") %>%plotPartial(main = "hohes Alter")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittleres.Alter") %>%plotPartial(main = "mittleres Alter")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedriges.Alter") %>%plotPartial(main = "niedriges Alter")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_AgeRange <- RFAgeRangeFinal
saveRDS(besttree_AgeRange, "./besttree_AgeRange.rds")

#load the model

besttree_AgeRange <- readRDS("./besttree_AgeRange.rds")
print(besttree_AgeRange)


#---------------------------------------------------------------------------------------------------

#######################
#Age: numeric (normal grid)
######################

data_Alter<- data[,c(24, 27:255)]

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.numeric(data_Alter$Alter)

#Are there NAs in the DV?
sum(is.na(data_Alter$Alter))  

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alter$Alter, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlter <- data_Alter[index,]
test_dfAlter <- data_Alter[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds ; here no sampling
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(667)
modelAlterRF <- train(Alter ~ ., 
                      data=train_dfAlter,
                      tuneGrid = myGrid,
                      method="ranger",
                      metric= "RMSE",  
                      na.action = na.omit,
                      num.tree = 500,
                      trControl = myControl, 
                      importance = 'impurity')

# Print model to console

modelAlterRF
summary(modelAlterRF)
plot(modelAlterRF)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlterRF, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlterRF <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlterRF

spearmanAlterRF <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlterRF

#save model to disk 

tree500_Alter <- modelAlterRF
saveRDS(tree500_Alter, "./tree500_Alter.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAlterRF1 <- train(Alter ~ ., 
                       data=train_dfAlter,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "RMSE", 
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelAlterRF1
summary(modelAlterRF1)
plot(modelAlterRF1)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlterRF1, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlter1 <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlter1

spearmanAlter1 <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlter1

#save model to disk 

tree1000_Alter <- modelAlterRF1
saveRDS(tree1000_Alter, "./tree1000_Alter.rds")


####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelAlterfinal <- modelAlterRF

# Print model
print(modelAlterfinal)

#output in terms of regression coefficients
summary(modelAlterfinal)

#evaluate variable importance 
varImp(modelAlterfinal)
plot(varImp(modelAlterfinal), 20, main = "Alter")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlterfinal, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlterfinal <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlterfinal

spearmanAlterfinal <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlterfinal


#testing baseline accuracy with mean and median

mean_Alter <- rep(mean(test_dfAlter$Alter), nrow(test_dfAlter))
median_Alter <- rep(median(test_dfAlter$Alter), nrow(test_dfAlter))

MAE(predictions, mean_Alter)
RMSE(predictions, mean_Alter)

MAE(predictions, median_Alter)
RMSE(predictions, median_Alter)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables


imp <- importance(modelAlterfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAlterfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Alter")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Alter <- modelAlterfinal
saveRDS(besttree_Alter, "./besttree_Alter.rds")

#load the model

besttree_Alter <- readRDS("./besttree_Alter.rds")
print(besttree_Alter)



#######################
#Age: numeric (test grid for regression)
######################

data_Alter<- data[,c(24, 27:255)]

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.numeric(data_Alter$Alter)

#Are there NAs in the DV?
sum(is.na(data_Alter$Alter))  

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alter$Alter, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlter <- data_Alter[index,]
test_dfAlter <- data_Alter[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds ; here no sampling
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(667)
modelAlterRF_mtry <- train(Alter ~ ., 
                      data=train_dfAlter,
                      tuneGrid = myGridnum,
                      method="ranger",
                      metric= "RMSE",  
                      na.action = na.omit,
                      num.tree = 500,
                      trControl = myControl, 
                      importance = 'impurity')

# Print model to console

modelAlterRF_mtry
summary(modelAlterRF_mtry)
plot(modelAlterRF_mtry)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlterRF_mtry, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlterRF <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlterRF

spearmanAlterRF <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlterRF

#save model to disk 

tree500_Alter_mtry <- modelAlterRF_mtry
saveRDS(tree500_Alter_mtry, "./tree500_Alter_mtry.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAlterRF1_mtry <- train(Alter ~ ., 
                       data=train_dfAlter,
                       tuneGrid = myGridnum,
                       method="ranger", 
                       metric= "RMSE", 
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelAlterRF1_mtry
summary(modelAlterRF1_mtry)
plot(modelAlterRF1_mtry)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlterRF1_mtry, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlter1 <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlter1

spearmanAlter1 <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlter1

#save model to disk 

tree1000_Alter_mtry <- modelAlterRF1_mtry
saveRDS(tree1000_Alter_mtry, "./tree1000_Alter_mtry.rds")


####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelAlterfinal_mtry <- modelAlterRF

# Print model
print(modelAlterfinal_mtry)

#output in terms of regression coefficients
summary(modelAlterfinal_mtry)

#evaluate variable importance 
varImp(modelAlterfinal_mtry)
plot(varImp(modelAlterfinal_mtry), 20, main = "Alter")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlterfinal_mtry, newdata=test_dfAlter)

#check performance measures  
MAE(predictions, test_dfAlter$Alter)
RMSE(predictions, test_dfAlter$Alter)
R2(predictions, test_dfAlter$Alter)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAlterfinal <- cor.test(predictions, test_dfAlter$Alter, method = "pearson")
pearsonAlterfinal

spearmanAlterfinal <- cor.test(predictions, test_dfAlter$Alter, method = "spearman")
spearmanAlterfinal


#testing baseline accuracy with mean and median

mean_Alter <- rep(mean(test_dfAlter$Alter), nrow(test_dfAlter))
median_Alter <- rep(median(test_dfAlter$Alter), nrow(test_dfAlter))

MAE(predictions, mean_Alter)
RMSE(predictions, mean_Alter)

MAE(predictions, median_Alter)
RMSE(predictions, median_Alter)


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables


imp <- importance(modelAlterfinal_mtry$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAlterfinal_mtry

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Alter")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Alter")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Alter_mtry <- modelAlterfinal_mtry
saveRDS(besttree_Alter_mtry, "./besttree_Alter_mtry.rds")





##################
#Gender: binary
##################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_GeschlechtMW <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Are there NAs in the DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich))  
data_GeschlechtMW <- data_GeschlechtMW %>% subset(data_GeschlechtMW$weiblich_maennlich != "NA")


#is the variable imbalanced?
table(data_GeschlechtMW$weiblich_maennlich) #Verteilung in Ordnung
max(table(data_GeschlechtMW$weiblich_maennlich)/sum(table(data_GeschlechtMW$weiblich_maennlich)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_GeschlechtMW$weiblich_maennlich, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfGeschlechtMW <- data_GeschlechtMW[index,]
test_dfGeschlechtMW <- data_GeschlechtMW[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = "all",
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

modelGeschlechtRF <- train(weiblich_maennlich ~ ., 
                           data=train_dfGeschlechtMW,
                           tuneGrid = myGrid,
                           method="ranger",
                           metric= "ROC",
                           na.action = na.omit,
                           num.tree = 500,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console

modelGeschlechtRF
summary(modelGeschlechtRF)
plot(modelGeschlechtRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelGeschlechtRF, newdata=test_dfGeschlechtMW)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfGeschlechtMW$weiblich_maennlich,
      predict(model, data, type = "prob")[, "weiblich"])
  
}

modelGeschlechtRF %>%
  test_roc(data = test_dfGeschlechtMW) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelGeschlechtRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGeschlechtMW)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Geschlecht <- modelGeschlechtRF
saveRDS(tree500_Geschlecht, "./tree500_Geschlecht.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelGeschlechtRF1 <- train(weiblich_maennlich ~ ., 
                            data=train_dfGeschlechtMW,
                            tuneGrid = myGrid,
                            method="ranger", 
                            metric= "ROC", 
                            na.action = na.omit,
                            num.tree = 1000,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelGeschlechtRF1
summary(modelGeschlechtRF1)
plot(modelGeschlechtRF1)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelGeschlechtRF1, newdata=test_dfGeschlechtMW)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfGeschlechtMW$weiblich_maennlich,
      predict(model, data, type = "prob")[, "weiblich"])
  
}

modelGeschlechtRF1 %>%
  test_roc(data = test_dfGeschlechtMW) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelGeschlechtRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGeschlechtMW)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#CC79A7", "#000000", "#009E73", "#0072B2", "#D55E00")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Geschlecht <- modelGeschlechtRF1
saveRDS(tree1000_Geschlecht, "./tree1000_Geschlecht.rds")


####-------tree 3: Final --------------------------------------------------

modelGeschlechtFinal <- modelGeschlechtRF

# Print model
print(modelGeschlechtFinal)

#output in terms of regression coefficients
summary(modelGeschlechtFinal)

#evaluate variable importance 
varImp(modelGeschlechtFinal)
plot(varImp(modelGeschlechtFinal), 20, main = "weiblich_maennlich")

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelGeschlechtFinal, newdata=test_dfGeschlechtMW)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfGeschlechtMW$weiblich_maennlich)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfGeschlechtMW$weiblich_maennlich,
      predict(model, data, type = "prob")[, "weiblich"])
  
}

#model auc: 
modelGeschlechtFinal %>%
  test_roc(data = test_dfGeschlechtMW) %>%
  auc()

#ROC plots
model_list <- list(ModelFinal = modelGeschlechtFinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGeschlechtMW)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelGeschlechtFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelGeschlechtFinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "weiblich") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "weiblich") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Geschlecht <- modelGeschlechtFinal
saveRDS(besttree_Geschlecht, "./besttree_Geschlecht.rds")

#load the model

besttree_Geschlecht <- readRDS("./besttree_Geschlecht.rds")
print(besttree_Geschlecht)



################
#East-West: binary
################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Ost_West <- data[,c(311, 27:255)]

data_Ost_West$Ost_West <- as.factor(data_Ost_West$Ost_West)

#Are there NAs in the DV?
sum(is.na(data_Ost_West$Ost_West))
data_Ost_West <- data_Ost_West %>% subset(data_Ost_West$Ost_West != "NA")


#is the variable imbalanced?
table(data_Ost_West$Ost_West)  
max(table(data_Ost_West$Ost_West)/sum(table(data_Ost_West$Ost_West)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Ost_West$Ost_West, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfOst_West <- data_Ost_West[index,]
test_dfOst_West <- data_Ost_West[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid",
)

####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelOst_West <- train(Ost_West ~ ., 
                       data=train_dfOst_West,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC",
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelOst_West
summary(modelOst_West)
plot(modelOst_West)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOst_West, newdata=test_dfOst_West)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfOst_West$Ost_West)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOst_West$Ost_West,
      predict(model, data, type = "prob")[, "Osten"])
  
}

modelOst_West %>%
  test_roc(data = test_dfOst_West) %>%
  auc()

# ROC-plot
model_list <- list(M1 = modelOst_West)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOst_West)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_OstWest <- modelOst_West
saveRDS(tree500_OstWest, "./tree500_OstWest.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelOst_West1 <- train(Ost_West ~ ., 
                        data=train_dfOst_West,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "ROC", 
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl, 
                        importance = 'impurity')

# Print model to console

modelOst_West1
summary(modelOst_West1)
plot(modelOst_West1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOst_West1, newdata=test_dfOst_West)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOst_West$Ost_West)



#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOst_West$Ost_West,
      predict(model, data, type = "prob")[, "Osten"])
  
}

modelOst_West1 %>%
  test_roc(data = test_dfOst_West) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOst_West1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOst_West)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Ost_West <- modelOst_West1
saveRDS(tree1000_Ost_West, "./tree1000_OstWest.rds")


####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelOst_Westfinal <- modelOst_West

# Print model
print(modelOst_Westfinal)

#output in terms of regression coefficients
summary(modelOst_Westfinal)

#evaluate variable importance 
varImp(modelOst_Westfinal)
plot(varImp(modelOst_Westfinal), 20, main = "Ost_West")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOst_Westfinal, newdata=test_dfOst_West)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOst_West$Ost_West)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOst_West$Ost_West,
      predict(model, data, type = "prob")[, "Osten"])
  
}

modelOst_Westfinal %>%
  test_roc(data = test_dfOst_West) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelOst_Westfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOst_West)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelOst_Westfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelOst_Westfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_OstWest <- modelOst_Westfinal
saveRDS(besttree_OstWest, "./besttree_OstWest.rds")

#load the model

besttree_OstWest <- readRDS("./besttree_OstWest.rds")
print(besttree_OstWest)




################
#Extraversion1: numeric
################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Extraversion<- data[,c(295, 27:255)]

data_Extraversion$Extraversion <- as.numeric(data_Extraversion$Extraversion)

#Are there NAs in the DV?
sum(is.na(data_Extraversion$Extraversion))  
data_Extraversion <- data_Extraversion%>% filter(Extraversion != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Extraversion$Extraversion, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfExtraversion <- data_Extraversion[index,]
test_dfExtraversion <- data_Extraversion[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelExtraversionRF <- train(Extraversion ~ ., 
                             data=train_dfExtraversion,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "RMSE",  
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelExtraversionRF
summary(modelExtraversionRF)
plot(modelExtraversionRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversionRF, newdata=test_dfExtraversion)

#check performance measures  
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversionRF <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversionRF

spearmanExtraversionRF <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversionRF

#save model to disk 

tree500_Extraversion1 <- modelExtraversionRF
saveRDS(tree500_Extraversion1, "./tree500_Extraversion1.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelExtraversionRF1 <- train(Extraversion ~ ., 
                              data=train_dfExtraversion,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "RMSE", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelExtraversionRF1
summary(modelExtraversionRF1)
plot(modelExtraversionRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversionRF1, newdata=test_dfExtraversion)


#check performance measures  
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

#calculate Pearson coefficient for predictions and actual values

pearsonExtraversion1 <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversion1

spearmanExtraversion1 <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversion1


#save model to disk 

tree1000_Extraversion1 <- modelExtraversionRF1
saveRDS(tree1000_Extraversion1, "./tree1000_Extraversion1.rds")



####-------tree 3: Final --------------------------------------------------

#finales Model definieren
set.seed(1997)
modelExtraversionfinal <- modelExtraversionRF1

# Print model
print(modelExtraversionfinal)

#output in terms of regression coefficients
summary(modelExtraversionfinal)

#evaluate variable importance 
varImp(modelExtraversionfinal)
plot(varImp(modelExtraversionfinal), 20, main = "Extraversion")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversionfinal, newdata=test_dfExtraversion)

#check performance measures  
MAE(predictions, test_dfExtraversion$Extraversion)
RMSE(predictions, test_dfExtraversion$Extraversion)
R2(predictions, test_dfExtraversion$Extraversion)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonExtraversionfinal <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "pearson")
pearsonExtraversionfinal

spearmanExtraversionfinal <- cor.test(predictions, test_dfExtraversion$Extraversion, method = "spearman")
spearmanExtraversionfinal


#testing baseline accuracy with mean and median

mean_Extraversion <- rep(mean(test_dfExtraversion$Extraversion), nrow(test_dfExtraversion))
median_Extraversion <- rep(median(test_dfExtraversion$Extraversion), nrow(test_dfExtraversion))

MAE(predictions, mean_Extraversion)
RMSE(predictions, mean_Extraversion)

MAE(predictions, median_Extraversion)
RMSE(predictions, median_Extraversion)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelExtraversionfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelExtraversionfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Extraversion")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Extraversion")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Extraversion1 <- modelExtraversionfinal
saveRDS(besttree_Extraversion1, "./besttree_Extraversion1.rds")

#load the model

besttree_Extraversion1 <- readRDS("./besttree_Extraversion1.rds")
print(besttree_Extraversion1)




###############
#Extraversion 2: binary
##############

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Extraversion2 <- data[,c(300, 27:255)]

data_Extraversion2$Extraversion2 <- as.factor(data_Extraversion2$Extraversion2)

#Are there NAs in the DV?
sum(is.na(data_Extraversion2$Extraversion2)) 
data_Extraversion2 <- data_Extraversion2 %>% subset(data_Extraversion2$Extraversion2 != "NA")


#is the variable imbalanced?
table(data_Extraversion2$Extraversion2) 
max(table(data_Extraversion2$Extraversion2)/sum(table(data_Extraversion2$Extraversion2))) 



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Extraversion2$Extraversion2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfExtraversion2 <- data_Extraversion2[index,]
test_dfExtraversion2 <- data_Extraversion2[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelExtraversion2 <- train(Extraversion2 ~ ., 
                            data=train_dfExtraversion2,
                            tuneGrid = myGrid,
                            method="ranger",
                            metric= "ROC",
                            na.action = na.omit,
                            num.tree = 500,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelExtraversion2
summary(modelExtraversion2)
plot(modelExtraversion2)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversion2, newdata=test_dfExtraversion2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfExtraversion2$Extraversion2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfExtraversion2$Extraversion2,
      predict(model, data, type = "prob")[, "Introvertiert"])
  
}

modelExtraversion2 %>%
  test_roc(data = test_dfExtraversion2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelExtraversion2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfExtraversion2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Extraversion2 <- modelExtraversion2
saveRDS(tree500_Extraversion2, "./tree500_Extraversion2.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelExtraversion2_1 <- train(Extraversion2 ~ ., 
                              data=train_dfExtraversion2,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "ROC", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelExtraversion2_1
summary(modelExtraversion2_1)
plot(modelExtraversion2_1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversion2_1, newdata=test_dfExtraversion2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfExtraversion2$Extraversion2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfExtraversion2$Extraversion2,
      predict(model, data, type = "prob")[, "Introvertiert"])
  
}

modelExtraversion2_1 %>%
  test_roc(data = test_dfExtraversion2) %>%
  auc()

# ROC-plot
model_list <- list(M1 = modelExtraversion2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfExtraversion2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_Extraversion2 <- modelExtraversion2_1
saveRDS(tree1000_Extraversion2, "./tree1000_Extraversion2.rds")


####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelExtraversion2final <- modelExtraversion2

# Print model
print(modelExtraversion2final)

#output in terms of regression coefficients
summary(modelExtraversion2final)

#evaluate variable importance 
varImp(modelExtraversion2final)
plot(varImp(modelExtraversion2final), 20, main = "Extraversion2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelExtraversion2final, newdata=test_dfExtraversion2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfExtraversion2$Extraversion2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfExtraversion2$Extraversion2,
      predict(model, data, type = "prob")[, "Introvertiert"])
  
}

modelExtraversion2final %>%
  test_roc(data = test_dfExtraversion2) %>%
  auc()

# ROC-plot
model_list <- list(M1 = modelExtraversion2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfExtraversion2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables


imp <- importance(modelExtraversion2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelExtraversion2final

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Extraversion2 <- modelExtraversion2final
saveRDS(besttree_Extraversion2, "./besttree_Extraversion2.rds")

#load the model

besttree_Extraversion2 <- readRDS("./besttree_Extraversion2.rds")
print(besttree_Extraversion2)



#####################
#Agreeableness 1: numeric
####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Agreeableness<- data[,c(296, 27:255)]

data_Agreeableness$Agreeableness <- as.numeric(data_Agreeableness$Agreeableness)

#Are there NAs in the DV?
sum(is.na(data_Agreeableness$Agreeableness))  
data_Agreeableness <- data_Agreeableness%>% filter(Agreeableness != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

index <- createDataPartition(data_Agreeableness$Agreeableness, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAgreeableness <- data_Agreeableness[index,]
test_dfAgreeableness <- data_Agreeableness[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelAgreeablenessRF <- train(Agreeableness ~ ., 
                              data=train_dfAgreeableness,
                              tuneGrid = myGrid,
                              method="ranger",
                              metric= "RMSE", 
                              na.action = na.omit,
                              num.tree = 500,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelAgreeablenessRF
summary(modelAgreeablenessRF)
plot(modelAgreeablenessRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeablenessRF, newdata=test_dfAgreeableness)

#check performance measures
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeablenessRF <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeablenessRF

spearmanAgreeablenessRF <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeablenessRF

#save model to disk 

tree500_Agreeableness1 <- modelAgreeablenessRF
saveRDS(tree500_Agreeableness1, "./tree500_Agreeableness1.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAgreeablenessRF1 <- train(Agreeableness ~ ., 
                               data=train_dfAgreeableness,
                               tuneGrid = myGrid,
                               method="ranger", 
                               metric= "RMSE", 
                               na.action = na.omit,
                               num.tree = 1000,
                               trControl = myControl, 
                               importance = 'impurity')

# Print model to console

modelAgreeablenessRF1
summary(modelAgreeablenessRF1)
plot(modelAgreeablenessRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeablenessRF1, newdata=test_dfAgreeableness)


#check performance measures  
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeableness1 <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeableness1

spearmanAgreeableness1 <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeableness1

#save model to disk 

tree1000_Agreeableness1 <- modelAgreeablenessRF1
saveRDS(tree1000_Agreeableness1, "./tree1000_Agreeableness1.rds")



####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
modelAgreeablenessfinal <- modelAgreeablenessRF

# Print model
print(modelAgreeablenessfinal)

#output in terms of regression coefficients
summary(modelAgreeablenessfinal)

#evaluate variable importance 
varImp(modelAgreeablenessfinal)
plot(varImp(modelAgreeablenessfinal), 20, main = "Agreeableness")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeablenessfinal, newdata=test_dfAgreeableness)


#check performance measures
MAE(predictions, test_dfAgreeableness$Agreeableness)
RMSE(predictions, test_dfAgreeableness$Agreeableness)
R2(predictions, test_dfAgreeableness$Agreeableness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonAgreeablenessfinal <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "pearson")
pearsonAgreeablenessfinal

spearmanAgreeablenessfinal <- cor.test(predictions, test_dfAgreeableness$Agreeableness, method = "spearman")
spearmanAgreeablenessfinal


#testing baseline accuracy with mean and median

mean_Agreeableness <- rep(mean(test_dfAgreeableness$Agreeableness), nrow(test_dfAgreeableness))
median_Agreeableness <- rep(median(test_dfAgreeableness$Agreeableness), nrow(test_dfAgreeableness))

MAE(predictions, mean_Agreeableness)
RMSE(predictions, mean_Agreeableness)

MAE(predictions, median_Agreeableness)
RMSE(predictions, median_Agreeableness)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAgreeablenessfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAgreeablenessfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Agreeableness")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Agreeableness")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Agreeableness1 <- modelAgreeablenessfinal
saveRDS(besttree_Agreeableness1, "./besttree_Agreeableness1.rds")

#load the model

besttree_Agreeableness1 <- readRDS("./besttree_Agreeableness1.rds")
print(besttree_Agreeableness1)




#####################
#Agreeableness2: binary
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Agreeableness2 <- data[,c(301, 27:255)]

data_Agreeableness2$Agreeableness2 <- as.factor(data_Agreeableness2$Agreeableness2)

#Are there NAs in the DV?
sum(is.na(data_Agreeableness2$Agreeableness2))  
data_Agreeableness2 <- data_Agreeableness2 %>% subset(data_Agreeableness2$Agreeableness2 != "NA")


#is the variable imbalanced?
table(data_Agreeableness2$Agreeableness2)
max(table(data_Agreeableness2$Agreeableness2)/sum(table(data_Agreeableness2$Agreeableness2)))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Agreeableness2$Agreeableness2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAgreeableness2 <- data_Agreeableness2[index,]
test_dfAgreeableness2 <- data_Agreeableness2[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

modelAgreeableness2 <- train(Agreeableness2 ~ ., 
                             data=train_dfAgreeableness2,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "ROC",
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelAgreeableness2
summary(modelAgreeableness2)
plot(modelAgreeableness2)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeableness2, newdata=test_dfAgreeableness2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfAgreeableness2$Agreeableness2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAgreeableness2$Agreeableness2,
      predict(model, data, type = "prob")[, "Not_Agreeable"])
  
}

modelAgreeableness2 %>%
  test_roc(data = test_dfAgreeableness2) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelAgreeableness2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAgreeableness2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_Agreeableness2 <- modelAgreeableness2
saveRDS(tree500_Agreeableness2, "./tree500_Agreeableness2.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelAgreeableness2_1 <- train(Agreeableness2 ~ ., 
                               data=train_dfAgreeableness2,
                               tuneGrid = myGrid,
                               method="ranger", 
                               metric= "ROC", 
                               na.action = na.omit,
                               num.tree = 1000,
                               trControl = myControl, 
                               importance = 'impurity')

# Print model to console

modelAgreeableness2_1
summary(modelAgreeableness2_1)
plot(modelAgreeableness2_1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeableness2_1, newdata=test_dfAgreeableness2)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfAgreeableness2$Agreeableness2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAgreeableness2$Agreeableness2,
      predict(model, data, type = "prob")[, "Not_Agreeable"])
  
}

modelAgreeableness2_1 %>%
  test_roc(data = test_dfAgreeableness2) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelAgreeableness2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAgreeableness2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Agreeableness2 <- modelAgreeableness2_1
saveRDS(tree1000_Agreeableness2, "./tree1000_Agreeableness2.rds")



####-------tree 3: Final --------------------------------------------------

#define final model
set.seed(1997)
modelAgreeableness2final <- modelAgreeableness2_1

# Print model
print(modelAgreeableness2final)

#output in terms of regression coefficients
summary(modelAgreeableness2final)

#evaluate variable importance 
varImp(modelAgreeableness2final)
plot(varImp(modelAgreeableness2final), 20, main = "Agreeableness2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAgreeableness2final, newdata=test_dfAgreeableness2)

# Create confusion matrix
confusionMatrix(data=predictions, test_dfAgreeableness2$Agreeableness2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAgreeableness2$Agreeableness2,
      predict(model, data, type = "prob")[, "Not_Agreeable"])
  
}

modelAgreeableness2final %>%
  test_roc(data = test_dfAgreeableness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAgreeableness2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAgreeableness2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAgreeableness2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAgreeableness2final

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Agreeableness2 <- modelAgreeableness2final
saveRDS(besttree_Agreeableness2, "./besttree_Agreeableness2.rds")

#load the model

besttree_Agreeableness2 <- readRDS("./besttree_Agreeableness2.rds")
print(besttree_Agreeableness2)




###################
#Conscentiousness1: numeric
###################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Conscientiousness<- data[,c(297, 27:255)]

data_Conscientiousness$Conscientiousness <- as.numeric(data_Conscientiousness$Conscientiousness)

#Are there NAs in the DV?
sum(is.na(data_Conscientiousness$Conscientiousness))  
data_Conscientiousness <- data_Conscientiousness%>% filter(Conscientiousness != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Conscientiousness$Conscientiousness, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfConscientiousness <- data_Conscientiousness[index,]
test_dfConscientiousness <- data_Conscientiousness[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelConscientiousnessRF <- train(Conscientiousness ~ ., 
                                  data=train_dfConscientiousness,
                                  tuneGrid = myGrid,
                                  method="ranger",
                                  metric= "RMSE",  
                                  na.action = na.omit,
                                  num.tree = 500,
                                  trControl = myControl, 
                                  importance = 'impurity')

# Print model to console

modelConscientiousnessRF
summary(modelConscientiousnessRF)
plot(modelConscientiousnessRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousnessRF, newdata=test_dfConscientiousness)

#check performance measures  
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousnessRF <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousnessRF

spearmanConscientiousnessRF <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousnessRF


#save model to disk 

tree500_Conscentiousness1 <- modelConscientiousnessRF
saveRDS(tree500_Conscentiousness1, "./tree500_Conscentiousness1.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelConscientiousnessRF1 <- train(Conscientiousness ~ ., 
                                   data=train_dfConscientiousness,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "RMSE", 
                                   na.action = na.omit,
                                   num.tree = 1000,
                                   trControl = myControl, 
                                   importance = 'impurity')

# Print model to console
modelConscientiousnessRF1
summary(modelConscientiousnessRF1)
plot(modelConscientiousnessRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousnessRF1, newdata=test_dfConscientiousness)

#check performance measures  
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousness1 <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousness1

spearmanConscientiousness1 <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousness1


#save model to disk 

tree1000_Conscentiousness1 <- modelConscientiousnessRF1
saveRDS(tree1000_Conscentiousness1, "./tree1000_Conscentiousness1.rds")


####-------tree 3: Final --------------------------------------------------

#define final model
set.seed(1997)
modelConscientiousnessfinal <- modelConscientiousnessRF1

# Print model
print(modelConscientiousnessfinal)

#output in terms of regression coefficients
summary(modelConscientiousnessfinal)

#evaluate variable importance 
varImp(modelConscientiousnessfinal)
plot(varImp(modelConscientiousnessfinal), 20, main = "Conscientiousness")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousnessfinal, newdata=test_dfConscientiousness)


#check performance measures  
MAE(predictions, test_dfConscientiousness$Conscientiousness)
RMSE(predictions, test_dfConscientiousness$Conscientiousness)
R2(predictions, test_dfConscientiousness$Conscientiousness)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonConscientiousnessfinal <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "pearson")
pearsonConscientiousnessfinal

spearmanConscientiousnessfinal <- cor.test(predictions, test_dfConscientiousness$Conscientiousness, method = "spearman")
spearmanConscientiousnessfinal


#testing baseline accuracy with mean and median

mean_Conscentiousness <- rep(mean(test_dfConscientiousness$Conscientiousness), nrow(test_dfConscientiousness))
median_Conscentiousness <- rep(median(test_dfConscientiousness$Conscientiousness), nrow(test_dfConscientiousness))

MAE(predictions, mean_Conscentiousness)
RMSE(predictions, mean_Conscentiousness)

MAE(predictions, median_Conscentiousness)
RMSE(predictions, median_Conscentiousness)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelConscientiousnessfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelConscientiousnessfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Conscientiousness")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Conscientiousness")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Conscentiousness1 <- modelConscientiousnessfinal
saveRDS(besttree_Conscentiousness1, "./besttree_Conscentiousness1.rds")

#load the model

besttree_Conscentiousness1 <- readRDS("./besttree_Conscentiousness1.rds")
print(besttree_Conscentiousness1)




#######################
#Conscentiousness2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Conscientiousness2 <- data[,c(302, 27:255)]

data_Conscientiousness2$Conscientiousness2 <- as.factor(data_Conscientiousness2$Conscientiousness2)

#Are there NAs in the DV?
sum(is.na(data_Conscientiousness2$Conscientiousness2))  
data_Conscientiousness2 <- data_Conscientiousness2 %>% subset(data_Conscientiousness2$Conscientiousness2 != "NA")


#is the variable imbalanced?
table(data_Conscientiousness2$Conscientiousness2) 
max(table(data_Conscientiousness2$Conscientiousness2)/sum(table(data_Conscientiousness2$Conscientiousness2)))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Conscientiousness2$Conscientiousness2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfConscientiousness2 <- data_Conscientiousness2[index,]
test_dfConscientiousness2 <- data_Conscientiousness2[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelConscientiousness2 <- train(Conscientiousness2 ~ ., 
                                 data=train_dfConscientiousness2,
                                 tuneGrid = myGrid,
                                 method="ranger",
                                 metric= "ROC",  
                                 na.action = na.omit,
                                 num.tree = 500,
                                 trControl = myControl, 
                                 importance = 'impurity')

# Print model to console

modelConscientiousness2
summary(modelConscientiousness2)
plot(modelConscientiousness2)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousness2, newdata=test_dfConscientiousness2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfConscientiousness2$Conscientiousness2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfConscientiousness2$Conscientiousness2,
      predict(model, data, type = "prob")[, "Not_Conscientious"])
  
}

modelConscientiousness2 %>%
  test_roc(data = test_dfConscientiousness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelConscientiousness2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfConscientiousness2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_Conscentiousness2 <- modelConscientiousness2
saveRDS(tree500_Conscentiousness2, "./tree500_Conscentiousness2.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelConscientiousness2_1 <- train(Conscientiousness2 ~ ., 
                                   data=train_dfConscientiousness2,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "ROC", 
                                   na.action = na.omit,
                                   num.tree = 1000,
                                   trControl = myControl, 
                                   importance = 'impurity')

# Print model to console
modelConscientiousness2_1
summary(modelConscientiousness2_1)
plot(modelConscientiousness2_1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousness2_1, newdata=test_dfConscientiousness2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfConscientiousness2$Conscientiousness2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfConscientiousness2$Conscientiousness2,
      predict(model, data, type = "prob")[, "Not_Conscientious"])
  
}

modelConscientiousness2_1 %>%
  test_roc(data = test_dfConscientiousness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelConscientiousness2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfConscientiousness2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_Conscentiousness2 <- modelConscientiousness2_1
saveRDS(tree1000_Conscentiousness2, "./tree1000_Conscentiousness2.rds")



####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelConscientiousness2final <- modelConscientiousness2

# Print model
print(modelConscientiousness2final)

#output in terms of regression coefficients
summary(modelConscientiousness2final)

#evaluate variable importance 
varImp(modelConscientiousness2final)
plot(varImp(modelConscientiousness2final), 20, main = "Conscientiousness2")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelConscientiousness2final, newdata=test_dfConscientiousness2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfConscientiousness2$Conscientiousness2)

#check for AUC 

test_roc <- function(model, data) {
  
  roc(test_dfConscientiousness2$Conscientiousness2,
      predict(model, data, type = "prob")[, "Not_Conscientious"])
  
}

modelConscientiousness2final %>%
  test_roc(data = test_dfConscientiousness2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelConscientiousness2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfConscientiousness2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelConscientiousness2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelConscientiousness2final

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Conscentiousness2 <- modelConscientiousness2final
saveRDS(besttree_Conscentiousness2, "./besttree_Conscentiousness2.rds")

#load the model

besttree_Conscentiousness2 <- readRDS("./besttree_Conscentiousness2.rds")
print(besttree_Conscentiousness2)



###################
#Emotional Stability1: numeric
###################
#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Emotional_stablity<- data[,c(298, 27:255)]

data_Emotional_stablity$Emotional_stablity <- as.numeric(data_Emotional_stablity$Emotional_stablity)

#Are there NAs in the DV?
sum(is.na(data_Emotional_stablity$Emotional_stablity))  



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Emotional_stablity$Emotional_stablity, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfEmotional_stablity <- data_Emotional_stablity[index,]
test_dfEmotional_stablity <- data_Emotional_stablity[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelEmotional_stablityRF <- train(Emotional_stablity ~ ., 
                                   data=train_dfEmotional_stablity,
                                   tuneGrid = myGrid,
                                   method="ranger",
                                   metric= "RMSE",  
                                   na.action = na.omit,
                                   num.tree = 500,
                                   trControl = myControl, 
                                   importance = 'impurity')

# Print model to console

modelEmotional_stablityRF
summary(modelEmotional_stablityRF)
plot(modelEmotional_stablityRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablityRF, newdata=test_dfEmotional_stablity)

#check performance measures  
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablityRF <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablityRF

spearmanEmotional_stablityRF <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablityRF

#save model to disk 

tree500_EmotionalStability <- modelEmotional_stablityRF
saveRDS(tree500_EmotionalStability, "./tree500_EmotionalStability.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelEmotional_stablityRF1 <- train(Emotional_stablity ~ ., 
                                    data=train_dfEmotional_stablity,
                                    tuneGrid = myGrid,
                                    method="ranger", 
                                    metric= "RMSE", 
                                    na.action = na.omit,
                                    num.tree = 1000,
                                    trControl = myControl, 
                                    importance = 'impurity')

# Print model to console
modelEmotional_stablityRF1
summary(modelEmotional_stablityRF1)
plot(modelEmotional_stablityRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablityRF1, newdata=test_dfEmotional_stablity)

#check performance measures  
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablity1 <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablity1

spearmanEmotional_stablity1 <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablity1

#save model to disk 

tree1000_EmotionalStability <- modelEmotional_stablityRF1
saveRDS(tree1000_EmotionalStability, "./tree1000_EmotionalStability.rds")


####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelEmotional_stablityfinal <- modelEmotional_stablityRF

# Print model
print(modelEmotional_stablityfinal)

#output in terms of regression coefficients
summary(modelEmotional_stablityfinal)

#evaluate variable importance 
varImp(modelEmotional_stablityfinal)
plot(varImp(modelEmotional_stablityfinal), 20, main = "Emotional_stablity")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablityfinal, newdata=test_dfEmotional_stablity)


#check performance measures  
MAE(predictions, test_dfEmotional_stablity$Emotional_stablity)
RMSE(predictions, test_dfEmotional_stablity$Emotional_stablity)
R2(predictions, test_dfEmotional_stablity$Emotional_stablity)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonEmotional_stablityfinal <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "pearson")
pearsonEmotional_stablityfinal

spearmanEmotional_stablityfinal <- cor.test(predictions, test_dfEmotional_stablity$Emotional_stablity, method = "spearman")
spearmanEmotional_stablityfinal


#testing baseline accuracy with mean and median

mean_EmotionalStability <- rep(mean(test_dfEmotional_stablity$Emotional_stablity), nrow(test_dfEmotional_stablity))
median_EmotionalStability <- rep(median(test_dfEmotional_stablity$Emotional_stablity), nrow(test_dfEmotional_stablity))

MAE(predictions, mean_EmotionalStability)
RMSE(predictions, mean_EmotionalStability)

MAE(predictions, median_EmotionalStability)
RMSE(predictions, median_EmotionalStability)

#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelEmotional_stablityfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelEmotional_stablityfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Emotional_stablity")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Emotional_stablity")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_EmotionalStability <- modelEmotional_stablityfinal
saveRDS(besttree_EmotionalStability, "./besttree_EmotionalStability.rds")

#load the model

besttree_EmotionalStability <- readRDS("./besttree_EmotionalStability.rds")
print(besttree_EmotionalStability)




######################
#Emotional Stability2: binary
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Emotional_stablity2 <- data[,c(303, 27:255)]

data_Emotional_stablity2$Emotional_stablity2 <- as.factor(data_Emotional_stablity2$Emotional_stablity2)

#Are there NAs in the DV?
sum(is.na(data_Emotional_stablity2$Emotional_stablity2))
data_Emotional_stablity2 <- data_Emotional_stablity2 %>% subset(data_Emotional_stablity2$Emotional_stablity2 != "NA")


#is the variable imbalanced?
table(data_Emotional_stablity2$Emotional_stablity2)  
max(table(data_Emotional_stablity2$Emotional_stablity2)/sum(table(data_Emotional_stablity2$Emotional_stablity2)))  



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Emotional_stablity2$Emotional_stablity2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfEmotional_stablity2 <- data_Emotional_stablity2[index,]
test_dfEmotional_stablity2 <- data_Emotional_stablity2[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelEmotional_stablity2 <- train(Emotional_stablity2 ~ ., 
                                  data=train_dfEmotional_stablity2,
                                  tuneGrid = myGrid,
                                  method="ranger",
                                  metric= "ROC",  
                                  na.action = na.omit,
                                  num.tree = 500,
                                  trControl = myControl, 
                                  importance = 'impurity')

# Print model to console
modelEmotional_stablity2
summary(modelEmotional_stablity2)
plot(modelEmotional_stablity2)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablity2, newdata=test_dfEmotional_stablity2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfEmotional_stablity2$Emotional_stablity2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfEmotional_stablity2$Emotional_stablity2,
      predict(model, data, type = "prob")[, "Unstable"])
  
}

modelEmotional_stablity2 %>%
  test_roc(data = test_dfEmotional_stablity2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelEmotional_stablity2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfEmotional_stablity2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_EmotionalStability2 <- modelEmotional_stablity2
saveRDS(tree500_EmotionalStability2, "./tree500_EmotionalStability2.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelEmotional_stablity2_1 <- train(Emotional_stablity2 ~ ., 
                                    data=train_dfEmotional_stablity2,
                                    tuneGrid = myGrid,
                                    method="ranger", 
                                    metric= "ROC", 
                                    na.action = na.omit,
                                    num.tree = 1000,
                                    trControl = myControl, 
                                    importance = 'impurity')

# Print model to console
modelEmotional_stablity2_1
summary(modelEmotional_stablity2_1)
plot(modelEmotional_stablity2_1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablity2_1, newdata=test_dfEmotional_stablity2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfEmotional_stablity2$Emotional_stablity2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfEmotional_stablity2$Emotional_stablity2,
      predict(model, data, type = "prob")[, "Unstable"])
  
}

modelEmotional_stablity2_1 %>%
  test_roc(data = test_dfEmotional_stablity2) %>%
  auc()

###nur für binär
#ROC plot
model_list <- list(M1 = modelEmotional_stablity2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfEmotional_stablity2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_EmotionalStability2 <- modelEmotional_stablity2_1
saveRDS(tree1000_EmotionalStability2, "./tree1000_EmotionalStability2.rds")



####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelEmotional_stablity2final <- modelEmotional_stablity2_1

# Print model
print(modelEmotional_stablity2final)

#output in terms of regression coefficients
summary(modelEmotional_stablity2final)

#evaluate variable importance 
varImp(modelEmotional_stablity2final)
plot(varImp(modelEmotional_stablity2final), 20, main = "Emotional_stablity2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelEmotional_stablity2final, newdata=test_dfEmotional_stablity2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfEmotional_stablity2$Emotional_stablity2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfEmotional_stablity2$Emotional_stablity2,
      predict(model, data, type = "prob")[, "Unstable"])
  
}

modelEmotional_stablity2final %>%
  test_roc(data = test_dfEmotional_stablity2) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#ROC plot
model_list <- list(M1 = modelEmotional_stablity2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfEmotional_stablity2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelEmotional_stablity2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelEmotional_stablity2final

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Stable") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Stable") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_EmotionalStability2 <- modelEmotional_stablity2final
saveRDS(besttree_EmotionalStability2, "./besttree_EmotionalStability2.rds")

#load the model

besttree_EmotionalStability2 <- readRDS("./besttree_EmotionalStability2.rds")
print(besttree_EmotionalStability2)




#####################
#Openness to Experiences1: numeric
####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Openness_to_Experiences<- data[,c(299, 27:255)]

data_Openness_to_Experiences$Openness_to_Experiences <- as.numeric(data_Openness_to_Experiences$Openness_to_Experiences)

#Are there NAs in the DV?
sum(is.na(data_Openness_to_Experiences$Openness_to_Experiences))  
data_Openness_to_Experiences <- data_Openness_to_Experiences%>% filter(Openness_to_Experiences != "NA")



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Openness_to_Experiences$Openness_to_Experiences, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfOpenness_to_Experiences <- data_Openness_to_Experiences[index,]
test_dfOpenness_to_Experiences <- data_Openness_to_Experiences[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(667)
modelOpenness_to_ExperiencesRF <- train(Openness_to_Experiences ~ ., 
                                        data=train_dfOpenness_to_Experiences,
                                        tuneGrid = myGrid,
                                        method="ranger",
                                        metric= "RMSE",  
                                        na.action = na.omit,
                                        num.tree = 500,
                                        trControl = myControl, 
                                        importance = 'impurity')

# Print model to console
modelOpenness_to_ExperiencesRF
summary(modelOpenness_to_ExperiencesRF)
plot(modelOpenness_to_ExperiencesRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_to_ExperiencesRF, newdata=test_dfOpenness_to_Experiences)

#check performance measures  
MAE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
RMSE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
R2(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonOpenness_to_ExperiencesRF <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "pearson")
pearsonOpenness_to_ExperiencesRF

spearmanOpenness_to_ExperiencesRF <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "spearman")
spearmanOpenness_to_ExperiencesRF


#save model to disk 

tree500_OpennessToExperiences1 <- modelOpenness_to_ExperiencesRF
saveRDS(tree500_OpennessToExperiences1, "./tree500_OpennessToExperiences1.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelOpenness_to_ExperiencesRF1 <- train(Openness_to_Experiences ~ ., 
                                         data=train_dfOpenness_to_Experiences,
                                         tuneGrid = myGrid,
                                         method="ranger", 
                                         metric= "RMSE", 
                                         na.action = na.omit,
                                         num.tree = 1000,
                                         trControl = myControl, 
                                         importance = 'impurity')

# Print model to console
modelOpenness_to_ExperiencesRF1
summary(modelOpenness_to_ExperiencesRF1)
plot(modelOpenness_to_ExperiencesRF1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_to_ExperiencesRF1, newdata=test_dfOpenness_to_Experiences)


#check performance measures  
MAE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
RMSE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
R2(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonOpenness_to_Experiences1 <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "pearson")
pearsonOpenness_to_Experiences1

spearmanOpenness_to_Experiences1 <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "spearman")
spearmanOpenness_to_Experiences1

#save model to disk 

tree1000_OpennessToExperiences1 <- modelOpenness_to_ExperiencesRF1
saveRDS(tree1000_OpennessToExperiences1, "./tree1000_OpennessToExperiences1.rds")


####-------tree 3: Final --------------------------------------------------

#define final Model
set.seed(1997)
modelOpenness_to_Experiencesfinal <- modelOpenness_to_ExperiencesRF1

# Print model
print(modelOpenness_to_Experiencesfinal)

#output in terms of regression coefficients
summary(modelOpenness_to_Experiencesfinal)

#evaluate variable importance 
varImp(modelOpenness_to_Experiencesfinal)
plot(varImp(modelOpenness_to_Experiencesfinal), 20, main = "Openness_to_Experiences")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_to_Experiencesfinal, newdata=test_dfOpenness_to_Experiences)


#check performance measures  
MAE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
RMSE(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)
R2(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonOpenness_to_Experiencesfinal <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "pearson")
pearsonOpenness_to_Experiencesfinal

spearmanOpenness_to_Experiencesfinal <- cor.test(predictions, test_dfOpenness_to_Experiences$Openness_to_Experiences, method = "spearman")
spearmanOpenness_to_Experiencesfinal

#testing baseline accuracy with mean and median

mean_OpennessToExp <- rep(mean(test_dfOpenness_to_Experiences$Openness_to_Experiences), nrow(test_dfOpenness_to_Experiences))
median_OpennessToExp <- rep(median(test_dfOpenness_to_Experiences$Openness_to_Experiences), nrow(test_dfOpenness_to_Experiences))

MAE(predictions, mean_OpennessToExp)
RMSE(predictions, mean_OpennessToExp)

MAE(predictions, median_OpennessToExp)
RMSE(predictions, median_OpennessToExp)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelOpenness_to_Experiencesfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelOpenness_to_Experiencesfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial(main = "Openness_to_Experiences")
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial(main = "Openness_to_Experiences")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_OpennessToExperiences1 <- modelOpenness_to_Experiencesfinal
saveRDS(besttree_OpennessToExperiences1, "./besttree_OpennessToExperiences1.rds")

#load the model

besttree_OpennessToExperiences1 <- readRDS("./besttree_OpennessToExperiences1.rds")
print(besttree_OpennessToExperiences1)





#######################
#Openness to Experiences2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Openness_Experiences2 <- data[,c(304, 27:255)]

data_Openness_Experiences2$Openness_Experiences2 <- as.factor(data_Openness_Experiences2$Openness_Experiences2)

#Are there NAs in the DV?
sum(is.na(data_Openness_Experiences2$Openness_Experiences2))  
data_Openness_Experiences2 <- data_Openness_Experiences2 %>% subset(data_Openness_Experiences2$Openness_Experiences2 != "NA")


#is the variable imbalanced?
table(data_Openness_Experiences2$Openness_Experiences2)  
max(table(data_Openness_Experiences2$Openness_Experiences2)/sum(table(data_Openness_Experiences2$Openness_Experiences2)))  



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Openness_Experiences2$Openness_Experiences2, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfOpenness_Experiences2 <- data_Openness_Experiences2[index,]
test_dfOpenness_Experiences2 <- data_Openness_Experiences2[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelOpenness_Experiences2 <- train(Openness_Experiences2 ~ ., 
                                    data=train_dfOpenness_Experiences2,
                                    tuneGrid = myGrid,
                                    method="ranger",
                                    metric= "ROC",  
                                    na.action = na.omit,
                                    num.tree = 500,
                                    trControl = myControl, 
                                    importance = 'impurity')

# Print model to console
modelOpenness_Experiences2
summary(modelOpenness_Experiences2)
plot(modelOpenness_Experiences2)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_Experiences2, newdata=test_dfOpenness_Experiences2)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfOpenness_Experiences2$Openness_Experiences2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOpenness_Experiences2$Openness_Experiences2,
      predict(model, data, type = "prob")[, "Closed"])
  
}

modelOpenness_Experiences2 %>%
  test_roc(data = test_dfOpenness_Experiences2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOpenness_Experiences2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOpenness_Experiences2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_OpennessToExperiences2 <- modelOpenness_Experiences2
saveRDS(tree500_OpennessToExperiences2, "./tree500_OpennessToExperiences2.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelOpenness_Experiences2_1 <- train(Openness_Experiences2 ~ ., 
                                      data=train_dfOpenness_Experiences2,
                                      tuneGrid = myGrid,
                                      method="ranger", 
                                      metric= "ROC", 
                                      na.action = na.omit,
                                      num.tree = 1000,
                                      trControl = myControl, 
                                      importance = 'impurity')

# Print model to console
modelOpenness_Experiences2_1
summary(modelOpenness_Experiences2_1)
plot(modelOpenness_Experiences2_1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_Experiences2_1, newdata=test_dfOpenness_Experiences2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOpenness_Experiences2$Openness_Experiences2)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOpenness_Experiences2$Openness_Experiences2,
      predict(model, data, type = "prob")[, "Closed"])
  
}

modelOpenness_Experiences2_1 %>%
  test_roc(data = test_dfOpenness_Experiences2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOpenness_Experiences2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOpenness_Experiences2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

trees1000_OpennessToExperiences2 <- modelOpenness_Experiences2_1
saveRDS(trees1000_OpennessToExperiences2, "./trees1000_OpennessToExperiences2.rds")


####-------tree 3: Final --------------------------------------------------

set.seed(1997)
modelOpenness_Experiences2final <- modelOpenness_Experiences2_1

# Print model
print(modelOpenness_Experiences2final)

#output in terms of regression coefficients
summary(modelOpenness_Experiences2final)

#evaluate variable importance 
varImp(modelOpenness_Experiences2final)
plot(varImp(modelOpenness_Experiences2final), 20, main = "Openness_Experiences2")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelOpenness_Experiences2final, newdata=test_dfOpenness_Experiences2)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfOpenness_Experiences2$Openness_Experiences2)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfOpenness_Experiences2$Openness_Experiences2,
      predict(model, data, type = "prob")[, "Closed"])
  
}

modelOpenness_Experiences2final %>%
  test_roc(data = test_dfOpenness_Experiences2) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelOpenness_Experiences2final)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfOpenness_Experiences2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelOpenness_Experiences2final$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelOpenness_Experiences2final

PartialPlots %>% partial(pred.var = impvar[1],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19],which.class = "Open") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20],which.class = "Open") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_OpennessToExperiences2 <- modelOpenness_Experiences2final
saveRDS(besttree_OpennessToExperiences2, "./besttree_OpennessToExperiences2.rds")

#load the model

besttree_OpennessToExperiences2 <- readRDS("./besttree_OpennessToExperiences2.rds")
print(besttree_OpennessToExperiences2)




##################
#Alcohol group (Frequency), categorical
##################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Alkoholgruppe <- data[,c(318, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Alkoholgruppe$Alkoholgruppe))
data_Alkoholgruppe <- data_Alkoholgruppe %>% subset(data_Alkoholgruppe$Alkoholgruppe != "NA")


#is the variable imbalanced?
table(data_Alkoholgruppe$Alkoholgruppe) 
max(table(data_Alkoholgruppe$Alkoholgruppe)/sum(table(data_Alkoholgruppe$Alkoholgruppe)))  

#IV als Faktor:
data_Alkoholgruppe$Alkoholgruppe <- as.factor(data_Alkoholgruppe$Alkoholgruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alkoholgruppe$Alkoholgruppe, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAlkoholgruppe <- data_Alkoholgruppe[index,]
test_dfAlkoholgruppe <- data_Alkoholgruppe[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
RFAlkoholgruppe <- train(Alkoholgruppe ~ ., 
                         data=train_dfAlkoholgruppe,
                         tuneGrid = myGrid,
                         method="ranger", 
                         metric= "Kappa",
                         num.tree = 500,
                         trControl = myControl1, 
                         na.action = na.omit,
                         importance = 'impurity')

# Print models to console

RFAlkoholgruppe
summary(RFAlkoholgruppe)
plot(RFAlkoholgruppe)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFAlkoholgruppe, newdata=test_dfAlkoholgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfAlkoholgruppe$Alkoholgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAlkoholgruppe$Alkoholgruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFAlkoholgruppe %>%
  test_roc(data = test_dfAlkoholgruppe) %>%
  auc()

#save model to disk 

tree500_Alkoholgruppe <- RFAlkoholgruppe
saveRDS(tree500_Alkoholgruppe, "./tree500_Alkoholgruppe.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFAlkoholgruppe1 <- train(Alkoholgruppe ~ ., 
                          data=train_dfAlkoholgruppe, 
                          method="ranger", metric= "Kappa",
                          tuneGrid = myGrid,
                          na.action = na.omit,
                          num.tree = 1000,
                          trControl = myControl1, 
                          importance = 'impurity')

# Print models
RFAlkoholgruppe1
summary(RFAlkoholgruppe1)
plot(RFAlkoholgruppe1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFAlkoholgruppe1, newdata=test_dfAlkoholgruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAlkoholgruppe$Alkoholgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAlkoholgruppe$Alkoholgruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFAlkoholgruppe1 %>%
  test_roc(data = test_dfAlkoholgruppe) %>%
  auc()


#save model to disk 

tree1000_Alkoholgruppe <- RFAlkoholgruppe1
saveRDS(tree1000_Alkoholgruppe, "./tree1000_Alkoholgruppe.rds")


####-------tree 3: Final --------------------------------------------------

#define final model
set.seed(1997)
RFAlkoholgruppefinal <- RFAlkoholgruppe1

# Print models
RFAlkoholgruppefinal
summary(RFAlkoholgruppefinal)

#evaluate variable importance 
varImp(RFAlkoholgruppefinal)
plot(varImp(RFAlkoholgruppefinal), 20, main = "Alkoholgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAlkoholgruppefinal, newdata=test_dfAlkoholgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAlkoholgruppe$Alkoholgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfAlkoholgruppe$Alkoholgruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFAlkoholgruppefinal %>%
  test_roc(data = test_dfAlkoholgruppe) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFAlkoholgruppefinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFAlkoholgruppefinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main= "Niedrig")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main= "Hoch")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Alkoholgruppe <- RFAlkoholgruppefinal
saveRDS(besttree_Alkoholgruppe, "./besttree_Alkoholgruppe.rds")

#load the model

besttree_Alkoholgruppe <- readRDS("./besttree_Alkoholgruppe.rds")
print(besttree_Alkoholgruppe)




######################
#Alcohol consumption yes/no (binary)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Alk_ja_nein <- data[,c(319, 27:255)]

data_Alk_ja_nein$Alkohol_ja_nein <- as.factor(data_Alk_ja_nein$Alkohol_ja_nein)

#Are there NAs in the DV?
sum(is.na(data_Alk_ja_nein$Alkohol_ja_nein))  
data_Alk_ja_nein <- data_Alk_ja_nein %>% subset(data_Alk_ja_nein$Alkohol_ja_nein != "NA")


#is the variable imbalanced?
table(data_Alk_ja_nein$Alkohol_ja_nein)  
max(table(data_Alk_ja_nein$Alkohol_ja_nein)/sum(table(data_Alk_ja_nein$Alkohol_ja_nein)))  



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Alk_ja_nein$Alkohol_ja_nein, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfAlk_ja_nein <- data_Alk_ja_nein[index,]
test_dfAlk_ja_nein <- data_Alk_ja_nein[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelAlk_ja_nein <- train(Alkohol_ja_nein ~ ., 
                          data=train_dfAlk_ja_nein,
                          tuneGrid = myGrid,
                          method="ranger",
                          metric= "ROC",  
                          na.action = na.omit,
                          num.tree = 500,
                          trControl = myControl, 
                          importance = 'impurity')

# Print model to console

modelAlk_ja_nein
summary(modelAlk_ja_nein)
plot(modelAlk_ja_nein)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlk_ja_nein, newdata=test_dfAlk_ja_nein)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfAlk_ja_nein$Alkohol_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlk_ja_nein$Alkohol_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelAlk_ja_nein %>%
  test_roc(data = test_dfAlk_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAlk_ja_nein)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlk_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_Alk_janein <- modelAlk_ja_nein
saveRDS(tree500_Alk_janein, "./tree500_Alk_janein.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelAlk_ja_nein1 <- train(Alkohol_ja_nein ~ ., 
                           data=train_dfAlk_ja_nein,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", 
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console
modelAlk_ja_nein1
summary(modelAlk_ja_nein1)
plot(modelAlk_ja_nein1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlk_ja_nein1, newdata=test_dfAlk_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfAlk_ja_nein$Alkohol_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlk_ja_nein$Alkohol_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelAlk_ja_nein1 %>%
  test_roc(data = test_dfAlk_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAlk_ja_nein1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlk_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Alk_janein <- modelAlk_ja_nein1
saveRDS(tree1000_Alk_janein, "./tree1000_Alk_janein.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
modelAlk_ja_neinfinal <- modelAlk_ja_nein

# Print model
print(modelAlk_ja_neinfinal)

#output in terms of regression coefficients
summary(modelAlk_ja_neinfinal)

#evaluate variable importance 
varImp(modelAlk_ja_neinfinal)
plot(varImp(modelAlk_ja_neinfinal), 20, main = "Alkohol_ja_nein")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelAlk_ja_neinfinal, newdata=test_dfAlk_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfAlk_ja_nein$Alkohol_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlk_ja_nein$Alkohol_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelAlk_ja_neinfinal %>%
  test_roc(data = test_dfAlk_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelAlk_ja_neinfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlk_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAlk_ja_neinfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelAlk_ja_neinfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Alk_janein <- modelAlk_ja_neinfinal
saveRDS(besttree_Alk_janein, "./besttree_Alk_janein.rds")

#load the model

besttree_Alk_janein <- readRDS("./besttree_Alk_janein.rds")
print(besttree_Alk_janein)




###################
#Cigarette Group: Frequency (Categorical)
###################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Zigarettengruppe <- data[,c(320, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Zigarettengruppe$Zigarettengruppe))
data_Zigarettengruppe <- data_Zigarettengruppe %>% subset(data_Zigarettengruppe$Zigarettengruppe != "NA")


#is the variable imbalanced?
table(data_Zigarettengruppe$Zigarettengruppe) 
max(table(data_Zigarettengruppe$Zigarettengruppe)/sum(table(data_Zigarettengruppe$Zigarettengruppe)))  

#IV als Faktor:
data_Zigarettengruppe$Zigarettengruppe <- as.factor(data_Zigarettengruppe$Zigarettengruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Zigarettengruppe$Zigarettengruppe, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfZigarettengruppe <- data_Zigarettengruppe[index,]
test_dfZigarettengruppe <- data_Zigarettengruppe[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

#set random seed again 

set.seed(1997)
RFZigarettengruppe <- train(Zigarettengruppe ~ ., 
                            data=train_dfZigarettengruppe,
                            tuneGrid = myGrid,
                            method="ranger", 
                            metric= "Kappa",
                            num.tree = 500,
                            trControl = myControl1, 
                            na.action = na.omit,
                            importance = 'impurity')

# Print models to console

RFZigarettengruppe
summary(RFZigarettengruppe)
plot(RFZigarettengruppe)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFZigarettengruppe, newdata=test_dfZigarettengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfZigarettengruppe$Zigarettengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfZigarettengruppe$Zigarettengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFZigarettengruppe %>%
  test_roc(data = test_dfZigarettengruppe) %>%
  auc()

#save model to disk 

tree500_Zigarettengruppe <- RFZigarettengruppe
saveRDS(tree500_Zigarettengruppe, "./tree500_Zigarettengruppe.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#build 1000 trees --> the more the better?

set.seed(1997)
RFZigarettengruppe1 <- train(Zigarettengruppe ~ ., 
                             data=train_dfZigarettengruppe, 
                             method="ranger", metric= "Kappa",
                             tuneGrid = myGrid,
                             na.action = na.omit,
                             num.tree = 1000,
                             trControl = myControl1, 
                             importance = 'impurity')

# Print models
RFZigarettengruppe1
summary(RFZigarettengruppe1)
plot(RFZigarettengruppe1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFZigarettengruppe1, newdata=test_dfZigarettengruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfZigarettengruppe$Zigarettengruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfZigarettengruppe$Zigarettengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFZigarettengruppe1 %>%
  test_roc(data = test_dfZigarettengruppe) %>%
  auc()

#save model to disk 

tree1000_Zigarettengruppe <- RFZigarettengruppe1
saveRDS(tree1000_Zigarettengruppe, "./tree1000_Zigarettengruppe.rds")




####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFZigarettengruppefinal <- RFZigarettengruppe

# Print models
RFZigarettengruppefinal
summary(RFZigarettengruppefinal)

#evaluate variable importance 
varImp(RFZigarettengruppefinal)
plot(varImp(RFZigarettengruppefinal), 20, main = "Zigarettengruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFZigarettengruppefinal, newdata=test_dfZigarettengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfZigarettengruppe$Zigarettengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfZigarettengruppe$Zigarettengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFZigarettengruppefinal %>%
  test_roc(data = test_dfZigarettengruppe) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFZigarettengruppefinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFZigarettengruppefinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main= "Niedrig")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main= "Hoch")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Zigarettengruppe <- RFZigarettengruppefinal
saveRDS(besttree_Zigarettengruppe, "./besttree_Zigarettengruppe.rds")

#load the model

besttree_Zigarettengruppe <- readRDS("./besttree_Zigarettengruppe.rds")
print(besttree_Zigarettengruppe)




###################
#Cigarette Consumption yes/no (binary)
###################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Zig_ja_nein <- data[,c(321, 27:255)]

data_Zig_ja_nein$Zigaretten_ja_nein <- as.factor(data_Zig_ja_nein$Zigaretten_ja_nein)

#Are there NAs in the DV?
sum(is.na(data_Zig_ja_nein$Zigaretten_ja_nein))  
data_Zig_ja_nein <- data_Zig_ja_nein %>% subset(data_Zig_ja_nein$Zigaretten_ja_nein != "NA")


#is the variable imbalanced?
table(data_Zig_ja_nein$Zigaretten_ja_nein)  
max(table(data_Zig_ja_nein$Zigaretten_ja_nein)/sum(table(data_Zig_ja_nein$Zigaretten_ja_nein)))  


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Zig_ja_nein$Zigaretten_ja_nein, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfZig_ja_nein <- data_Zig_ja_nein[index,]
test_dfZig_ja_nein <- data_Zig_ja_nein[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelZig_ja_nein <- train(Zigaretten_ja_nein ~ ., 
                          data=train_dfZig_ja_nein,
                          tuneGrid = myGrid,
                          method="ranger",
                          metric= "ROC",  
                          na.action = na.omit,
                          num.tree = 500,
                          trControl = myControl, 
                          importance = 'impurity')

# Print model to console

modelZig_ja_nein
summary(modelZig_ja_nein)
plot(modelZig_ja_nein)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelZig_ja_nein, newdata=test_dfZig_ja_nein)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfZig_ja_nein$Zigaretten_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfZig_ja_nein$Zigaretten_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelZig_ja_nein %>%
  test_roc(data = test_dfZig_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelZig_ja_nein)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfZig_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_Zigaretten_janein <- modelZig_ja_nein
saveRDS(tree500_Zigaretten_janein, "./tree500_Zigaretten_janein.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!
set.seed(1997)
modelZig_ja_nein1 <- train(Zigaretten_ja_nein ~ ., 
                           data=train_dfZig_ja_nein,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", 
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console
modelZig_ja_nein1
summary(modelZig_ja_nein1)
plot(modelZig_ja_nein1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelZig_ja_nein1, newdata=test_dfZig_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfZig_ja_nein$Zigaretten_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfZig_ja_nein$Zigaretten_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelZig_ja_nein1 %>%
  test_roc(data = test_dfZig_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelZig_ja_nein1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfZig_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Zigaretten_janein <- modelZig_ja_nein1
saveRDS(tree1000_Zigaretten_janein, "./tree1000_Zigaretten_janein.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
modelZig_ja_neinfinal <- modelZig_ja_nein1


# Print model
print(modelZig_ja_neinfinal)

#output in terms of regression coefficients
summary(modelZig_ja_neinfinal)

#evaluate variable importance 
varImp(modelZig_ja_neinfinal)
plot(varImp(modelZig_ja_neinfinal), 20, main = "Zigaretten_ja_nein")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelZig_ja_neinfinal, newdata=test_dfZig_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfZig_ja_nein$Zigaretten_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfZig_ja_nein$Zigaretten_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelZig_ja_neinfinal %>%
  test_roc(data = test_dfZig_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelZig_ja_neinfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfZig_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelZig_ja_neinfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelZig_ja_neinfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Zigaretten_janein <- modelZig_ja_neinfinal
saveRDS(besttree_Zigaretten_janein, "./besttree_Zigaretten_janein.rds")

#load the model

besttree_Zigaretten_janein <- readRDS("./besttree_Zigaretten_janein.rds")
print(besttree_Zigaretten_janein)




######################
#Drug Group: Frequency (Categorical)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Drogengruppe <- data[,c(322, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Drogengruppe$Drogengruppe))
data_Drogengruppe <- data_Drogengruppe %>% subset(data_Drogengruppe$Drogengruppe != "NA")


#is the variable imbalanced?
table(data_Drogengruppe$Drogengruppe) 
max(table(data_Drogengruppe$Drogengruppe)/sum(table(data_Drogengruppe$Drogengruppe)))  

#IV als Faktor:
data_Drogengruppe$Drogengruppe <- as.factor(data_Drogengruppe$Drogengruppe)



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Drogengruppe$Drogengruppe, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfDrogengruppe <- data_Drogengruppe[index,]
test_dfDrogengruppe <- data_Drogengruppe [-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
RFDrogengruppe <- train(Drogengruppe ~ ., 
                        data=train_dfDrogengruppe,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "Kappa",
                        num.tree = 500,
                        trControl = myControl1, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print models to console

RFDrogengruppe
summary(RFDrogengruppe)
plot(RFDrogengruppe)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFDrogengruppe, newdata=test_dfDrogengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfDrogengruppe$Drogengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfDrogengruppe$Drogengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFDrogengruppe %>%
  test_roc(data = test_dfDrogengruppe) %>%
  auc()

#save model to disk 

tree500_Drogengruppe <- RFDrogengruppe
saveRDS(tree500_Drogengruppe, "./tree500_Drogengruppe.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#build 1000 trees --> the more the better?

set.seed(1997)
RFDrogengruppe1 <- train(Drogengruppe ~ ., 
                         data=train_dfDrogengruppe, 
                         method="ranger", metric= "Kappa",
                         tuneGrid = myGrid,
                         na.action = na.omit,
                         num.tree = 1000,
                         trControl = myControl1, 
                         importance = 'impurity')

# Print models
RFDrogengruppe1
summary(RFDrogengruppe1)
plot(RFDrogengruppe1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDrogengruppe1, newdata=test_dfDrogengruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDrogengruppe$Drogengruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfDrogengruppe$Drogengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFDrogengruppe1 %>%
  test_roc(data = test_dfDrogengruppe) %>%
  auc()


#save model to disk 

tree1000_Drogengruppe <- RFDrogengruppe1
saveRDS(tree1000_Drogengruppe, "./tree1000_Drogengruppe.rds")



####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFDrogengruppefinal <- RFDrogengruppe

# Print models
RFDrogengruppefinal
summary(RFDrogengruppefinal)

#evaluate variable importance 
varImp(RFDrogengruppefinal)
plot(varImp(RFDrogengruppefinal), 20, main = "Drogengruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFDrogengruppefinal, newdata=test_dfDrogengruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfDrogengruppe$Drogengruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfDrogengruppe$Drogengruppe,
                 predict(model, data, type = "prob")[, "kein_Konsum"])
  
}

RFDrogengruppefinal %>%
  test_roc(data = test_dfDrogengruppe) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFDrogengruppefinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFDrogengruppefinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main= "Niedrig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main= "Niedrig")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "kein_Konsum") %>%plotPartial(main= "kein_Konsum")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main= "Hoch")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main= "Hoch")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Drogengruppe <- RFDrogengruppefinal
saveRDS(besttree_Drogengruppe, "./besttree_Drogengruppe.rds")

#load the model

besttree_Drogengruppe <- readRDS("./besttree_Drogengruppe.rds")
print(besttree_Drogengruppe)




#########################
#Drug Consumption yes/no (binary)
########################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

data_Drogen_ja_nein <- data[,c(323, 27:255)]

data_Drogen_ja_nein$Drogen_ja_nein <- as.factor(data_Drogen_ja_nein$Drogen_ja_nein)

#Are there NAs in the DV?
sum(is.na(data_Drogen_ja_nein$Drogen_ja_nein))  
data_Drogen_ja_nein <- data_Drogen_ja_nein %>% subset(data_Drogen_ja_nein$Drogen_ja_nein != "NA")

#is the variable imbalanced?
table(data_Drogen_ja_nein$Drogen_ja_nein)  
max(table(data_Drogen_ja_nein$Drogen_ja_nein)/sum(table(data_Drogen_ja_nein$Drogen_ja_nein)))  


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Drogen_ja_nein$Drogen_ja_nein, p=.8, list= FALSE, times= 1)

# Create train_dfDrogen_ja_nein & test_dfDrogen_ja_nein
train_dfDrogen_ja_nein <- data_Drogen_ja_nein[index,]
test_dfDrogen_ja_nein <- data_Drogen_ja_nein[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelDrogen_ja_nein <- train(Drogen_ja_nein ~ ., 
                             data=train_dfDrogen_ja_nein,
                             tuneGrid = myGrid,
                             method="ranger",
                             metric= "ROC",  
                             na.action = na.omit,
                             num.tree = 500,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console
modelDrogen_ja_nein
summary(modelDrogen_ja_nein)
plot(modelDrogen_ja_nein)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelDrogen_ja_nein, newdata=test_dfDrogen_ja_nein)

# Create confusion matrix   
confusionMatrix(data=predictions, test_dfDrogen_ja_nein$Drogen_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfDrogen_ja_nein$Drogen_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelDrogen_ja_nein %>%
  test_roc(data = test_dfDrogen_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelDrogen_ja_nein)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDrogen_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Drogen_janein <- modelDrogen_ja_nein
saveRDS(tree500_Drogen_janein, "./tree500_Drogen_janein.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelDrogen_ja_nein1 <- train(Drogen_ja_nein ~ ., 
                              data=train_dfDrogen_ja_nein,
                              tuneGrid = myGrid,
                              method="ranger", 
                              metric= "ROC", 
                              na.action = na.omit,
                              num.tree = 1000,
                              trControl = myControl, 
                              importance = 'impurity')

# Print model to console

modelDrogen_ja_nein1
summary(modelDrogen_ja_nein1)
plot(modelDrogen_ja_nein1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelDrogen_ja_nein1, newdata=test_dfDrogen_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfDrogen_ja_nein$Drogen_ja_nein)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfDrogen_ja_nein$Drogen_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelDrogen_ja_nein1 %>%
  test_roc(data = test_dfDrogen_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelDrogen_ja_nein1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDrogen_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_Drogen_janein <- modelDrogen_ja_nein1
saveRDS(tree1000_Drogen_janein, "./tree1000_Drogen_janein.rds")


####-------tree 3: Final --------------------------------------------------

#define final model
set.seed(1997)
modelDrogen_ja_neinfinal <- modelDrogen_ja_nein

# Print model
print(modelDrogen_ja_neinfinal)

#output in terms of regression coefficients
summary(modelDrogen_ja_neinfinal)

#evaluate variable importance 
varImp(modelDrogen_ja_neinfinal)
plot(varImp(modelDrogen_ja_neinfinal), 20, main = "Drogen_ja_nein")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelDrogen_ja_neinfinal, newdata=test_dfDrogen_ja_nein)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfDrogen_ja_nein$Drogen_ja_nein)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfDrogen_ja_nein$Drogen_ja_nein,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelDrogen_ja_neinfinal %>%
  test_roc(data = test_dfDrogen_ja_nein) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelDrogen_ja_neinfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDrogen_ja_nein)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelDrogen_ja_neinfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelDrogen_ja_neinfinal

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Drogen_janein <- modelDrogen_ja_neinfinal
saveRDS(besttree_Drogen_janein, "./besttree_Drogen_janein.rds")

#load the model

besttree_Drogen_janein <- readRDS("./besttree_Drogen_janein.rds")
print(besttree_Drogen_janein)



#######################
#Parties: categorical
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Partei <- data[,c(7, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Partei$Wahl_Partei))  
#"Others" as NA to remove them from analysis
data_Partei <- data_Partei %>% replace_with_na_all(condition = ~.x == "Sonstige:")
#Dataset without NAs
data_Partei <- data_Partei %>% subset(data_Partei$Wahl_Partei != "NA")


#is the variable imbalanced?
table(data_Partei$Wahl_Partei) 
max(table(data_Partei$Wahl_Partei)/sum(table(data_Partei$Wahl_Partei))) 

#IV als Faktor:
data_Partei$Wahl_Partei <- as.factor(data_Partei$Wahl_Partei)

#Variablennamen anpassen für Analyse
data_Partei <- data_Partei %>% mutate(Wahl_Partei = case_when(Wahl_Partei == "CDU/CSU" ~ 'CDU_CSU',
                                                              Wahl_Partei == "SPD" ~ 'SPD',
                                                              Wahl_Partei == "Bündnis 90/Die Grünen" ~ 'Die_Gruenen',
                                                              Wahl_Partei == "FDP" ~ 'FDP',
                                                              Wahl_Partei == "AfD" ~ 'AfD',
                                                              Wahl_Partei == "Die Linke" ~ 'Die_Linke',
                                                              Wahl_Partei == "Die Partei" ~ 'Die_Partei',
                                                              Wahl_Partei == "Ich würde nicht wählen gehen" ~ 'Nichtwaehler'))



#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Partei$Wahl_Partei, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfPartei <- data_Partei[index,]
test_dfPartei <- data_Partei[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation 

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFPartei_1 <- train(Wahl_Partei ~ ., 
                    data=train_dfPartei,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFPartei_1
summary(RFPartei_1)
plot(RFPartei_1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFPartei_1, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfPartei$Wahl_Partei))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model auc
RFPartei_1 %>%
  test_roc(data = test_dfPartei) %>%
  auc()

#save model to disk 

tree500_WahlPartei <- RFPartei_1
saveRDS(tree500_WahlPartei, "./tree500_WahlPartei.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

set.seed(1997)
RFPartei_2 <- train(Wahl_Partei ~ ., 
                    data=train_dfPartei, 
                    method="ranger", metric= "Kappa",
                    tuneGrid = myGrid,
                    na.action = na.omit,
                    num.tree = 1000,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models
RFPartei_2
summary(RFPartei_2)
plot(RFPartei_2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFPartei_2, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfPartei$Wahl_Partei))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model auc
RFPartei_2 %>%
  test_roc(data = test_dfPartei) %>%
  auc()


#save model to disk 

tree1000_WahlPartei <- RFPartei_2
saveRDS(tree1000_WahlPartei, "./tree1000_WahlPartei.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFPartei_fin <- RFPartei_2

# Print models
RFPartei_fin
summary(RFPartei_fin)

#evaluate variable importance 
varImp(RFPartei_fin)
plot(varImp(RFPartei_fin), 20, main = "Wahl_Partei")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFPartei_fin, newdata=test_dfPartei)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfPartei$Wahl_Partei))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfPartei$Wahl_Partei,
                 predict(model, data, type = "prob")[, "Nichtwaehler"])
  
}

#model AUC: 
RFPartei_fin %>%
  test_roc(data = test_dfPartei) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFPartei_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFPartei_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "AfD") %>%plotPartial(main = "AfD")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "AfD") %>%plotPartial(main = "AfD")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "CDU_CSU") %>%plotPartial(main = "CDU/CSU")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Gruenen") %>%plotPartial(main = "Die Grünen")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Linke") %>%plotPartial(main = "Die Linke")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Die_Partei") %>%plotPartial(main = "Die Partei")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "FDP") %>%plotPartial(main = "FDP")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "FDP") %>%plotPartial(main = "FDP")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Nichtwaehler") %>%plotPartial(main = "Nichtwähler")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "SPD") %>%plotPartial(main = "SPD")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "SPD") %>%plotPartial(main = "SPD")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Partei <- RFPartei_fin
saveRDS(besttree_Partei, "./besttree_Partei.rds")

#load the model

besttree_Partei <- readRDS("./besttree_Partei.rds")
print(besttree_Partei)





#######################
#AfD: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_AfD <- data[,c(341, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_AfD$AfD_Waehler))  
data_AfD <- data_AfD %>% subset(data_AfD$AfD_Waehler != "NA")


#is the variable imbalanced?
table(data_AfD$AfD_Waehler)  
max(table(data_AfD$AfD_Waehler)/sum(table(data_AfD$AfD_Waehler)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AfD$AfD_Waehler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAfD <- data_AfD[index,]
test_dfAfD <- data_AfD[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
RfAfD_1 <- train(AfD_Waehler ~ ., 
                 data=train_dfAfD,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RfAfD_1
summary(RfAfD_1)
plot(RfAfD_1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RfAfD_1, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RfAfD_1 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


# ROC plot
model_list <- list(Model1 = RfAfD_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAfD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_AfD <- RfAfD_1
saveRDS(tree500_AfD, "./tree500_AfD.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

#set random seed again 

set.seed(1997)
RfAfD_2 <- train(AfD_Waehler ~ ., 
                 data=train_dfAfD,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RfAfD_2
summary(RfAfD_2)
plot(RfAfD_2)


# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RfAfD_2, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfAfD$AfD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RfAfD_2 %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model2 = RfAfD_2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAfD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_AfD <- RfAfD_2
saveRDS(tree1000_AfD, "./tree1000_AfD.rds")



####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RFAfD_fin <- RfAfD_1

# Print models
RFAfD_fin
summary(RFAfD_fin)

#evaluate variable importance 
varImp(RFAfD_fin)
plot(varImp(RFAfD_fin), 20, main = "Afd_Waehler")

# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFAfD_fin, newdata=test_dfAfD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfAfD$AfD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfAfD$AfD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFAfD_fin %>%
  test_roc(data = test_dfAfD) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RfAfD_1,
                   Model2 = RfAfD_2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAfD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFAfD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFAfD_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "AfD Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_AfD <- RFAfD_fin
saveRDS(besttree_AfD, "./besttree_AfD.rds")

#load the model

Tree_AfD <- readRDS("./besttree_AfD.rds")
print(Tree_AfD)




#######################
#Die Grünen: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Gruen <- data[,c(339, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Gruen$Gruene_Waehler))  
data_Gruen <- data_Gruen %>% subset(data_Gruen$Gruene_Waehler != "NA")


#is the variable imbalanced?
table(data_Gruen$Gruene_Waehler) 
max(table(data_Gruen$Gruene_Waehler)/sum(table(data_Gruen$Gruene_Waehler))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Gruen$Gruene_Waehler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfGruen <- data_Gruen[index,]
test_dfGruen <- data_Gruen[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_Gruene1 <- train(Gruene_Waehler ~ ., 
                    data=train_dfGruen,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RF_Gruene1
summary(RF_Gruene1)
plot(RF_Gruene1)


# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Gruene1, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGruen$Gruene_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RF_Gruene1 %>%
  test_roc(data = test_dfGruen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RF_Gruene1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGruen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_Gruene <- RF_Gruene1
saveRDS(tree500_Gruene, "./tree500_Gruene.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

#set random seed again 
set.seed(1997)
RF_Gruene2 <- train(Gruene_Waehler ~ ., 
                    data=train_dfGruen,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RF_Gruene2
summary(RF_Gruene2)
plot(RF_Gruene2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Gruene2, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGruen$Gruene_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_Gruene2 %>%
  test_roc(data = test_dfGruen) %>%
  auc()


#ROC plot
model_list <- list(Model2 = RF_Gruene2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGruen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Gruene <- RF_Gruene2
saveRDS(tree1000_Gruene, "./tree1000_Gruene.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFGruene_fin <- RF_Gruene1

# Print models
RFGruene_fin
summary(RFGruene_fin)


#evaluate variable importance 
varImp(RFGruene_fin)
plot(varImp(RFGruene_fin), 20, main = "Gruene_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFGruene_fin, newdata=test_dfGruen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGruen$Gruene_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGruen$Gruene_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RFGruene_fin %>%
  test_roc(data = test_dfGruen) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Gruene1,
                   Model2 = RF_Gruene2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGruen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFGruene_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGruene_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "Grüne Wähler")

#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Grün <- RFGruene_fin
saveRDS(besttree_Grün, "./besttree_Grün.rds")

#load the model

besttree_Grün <- readRDS("./besttree_Grün.rds")
print(besttree_Grün)



#######################
#CDU/CSU: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_CDU <- data[,c(337, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_CDU$CDU_CSU_Waehler))  
data_CDU <- data_CDU %>% subset(data_CDU$CDU_CSU_Waehler != "NA")
data_CDU$CDU_CSU_Waehler <- as.factor(data_CDU$CDU_CSU_Waehler)


#is the variable imbalanced?
table(data_CDU$CDU_CSU_Waehler) 
max(table(data_CDU$CDU_CSU_Waehler)/sum(table(data_CDU$CDU_CSU_Waehler))) 

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_CDU$CDU_CSU_Waehler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfCDU <- data_CDU[index,]
test_dfCDU <- data_CDU[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)

####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_CDU1 <- train(CDU_CSU_Waehler ~ ., 
                 data=train_dfCDU,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console
RF_CDU1
summary(RF_CDU1)
plot(RF_CDU1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_CDU1, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfCDU$CDU_CSU_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_CDU1 %>%
  test_roc(data = test_dfCDU) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RF_CDU1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfCDU)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_CDU <- RF_CDU1
saveRDS(tree500_CDU, "./tree500_CDU.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

set.seed(1997)
RF_CDU2 <- train(CDU_CSU_Waehler ~ ., 
                 data=train_dfCDU,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_CDU2
summary(RF_CDU2)
plot(RF_CDU2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_CDU2, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfCDU$CDU_CSU_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_CDU2 %>%
  test_roc(data = test_dfCDU) %>%
  auc()


#ROC plot
model_list <- list(Model2 = RF_CDU2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfCDU)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_CDU <- RF_CDU2
saveRDS(tree1000_CDU, "./tree1000_CDU.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RF_CDU_fin <- RF_CDU2

# Print models
RF_CDU_fin
summary(RF_CDU_fin)

#evaluate variable importance 
varImp(RF_CDU_fin)
plot(varImp(RF_CDU_fin), 20, main = "CDU_CSU_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_CDU_fin, newdata=test_dfCDU)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfCDU$CDU_CSU_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfCDU$CDU_CSU_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RF_CDU_fin %>%
  test_roc(data = test_dfCDU) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_CDU1,
                   Model2 = RF_CDU2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfCDU)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RF_CDU_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_CDU_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "CDU Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_CDU <- RF_CDU_fin
saveRDS(besttree_CDU, "./besttree_CDU.rds")

#load the model

besttree_CDU <- readRDS("./besttree_CDU.rds")
print(besttree_CDU)




#######################
#Die Linke: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Linke <- data[,c(342, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Linke$Linke_Waehler)) 
data_Linke <- data_Linke %>% subset(data_Linke$Linke_Waehler != "NA")
data_Linke$Linke_Waehler <- as.factor(data_Linke$Linke_Waehler)


#is the variable imbalanced?
table(data_Linke$Linke_Waehler) 
max(table(data_Linke$Linke_Waehler)/sum(table(data_Linke$Linke_Waehler))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Linke$Linke_Waehler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfLinke <- data_Linke[index,]
test_dfLinke <- data_Linke[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_Linke1 <- train(Linke_Waehler ~ ., 
                   data=train_dfLinke,
                   tuneGrid = myGrid,
                   method="ranger", 
                   metric= "ROC",
                   num.tree = 500,
                   na.action = na.omit,
                   trControl = myControl1, 
                   importance = 'impurity')

# Print models to console

RF_Linke1
summary(RF_Linke1)
plot(RF_Linke1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Linke1, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfLinke$Linke_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_Linke1 %>%
  test_roc(data = test_dfLinke) %>%
  auc()


#ROC plos
model_list <- list(Model1 = RF_Linke1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLinke)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Linke <- RF_Linke1
saveRDS(tree500_Linke, "./tree500_Linke.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

set.seed(1997)
RF_Linke2 <- train(Linke_Waehler ~ ., 
                   data=train_dfLinke,
                   tuneGrid = myGrid,
                   method="ranger", 
                   metric= "ROC",
                   num.tree = 1000,
                   na.action = na.omit,
                   trControl = myControl1, 
                   importance = 'impurity')

# Print models to console

RF_Linke2
summary(RF_Linke2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Linke2, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfLinke$Linke_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_Linke2 %>%
  test_roc(data = test_dfLinke) %>%
  auc()


#ROC plot
model_list <- list(Model2 = RF_Linke2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLinke)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Linke <- RF_Linke2
saveRDS(tree1000_Linke, "./tree1000_Linke.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RF_Linke_fin <- RF_Linke2


# Print models
RF_Linke_fin
summary(RF_Linke_fin)

#evaluate variable importance 
varImp(RF_Linke_fin)
plot(varImp(RF_Linke_fin), 20, main = "Linke_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_Linke_fin, newdata=test_dfLinke)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfLinke$Linke_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLinke$Linke_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RF_Linke_fin %>%
  test_roc(data = test_dfLinke) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Linke1,
                   Model2 = RF_Linke2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLinke)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RF_Linke_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_Linke_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "Linke Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Linke <- RF_Linke_fin
saveRDS(besttree_Linke, "./besttree_Linke.rds")

#load the model

besttree_Linke <- readRDS("./besttree_Linke.rds")
print(besttree_Linke)




#######################
#SPD: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_SPD <- data[,c(338, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_SPD$SPD_Waehler)) 
data_SPD <- data_SPD %>% subset(data_SPD$SPD_Waehler != "NA")
data_SPD$SPD_Waehler <- as.factor(data_SPD$SPD_Waehler)


#is the variable imbalanced?
table(data_SPD$SPD_Waehler) 
max(table(data_SPD$SPD_Waehler)/sum(table(data_SPD$SPD_Waehler))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_SPD$SPD_Waehler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfSPD <- data_SPD[index,]
test_dfSPD <- data_SPD[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)

####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_SPD1 <- train(SPD_Waehler ~ ., 
                 data=train_dfSPD,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_SPD1
summary(RF_SPD1)
plot(RF_SPD1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_SPD1, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSPD$SPD_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_SPD1 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RF_SPD1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSPD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_SPD <- RF_SPD1
saveRDS(tree500_SPD, "./tree500_SPD.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

#set random seed again 

set.seed(1997)
RF_SPD2 <- train(SPD_Waehler ~ ., 
                 data=train_dfSPD,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_SPD2
summary(RF_SPD2)
plot(RF_SPD2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_SPD2, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSPD$SPD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_SPD2 %>%
  test_roc(data = test_dfSPD) %>%
  auc()


# ROC plot
model_list <- list(Model2 = RF_SPD2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSPD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_SPD <- RF_SPD2
saveRDS(tree1000_SPD, "./tree1000_SPD.rds")


####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)
RF_SPD_fin <- RF_SPD2

# Print models
RF_SPD_fin
summary(RF_SPD_fin)


#evaluate variable importance 
varImp(RF_SPD_fin)
plot(varImp(RF_SPD_fin), 20, main = "SPD_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_SPD_fin, newdata=test_dfSPD)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSPD$SPD_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSPD$SPD_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_SPD_fin %>%
  test_roc(data = test_dfSPD) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_SPD1,
                   Model2 = RF_SPD2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSPD)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RF_SPD_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_SPD_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "SPD Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_SPD <- RF_SPD_fin
saveRDS(besttree_SPD, "./besttree_SPD.rds")

#load the model

besttree_SPD <- readRDS("./besttree_SPD.rds")
print(besttree_SPD)




#######################
#FDP: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_FDP <- data[,c(340, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_FDP$FDP_Waehler)) 
data_FDP <- data_FDP %>% subset(data_FDP$FDP_Waehler != "NA")
data_FDP$FDP_Waehler <- as.factor(data_FDP$FDP_Waehler)


#is the variable imbalanced?
table(data_FDP$FDP_Waehler) 
max(table(data_FDP$FDP_Waehler)/sum(table(data_FDP$FDP_Waehler))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_FDP$FDP_Waehler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfFDP <- data_FDP[index,]
test_dfFDP <- data_FDP[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_FDP1 <- train(FDP_Waehler ~ ., 
                 data=train_dfFDP,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 500,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_FDP1
summary(RF_FDP1)
plot(RF_FDP1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_FDP1, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfFDP$FDP_Waehler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_FDP1 %>%
  test_roc(data = test_dfFDP) %>%
  auc()


# ROC plot
model_list <- list(Model1 = RF_FDP1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfFDP)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_FDP <- RF_FDP1
saveRDS(tree500_FDP, "./tree500_FDP.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

#set random seed again 
set.seed(1997)
RF_FDP2 <- train(FDP_Waehler ~ ., 
                 data=train_dfFDP,
                 tuneGrid = myGrid,
                 method="ranger", 
                 metric= "ROC",
                 num.tree = 1000,
                 na.action = na.omit,
                 trControl = myControl1, 
                 importance = 'impurity')

# Print models to console

RF_FDP2
summary(RF_FDP2)
plot(RF_FDP2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_FDP2, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfFDP$FDP_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_FDP2 %>%
  test_roc(data = test_dfFDP) %>%
  auc()


#ROC plot
model_list <- list(Model2 = RF_FDP2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfFDP)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_FDP <- RF_FDP2
saveRDS(tree1000_FDP, "./tree1000_FDP.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RF_FDP_fin <- RF_FDP2


# Print models
RF_FDP_fin
summary(RF_FDP_fin)


#evaluate variable importance 
varImp(RF_FDP_fin)
plot(varImp(RF_FDP_fin), 20, main = "FDP_Waehler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_FDP_fin, newdata=test_dfFDP)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfFDP$FDP_Waehler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfFDP$FDP_Waehler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RF_FDP_fin %>%
  test_roc(data = test_dfFDP) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_FDP1,
                   Model2 = RF_FDP2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfFDP)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RF_FDP_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_FDP_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "FDP Wähler")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_FDP <- RF_FDP_fin
saveRDS(besttree_FDP, "./besttree_FDP.rds")

#load the model

besttree_FDP <- readRDS("./besttree_FDP.rds")
print(besttree_FDP)





#######################
#Non-voters: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Nichtwahler <- data[,c(343, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Nichtwahler$Nichtwahler))
data_Nichtwahler <- data_Nichtwahler %>% subset(data_Nichtwahler$Nichtwahler != "NA")
data_Nichtwahler$Nichtwahler <- as.factor(data_Nichtwahler$Nichtwahler)


#is the variable imbalanced?
table(data_Nichtwahler$Nichtwahler) 
max(table(data_Nichtwahler$Nichtwahler)/sum(table(data_Nichtwahler$Nichtwahler))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Nichtwahler$Nichtwahler, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfNichtwahler <- data_Nichtwahler[index,]
test_dfNichtwahler <- data_Nichtwahler[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RF_Nichtwahler1 <- train(Nichtwahler ~ ., 
                         data=train_dfNichtwahler,
                         tuneGrid = myGrid,
                         method="ranger", 
                         metric= "ROC",
                         num.tree = 500,
                         na.action = na.omit,
                         trControl = myControl1, 
                         importance = 'impurity')

# Print models to console

RF_Nichtwahler1
summary(RF_Nichtwahler1)
plot(RF_Nichtwahler1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RF_Nichtwahler1, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfNichtwahler$Nichtwahler))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_Nichtwahler1 %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()


# ROC plot
model_list <- list(Model1 = RF_Nichtwahler1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfNichtwahler)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Nichtwahler <- RF_Nichtwahler1
saveRDS(tree500_Nichtwahler, "./tree500_Nichtwahler.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

 #build 1000 trees --> the more the better?

#set random seed again 
set.seed(1997)
RF_Nichtwahler2 <- train(Nichtwahler ~ ., 
                         data=train_dfNichtwahler,
                         tuneGrid = myGrid,
                         method="ranger", 
                         metric= "ROC",
                         num.tree = 1000,
                         na.action = na.omit,
                         trControl = myControl1, 
                         importance = 'impurity')

# Print models to console

RF_Nichtwahler2
summary(RF_Nichtwahler2)
plot(RF_Nichtwahler2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RF_Nichtwahler2, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfNichtwahler$Nichtwahler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_Nichtwahler2 %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()


#ROC plot
model_list <- list(Model2 = RF_Nichtwahler2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfNichtwahler)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Nichtwahler <- RF_Nichtwahler2
saveRDS(tree1000_Nichtwahler, "./tree1000_Nichtwahler.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RF_Nichtwahler_fin <- RF_Nichtwahler2


# Print models
RF_Nichtwahler_fin
summary(RF_Nichtwahler_fin)


#evaluate variable importance 
varImp(RF_Nichtwahler_fin)
plot(varImp(RF_Nichtwahler_fin), 20, main = "Nichtwahler")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RF_Nichtwahler_fin, newdata=test_dfNichtwahler)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfNichtwahler$Nichtwahler))

#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfNichtwahler$Nichtwahler,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RF_Nichtwahler_fin %>%
  test_roc(data = test_dfNichtwahler) %>%
  auc()

#compare different ROC plots
model_list <- list(Model1 = RF_Nichtwahler1,
                   Model2 = RF_Nichtwahler2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfNichtwahler)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RF_Nichtwahler_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RF_Nichtwahler_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial(main = "Nichtwähler")

#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Nichtwahler <- RF_Nichtwahler_fin
saveRDS(besttree_Nichtwahler, "./besttree_Nichtwahler.rds")

#load the model

besttree_Nichtwahler <- readRDS("./besttree_Nichtwahler.rds")
print(besttree_Nichtwahler)





#######################
#Corona Hardliner: binary
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Hardliner <- data[,c(307, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Hardliner$Corona_Hardliner)) 
data_Hardliner <- data_Hardliner %>% subset(data_Hardliner$Corona_Hardliner != "NA")


#is the variable imbalanced?
table(data_Hardliner$Corona_Hardliner) 
max(table(data_Hardliner$Corona_Hardliner)/sum(table(data_Hardliner$Corona_Hardliner)))

#IV als Faktor:
data_Hardliner$Corona_Hardliner <- as.factor(data_Hardliner$Corona_Hardliner)


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Hardliner$Corona_Hardliner, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfHardliner <- data_Hardliner[index,]
test_dfHardliner <- data_Hardliner[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# no sampling needed

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size


set.seed(1997)
RFHardliner1 <- train(Corona_Hardliner ~ ., 
                      data=train_dfHardliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 500,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFHardliner1
summary(RFHardliner1)
plot(RFHardliner1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFHardliner1, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RFHardliner1 %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#check ROC plots
model_list <- list(Model1 = RFHardliner1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHardliner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree500_Corona_Hardliner <- RFHardliner1
saveRDS(tree500_Corona_Hardliner, "./tree500_Corona_Hardliner.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

#set random seed again 
set.seed(1997)
RFHardliner2 <- train(Corona_Hardliner ~ ., 
                      data=train_dfHardliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 1000,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFHardliner2
summary(RFHardliner2)
plot(RFHardliner2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFHardliner2, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFHardliner2 %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFHardliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHardliner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

# best model with 500

tree1000_Corona_Hardliner <- RFHardliner2
saveRDS(tree1000_Corona_Hardliner, "./tree1000_Corona_Hardliner.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFHardliner_fin <- RFHardliner1

# Print models
RFHardliner_fin
summary(RFHardliner_fin)


#evaluate variable importance 
varImp(RFHardliner_fin)
plot(varImp(RFHardliner_fin), 20, main = "Corona_Hardliner")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFHardliner_fin, newdata=test_dfHardliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfHardliner$Corona_Hardliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfHardliner$Corona_Hardliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFHardliner_fin %>%
  test_roc(data = test_dfHardliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFHardliner1,
                   Model2 = RFHardliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHardliner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFHardliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFHardliner_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Hardliner <- RFHardliner_fin
saveRDS(besttree_Hardliner, "./besttree_Hardliner_janein.rds")

#load the model

besttree_Hardliner <- readRDS("./besttree_Hardliner_janein.rds")
print(besttree_Hardliner)






#######################
#Corona Softliner: binary
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Softliner <- data[,c(308, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Softliner$Corona_Softliner)) 
data_Softliner <- data_Softliner %>% subset(data_Softliner$Corona_Softliner != "NA")


#is the variable imbalanced?
table(data_Softliner$Corona_Softliner) 
max(table(data_Softliner$Corona_Softliner)/sum(table(data_Softliner$Corona_Softliner)))

#IV als Faktor:
data_Softliner$Corona_Softliner <- as.factor(data_Softliner$Corona_Softliner)


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Softliner$Corona_Softliner, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSoftliner <- data_Softliner[index,]
test_dfSoftliner <- data_Softliner[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# apply smote sampling and 10-fold CV

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote",
  search = "grid"
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFSoftliner1 <- train(Corona_Softliner ~ ., 
                      data=train_dfSoftliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 500,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSoftliner1
summary(RFSoftliner1)
plot(RFSoftliner1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSoftliner1, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFSoftliner1 %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFSoftliner1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSoftliner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Corona_Softliner <- RFSoftliner1
saveRDS(tree500_Corona_Softliner, "./tree500_Corona_Softliner.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFSoftliner2 <- train(Corona_Softliner ~ ., 
                      data=train_dfSoftliner,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 1000,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSoftliner2
summary(RFSoftliner2)
plot(RFSoftliner2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSoftliner2, newdata=test_dfSoftliner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFSoftliner2 %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSoftliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSoftliner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_Corona_Softliner <- RFSoftliner2
saveRDS(tree1000_Corona_Softliner, "./tree1000_Corona_Softliner.rds")


####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)
RFSoftliner_fin <- RFSoftliner1

# Print models
RFSoftliner_fin
summary(RFSoftliner_fin)


#evaluate variable importance 
varImp(RFSoftliner_fin)
plot(varImp(RFSoftliner_fin), 20, main = "Corona_Softliner")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSoftliner_fin, newdata=test_dfSoftliner)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSoftliner$Corona_Softliner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSoftliner$Corona_Softliner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFSoftliner_fin %>%
  test_roc(data = test_dfSoftliner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSoftliner1,
                   Model2 = RFSoftliner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSoftliner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFSoftliner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFSoftliner_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Softliner <- RFSoftliner_fin
saveRDS(besttree_Softliner, "./besttree_Softlinerjanein.rds")

#load the model

RFSoftliner_fin <- readRDS("./besttree_Softlinerjanein.rds")
print(RFSoftliner_fin)






#######################
#Corona Sceptics: binary
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Skeptiker <- data[,c(309, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Skeptiker$Corona_Skeptiker))
data_Skeptiker <- data_Skeptiker %>% subset(data_Skeptiker$Corona_Skeptiker != "NA")


#is the variable imbalanced?
table(data_Skeptiker$Corona_Skeptiker) 
max(table(data_Skeptiker$Corona_Skeptiker)/sum(table(data_Skeptiker$Corona_Skeptiker))) 

#IV als Faktor:
data_Skeptiker$Corona_Skeptiker <- as.factor(data_Skeptiker$Corona_Skeptiker)


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Skeptiker$Corona_Skeptiker, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSkeptiker <- data_Skeptiker[index,]
test_dfSkeptiker <- data_Skeptiker[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# sample with smote

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test for 500 trees

set.seed(1997)
RFSkeptiker1 <- train(Corona_Skeptiker ~ ., 
                      data=train_dfSkeptiker,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 500,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSkeptiker1
summary(RFSkeptiker1)
plot(RFSkeptiker1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSkeptiker1, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFSkeptiker1 %>%
  test_roc(data = test_dfSkeptiker) %>%
  auc()


#check ROC plots
model_list <- list(Model1 = RFSkeptiker1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSkeptiker)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Corona_Skeptiker <- RFSkeptiker1
saveRDS(tree500_Corona_Skeptiker, "./tree500_Corona_Skeptiker.rds")

####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

#set random seed again 

set.seed(1997)
RFSkeptiker2 <- train(Corona_Skeptiker ~ ., 
                      data=train_dfSkeptiker,
                      tuneGrid = myGrid,
                      method="ranger", 
                      metric= "ROC",
                      num.tree = 1000,
                      na.action = na.omit,
                      trControl = myControl1, 
                      importance = 'impurity')

# Print models to console

RFSkeptiker2
summary(RFSkeptiker2)
plot(RFSkeptiker2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSkeptiker2, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFSkeptiker2 %>%
  test_roc(data = test_dfSkeptiker) %>%
  auc()


#ROC plots
model_list <- list(Model1 = RFSkeptiker2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSkeptiker)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

tree1000_Corona_Skeptiker <- RFSkeptiker2
saveRDS(tree1000_Corona_Skeptiker, "./tree1000_Corona_Skeptiker.rds")

####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)
RFSkeptiker_fin <- RFSkeptiker1

# Print models
RFSkeptiker_fin
summary(RFSkeptiker_fin)


#evaluate variable importance 
varImp(RFSkeptiker_fin)
plot(varImp(RFSkeptiker_fin), 20, main = "Corona_Skeptiker")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSkeptiker_fin, newdata=test_dfSkeptiker)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSkeptiker$Corona_Skeptiker))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfSkeptiker$Corona_Skeptiker,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFSkeptiker_fin %>%
  test_roc(data = test_dfSkeptiker) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFSkeptiker1,
                   Model2 = RFSkeptiker2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfSkeptiker)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFSkeptiker_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFSkeptiker_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Skeptiker <- RFSkeptiker_fin
saveRDS(besttree_Skeptiker, "./besttree_Skeptikerjanein.rds")

#load the model

besttree_Skeptiker <- readRDS("./besttree_Skeptikerjanein.rds")
print(besttree_Skeptiker)





#######################
#Corona Deniers: binary
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Leugner <- data[,c(310, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Leugner$Corona_Leugner))
data_Leugner <- data_Leugner %>% subset(data_Leugner$Corona_Leugner != "NA")


#is the variable imbalanced?
table(data_Leugner$Corona_Leugner) 
max(table(data_Leugner$Corona_Leugner)/sum(table(data_Leugner$Corona_Leugner))) 

#IV als Faktor:
data_Leugner$Corona_Leugner <- as.factor(data_Leugner$Corona_Leugner)


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Leugner$Corona_Leugner, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfLeugner <- data_Leugner[index,]
test_dfLeugner <- data_Leugner[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# 10-fold CV + resampling

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote",
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of 500 trees

set.seed(1997)
RFLeugner1 <- train(Corona_Leugner ~ ., 
                    data=train_dfLeugner,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFLeugner1
summary(RFLeugner1)
plot(RFLeugner1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFLeugner1, newdata=test_dfLeugner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfLeugner$Corona_Leugner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLeugner$Corona_Leugner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc:
RFLeugner1 %>%
  test_roc(data = test_dfLeugner) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFLeugner1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLeugner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Corona_Leugner <- RFLeugner1
saveRDS(tree500_Corona_Leugner, "./tree500_Corona_Leugner.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFLeugner2 <- train(Corona_Leugner ~ ., 
                    data=train_dfLeugner,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFLeugner2
summary(RFLeugner2)
plot(RFLeugner2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFLeugner2, newdata=test_dfLeugner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfLeugner$Corona_Leugner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLeugner$Corona_Leugner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFLeugner2 %>%
  test_roc(data = test_dfLeugner) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RFLeugner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLeugner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_CoronaLeugner <- RFLeugner2
saveRDS(tree1000_CoronaLeugner, "./tree1000_CoronaLeugner.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFLeugner_fin <- RFLeugner2

# Print models
RFLeugner_fin
summary(RFLeugner_fin)


#evaluate variable importance 
varImp(RFLeugner_fin)
plot(varImp(RFLeugner_fin), 20, main = "Corona_Leugner")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFLeugner_fin, newdata=test_dfLeugner)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfLeugner$Corona_Leugner))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfLeugner$Corona_Leugner,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc: 
RFLeugner_fin %>%
  test_roc(data = test_dfLeugner) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFLeugner1,
                   Model2 = RFLeugner2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfLeugner)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curves

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFLeugner_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFLeugner_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Leugner <- RFLeugner_fin
saveRDS(besttree_Leugner, "./besttree_Leugner_janein.rds")

#load the model

besttree_Leugner <- readRDS("./besttree_Leugner_janein.rds")
print(besttree_Leugner)





#######################
#Hardliner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Hardliner_num <- data[,c(278, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein))

#is the variable imbalanced?
table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein) 
max(table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein)/sum(table(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Hardliner_num$Corona_Massnahmen_muessten_haerter_sein, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfHard_num <- data_Hardliner_num[index,]
test_dfHard_num <- data_Hardliner_num[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFHard_num1 <- train(Corona_Massnahmen_muessten_haerter_sein ~ ., 
                     data=train_dfHard_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 500,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFHard_num1
summary(RFHard_num1)
plot(RFHard_num1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFHard_num1, newdata=test_dfHard_num)

MAE(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
RMSE(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
R2(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearson_Hard1 <- cor.test(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "pearson")
pearson_Hard1

spearmanHard1 <- cor.test(predictions1, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "spearman")
spearmanHard1


#save model to disk 

tree500_CoronaHardliner_num <- RFHard_num1
saveRDS(tree500_CoronaHardliner_num, "./tree500_CoronaHardliner_num.rds")

####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)

RFHard_num2 <- train(Corona_Massnahmen_muessten_haerter_sein ~ ., 
                     data=train_dfHard_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 1000,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFHard_num2
summary(RFHard_num2)
plot(RFHard_num2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFHard_num2, newdata=test_dfHard_num)


MAE(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
RMSE(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
R2(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonHard2 <- cor.test(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "pearson")
pearsonHard2

spearmanHard2 <- cor.test(predictions2, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "spearman")
spearmanHard2

#both quite bad but 1000 is a bit better

#save model to disk 

tree1000_CoronaHardliner_num <- RFHard_num2
saveRDS(tree1000_CoronaHardliner_num, "./tree1000_CoronaHardliner_num.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)

RFHard_num_fin <- RFHard_num2

# Print model
RFHard_num_fin
summary(RFHard_num_fin)

#evaluate variable importance 
varImp(RFHard_num_fin)
plot(varImp(RFHard_num_fin), 20, main = "Corona_Massnahmen_muessten_haerter_sein")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFHard_num_fin, newdata=test_dfHard_num)

MAE(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
RMSE(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)
R2(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonHard3 <- cor.test(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "pearson")
pearsonHard3

spearmanHard3 <- cor.test(predictions3, test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein, method = "spearman")
spearmanHard3


#testing baseline accuracy with mean and median

mean_Hardliner <- rep(mean(test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein), nrow(test_dfHard_num))
median_Hardliner <- rep(median(test_dfHard_num$Corona_Massnahmen_muessten_haerter_sein), nrow(test_dfHard_num))

MAE(predictions3, mean_Hardliner)
RMSE(predictions3, mean_Hardliner)

MAE(predictions3, median_Hardliner)
RMSE(predictions3, median_Hardliner)


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFHard_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- RFHard_num_fin

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Hardliner_num <- RFHard_num_fin
saveRDS(besttree_Hardliner_num, "./besttree_Hard_num.rds")

#load the model

besttree_Hardliner_num <- readRDS("./besttree_Hard_num.rds")
print(besttree_Hardliner_num)





#######################
#Softliner 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Softliner_num <- data[,c(277, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Softliner_num$Corona_Massnahmen_uebertrieben)) 

#is the variable imbalanced?
table(data_Softliner_num$Corona_Massnahmen_uebertrieben) 
max(table(data_Softliner_num$Corona_Massnahmen_uebertrieben)/sum(table(data_Softliner_num$Corona_Massnahmen_uebertrieben))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Softliner_num$Corona_Massnahmen_uebertrieben, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfSoft_num <- data_Softliner_num[index,]
test_dfSoft_num <- data_Softliner_num[-index,]



#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFSoft_num1 <- train(Corona_Massnahmen_uebertrieben ~ ., 
                     data=train_dfSoft_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 500,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFSoft_num1
summary(RFSoft_num1)
plot(RFSoft_num1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFSoft_num1, newdata=test_dfSoft_num)

MAE(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
RMSE(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
R2(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSoft1 <- cor.test(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "pearson")
pearsonSoft1

spearmanSoft1 <- cor.test(predictions, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "spearman")
spearmanSoft1

#save model to disk 

tree500_CoronaSoftliner_num <- RFSoft_num1
saveRDS(tree500_CoronaSoftliner_num, "./tree500_CoronaSoftliner_num.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#getunte Werte und num.tree ausprobieren --> ist mehr besser?

set.seed(1997)
RFSoft_num2 <- train(Corona_Massnahmen_uebertrieben ~ ., 
                     data=train_dfSoft_num,
                     tuneGrid = myGrid,
                     method="ranger", 
                     metric= "RMSE",
                     num.tree = 1000,
                     trControl = myControl, 
                     na.action = na.omit,
                     importance = 'impurity')

# Print model to console

RFSoft_num2
summary(RFSoft_num2)
plot(RFSoft_num2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSoft_num2, newdata=test_dfSoft_num)


MAE(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
RMSE(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
R2(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSoft2 <- cor.test(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "pearson")
pearsonSoft2

spearmanSoft2 <- cor.test(predictions2, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "spearman")
spearmanSoft2

#save model to disk 

tree1000_CoronaSoftliner_num <- RFSoft_num2
saveRDS(tree1000_CoronaSoftliner_num, "./tree1000_CoronaSoftliner_num.rds")


####-------tree 3: Final --------------------------------------------------

#finales Model

set.seed(1997)

RFSoft_num_fin <- RFSoft_num2

# Print model
RFSoft_num_fin
summary(RFSoft_num_fin)

#evaluate variable importance 
varImp(RFSoft_num_fin)
plot(varImp(RFSoft_num_fin), 20, main = "Corona_Massnahmen_uebertrieben")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSoft_num_fin, newdata=test_dfSoft_num)

MAE(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
RMSE(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben)
R2(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSoft3 <- cor.test(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "pearson")
pearsonSoft3

spearmanSoft3 <- cor.test(predictions3, test_dfSoft_num$Corona_Massnahmen_uebertrieben, method = "spearman")
spearmanSoft3


#testing baseline accuracy with mean and median

mean_Softliner <- rep(mean(test_dfSoft_num$Corona_Massnahmen_uebertrieben), nrow(test_dfSoft_num))
median_Softliner <- rep(median(test_dfSoft_num$Corona_Massnahmen_uebertrieben), nrow(test_dfSoft_num))

MAE(predictions3, mean_Softliner)
RMSE(predictions3, mean_Softliner)

MAE(predictions3, median_Softliner)
RMSE(predictions3, median_Softliner)


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFSoft_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFSoft_num_fin

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Softliner_num <- RFSoft_num_fin
saveRDS(besttree_Softliner_num, "./besttree_SoftNum.rds")

#load the model

besttree_Softliner_num <- readRDS("./besttree_SoftNum.rds")
print(besttree_Softliner_num)





#######################
#Sceptics 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Skeptik_num <- data[,c(279, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe)) 

#is the variable imbalanced?
table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe) 
max(table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe)/sum(table(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe))) 

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Skeptik_num$Corona_ist_harmlos_gleich_Grippe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_Skeptiker_num <- data_Skeptik_num[index,]
test_Skeptiker_num <- data_Skeptik_num[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFSkeptiker_num1 <- train(Corona_ist_harmlos_gleich_Grippe ~ ., 
                          data=train_Skeptiker_num,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "RMSE",
                          num.tree = 500,
                          trControl = myControl, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print model to console

RFSkeptiker_num1
summary(RFSkeptiker_num1)
plot(RFSkeptiker_num1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFSkeptiker_num1, newdata=test_Skeptiker_num)

MAE(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
RMSE(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
R2(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSkeptiker1 <- cor.test(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "pearson")
pearsonSkeptiker1

spearmanSkeptiker1 <- cor.test(predictions1, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "spearman")
spearmanSkeptiker1

#save model to disk 

tree500_CoronaSkeptiker_num <- RFSkeptiker_num1
saveRDS(tree500_CoronaSkeptiker_num, "./tree500_CoronaSkeptiker_num.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)

RFSkeptikernum_2 <- train(Corona_ist_harmlos_gleich_Grippe ~ ., 
                          data=train_Skeptiker_num,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "RMSE",
                          num.tree = 1000,
                          trControl = myControl, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print model to console

RFSkeptikernum_2
summary(RFSkeptikernum_2)
plot(RFSkeptiker_num2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSkeptikernum_2, newdata=test_Skeptiker_num)


MAE(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
RMSE(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
R2(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSkeptiker2 <- cor.test(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "pearson")
pearsonSkeptiker2

spearmanSkeptiker2 <- cor.test(predictions2, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "spearman")
spearmanSkeptiker2

#save model to disk 

tree1000_CoronaSkeptiker_num <- RFSkeptikernum_2
saveRDS(tree1000_CoronaSkeptiker_num, "./tree1000_CoronaSkeptiker_num.rds")

####-------tree 3: Final --------------------------------------------------

#final Model

set.seed(1997)

RFSkeptikernum_fin <- RFSkeptiker_num1

# Print model
RFSkeptikernum_fin
summary(RFSkeptikernum_fin)

#evaluate variable importance 
varImp(RFSkeptikernum_fin)
plot(varImp(RFSkeptikernum_fin), 20, main = "Corona_ist_harmlos_gleich_Grippe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- as.numeric(predict(RFSkeptikernum_fin, newdata=test_Skeptiker_num))

MAE(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
RMSE(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)
R2(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonSkeptiker_fin <- cor.test(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "pearson")
pearsonSkeptiker_fin

spearmanSkeptiker_fin <- cor.test(predictions3, test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe, method = "spearman")
spearmanSkeptiker_fin

#testing baseline accuracy with mean and median

mean_Skeptiker <- rep(mean(test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe), nrow(test_Skeptiker_num))
median_Skeptiker <- rep(median(test_Skeptiker_num$Corona_ist_harmlos_gleich_Grippe), nrow(test_Skeptiker_num))

MAE(predictions3, mean_Skeptiker)
RMSE(predictions3, mean_Skeptiker)

MAE(predictions3, median_Skeptiker)
RMSE(predictions3, median_Skeptiker)


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFSkeptikernum_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFSkeptikernum_fin

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Skeptiker_num <- RFSkeptikernum_fin
saveRDS(besttree_Skeptiker_num, "./besttree_Skeptik_num.rds")

#load the model

besttree_Skeptiker_num <- readRDS("./besttree_Skeptik_num.rds")
print(besttree_Skeptiker_num)








#######################
#Deniers 1-7: numeric
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Leugner_num <- data[,c(280, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Leugner_num$Glaube_nicht_an_Corona)) 

#is the variable imbalanced?
table(data_Leugner_num$Glaube_nicht_an_Corona) 
max(table(data_Leugner_num$Glaube_nicht_an_Corona)/sum(table(data_Leugner_num$Glaube_nicht_an_Corona))) 


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Leugner_num$Glaube_nicht_an_Corona, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfLeugner_num <- data_Leugner_num[index,]
test_dfLeugner_num <- data_Leugner_num[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFLeugner_num1 <- train(Glaube_nicht_an_Corona ~ ., 
                        data=train_dfLeugner_num,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "RMSE",
                        num.tree = 500,
                        trControl = myControl, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print model to console

RFLeugner_num1
summary(RFLeugner_num1)
plot(RFLeugner_num1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFLeugner_num1, newdata=test_dfLeugner_num)

MAE(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona)
RMSE(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona)
R2(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonLeugner1 <- cor.test(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "pearson")
pearsonLeugner1

spearmanLeugner1 <- cor.test(predictions1, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "spearman")
spearmanLeugner1

#save model to disk 

tree500_CoronaLeugner_num <- RFLeugner_num1
saveRDS(tree500_CoronaLeugner_num, "./tree500_CoronaLeugner_num.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFLeugner_num2 <- train(Glaube_nicht_an_Corona ~ ., 
                        data=train_dfLeugner_num,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "RMSE",
                        num.tree = 1000,
                        trControl = myControl, 
                        na.action = na.omit,
                        importance = 'impurity')

# Print model to console

RFLeugner_num2
summary(RFLeugner_num2)
plot(RFLeugner_num2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFLeugner_num2, newdata=test_dfLeugner_num)


MAE(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona)
RMSE(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona)
R2(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonLeugner2 <- cor.test(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "pearson")
pearsonLeugner2

spearmanLeugner2 <- cor.test(predictions2, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "spearman")
spearmanLeugner2

#save model to disk 

tree1000_CoronaLeugner_num <- RFLeugner_num2
saveRDS(tree1000_CoronaLeugner_num, "./tree1000_CoronaLeugner_num.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)

RFLeugner_num_fin <- RFLeugner_num2

# Print model
RFLeugner_num_fin
summary(RFLeugner_num_fin)

#evaluate variable importance 
varImp(RFLeugner_num_fin)
plot(varImp(RFLeugner_num_fin), 20, main = "Glaube_nicht_an_Corona")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFLeugner_num_fin, newdata=test_dfLeugner_num)

MAE(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona)
RMSE(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona)
R2(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonLeugner_fin <- cor.test(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "pearson")
pearsonLeugner_fin

spearmanLeugner_fin <- cor.test(predictions3, test_dfLeugner_num$Glaube_nicht_an_Corona, method = "spearman")
spearmanLeugner_fin

#testing baseline accuracy with mean and median

mean_Leugner <- rep(mean(test_dfLeugner_num$Glaube_nicht_an_Corona), nrow(test_dfLeugner_num))
median_Leugner <- rep(median(test_dfLeugner_num$Glaube_nicht_an_Corona), nrow(test_dfLeugner_num))

MAE(predictions3, mean_Leugner)
RMSE(predictions3, mean_Leugner)

MAE(predictions3, median_Leugner)
RMSE(predictions3, median_Leugner)

#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFLeugner_num_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFLeugner_num_fin

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Leugner_num <- RFLeugner_num_fin
saveRDS(besttree_Leugner_num, "./besttree_Leugner_num.rds")

#load the model

besttree_Leugner_num <- readRDS("./besttree_Leugner_num.rds")
print(besttree_Leugner_num)




#######################
#Income Group: Categorical (high, medium, low)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Einkommen <- data[,c(315, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Einkommen$Einkommensgruppe)) #121 NAs
data_Einkommen <- data_Einkommen %>% subset(data_Einkommen$Einkommensgruppe != "NA")


#is the variable imbalanced?
table(data_Einkommen$Einkommensgruppe) #hohes Einkommen ist unterrepräsentiert, verhältnis ca. 1:6:10 --> Korrektur notwendig!
max(table(data_Einkommen$Einkommensgruppe)/sum(table(data_Einkommen$Einkommensgruppe)))  

#IV als Faktor:
data_Einkommen$Einkommensgruppe <- as.factor(data_Einkommen$Einkommensgruppe)

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Einkommen$Einkommensgruppe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfEinkommen <- data_Einkommen[index,]
test_dfEinkommen <- data_Einkommen[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFEinkommen_1 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen,
                       tuneGrid = myGrid,
                       method="ranger", 
                       metric= "Kappa",
                       num.tree = 500,
                       trControl = myControl1, 
                       na.action = na.omit,
                       importance = 'impurity')

# Print models to console

RFEinkommen_1
summary(RFEinkommen_1)
plot(RFEinkommen_1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFEinkommen_1, newdata=test_dfEinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfEinkommen$Einkommensgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc
RFEinkommen_1 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()

#save model to disk 

tree500_Einkommen <- RFEinkommen_1
saveRDS(tree500_Einkommen, "./tree500_Einkommen.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFEinkommen_2 <- train(Einkommensgruppe ~ ., 
                       data=train_dfEinkommen, 
                       method="ranger", metric= "Kappa",
                       tuneGrid = myGrid,
                       na.action = na.omit,
                       num.tree = 1000,
                       trControl = myControl1, 
                       importance = 'impurity')

# Print models
RFEinkommen_2
summary(RFEinkommen_2)
plot(RFEinkommen_2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFEinkommen_2, newdata=test_dfEinkommen)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfEinkommen$Einkommensgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFEinkommen_2 %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()


#save model to disk 

tree1000_Einkommen <- RFEinkommen_2
saveRDS(tree1000_Einkommen, "./tree1000_Einkommen.rds")

####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFEinkommen_fin <- RFEinkommen_1

# Print models
RFEinkommen_fin
summary(RFEinkommen_fin)

#evaluate variable importance 
varImp(RFEinkommen_fin)
plot(varImp(RFEinkommen_fin), 20, main = "Einkommensgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFEinkommen_fin, newdata=test_dfEinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfEinkommen$Einkommensgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfEinkommen$Einkommensgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc
RFEinkommen_fin %>%
  test_roc(data = test_dfEinkommen) %>%
  auc()



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFEinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFEinkommen_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main = "niedrig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main = "niedrig")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittel") %>%plotPartial(main = "mittel")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittel") %>%plotPartial(main = "mittel")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main = "hoch")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main = "hoch")



#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Einkommen <- RFEinkommen_fin
saveRDS(besttree_Einkommen, "./besttree_Einkommen.rds")

#load the model

besttree_Einkommen <- readRDS("./besttree_Einkommen.rds")
print(besttree_Einkommen)




#######################
#above or below average income (binary)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Durchschnittseinkommen <- data[,c(344, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Durchschnittseinkommen$Durchschnittseinkommen)) #121 NAs
data_Durchschnittseinkommen <- data_Durchschnittseinkommen %>% subset(data_Durchschnittseinkommen$Durchschnittseinkommen != "NA")


#is the variable imbalanced?
table(data_Durchschnittseinkommen$Durchschnittseinkommen) #in Ordnung, ca. 1:2
max(table(data_Durchschnittseinkommen$Durchschnittseinkommen)/sum(table(data_Durchschnittseinkommen$Durchschnittseinkommen)))

#IV als Faktor:
data_Durchschnittseinkommen$Durchschnittseinkommen <- as.factor(data_Durchschnittseinkommen$Durchschnittseinkommen)

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Durchschnittseinkommen$Durchschnittseinkommen, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfDurchschnittseinkommen <- data_Durchschnittseinkommen[index,]
test_dfDurchschnittseinkommen <- data_Durchschnittseinkommen[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# no sampling needed

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFDurchschnittseinkommen1 <- train(Durchschnittseinkommen ~ ., 
                                   data=train_dfDurchschnittseinkommen,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 500,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFDurchschnittseinkommen1
summary(RFDurchschnittseinkommen1)
plot(RFDurchschnittseinkommen1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFDurchschnittseinkommen1, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc
RFDurchschnittseinkommen1 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFDurchschnittseinkommen1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDurchschnittseinkommen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Durchschnittseinkommen <- RFDurchschnittseinkommen1
saveRDS(tree500_Durchschnittseinkommen, "./tree500_Durchschnittseinkommen.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#build 1000 trees --> the more the better?

set.seed(1997)
RFDurchschnittseinkommen2 <- train(Durchschnittseinkommen ~ ., 
                                   data=train_dfDurchschnittseinkommen,
                                   tuneGrid = myGrid,
                                   method="ranger", 
                                   metric= "ROC",
                                   num.tree = 1000,
                                   na.action = na.omit,
                                   trControl = myControl1, 
                                   importance = 'impurity')

# Print models to console

RFDurchschnittseinkommen2
summary(RFDurchschnittseinkommen2)
plot(RFDurchschnittseinkommen2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFDurchschnittseinkommen2, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc
RFDurchschnittseinkommen2 %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RFDurchschnittseinkommen2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDurchschnittseinkommen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#grows very similar trees, thus keep 500 --> we don't need to grow 1000

#save model to disk 

tree1000_Durchschnittseinkommen <- RFDurchschnittseinkommen2
saveRDS(tree1000_Durchschnittseinkommen, "./tree1000_Durchschnittseinkommen.rds")



####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFDurchschnittseinkommen_fin <- RFDurchschnittseinkommen1

# Print models
RFDurchschnittseinkommen_fin
summary(RFDurchschnittseinkommen_fin)


#evaluate variable importance 
varImp(RFDurchschnittseinkommen_fin)
plot(varImp(RFDurchschnittseinkommen_fin), 20, main = "Durchschnittseinkommen")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFDurchschnittseinkommen_fin, newdata=test_dfDurchschnittseinkommen)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfDurchschnittseinkommen$Durchschnittseinkommen))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfDurchschnittseinkommen$Durchschnittseinkommen,
      predict(model, data, type = "prob")[, "mehr2000"])
  
}

#model auc: 
RFDurchschnittseinkommen_fin %>%
  test_roc(data = test_dfDurchschnittseinkommen) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFDurchschnittseinkommen1,
                   Model2 = RFDurchschnittseinkommen2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfDurchschnittseinkommen)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curves

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFDurchschnittseinkommen_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFDurchschnittseinkommen_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mehr2000") %>%plotPartial(main = "Einfluss auf über Durchschnitt")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Durchschnittseinkommen <- RFDurchschnittseinkommen_fin
saveRDS(besttree_Durchschnittseinkommen, "./besttree_Durchschnittseinkommen.rds")

#load the model

besttree_Durchschnittseinkommen <- readRDS("./besttree_Durchschnittseinkommen.rds")
print(besttree_Durchschnittseinkommen)




#######################
#Green Values: numeric (with general grid)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green1 <- data[,c(305, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Green1$Green_Values))

#is the variable imbalanced?
table(data_Green1$Green_Values)
max(table(data_Green1$Green_Values)/sum(table(data_Green1$Green_Values)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green1$Green_Values, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen1 <- data_Green1[index,]
test_dfGreen1 <- data_Green1[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFGreen1_1 <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 500,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_1
summary(RFGreen1_1)
plot(RFGreen1_1)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_1, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_1

spearmanGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_1

#save model to disk 

tree500_GreenValues_num <- RFGreen1_1
saveRDS(tree500_GreenValues_num, "./tree500_GreenValues_num.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#build 1000 trees --> the more the better?

set.seed(1997)

RFGreen1_2 <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 1000,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_2
summary(RFGreen1_2)
plot(RFGreen1_2)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_2, newdata=test_dfGreen1)


MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_2

spearmanGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_2

#save model to disk 

tree1000_GreenValues_num <- RFGreen1_2
saveRDS(tree1000_GreenValues_num, "./tree1000_GreenValues_num.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)

RFGreen1_fin <- RFGreen1_2

# Print model
RFGreen1_fin
summary(RFGreen1_fin)

#evaluate variable importance 
varImp(RFGreen1_fin)
plot(varImp(RFGreen1_fin), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_fin, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_fin <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_fin

spearmanGreen1_fin <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_fin

#testing baseline accuracy with mean and median

mean_Green <- rep(mean(test_dfGreen1$Green_Values), nrow(test_dfGreen1))
median_Green <- rep(median(test_dfGreen1$Green_Values), nrow(test_dfGreen1))

MAE(predictions, mean_Green)
RMSE(predictions, mean_Green)

MAE(predictions, median_Green)
RMSE(predictions, median_Green)

#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFGreen1_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGreen1_fin

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Green1 <- RFGreen1_fin
saveRDS(besttree_Green1, "./besttree_Green1.rds")



#######################
#Green Values: numeric (with test grid for mtry)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green1 <- data[,c(305, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Green1$Green_Values))

#is the variable imbalanced?
table(data_Green1$Green_Values)
max(table(data_Green1$Green_Values)/sum(table(data_Green1$Green_Values)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green1$Green_Values, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen1 <- data_Green1[index,]
test_dfGreen1 <- data_Green1[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  allowParallel=TRUE,
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFGreen1_1_mtry <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGridnum,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 500,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_1_mtry
summary(RFGreen1_1_mtry)
plot(RFGreen1_1_mtry)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_1_mtry, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_1

spearmanGreen1_1 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_1

#save model to disk 

tree500_GreenValues_num_try <- RFGreen1_1_mtry
saveRDS(tree500_GreenValues_num_try, "./tree500_GreenValues_num_try.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#build 1000 trees --> the more the better?

set.seed(1997)

RFGreen1_2_mtry <- train(Green_Values ~ ., 
                    data=train_dfGreen1,
                    tuneGrid = myGridnum,
                    method="ranger", 
                    metric= "RMSE",
                    num.tree = 1000,
                    trControl = myControl, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print model to console

RFGreen1_2_mtry
summary(RFGreen1_2_mtry)
plot(RFGreen1_2_mtry)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_2_mtry, newdata=test_dfGreen1)


MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_2

spearmanGreen1_2 <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_2

#save model to disk 

tree1000_GreenValues_num <- RFGreen1_2_mtry
saveRDS(tree1000_GreenValues_num, "./tree1000_GreenValues_num.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)

RFGreen1_fin_mtry <- RFGreen1_2_mtry

# Print model
RFGreen1_fin_mtry
summary(RFGreen1_fin_mtry)

#evaluate variable importance 
varImp(RFGreen1_fin_mtry)
plot(varImp(RFGreen1_fin_mtry), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFGreen1_fin_mtry, newdata=test_dfGreen1)

MAE(predictions, test_dfGreen1$Green_Values)
RMSE(predictions, test_dfGreen1$Green_Values)
R2(predictions, test_dfGreen1$Green_Values)

#calculate Pearson coefficient for predictions and actual values
# Correlations with significance levels

pearsonGreen1_fin <- cor.test(predictions, test_dfGreen1$Green_Values, method = "pearson")
pearsonGreen1_fin

spearmanGreen1_fin <- cor.test(predictions, test_dfGreen1$Green_Values, method = "spearman")
spearmanGreen1_fin

#testing baseline accuracy with mean and median

mean_Green <- rep(mean(test_dfGreen1$Green_Values), nrow(test_dfGreen1))
median_Green <- rep(median(test_dfGreen1$Green_Values), nrow(test_dfGreen1))

MAE(predictions, mean_Green)
RMSE(predictions, mean_Green)

MAE(predictions, median_Green)
RMSE(predictions, median_Green)

#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFGreen1_fin_mtry$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGreen1_fin_mtry

PartialPlots %>% partial(pred.var = impvar[1]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19]) %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20]) %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Green1_mtry <- RFGreen1_fin_mtry
saveRDS(besttree_Green1_mtry, "./besttree_Green1_mtry.rds")






#######################
#Green Values 2: binary
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Green2 <- data[,c(306, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Green2$Green2))  

#is the variable imbalanced?
table(data_Green2$Green2)
max(table(data_Green2$Green2)/sum(table(data_Green2$Green2))) 

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Green2$Green2, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfGreen2 <- data_Green2[index,]
test_dfGreen2 <- data_Green2[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)

myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

set.seed(1997)
RFGreen2_1 <- train(Green2 ~ ., 
                    data=train_dfGreen2,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 500,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFGreen2_1
summary(RFGreen2_1)
plot(RFGreen2_1)

# predict outcome using model from train_df applied to the test_df
predictions1 <- predict(RFGreen2_1, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions1), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFGreen2_1 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()


#check ROC plot
model_list <- list(Model1 = RFGreen2_1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGreen2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_GreenValues_bin <- RFGreen2_1
saveRDS(tree500_GreenValues_bin, "./tree500_GreenValues_bin.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#build 1000 trees --> the more the better?

#set random seed again 
set.seed(1997)
RFGreen2_2 <- train(Green2 ~ ., 
                    data=train_dfGreen2,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "ROC",
                    num.tree = 1000,
                    na.action = na.omit,
                    trControl = myControl1, 
                    importance = 'impurity')

# Print models to console

RFGreen2_2
summary(RFGreen2_2)
plot(RFGreen2_2)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFGreen2_2, newdata=test_dfGreen2)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFGreen2_2 %>%
  test_roc(data = test_dfGreen2) %>%
  auc()


#ROC plot
model_list <- list(Model1 = RFGreen2_2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGreen2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_GreenValues_bin <- RFGreen2_2
saveRDS(tree1000_GreenValues_bin, "./tree1000_GreenValues_bin.rds")


####-------tree 3: Final --------------------------------------------------

#define final model

set.seed(1997)
RFGreen2_fin <- RFGreen2_2

# Print models
RFGreen2_fin
summary(RFGreen2_fin)


#evaluate variable importance 
varImp(RFGreen2_fin)
plot(varImp(RFGreen2_fin), 20, main = "Green_Values")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFGreen2_fin, newdata=test_dfGreen2)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfGreen2$Green2))


#check for auc
test_roc <- function(model, data) {
  
  roc(test_dfGreen2$Green2,
      predict(model, data, type = "prob")[, "Ja"])
  
}

#model auc
RFGreen2_fin %>%
  test_roc(data = test_dfGreen2) %>%
  auc()


#compare different ROC plots
model_list <- list(Model1 = RFGreen2_1,
                   Model2 = RFGreen2_2)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfGreen2)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFGreen2_fin$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFGreen2_fin

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Green2 <- RFGreen2_fin
saveRDS(besttree_Green2, "./besttree_Green2.rds")

#load the model

besttree_Green2 <- readRDS("./besttree_Green2.rds")
print(besttree_Green2)




#######################
#Occupation: Categorical (7 classes)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Beschaeftigung <- data[,c(10, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Beschaeftigung$Beschaeftigung))
data_Beschaeftigung <- data_Beschaeftigung %>% subset(data_Beschaeftigung$Beschaeftigung != "NA")


#is the variable imbalanced?
table(data_Beschaeftigung$Beschaeftigung) 
max(table(data_Beschaeftigung$Beschaeftigung)/sum(table(data_Beschaeftigung$Beschaeftigung))) 

#IV als Faktor:
data_Beschaeftigung$Beschaeftigung <- as.factor(data_Beschaeftigung$Beschaeftigung)

#Variablennamen anpassen für Analyse
data_Beschaeftigung <- data_Beschaeftigung %>% mutate(Beschaeftigung = case_when(Beschaeftigung == "Arbeitslos/-suchend" ~ 'Arbeitslos_suchend',
                                                                                 Beschaeftigung == "Auszubildende/r" ~ 'Auszubildende_r',
                                                                                 Beschaeftigung == "Berufstätige/r" ~ 'Berufstätige_r',
                                                                                 Beschaeftigung == "Hausfrau/-mann" ~ 'Hausfrau_mann',
                                                                                 Beschaeftigung == "Renter/in" ~ 'Renter_in',
                                                                                 Beschaeftigung == "Schüler/in" ~ 'Schüler_in',
                                                                                 Beschaeftigung == "Student/in" ~ 'Student_in'))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Beschaeftigung$Beschaeftigung, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBeschaeftigung <- data_Beschaeftigung[index,]
test_dfBeschaeftigung <- data_Beschaeftigung[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFBeschaeftigung <- train(Beschaeftigung ~ ., 
                          data=train_dfBeschaeftigung,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "Kappa",
                          num.tree = 500,
                          trControl = myControl1, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print models to console

RFBeschaeftigung
summary(RFBeschaeftigung)
plot(RFBeschaeftigung)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBeschaeftigung, newdata=test_dfBeschaeftigung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBeschaeftigung$Beschaeftigung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeschaeftigung$Beschaeftigung,
                 predict(model, data, type = "prob")[, "Arbeitslos_suchend"])
  
}

#model auc: 
RFBeschaeftigung %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()

#save model to disk 

tree500_Beschaeftigung <- RFBeschaeftigung
saveRDS(tree500_Beschaeftigung, "./tree500_Beschaeftigung.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFBeschaeftigung1 <- train(Beschaeftigung ~ ., 
                           data=train_dfBeschaeftigung, 
                           method="ranger", metric= "Kappa",
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl1, 
                           importance = 'impurity')

# Print models
RFBeschaeftigung1
summary(RFBeschaeftigung1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBeschaeftigung1, newdata=test_dfBeschaeftigung)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBeschaeftigung$Beschaeftigung))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeschaeftigung$Beschaeftigung,
                 predict(model, data, type = "prob")[, "Arbeitslos_suchend"])
  
}

#model auc: 
RFBeschaeftigung1 %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()


#save model to disk 

tree1000_Beschaeftigung <- RFBeschaeftigung1
saveRDS(tree1000_Beschaeftigung, "./tree1000_Beschaeftigung.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBeschaeftigungFinal <- RFBeschaeftigung1

# Print models
RFBeschaeftigungFinal 
summary(RFBeschaeftigungFinal )

#evaluate variable importance 
varImp(RFBeschaeftigungFinal )
plot(varImp(RFBeschaeftigungFinal ), 20, main = "Beschaeftigung")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBeschaeftigungFinal , newdata=test_dfBeschaeftigung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBeschaeftigung$Beschaeftigung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeschaeftigung$Beschaeftigung,
                 predict(model, data, type = "prob")[, "Arbeitslos_suchend"])
  
}

#model auc: 
RFBeschaeftigungFinal %>%
  test_roc(data = test_dfBeschaeftigung) %>%
  auc()



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFBeschaeftigungFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFBeschaeftigungFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Arbeitslos_suchend") %>%plotPartial (main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Arbeitslos_suchend") %>%plotPartial(main = "Arbeitslos_suchend")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Auszubildende_r") %>%plotPartial(main = "Auszubildende_r")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Berufstätige_r") %>%plotPartial(main = "Berufstätige_r")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Hausfrau_mann") %>%plotPartial(main = "Hausfrau_mann")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Rentner_in") %>%plotPartial(main = "Rentner_in")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Schüler_in") %>%plotPartial(main = "Schüler_in")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Student_in") %>%plotPartial(main = "Student_in")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Student_in") %>%plotPartial(main = "Student_in")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 


besttree_Beschaeftigung <- RFBeschaeftigungFinal
saveRDS(besttree_Beschaeftigung, "./besttree_Beschaeftigung.rds")





#####################
#Working or not (binary)
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Arbeitend_oder_nicht <- data[,c(317, 27:255)]

# Convert to factor
data_Arbeitend_oder_nicht$Arbeitend_oder_nicht = as.factor(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)


#Are there NAs in the DV?
sum(is.na(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)) 
data_Arbeitend_oder_nicht <- data_Arbeitend_oder_nicht %>% subset(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht != "NA")


#is the variable imbalanced?
table(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)
max(table(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)/sum(table(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht)))



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Arbeitend_oder_nicht$Arbeitend_oder_nicht, p=.8, list= FALSE, times= 1)

# split into train and test set

train_dfArbeitend_oder_nicht <- data_Arbeitend_oder_nicht[index,]
test_dfArbeitend_oder_nicht <- data_Arbeitend_oder_nicht[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelArbeitend_oder_nichtRF <- train(Arbeitend_oder_nicht ~ ., 
                                     data=train_dfArbeitend_oder_nicht,
                                     tuneGrid = myGrid,
                                     method="ranger",
                                     metric= "ROC",  
                                     na.action = na.omit,
                                     num.tree = 500,
                                     trControl = myControl, 
                                     importance = 'impurity')

# Print model to console

modelArbeitend_oder_nichtRF
summary(modelArbeitend_oder_nichtRF)
plot(modelArbeitend_oder_nichtRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelArbeitend_oder_nichtRF, newdata=test_dfArbeitend_oder_nicht)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelArbeitend_oder_nichtRF %>%
  test_roc(data = test_dfArbeitend_oder_nicht) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelArbeitend_oder_nichtRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfArbeitend_oder_nicht)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtRF
saveRDS(tree500_Arbeitend_oder_nicht, "./tree500_Arbeitend_oder_nicht.rds")

####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelArbeitend_oder_nichtRF1 <- train(Arbeitend_oder_nicht ~ ., 
                                      data=train_dfArbeitend_oder_nicht,
                                      tuneGrid = myGrid,
                                      method="ranger", 
                                      metric= "ROC", 
                                      na.action = na.omit,
                                      num.tree = 1000,
                                      trControl = myControl, 
                                      importance = 'impurity')

# Print model to console

modelArbeitend_oder_nichtRF1
summary(modelArbeitend_oder_nichtRF1)
plot(modelArbeitend_oder_nichtRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelArbeitend_oder_nichtRF1, newdata=test_dfArbeitend_oder_nicht)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelArbeitend_oder_nichtRF1 %>%
  test_roc(data = test_dfArbeitend_oder_nicht) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelArbeitend_oder_nichtRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfArbeitend_oder_nicht)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtRF1
saveRDS(tree1000_Arbeitend_oder_nicht, "./tree1000_Arbeitend_oder_nicht.rds")


####-------tree 3: Final --------------------------------------------------

#define final model
modelArbeitend_oder_nichtfinal <- modelArbeitend_oder_nichtRF


# Print model
print(modelArbeitend_oder_nichtfinal)

#output in terms of regression coefficients
summary(modelArbeitend_oder_nichtfinal)

#evaluate variable importance 
varImp(modelArbeitend_oder_nichtfinal)
plot(varImp(modelArbeitend_oder_nichtfinal), 20, main = "Arbeitend oder Nicht")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelArbeitend_oder_nichtfinal, newdata=test_dfArbeitend_oder_nicht)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfArbeitend_oder_nicht$Arbeitend_oder_nicht,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelArbeitend_oder_nichtfinal %>%
  test_roc(data = test_dfArbeitend_oder_nicht) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelArbeitend_oder_nichtfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfArbeitend_oder_nicht)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelArbeitend_oder_nichtfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelArbeitend_oder_nichtfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

final_Arbeitend_oder_nicht <- modelArbeitend_oder_nichtfinal
saveRDS(final_Arbeitend_oder_nicht, "./final_Arbeitend_oder_nicht.rds")




################
#Level of Education (categorical: high, medium, low)
###############

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Bildungsgruppe <- data[,c(314, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Bildungsgruppe$Bildungsgruppe))
data_Bildungsgruppe <- data_Bildungsgruppe %>% subset(data_Bildungsgruppe$Bildungsgruppe != "NA")


#is the variable imbalanced?
table(data_Bildungsgruppe$Bildungsgruppe) 
max(table(data_Bildungsgruppe$Bildungsgruppe)/sum(table(data_Bildungsgruppe$Bildungsgruppe))) 

#IV als Faktor:
data_Bildungsgruppe$Bildungsgruppe <- as.factor(data_Bildungsgruppe$Bildungsgruppe)

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Bildungsgruppe$Bildungsgruppe, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBildungsgruppe <- data_Bildungsgruppe[index,]
test_dfBildungsgruppe <- data_Bildungsgruppe[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  #sampling = "smote", 
  search = "grid"
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFBildungsgruppe <- train(Bildungsgruppe ~ ., 
                          data=train_dfBildungsgruppe,
                          tuneGrid = myGrid,
                          method="ranger", 
                          metric= "Kappa",
                          num.tree = 500,
                          trControl = myControl1, 
                          na.action = na.omit,
                          importance = 'impurity')

# Print models to console

RFBildungsgruppe
summary(RFBildungsgruppe)
plot(RFBildungsgruppe)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBildungsgruppe, newdata=test_dfBildungsgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBildungsgruppe$Bildungsgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBildungsgruppe$Bildungsgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFBildungsgruppe %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()


#save model to disk 

tree500_Bildungsgruppe <- RFBildungsgruppe
saveRDS(tree500_Bildungsgruppe, "./tree500_Bildungsgruppe.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFBildungsgruppe1 <- train(Bildungsgruppe ~ ., 
                           data=train_dfBildungsgruppe, 
                           method="ranger", metric= "Kappa",
                           tuneGrid = myGrid,
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl1, 
                           importance = 'impurity')

# Print models
RFBildungsgruppe1
summary(RFBildungsgruppe1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBildungsgruppe1, newdata=test_dfBildungsgruppe)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBildungsgruppe$Bildungsgruppe))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBildungsgruppe$Bildungsgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFBildungsgruppe1 %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()


#save model to disk 

tree1000_Bildungsgruppe <- RFBildungsgruppe1
saveRDS(tree1000_Bildungsgruppe, "./tree1000_Bildungsgruppe.rds")

####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBildungsgruppeFinal <- RFBildungsgruppe1

# Print models
RFBildungsgruppeFinal 
summary(RFBildungsgruppeFinal )

#evaluate variable importance 
varImp(RFBildungsgruppeFinal )
plot(varImp(RFBildungsgruppeFinal ), 20, main = "Bildungsgruppe")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBildungsgruppeFinal , newdata=test_dfBildungsgruppe)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBildungsgruppe$Bildungsgruppe))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBildungsgruppe$Bildungsgruppe,
                 predict(model, data, type = "prob")[, "niedrig"])
  
}

#model auc: 
RFBildungsgruppeFinal %>%
  test_roc(data = test_dfBildungsgruppe) %>%
  auc()




#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFBildungsgruppeFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFBildungsgruppeFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "hoch") %>%plotPartial (main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "hoch") %>%plotPartial(main = "hoher Bildungsstand")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "mittel") %>%plotPartial(main = "mittlerer Bildungsstand")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "niedrig") %>%plotPartial(main = "niedriger Bildungsstand")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------


#save model to disk 

besttree_Bildungsgruppe <- RFBildungsgruppeFinal
saveRDS(besttree_Bildungsgruppe, "./besttree_Bildungsgruppe.rds")




#######################
#Religion: Categorical 
######################

#-------------------------------------Data Pre-Processing---------------------------------------------

#define data for analysis
data_Religion <- data[,c(12, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Religion$Religion)) 
#"Others" as NA to remove them from analysis
data_Religion <- data_Religion %>% replace_with_na_all(condition = ~.x == "Sonstige:")

#change variable names for analysis; change "Judentum" to NA also because of too few observations
data_Religion <- data_Religion %>% mutate(Religion = case_when(Religion == "Christentum" ~ 'Christentum',
                                                               Religion == "Ich fühle mich keiner Religion zugehörig" ~ 'Nicht_zugehörig',
                                                               Religion == "Islam" ~ 'Islam',
                                                               Religion == "Judentum" ~ 'NA'))
#Dataset without NAs
data_Religion <- data_Religion %>% subset(data_Religion$Religion != "NA")

#is the variable imbalanced?
table(data_Religion$Religion) 
max(table(data_Religion$Religion)/sum(table(data_Religion$Religion)))

#IV als Faktor:
data_Religion$Religion <- as.factor(data_Religion$Religion)


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Religion$Religion, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfReligion <- data_Religion[index,]
test_dfReligion <- data_Religion[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)



####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 
set.seed(1997)
RFReligion <- train(Religion ~ ., 
                    data=train_dfReligion,
                    tuneGrid = myGrid,
                    method="ranger", 
                    metric= "Kappa",
                    num.tree = 500,
                    trControl = myControl1, 
                    na.action = na.omit,
                    importance = 'impurity')

# Print models to console

RFReligion
summary(RFReligion)
plot(RFReligion)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFReligion, newdata=test_dfReligion)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfReligion$Religion))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligion %>%
  test_roc(data = test_dfReligion) %>%
  auc()

#save model to disk 

tree500_Religion <- RFReligion
saveRDS(tree500_Religion, "./tree500_Religion.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFReligion1 <- train(Religion ~ ., 
                     data=train_dfReligion, 
                     method="ranger", metric= "Kappa",
                     tuneGrid = myGrid,
                     na.action = na.omit,
                     num.tree = 1000,
                     trControl = myControl1, 
                     importance = 'impurity')

# Print models
RFReligion1
summary(RFReligion1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFReligion1, newdata=test_dfReligion)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfReligion$Religion))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc
RFReligion1 %>%
  test_roc(data = test_dfReligion) %>%
  auc()


#save model to disk 
tree1000_Religion <- RFReligion1
saveRDS(tree1000_Religion, "./tree1000_Religion.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFReligionFinal <- RFReligion1

# Print models
RFReligionFinal 
summary(RFReligionFinal )

#evaluate variable importance 
varImp(RFReligionFinal )
plot(varImp(RFReligionFinal ), 20, main = "Religion")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFReligionFinal , newdata=test_dfReligion)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfReligion$Religion))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfReligion$Religion,
                 predict(model, data, type = "prob")[, "Christentum"])
  
}

#model auc: 
RFReligionFinal %>%
  test_roc(data = test_dfReligion) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFReligionFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- RFReligionFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Christentum") %>%plotPartial (main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Christentum") %>%plotPartial(main = "Christentum")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Christentum") %>%plotPartial(main = "Christentum")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Nicht_zugehörig") %>%plotPartial(main = "Ich fühle mich keiner Religion zugehörig")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Islam") %>%plotPartial(main = "Islam")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Islam") %>%plotPartial(main = "Islam")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Religion <- RFReligionFinal
saveRDS(besttree_Religion, "./besttree_Religion.rds")





#######################
#Religiosity yes/no (binary) 
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Religioes <- data[,c(333, 27:255)]

# Convert DV to factor
data_Religioes$Religioes = as.factor(data_Religioes$Religioes)


#Are there NAs in the DV?
sum(is.na(data_Religioes$Religioes))  
data_Religioes <- data_Religioes %>% subset(data_Religioes$Religioes != "NA")


#is the variable imbalanced?
table(data_Religioes$Religioes) #Verteilung in Ordnung
max(table(data_Religioes$Religioes)/sum(table(data_Religioes$Religioes))) 

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Religioes$Religioes, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht
train_dfReligioes <- data_Religioes[index,]
test_dfReligioes <- data_Religioes[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelReligioesRF <- train(Religioes ~ ., 
                          data=train_dfReligioes,
                          tuneGrid = myGrid,
                          method="ranger",
                          metric= "ROC",  
                          na.action = na.omit,
                          num.tree = 500,
                          trControl = myControl, 
                          importance = 'impurity')

# Print model to console

modelReligioesRF
summary(modelReligioesRF)
plot(modelReligioesRF)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelReligioesRF, newdata=test_dfReligioes)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfReligioes$Religioes)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfReligioes$Religioes,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelReligioesRF %>%
  test_roc(data = test_dfReligioes) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelReligioesRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfReligioes)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Religioes <- modelReligioesRF
saveRDS(tree500_Religioes, "./tree500_Religioes.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelReligioesRF1 <- train(Religioes ~ ., 
                           data=train_dfReligioes,
                           tuneGrid = myGrid,
                           method="ranger", 
                           metric= "ROC", 
                           na.action = na.omit,
                           num.tree = 1000,
                           trControl = myControl, 
                           importance = 'impurity')

# Print model to console

modelReligioesRF1
summary(modelReligioesRF1)
plot(modelReligioesRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelReligioesRF1, newdata=test_dfReligioes)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfReligioes$Religioes)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfReligioes$Religioes,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelReligioesRF1 %>%
  test_roc(data = test_dfReligioes) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelReligioesRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfReligioes)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Religioes <- modelReligioesRF1
saveRDS(tree1000_Religioes, "./tree1000_Religioes.rds")

####-------tree 3: Final --------------------------------------------------

modelReligioesfinal <- modelReligioesRF1

# Print model
print(modelReligioesfinal)

#output in terms of regression coefficients
summary(modelReligioesfinal)

#evaluate variable importance 
varImp(modelReligioesfinal)
plot(varImp(modelReligioesfinal), 20, main = "Religioes")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelReligioesfinal, newdata=test_dfReligioes)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfReligioes$Religioes)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfReligioes$Religioes,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelReligioesfinal %>%
  test_roc(data = test_dfReligioes) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelReligioesfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfReligioes)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelReligioesfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelReligioesfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------


#save model to disk 

besttree_Religioes <- modelReligioesfinal
saveRDS(besttree_Religioes, "./besttree_Religioes.rds")



####################
#Christianity vs Islam (binary)
#######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_IslamChrist <- data[,c(334, 27:255)]

# Convert IV to factor
data_IslamChrist$Islam_oder_Christ = as.factor(data_IslamChrist$Islam_oder_Christ)


#Are there NAs in the DV?
sum(is.na(data_IslamChrist$Islam_oder_Christ)) #810 NAs
data_IslamChrist <- data_IslamChrist %>% subset(data_IslamChrist$Islam_oder_Christ != "NA")


#is the variable imbalanced?
table(data_IslamChrist$Islam_oder_Christ)
max(table(data_IslamChrist$Islam_oder_Christ)/sum(table(data_IslamChrist$Islam_oder_Christ)))  


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_IslamChrist$Islam_oder_Christ, p=.8, list= FALSE, times= 1)

#split into test and train dataset

train_dfIslamChrist <- data_IslamChrist[index,]
test_dfIslamChrist <- data_IslamChrist[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelIslamChristRF <- train(Islam_oder_Christ ~ ., 
                            data=train_dfIslamChrist,
                            tuneGrid = myGrid,
                            method="ranger",
                            metric= "ROC",  
                            na.action = na.omit,
                            num.tree = 500,
                            trControl = myControl, 
                            importance = 'impurity')

# Print model to console

modelIslamChristRF
summary(modelIslamChristRF)
plot(modelIslamChristRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelIslamChristRF, newdata=test_dfIslamChrist)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfIslamChrist$Islam_oder_Christ)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfIslamChrist$Islam_oder_Christ,
      predict(model, data, type = "prob")[, "Christentum"])
  
}

modelIslamChristRF %>%
  test_roc(data = test_dfIslamChrist) %>%
  auc()

###nur für binär (von hier bis Ende des Abschnitts)
#ROC plot
model_list <- list(M1 = modelIslamChristRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfIslamChrist)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Islam_Christ <- modelIslamChristRF
saveRDS(tree500_Islam_Christ , "./tree500_Islam_Christ .rds")




####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelIslamChristRF1 <- train(Islam_oder_Christ ~ ., 
                             data=train_dfIslamChrist,
                             tuneGrid = myGrid,
                             method="ranger", 
                             metric= "ROC", 
                             na.action = na.omit,
                             num.tree = 1000,
                             trControl = myControl, 
                             importance = 'impurity')

# Print model to console

modelIslamChristRF1
summary(modelIslamChristRF1)
plot(modelIslamChristRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelIslamChristRF1, newdata=test_dfIslamChrist)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfIslamChrist$Islam_oder_Christ)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfIslamChrist$Islam_oder_Christ,
      predict(model, data, type = "prob")[, "Christentum"])
  
}

modelIslamChristRF1 %>%
  test_roc(data = test_dfIslamChrist) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelIslamChristRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfIslamChrist)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Islam_Christ <- modelIslamChristRF1
saveRDS(tree1000_Islam_Christ , "./tree1000_Islam_Christ .rds")

####-------tree 3: Final --------------------------------------------------


modelIslamChristfinal <- modelIslamChristRF

# Print model
print(modelIslamChristfinal)

#output in terms of regression coefficients
summary(modelIslamChristfinal)

#evaluate variable importance 
varImp(modelIslamChristfinal)
plot(varImp(modelIslamChristfinal), 20, main = "Islam_oder_Christ")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelIslamChristfinal, newdata=test_dfIslamChrist)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfIslamChrist$Islam_oder_Christ)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfIslamChrist$Islam_oder_Christ,
      predict(model, data, type = "prob")[, "Christentum"])
  
}

modelIslamChristfinal %>%
  test_roc(data = test_dfIslamChrist) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelIslamChristfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfIslamChrist)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(modelIslamChristfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- modelIslamChristfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Islam") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Islam") %>%plotPartial

#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 
bestree_Islam_Christ <- modelIslamChristRFfinal
saveRDS(besttree_Islam_Christ , "./besttree_Islam_Christ .rds")




####################
#Migration background: yes or no (binary)
#####################

#---------------------------------------------------DATA PRE-PROCESSING-----------------------------------------------------

#define data for analysis
data_Migrationshintergrund <- data[,c(14, 27:255)]


# Convert  DV to factor
data_Migrationshintergrund$Migrationshintergrund = as.factor(data_Migrationshintergrund$Migrationshintergrund)


#Are there NAs in the DV?
sum(is.na(data_Migrationshintergrund$Migrationshintergrund))  
data_Migrationshintergrund <- data_Migrationshintergrund %>% subset(data_Migrationshintergrund$Migrationshintergrund != "NA")


#is the variable imbalanced?
table(data_Migrationshintergrund$Migrationshintergrund) #Verteilung in Ordnung
max(table(data_Migrationshintergrund$Migrationshintergrund)/sum(table(data_Migrationshintergrund$Migrationshintergrund)))  



#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Migrationshintergrund$Migrationshintergrund, p=.8, list= FALSE, times= 1)

# Create train & test dataset

train_dfMigrationshintergrund <- data_Migrationshintergrund[index,]
test_dfMigrationshintergrund <- data_Migrationshintergrund[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelMigrationshintergrundRF <- train(Migrationshintergrund ~ ., 
                                      data=train_dfMigrationshintergrund,
                                      tuneGrid = myGrid,
                                      method="ranger",
                                      metric= "ROC",  
                                      na.action = na.omit,
                                      num.tree = 500,
                                      trControl = myControl, 
                                      importance = 'impurity')

# Print model to console

modelMigrationshintergrundRF
summary(modelMigrationshintergrundRF)
plot(modelMigrationshintergrundRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelMigrationshintergrundRF, newdata=test_dfMigrationshintergrund)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfMigrationshintergrund$Migrationshintergrund)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfMigrationshintergrund$Migrationshintergrund,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelMigrationshintergrundRF %>%
  test_roc(data = test_dfMigrationshintergrund) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelMigrationshintergrundRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfMigrationshintergrund)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Migrationshintergrund <- modelMigrationshintergrundRF
saveRDS(tree500_Migrationshintergrund, "./tree500_Migrationshintergrund.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelMigrationshintergrundRF1 <- train(Migrationshintergrund ~ ., 
                                       data=train_dfMigrationshintergrund,
                                       tuneGrid = myGrid,
                                       method="ranger", 
                                       metric= "ROC", 
                                       na.action = na.omit,
                                       num.tree = 1000,
                                       trControl = myControl, 
                                       importance = 'impurity')

# Print model to console

modelMigrationshintergrundRF1
summary(modelMigrationshintergrundRF1)
plot(modelMigrationshintergrundRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelMigrationshintergrundRF1, newdata=test_dfMigrationshintergrund)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfMigrationshintergrund$Migrationshintergrund)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfMigrationshintergrund$Migrationshintergrund,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelMigrationshintergrundRF1 %>%
  test_roc(data = test_dfMigrationshintergrund) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelMigrationshintergrundRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfMigrationshintergrund)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree1000_Migrationshintergrund <- modelMigrationshintergrundRF1
saveRDS(tree1000_Migrationshintergrund, "./tree1000_Migrationshintergrund.rds")


####-------tree 3: Final --------------------------------------------------

#define final model
modelMigrationshintergrundfinal <- modelMigrationshintergrundRF

# Print model
print(modelMigrationshintergrundfinal)

#output in terms of regression coefficients
summary(modelMigrationshintergrundfinal)

#evaluate variable importance 
varImp(modelMigrationshintergrundfinal)
plot(varImp(modelMigrationshintergrundfinal), 20, main = "Migrationshintergrund")

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelMigrationshintergrundfinal, newdata=test_dfMigrationshintergrund)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfMigrationshintergrund$Migrationshintergrund)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfMigrationshintergrund$Migrationshintergrund,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelMigrationshintergrundfinal %>%
  test_roc(data = test_dfMigrationshintergrund) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelMigrationshintergrundfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfMigrationshintergrund)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelMigrationshintergrundfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelMigrationshintergrundfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 
bestree_Migrationshintergrund <- modelMigrationshintergrundRFfinal
saveRDS(besttree_Migrationshintergrund, "./besttree_Migrationshintergrund.rds")




#######################
#Sexual Orientation (Categorical, 3 groups: Bisexual, Homosexual, Heterosexual)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#define data for analysis
data_Sexuelle_Orientierung <- data[,c(16, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Sexuelle_Orientierung$Sexuelle_Orientierung))
#"Others" as NA to remove them from analysis
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% replace_with_na_all(condition = ~.x == "Sonstiges:")
#Dataset without NAs
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% subset(data_Sexuelle_Orientierung$Sexuelle_Orientierung != "NA")

#is the variable imbalanced?
table(data_Sexuelle_Orientierung$Sexuelle_Orientierung) 
max(table(data_Sexuelle_Orientierung$Sexuelle_Orientierung)/sum(table(data_Sexuelle_Orientierung$Sexuelle_Orientierung)))  

#DV as factor:
data_Sexuelle_Orientierung$Sexuelle_Orientierung <- as.factor(data_Sexuelle_Orientierung$Sexuelle_Orientierung)

#set appropriate variable names for analysis
data_Sexuelle_Orientierung <- data_Sexuelle_Orientierung %>% mutate(Sexuelle_Orientierung = case_when(Sexuelle_Orientierung == "1" ~ 'Bisexuell',
                                                                                                      Sexuelle_Orientierung == "2" ~ 'Heterosexuell',
                                                                                                      Sexuelle_Orientierung == "3" ~ 'Homosexuell'))

#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Sexuelle_Orientierung$Sexuelle_Orientierung, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfSexuelle_Orientierung <- data_Sexuelle_Orientierung[index,]
test_dfSexuelle_Orientierung <- data_Sexuelle_Orientierung[-index,]

#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------

# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  sampling = "smote", 
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 

set.seed(1997)
RFSexuelle_Orientierung <- train(Sexuelle_Orientierung ~ ., 
                                 data=train_dfSexuelle_Orientierung,
                                 tuneGrid = myGrid,
                                 method="ranger", 
                                 metric= "Kappa",
                                 num.tree = 500,
                                 trControl = myControl1, 
                                 na.action = na.omit,
                                 importance = 'impurity')

# Print models to console

RFSexuelle_Orientierung
summary(RFSexuelle_Orientierung)
plot(RFSexuelle_Orientierung)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFSexuelle_Orientierung, newdata=test_dfSexuelle_Orientierung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_Orientierung %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()

#save model to disk 

tree500_Sexuelle_Orientierung <- RFSexuelle_Orientierung
saveRDS(tree500_Sexuelle_Orientierung, "./tree500_Sexuelle_Orientierung.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFSexuelle_Orientierung1 <- train(Sexuelle_Orientierung ~ ., 
                                  data=train_dfSexuelle_Orientierung, 
                                  method="ranger", metric= "Kappa",
                                  tuneGrid = myGrid,
                                  na.action = na.omit,
                                  num.tree = 1000,
                                  trControl = myControl1, 
                                  importance = 'impurity')

# Print models
RFSexuelle_Orientierung1
summary(RFSexuelle_Orientierung1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFSexuelle_Orientierung1, newdata=test_dfSexuelle_Orientierung)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_Orientierung1 %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()


#save model to disk 

tree1000_Sexuelle_Orientierung <- RFSexuelle_Orientierung1
saveRDS(tree1000_Sexuelle_Orientierung, "./tree1000_Sexuelle_Orientierung.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFSexuelle_OrientierungFinal <- RFSexuelle_Orientierung

# Print models
RFSexuelle_OrientierungFinal 
summary(RFSexuelle_OrientierungFinal )

#evaluate variable importance 
varImp(RFSexuelle_OrientierungFinal )
plot(varImp(RFSexuelle_OrientierungFinal ), 20, main = "Sexuelle_Orientierung")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFSexuelle_OrientierungFinal , newdata=test_dfSexuelle_Orientierung)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfSexuelle_Orientierung$Sexuelle_Orientierung))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfSexuelle_Orientierung$Sexuelle_Orientierung,
                 predict(model, data, type = "prob")[, "Bisexuell"])
  
}

#model auc: 
RFSexuelle_OrientierungFinal %>%
  test_roc(data = test_dfSexuelle_Orientierung) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(RFSexuelle_OrientierungFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- RFSexuelle_OrientierungFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Bisexuell") %>%plotPartial (main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Bisexuell") %>%plotPartial(main = "Bisexuell")

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Heterosexuell") %>%plotPartial(main = "Heterosexuell")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Homosexuell") %>%plotPartial(main = "Homosexuell")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Sexuelle_Orientierung <- RFSexuelle_OrientierungFinal
saveRDS(besttree_Sexuelle_Orientierung, "./besttree_Sexuelle_Orientierung.rds")






#####################
#Heterosexual yes or no: binary
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#select data
data_Hetero <- data[,c(335, 27:255)]


# Convert DV to factor
data_Hetero$Heterosexuell = as.factor(data_Hetero$Heterosexuell)


#Are there NAs in the DV?
sum(is.na(data_Hetero$Heterosexuell))  
data_Hetero <- data_Hetero %>% subset(data_Hetero$Heterosexuell != "NA")


#is the variable imbalanced?
table(data_Hetero$Heterosexuell) #Verteilung in Ordnung
max(table(data_Hetero$Heterosexuell)/sum(table(data_Hetero$Heterosexuell)))  


#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Hetero$Heterosexuell, p=.8, list= FALSE, times= 1)

# Create train_df & test_df
train_dfHetero <- data_Hetero[index,]
test_dfHetero <- data_Hetero[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelHeteroRF <- train(Heterosexuell ~ ., 
                       data=train_dfHetero,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC",  
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelHeteroRF
summary(modelHeteroRF)
plot(modelHeteroRF)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelHeteroRF, newdata=test_dfHetero)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeteroRF %>%
  test_roc(data = test_dfHetero) %>%
  auc()


#ROC plot
model_list <- list(M1 = modelHeteroRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHetero)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Hetero <- modelHeteroRF
saveRDS(tree500_Hetero, "./tree500_Hetero.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelHeteroRF1 <- train(Heterosexuell ~ ., 
                        data=train_dfHetero,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "ROC", 
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl, 
                        importance = 'impurity')

# Print model to console

modelHeteroRF1
summary(modelHeteroRF1)
plot(modelHeteroRF1)


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelHeteroRF1, newdata=test_dfHetero)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeteroRF1 %>%
  test_roc(data = test_dfHetero) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelHeteroRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHetero)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Hetero <- modelHeteroRF1
saveRDS(tree1000_Hetero, "./tree1000_Hetero.rds")


####-------tree 3: Final --------------------------------------------------

#finalmodel
modelHeterofinal <- modelHeteroRF

# Print model
print(modelHeterofinal)

#output in terms of regression coefficients
summary(modelHeterofinal)

#evaluate variable importance 
varImp(modelHeterofinal)
plot(varImp(modelHeterofinal), 20, main = "Heterosexuell")


# predict outcome using model from train_df applied to the test_df
predictions <- predict(modelHeterofinal, newdata=test_dfHetero)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfHetero$Heterosexuell)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfHetero$Heterosexuell,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelHeterofinal %>%
  test_roc(data = test_dfHetero) %>%
  auc()

#ROC plot
model_list <- list(M1 = modelHeterofinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfHetero)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelHeterofinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelHeterofinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Hetero <- modelHeterofinal
saveRDS(besttree_Hetero, "./besttree_Hetero.rds")

#load the model

besttree_Hetero <- readRDS("./besttree_Hetero.rds")
print(besttree_Hetero)





####################
#Single or in a relationship (binary)
#####################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

#select data
data_AlleinBeziehung <- data[,c(316, 27:255)]


# Convert factor names of trial to caret compatible format (1 and 0 as numbers are not allowed)
data_AlleinBeziehung$Allein_vs_Beziehung = as.factor(data_AlleinBeziehung$Allein_vs_Beziehung)


#Are there NAs in the DV?
sum(is.na(data_AlleinBeziehung$Allein_vs_Beziehung))
data_AlleinBeziehung <- data_AlleinBeziehung %>% subset(data_AlleinBeziehung$Allein_vs_Beziehung != "NA")


#is the variable imbalanced?
table(data_AlleinBeziehung$Allein_vs_Beziehung) 
max(table(data_AlleinBeziehung$Allein_vs_Beziehung)/sum(table(data_AlleinBeziehung$Allein_vs_Beziehung))) 

#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_AlleinBeziehung$Allein_vs_Beziehung, p=.8, list= FALSE, times= 1)

# Create train_df & test_df

train_dfAlleinBeziehung <- data_AlleinBeziehung[index,]
test_dfAlleinBeziehung <- data_AlleinBeziehung[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  search = "grid",
)

####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)

modelAlleinBeziehungRF <- train(Allein_vs_Beziehung ~ ., 
                                data=train_dfAlleinBeziehung,
                                tuneGrid = myGrid,
                                method="ranger",
                                metric= "ROC", 
                                na.action = na.omit,
                                num.tree = 500,
                                trControl = myControl, 
                                importance = 'impurity')

# Print model to console

modelAlleinBeziehungRF
summary(modelAlleinBeziehungRF)
plot(modelAlleinBeziehungRF)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelAlleinBeziehungRF, newdata=test_dfAlleinBeziehung)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfAlleinBeziehung$Allein_vs_Beziehung)



#check for AU
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungRF %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#ROC-plot
model_list <- list(M1 = modelAlleinBeziehungRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlleinBeziehung)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_AlleinBeziehung <- modelAlleinBeziehungRF
saveRDS(tree500_AlleinBeziehung, "./tree500_AlleinBeziehung.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelAlleinBeziehungRF1 <- train(Allein_vs_Beziehung ~ ., 
                                 data=train_dfAlleinBeziehung,
                                 tuneGrid = myGrid,
                                 method="ranger", 
                                 metric= "ROC", 
                                 na.action = na.omit,
                                 num.tree = 1000,
                                 trControl = myControl, 
                                 importance = 'impurity')

# Print model to console

modelAlleinBeziehungRF1
summary(modelAlleinBeziehungRF1)
plot(modelAlleinBeziehungRF1)

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(modelAlleinBeziehungRF1, newdata=test_dfAlleinBeziehung)

# Create confusion matrix  
confusionMatrix(data=predictions1, test_dfAlleinBeziehung$Allein_vs_Beziehung)


#check for AUC
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungRF1 %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#ROC-plot
model_list <- list(M1 = modelAlleinBeziehungRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlleinBeziehung)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_AlleinBeziehung <- modelAlleinBeziehungRF1
saveRDS(tree1000_AlleinBeziehung, "./tree1000_AlleinBeziehung.rds")



####-------tree 3: Final --------------------------------------------------

modelAlleinBeziehungfinal <- modelAlleinBeziehungRF

# Print model
print(modelAlleinBeziehungfinal)

#output in terms of regression coefficients
summary(modelAlleinBeziehungfinal)

#evaluate variable importance 
varImp(modelAlleinBeziehungfinal)
plot(varImp(modelAlleinBeziehungfinal), 20, main = "Allein_vs_Beziehung")


# predict outcome using model from train_df applied to the test_df

predictions2 <- predict(modelAlleinBeziehungfinal, newdata=test_dfAlleinBeziehung)

# Create confusion matrix  
confusionMatrix(data=predictions2, test_dfAlleinBeziehung$Allein_vs_Beziehung)


#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfAlleinBeziehung$Allein_vs_Beziehung,
      predict(model, data, type = "prob")[, "Allein"])
  
}

modelAlleinBeziehungfinal %>%
  test_roc(data = test_dfAlleinBeziehung) %>%
  auc()


#ROC plot
model_list <- list(M1 = modelAlleinBeziehungfinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfAlleinBeziehung)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#--------------Variable Direction: Partial Plots-----------------------------------------


#checking direction of the 20 most important variables

imp <- importance(modelAlleinBeziehungfinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]


PartialPlots <- modelAlleinBeziehungfinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Allein") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Allein") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_allein_beziehung <- modelAlleinBeziehungfinal
saveRDS(besttree_allein_beziehung, "./besttree_allein_beziehung.rds")

#load the model

besttree_allein_beziehung <- readRDS("./besttree_allein_beziehung.rds")
print(besttree_allein_beziehung)



#######################
#Relationship status: Categorical 
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------


#define data for analysis
data_Beziehungsstatus <- data[,c(18, 27:255)]

#Are there NAs in the DV?
sum(is.na(data_Beziehungsstatus$Beziehungsstatus)) #17 NAs
data_Beziehungsstatus <- data_Beziehungsstatus %>% subset(data_Beziehungsstatus$Beziehungsstatus != "NA")


#is the variable imbalanced?
table(data_Beziehungsstatus$Beziehungsstatus)
max(table(data_Beziehungsstatus$Beziehungsstatus)/sum(table(data_Beziehungsstatus$Beziehungsstatus)))

#IV als Faktor:
data_Beziehungsstatus$Beziehungsstatus <- as.factor(data_Beziehungsstatus$Beziehungsstatus)

#adapt variable names for analysis: only three categories, summarize single, divorced and widowed
data_Beziehungsstatus <- data_Beziehungsstatus %>% mutate(Beziehungsstatus = case_when(Beziehungsstatus == "Geschieden" ~ 'Single',
                                                                                       Beziehungsstatus == "In einer Beziehung" ~ 'In_Beziehung',
                                                                                       Beziehungsstatus == "Single" ~ 'Single',
                                                                                       Beziehungsstatus == "Verheiratet" ~ 'Verheiratet',
                                                                                       Beziehungsstatus == "Verwitwet" ~ 'Single'))

#is the variable imbalanced?
table(data_Beziehungsstatus$Beziehungsstatus)
max(table(data_Beziehungsstatus$Beziehungsstatus)/sum(table(data_Beziehungsstatus$Beziehungsstatus)))


#----------------------------------------DATA PARTITIONING------------------------------------

#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values

index <- createDataPartition(data_Beziehungsstatus$Beziehungsstatus, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_dfGeschlecht

train_dfBeziehungsstatus <- data_Beziehungsstatus[index,]
test_dfBeziehungsstatus <- data_Beziehungsstatus[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation

set.seed(1997)
myControl1 = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = defaultSummary, 
  classProbs = TRUE, 
  allowParallel=TRUE,
  search = "grid"
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size for 500 trees

#set random seed again 
set.seed(1997)
RFBeziehungsstatus <- train(Beziehungsstatus ~ ., 
                            data=train_dfBeziehungsstatus,
                            tuneGrid = myGrid,
                            method="ranger", 
                            metric= "Kappa",
                            num.tree = 500,
                            trControl = myControl1, 
                            na.action = na.omit,
                            importance = 'impurity')

# Print models to console
RFBeziehungsstatus
summary(RFBeziehungsstatus)
plot(RFBeziehungsstatus)

# predict outcome using model from train_df applied to the test_df
predictions <- predict(RFBeziehungsstatus, newdata=test_dfBeziehungsstatus)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatus %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()

#save model to disk 

tree500_Beziehungsstatus <- RFBeziehungsstatus
saveRDS(tree500_Beziehungsstatus, "./tree500_Beziehungsstatus.rds")



####-------tree 2: test higher num.tree --------------------------------------------------

#try for num.tree = 1000 --> is more better?

set.seed(1997)
RFBeziehungsstatus1 <- train(Beziehungsstatus ~ ., 
                             data=train_dfBeziehungsstatus, 
                             method="ranger", metric= "Kappa",
                             tuneGrid = myGrid,
                             na.action = na.omit,
                             num.tree = 1000,
                             trControl = myControl1, 
                             importance = 'impurity')

# Print models
RFBeziehungsstatus1
summary(RFBeziehungsstatus1)

# predict outcome using model from train_df applied to the test_df
predictions2 <- predict(RFBeziehungsstatus1, newdata=test_dfBeziehungsstatus)


# Create confusion matrix
confusionMatrix(data=as.factor(predictions2), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))


#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatus1 %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()


#save model to disk 

tree1000_Beziehungsstatus <- RFBeziehungsstatus1
saveRDS(tree1000_Beziehungsstatus, "./tree1000_Beziehungsstatus.rds")


####-------tree 3: Final --------------------------------------------------

#final model

set.seed(1997)
RFBeziehungsstatusFinal <- RFBeziehungsstatus1

# Print models
RFBeziehungsstatusFinal 
summary(RFBeziehungsstatusFinal )

#evaluate variable importance 
varImp(RFBeziehungsstatusFinal )
plot(varImp(RFBeziehungsstatusFinal ), 20, main = "Beziehungsstatus")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(RFBeziehungsstatusFinal , newdata=test_dfBeziehungsstatus)

# Create confusion matrix
confusionMatrix(data=as.factor(predictions3), as.factor(test_dfBeziehungsstatus$Beziehungsstatus))

#check for auc
test_roc <- function(model, data) {
  
  multiclass.roc(test_dfBeziehungsstatus$Beziehungsstatus,
                 predict(model, data, type = "prob")[, "Single"])
  
}

#model auc: 
RFBeziehungsstatusFinal %>%
  test_roc(data = test_dfBeziehungsstatus) %>%
  auc()


#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(RFBeziehungsstatusFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- RFBeziehungsstatusFinal 

PartialPlots %>% partial(pred.var = impvar[1], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "In_Beziehung") %>%plotPartial(main = "In einer Beziehung")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Single") %>%plotPartial(main = "Single")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Single") %>%plotPartial(main = "Single")


PartialPlots %>% partial(pred.var = impvar[1], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Verheiratet") %>%plotPartial(main = "Verheiratet")


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Beziehungsstatus <- RFBeziehungsstatusFinal
saveRDS(besttree_Beziehungsstatus, "./besttree_Beziehungsstatus.rds")

#load the model

besttree_Beziehungsstatus <- readRDS("./besttree_Beziehungsstatus.rds")
print(besttree_Beziehungsstatus)



#######################
#Children yes/no (binary)
######################

#--------------------------------------DATA PRE-PROCESSING------------------------------------------

# choose relevant columns
data_Kinder <- data[,c(288, 27:255)]


# Convert factor names of trial to caret compatible format (1 and 0 as numbers are not allowed)
data_Kinder$Kinder = as.character(data_Kinder$Kinder)
data_Kinder$Kinder[data_Kinder$Kinder == "2"] = "Nein"
data_Kinder$Kinder[data_Kinder$Kinder == "1"] = "Ja"
data_Kinder$Kinder = as.factor(data_Kinder$Kinder)


# Change order of factor levels such that "Yes" is interpreted as positive and "No" is interpreted as negative
levels(data_Kinder$Kinder)
data_Kinder$Kinder = factor(data_Kinder$Kinder, levels = c("Ja", "Nein"))
levels(data_Kinder$Kinder)


#Are there NAs in the DV?
sum(is.na(data_Kinder$Kinder))
data_Kinder <- data_Kinder %>% subset(data_Kinder$Kinder != "NA")


#is the variable imbalanced?
table(data_Kinder$Kinder) 
max(table(data_Kinder$Kinder)/sum(table(data_Kinder$Kinder))) 


#----------------------------------------DATA PARTITIONING------------------------------------


#Training und Test Dataset
set.seed(1997)

# Partitioning of the data: Create index matrix of selected values
index <- createDataPartition(data_Kinder$Kinder, p=.8, list= FALSE, times= 1)

# Create train_dfGeschlecht & test_df

train_dfKinder <- data_Kinder[index,]
test_dfKinder <- data_Kinder[-index,]


#---------------------------------------------------RANDOM FOREST----------------------------------------------------

#--------------------------------------------BUILDING AND TRAINING THE MODEL---------------------------------------------


# Specify the type of training method used & number of folds --> 10-fold Cross-Validation
set.seed(1997)
myControl = trainControl(
  method = "cv",
  number = 10, 
  verboseIter = TRUE,
  summaryFunction = twoClassSummary,  
  classProbs = TRUE,
  allowParallel=TRUE,
  sampling = "smote",  
  search = "grid",
)


####-------tree 1: tuning of mtry, splitrule and min.node.size for 500 trees  --------------------------------------------------

# test of the ideal mtry, splitrule and min-node.size

set.seed(1997)
modelKinderRF <- train(Kinder ~ ., 
                       data=train_dfKinder,
                       tuneGrid = myGrid,
                       method="ranger",
                       metric= "ROC",  
                       na.action = na.omit,
                       num.tree = 500,
                       trControl = myControl, 
                       importance = 'impurity')

# Print model to console

modelKinderRF
summary(modelKinderRF)
plot(modelKinderRF)

# predict outcome using model from train_df applied to the test_df

predictions <- predict(modelKinderRF, newdata=test_dfKinder)

# Create confusion matrix  
confusionMatrix(data=predictions, test_dfKinder$Kinder)

#check for AUC
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderRF %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#  ROC-plot
model_list <- list(M1 = modelKinderRF)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfKinder)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)

#save model to disk 

tree500_Kinder <- modelKinderRF
saveRDS(tree500_Kinder, "./tree500_Kinder.rds")


####-------tree 2: test higher num.tree --------------------------------------------------

# test of ideal num.tree --> try if numtree 1000 leads to better results!

set.seed(1997)
modelKinderRF1 <- train(Kinder ~ ., 
                        data=train_dfKinder,
                        tuneGrid = myGrid,
                        method="ranger", 
                        metric= "ROC", 
                        na.action = na.omit,
                        num.tree = 1000,
                        trControl = myControl, 
                        importance = 'impurity')

# Print model to console

modelKinderRF1
summary(modelKinderRF1)
plot(modelKinderRF1)

# predict outcome using model from train_df applied to the test_df

predictions1 <- predict(modelKinderRF1, newdata=test_dfKinder)

# Create confusion matrix  
confusionMatrix(data=predictions1, test_dfKinder$Kinder)


#check for AUC
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderRF1 %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#ROC-plot
model_list <- list(M1 = modelKinderRF1)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfKinder)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)


#save model to disk 

tree1000_Kinder <- modelKinderRF1
saveRDS(tree1000_Kinder, "./tree1000_Kinder.rds")


####-------tree 3: Final --------------------------------------------------

#define final model
modelKinderFinal <- modelKinderRF

# Print model
print(modelKinderFinal)

#output in terms of regression coefficients
summary(modelKinderFinal)

#evaluate variable importance 
varImp(modelKinderFinal)
plot(varImp(modelKinderFinal), 20, main = "weiblich_maennlich")


# predict outcome using model from train_df applied to the test_df
predictions3 <- predict(modelKinderFinal, newdata=test_dfKinder)

# Create confusion matrix  
confusionMatrix(data=predictions3, test_dfKinder$Kinder)

#check for AUC 
test_roc <- function(model, data) {
  
  roc(test_dfKinder$Kinder,
      predict(model, data, type = "prob")[, "Ja"])
  
}

modelKinderFinal %>%
  test_roc(data = test_dfKinder) %>%
  auc()

#ROC plot
model_list <- list(ModelFinal = modelKinderFinal)

model_list_roc <- model_list %>%
  map(test_roc, data = test_dfKinder)

model_list_roc %>%
  map(auc)

results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    tibble(tpr = the_roc$sensitivities,
           fpr = 1 - the_roc$specificities,
           model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve 
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)



#--------------Variable Direction: Partial Plots-----------------------------------------

#checking direction of the 20 most important variables

imp <- importance(modelKinderFinal$finalModel)
imp <- as.data.frame(imp)
impvar <- rownames(imp)[order(imp[1], decreasing=TRUE)]
impvar <- impvar[1:20]

PartialPlots <- modelKinderFinal

PartialPlots %>% partial(pred.var = impvar[1], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[2], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[3], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[4], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[5], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[6], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[7], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[8], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[9], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[10], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[11], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[12], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[13], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[14], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[15], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[16], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[17], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[18], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[19], which.class = "Ja") %>%plotPartial
PartialPlots %>% partial(pred.var = impvar[20], which.class = "Ja") %>%plotPartial


#------------------------------------------------WHEN BEST MODEL IS FOUND-----------------------------------------------------

#save model to disk 

besttree_Kinder <- modelKinderFinal
saveRDS(besttree_Kinder, "./besttree_Kinder.rds")

#load the model

besttree_Kinder <- readRDS("./besttree_Kinder.rds")
print(besttree_Kinder)



