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

str(data) #alles wie gewollt
  
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

#"keine Angaben" als NA umcoden, um aus Analyse auszuschließen
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


#Version 2: mit je nur 2 Ausgängen coden: <4 oder >4; 4 = NA
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

#Wohnort innerhalb Deutschland: alte vs neue Bundesländer
data <- data %>% mutate(Ost_West = case_when(PLZ <= 19 ~ 'Osten',
                                                      PLZ >= 20 & PLZ <= 38 ~ 'Westen',
                                                      PLZ == 39 ~ 'Osten',
                                                      PLZ >= 40 & PLZ <= 97 ~ 'Westen',
                                                      PLZ >= 98 ~ 'Osten'))
#Altersgruppe: jung, mittel, alt
data <- data %>% mutate(Age_Range = case_when(Alter >= 45 ~ 'hohes.Alter',
                                                      Alter >= 30  & Alter <= 44 ~ 'mittleres.Alter',
                                                      Alter <= 29 ~ 'niedriges.Alter'))

#Geschlecht: nur weiblich und männlich
data <- data %>% mutate(weiblich_maennlich = case_when(Geschlecht == 1 ~ 'weiblich',
                                                      Geschlecht == 2 ~ 'männlich'))


#Bildung: niedrig, mittel, hoch
data <- data %>% mutate(Bildungsgruppe = case_when(Bildungsabschluss == "(Noch) kein Abschluss" ~ 'niedrig',
                                                           Bildungsabschluss == "Hauptschulabschluss" ~ 'niedrig',
                                                           Bildungsabschluss == "Realschulabschluss" ~ 'niedrig',
                                                           Bildungsabschluss == "Abitur" ~ 'mittel',
                                                           Bildungsabschluss == "Hochschulabschluss (Bachelor oder Master)" ~ 'hoch',
                                                           Bildungsabschluss == "Promotion" ~ 'hoch'))

#Einkommen: hohe, mittlere, niedrige Einkommensgruppe
data <- data %>% mutate(Einkommensgruppe = case_when(Nettoeinkommen == "0 € - 1000 €" ~ 'niedrig',
                                                               Nettoeinkommen == "1001 € - 2000€" ~ 'niedrig',
                                                               Nettoeinkommen == "2001 € - 3000 €" ~ 'mittel',
                                                               Nettoeinkommen == "3001 € - 4000 €" ~ 'mittel',
                                                               Nettoeinkommen == "4001 € - 5000 €" ~ 'hoch',
                                                               Nettoeinkommen == "Mehr als 5000 €" ~ 'hoch'))

#Beziehungsstatus: alleine oder in Beziehung
data <- data %>% mutate(Allein_vs_Beziehung = case_when(Beziehungsstatus == "Single" ~ 'Allein',
                                                    Beziehungsstatus == "Geschieden" ~ 'Allein',
                                                    Beziehungsstatus == "Verwitwet" ~ 'Allein',
                                                    Beziehungsstatus == "In einer Beziehung" ~ 'Beziehung',
                                                    Beziehungsstatus == "Verheiratet" ~ 'Beziehung'))

#Beschäftigung: arbeitend oder nicht arbeitend
data <- data %>% mutate(Arbeitend_oder_nicht = case_when(Beschaeftigung == "Arbeitslos/-suchend" ~ 'Nein',
                                                         Beschaeftigung == "Auszubildende/r" ~ 'Ja',
                                                         Beschaeftigung == "Berufstätige/r" ~ 'Ja',
                                                         Beschaeftigung == "Hausfrau/-mann" ~ 'Nein',
                                                         Beschaeftigung == "Rentner/in" ~ 'Nein',
                                                         Beschaeftigung == "Schüler/in" ~ 'Nein',
                                                         Beschaeftigung == "Student/in" ~ 'Nein'))


#Alkohol Konsum: nie, selten, häufig (3 Ausprägungen) oder Konsum ja nein (2 Ausprägungen)
data <- data %>% mutate(Alkoholgruppe = case_when(`Alkohol_Konsum` == "Nein" ~ 'kein_Konsum',
                                                          `Alkohol_Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedrig',
                                                          `Alkohol_Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedrig',
                                                          `Alkohol_Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoch',
                                                          `Alkohol_Konsum` == "Ja, mehrmals pro Woche" ~ 'hoch',
                                                          `Alkohol_Konsum` == "Ja, täglich" ~ 'hoch'))

data <- data %>% mutate(Alkohol_ja_nein = ifelse(Alkohol_Konsum == "Nein", "Nein", "Ja"))

#Zigaretten Konsum: nie, selten, häufig (3 Ausprägungen) oder Konsum ja nein (2 Ausprägungen)
data <- data %>% mutate(Zigarettengruppe = case_when(`Zigaretten_Konsum` == "Nein" ~ 'kein_Konsum',
                                                                `Zigaretten_Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedrig',
                                                                `Zigaretten_Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedrig',
                                                                `Zigaretten_Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoch',
                                                                `Zigaretten_Konsum` == "Ja, mehrmals pro Woche" ~ 'hoch',
                                                                `Zigaretten_Konsum` == "Ja, täglich" ~ 'hoch'))

data <- data %>% mutate(Zigaretten_ja_nein = ifelse(Zigaretten_Konsum == "Nein", "Nein", "Ja"))


#Drogen Konsum: nie, selten, häufig (3 Ausprägungen) oder Konsum ja nein (2 Ausprägungen)
data <- data %>% mutate(Drogengruppe = case_when(`Drogen_Konsum` == "Nein" ~ 'kein_Konsum',
                                                        `Drogen_Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedrig',
                                                        `Drogen_Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedrig',
                                                        `Drogen_Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoch',
                                                        `Drogen_Konsum` == "Ja, mehrmals pro Woche" ~ 'hoch',
                                                        `Drogen_Konsum` == "Ja, täglich" ~ 'hoch'))

data <- data %>% mutate(Drogen_ja_nein = ifelse(Drogen_Konsum == "Nein", "Nein", "Ja"))


#Ziele im Leben: Ziel wichtig (5 oder mehr) oder nicht?
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

#Religion: Religiös ja nein und Islam vs. Christemtum
data <- data %>% mutate(Religioes = case_when(Religion == "Christentum" ~ 'Ja',
                                              Religion == "Islam" ~ 'Ja',
                                              Religion == "Judentum" ~ 'Ja',
                                              Religion == "Ich fühle mich keiner Religion zugehörig" ~ 'Nein'))

data <- data %>% mutate(Islam_oder_Christ = case_when(Religion == "Christentum" ~ 'Christentum',
                                                      Religion == "Islam" ~ 'Islam'))

#sexuelle Orientierung: Heterosexuell ja oder nein?
data <- data %>% mutate(Heterosexuell = case_when(Sexuelle_Orientierung == "Heterosexuell" ~ 'Ja',
                                                  Sexuelle_Orientierung == "Homosexuell" ~ 'Nein',
                                                  Sexuelle_Orientierung == "Bisexuell" ~ 'Nein'))

#Anzahl Kinder
#wir wissen, dass einer unserer Privatkontakte sich verklickt hat und statt 20 Kindern 0 eingeben wollte --> Korrektur hierfür, um Datensatz nicht aussortieren zu müssen:
data$`Anzahl_Kinder`[data$`Anzahl_Kinder` == 20] <- 0

#weiter zusammenfassen: 0 Kinder, 1 Kind, 2 Kinder, 3+ Kinder
data <- data %>% mutate(Anzahl_Kinder_grob = case_when(Anzahl_Kinder == 0 ~ "0",
                                                       Anzahl_Kinder == 1 ~ "1",
                                                       Anzahl_Kinder == 2 ~ "2",
                                                       Anzahl_Kinder >2 ~ "3_oder_mehr"))

#Wahl Partei: zu nur zwei Ausgängen formulieren, z.B.: CDU ja oder nein
data <- data %>% mutate(CDU_CSU_Waehler = ifelse(Wahl_Partei == "CDU/CSU", "Ja", "Nein"))
data <- data %>% mutate(SPD_Waehler = ifelse(Wahl_Partei == "SPD", "Ja", "Nein"))
data <- data %>% mutate(Gruene_Waehler = ifelse(Wahl_Partei == "Bündnis 90/Die Grünen", "Ja", "Nein"))
data <- data %>% mutate(FDP_Waehler = ifelse(Wahl_Partei == "FDP", "Ja", "Nein"))
data <- data %>% mutate(AfD_Waehler = ifelse(Wahl_Partei == "AfD", "Ja", "Nein"))
data <- data %>% mutate(Linke_Waehler = ifelse(Wahl_Partei == "Die Linke", "Ja", "Nein"))
data <- data %>% mutate(Nichtwahler = ifelse(Wahl_Partei == "Ich würde nicht wählen gehen", "Ja", "Nein"))

#Einkommen: über oder unter Durchschnitt Nettoeinkommen
data <- data %>% mutate(Durchschnittseinkommen = case_when(Nettoeinkommen == "0 € - 1000 €" ~ 'weniger2000',
                                                           Nettoeinkommen == "1001 € - 2000€" ~ 'weniger2000',
                                                           Nettoeinkommen == "2001 € - 3000 €" ~ 'mehr2000',
                                                           Nettoeinkommen == "3001 € - 4000 €" ~ 'mehr2000',
                                                           Nettoeinkommen == "4001 € - 5000 €" ~ 'mehr2000',
                                                           Nettoeinkommen == "Mehr als 5000 €" ~ 'mehr2000'))


#####
#new info

#wie vielen Accounts folgt jeder der Befragten?
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
table(data$`Sexuelle_Orientierung`)
#sonstiges ist nicht weiter relevant für uns (nur n = 6)

#Children
table(data$`Anzahl_Kinder`) #most people without children
###necessary to clean 3 respondents with >10 children?
many_children <- data %>% subset(data$`Anzahl_Kinder` > 10) #prüfen ob sonstige Antworten Sinn ergeben --> keine Auffälligkeiten, daher kein Ausschluss notwendig

table(data$`Anzahl_Kinder`) #Alternative 1
table(data$Anzahl_Kinder_grob) #Alternative 2


#Education
table(data$Bildungsabschluss)
#zusammengefasst:
table(data$Bildungsgruppe)



#Beschäftigung
table(data$Beschaeftigung)
#zusammengefasst:
table(data$Arbeitend_oder_nicht)

#Migrationshintergrund
table(data$Migrationshintergrund)
round(table(data$Migrationshintergrund)/sum(table(data$Migrationshintergrund)), 2) #17% mit Migrationshintergrund

#woher Migration
table(data$`Woher_Vorfahren`) #Hintergrund vorwiegend aus Europa und Asien --> vermutlich viele Europäer und Türken


#Religion
table(data$Religion)

#auch offene Nennungen beachten?
table(data$`Religion_Sonstiges`) #keine Religion oft genug erwähnt, um sie nachträglich mit aufzunehmen (max. 3x)

#Religiösität
table(data$Religioes) #2/3 sind religiös nach eigener Angabe
#Islam vs. Christen
table(data$Islam_oder_Christ) #deutlich mehr Christen

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

#Variablen: Extraversion2, Agreeableness2 usw. teilen in zwei Gruppen auf:
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

#zusammengefasste Variable: Green Values ja oder nein
table(data$Green2)/sum(table(data$Green2)) #84% mit eher grünen Werten


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

#Zusammengefasst: Variablen zu Wichtigkeit des Ziels
table(data$Zugehoerigkeit_wichtig) 
table(data$Spannung_wichtig) 
table(data$Herzliche_Beziehung_wichtig) 
table(data$Selbstverwirklichung_wichtig) 
table(data$Respekt_wichtig) 
table(data$Spass_Freude_wichtig) 
table(data$Sicherheit_wichtig) 
table(data$Selbstachtung_wichtig) 
table(data$Erfolg_wichtig) 

#Parteien
round(table(data$`Wahl_Partei`)/sum(table(data$`Wahl_Partei`)), 2) #relative shares of voters in our dataset
#differs a bit from actual German voting data/ forecasts; mainly more Grüne and less CDU and SPD

#Parteien Sonstige - was tun?
table(data$`Wahl_Partei_Sonstiges`)
#Offene Nennungen Parteien: Aufnahme von "Die Partei"
data$`Wahl_Partei_Sonstiges` <- tolower(data$`Wahl_Partei_Sonstiges`)
data$`Wahl_Partei` <- ifelse(data$`Wahl_Partei_Sonstiges` %in% "die partei", "Die Partei", data$`Wahl_Partei`)

#auch für Die Partei Wähler Variable erstellen
data <- data %>% mutate(Die_Partei_Waehler = ifelse(Wahl_Partei == "Die Partei", "Ja", "Nein"))


Partei <- as.data.frame(table(data$`Wahl_Partei`)/sum(table(data$`Wahl_Partei`)))

Partei_Order <- c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "AfD", "Die Linke", "FDP", "Die Partei", "Sonstige:", "Ich würde nicht wählen gehen")
ggplot(Partei, aes(factor(Var1, levels = Partei_Order), Freq))+
  geom_col()+
  geom_text(aes(label = percent(Freq)), vjust = -1)+
  labs(x = "Parties", y = "", title = "Voters per Party")+
  ylim(0,0.3)



#Corona: 4 Gruppen eingeteilt: Hardliner, Softliner, Skeptiker, Leugner
table(data$Corona_Hardliner) #1166 Hardliner: Wollen härtere Maßnahmen
table(data$Corona_Softliner) #537 Softliner: Wollen softere Maßnahmen
table(data$Corona_Skeptiker) #299 Skeptiker: Bezweifeln Gefährlichkeit des Virus
table(data$Corona_Leugner) #124 Leugner: Glauben nicht an Virus



#Alkohol, Zigaretten, Drogen
table(data$Alkohol_Konsum)
table(data$Zigaretten_Konsum)
table(data$Drogen_Konsum)
#zusammengefasst: V1 mit 3 Ausprägungen
table(data$Alkoholgruppe)
table(data$Zigarettengruppe)
table(data$Drogengruppe)
#zusammengefasst: V2 mit 2 Ausprägungen
table(data$Alkohol_ja_nein)
table(data$Zigaretten_ja_nein)
table(data$Drogen_ja_nein)



#Korrelationen zwischen Accounts?
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



#PLZ --> Münster Region dominiert noch immer
table(reduced_set$PLZ)
ggplot(reduced_set, aes(x = PLZ))+
  geom_bar()+
  geom_text(stat = "count", aes(label =..count..), vjust = -1)+
  labs(x = "PLZ", y = "Count", title = "Abs. Count per PLZ")+
  ylim(0,200)

#PLZ zusammengefasst in Ost/West
table(reduced_set$Ost_West) #passt zum Deutschlandweiten Verhältnis von ca. 1:5

#Beziehungsstatus
table(reduced_set$Beziehungsstatus)
#zusammengefasst:
table(reduced_set$Allein_vs_Beziehung)


#sexuelle Orientierung
table(reduced_set$`Sexuelle_Orientierung`)
table(reduced_set$`Sexuelle_Orientierung`)/sum(table(reduced_set$`Sexuelle_Orientierung`))
#sonstiges ist nicht weiter relevant für uns (nur n = 6)

#Children
table(reduced_set$Kinder)
table(reduced_set$Kinder)/sum(table(reduced_set$Kinder))


#Education
table(reduced_set$Bildungsabschluss)
#zusammengefasst:
table(reduced_set$Bildungsgruppe)



#Beschäftigung
table(reduced_set$Beschaeftigung)
table(reduced_set$Beschaeftigung)/sum(table(reduced_set$Beschaeftigung))
#zusammengefasst:
table(reduced_set$Arbeitend_oder_nicht)

#Migrationshintergrund
table(reduced_set$Migrationshintergrund)
round(table(reduced_set$Migrationshintergrund)/sum(table(reduced_set$Migrationshintergrund)), 2) #17% mit Migrationshintergrund

#woher Migration
table(reduced_set$`Woher_Vorfahren`) #Hintergrund vorwiegend aus Europa und Asien --> vermutlich viele Europäer und Türken


#Religion
table(reduced_set$Religion)

#Religiösität
table(reduced_set$Religioes) #2/3 sind gläubig nach eigener Angabe
#Islam vs. Christen
table(reduced_set$Islam_oder_Christ) #deutlich mehr Christen

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

#Variablen: Extraversion2, Agreeableness2 usw. teilen in zwei Gruppen auf:
table(reduced_set$Extraversion2)/sum(table(reduced_set$Extraversion2)) #51% Introvertiert
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

#zusammengefasste Variable: Green Values ja oder nein
table(reduced_set$Green2)/sum(table(reduced_set$Green2)) #84% mit eher grünen Werten


#General Goals in Life
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
#für Analyse schwer, da stets viele hohe Werte und wenig Abweichungen!


#Zusammengefasst: Variablen zu Wichtigkeit des Ziels
table(reduced_set$Zugehoerigkeit_wichtig) 
table(reduced_set$Spannung_wichtig) 
table(reduced_set$Herzliche_Beziehung_wichtig) 
table(reduced_set$Selbstverwirklichung_wichtig) 
table(reduced_set$Respekt_wichtig) 
table(reduced_set$Spass_Freude_wichtig) 
table(reduced_set$Sicherheit_wichtig) 
table(reduced_set$Selbstachtung_wichtig) 
table(reduced_set$Erfolg_wichtig) 
#Problematik bleibt bestehen, jedes Ziel ist fast jedem wichtig, stark imbalanced

#Parteien
round(table(reduced_set$`Wahl_Partei`)/sum(table(reduced_set$`Wahl_Partei`)), 2) #relative shares of voters in our dataset
#quasi unverändert zu Daten vor Cleaning

Partei <- as.data.frame(table(reduced_set$`Wahl_Partei`)/sum(table(reduced_set$`Wahl_Partei`)))

Partei_Order <- c("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "AfD", "Die Linke", "FDP", "Die Partei", "Sonstige:", "Ich würde nicht wählen gehen")
ggplot(Partei, aes(factor(Var1, levels = Partei_Order), Freq))+
  geom_col()+
  geom_text(aes(label = percent(Freq)), vjust = -1)+
  labs(x = "Parties", y = "", title = "Voters per Party")+
  ylim(0,0.3)



#Corona: 4 Gruppen eingeteilt: Hardliner, Softliner, Skeptiker, Leugner
table(reduced_set$Corona_Hardliner) #1068 Hardliner: Wollen härtere Maßnahmen
table(reduced_set$Corona_Softliner) #480 Softliner: Wollen softere Maßnahmen
table(reduced_set$Corona_Skeptiker) #260 Skeptiker: Bezweifeln Gefährlichkeit des Virus
table(reduced_set$Corona_Leugner) #104 Leugner: Glauben nicht an Virus
#alle außer Hardliner sind imbalanced

#numerische Ausprägungen: 
summary(reduced_set$Corona_Massnahmen_muessten_haerter_sein) #Verteilung einigermaßen ausgeglichen
summary(reduced_set$Corona_Massnahmen_uebertrieben) #median ist 2, mean ist 2.845
summary(reduced_set$Corona_ist_harmlos_gleich_Grippe) #median ist 1
summary(reduced_set$Glaube_nicht_an_Corona) #median ist 1


#Alkohol, Zigaretten, Drogen
table(reduced_set$Alkohol_Konsum)
table(reduced_set$Zigaretten_Konsum)
table(reduced_set$Drogen_Konsum)
#zusammengefasst: V1 mit 3 Ausprägungen
table(reduced_set$Alkoholgruppe)
table(reduced_set$Zigarettengruppe)
table(reduced_set$Drogengruppe)
#zusammengefasst: V2 mit 2 Ausprägungen
table(reduced_set$Alkohol_ja_nein)
table(reduced_set$Zigaretten_ja_nein)
table(reduced_set$Drogen_ja_nein)



#Korrelationen zwischen Accounts?
cor_accounts_df <- as.data.frame(cor(Accounts))
view(cor_accounts_df)



#----------------------------------------------------
#first insights: growing a single tree for three selected variables: Age Range, Alter, Geschlecht

data <- reduced_set

#######################
#Alter: Age Ranges Categorical (hoch, mittel, niedrig)
######################

#define data for analysis
data_AgeRange <- data[,c(312, 27:255)]

#Gibt es NAs in der DV?
sum(is.na(data_AgeRange$Age_Range))
data_AgeRange <- data_AgeRange %>% subset(data_AgeRange$Age_Range != "NA")


#ist die Variable unbalanced?
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
#Alter: numerisch
######################

data_Alter<- data[,c(24, 27:255)]

cols_Alter <- names(data_Alter)
data_Alter$Alter <- as.numeric(data_Alter$Alter)

#Gibt es NAs in der DV?
sum(is.na(data_Alter$Alter)) #keine NAs

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
#Geschlecht
######################

data_GeschlechtMW <- data[,c(313, 27:255)]

cols_Geschlecht <- names(data_GeschlechtMW)
data_GeschlechtMW$weiblich_maennlich <- as.factor(data_GeschlechtMW$weiblich_maennlich)

#Gibt es NAs in der DV?
sum(is.na(data_GeschlechtMW$weiblich_maennlich))  
data_GeschlechtMW <- data_GeschlechtMW %>% subset(data_GeschlechtMW$weiblich_maennlich != "NA")


#ist die Variable unbalanced?
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





