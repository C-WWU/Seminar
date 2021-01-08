data_all_var$Altersgruppe <- 
  if ("Alter" >= 61) {
    print("Ueber60")
  } else if ("Alter" >= 55 & "Alter" < 60) {
    print("Unter60");    
  } else if ("Alter" >= 50 & "Alter" < 55) {
    print("Unter55");    
  } else if ("Alter" >= 45 & "Alter" < 50) {
    print("Unter50");
  } else if ("Alter" >= 40 & "Alter" < 45) {
    print("Unter45");    
  } else if ("Alter" >= 35 & "Alter" < 40) {
    print("Unter40");    
  } else if ("Alter" >= 30 & "Alter" < 35) {
    print("Unter35"); 
  } else if ("Alter" >= 25 & "Alter" < 30) {
    print("Unter30"); 
  } else if ("Alter" >= 20 & "Alter" < 25) {
    print("Unter25"); 
  } else {
    print("Unter20");    
  }

#Bildung Gruppieren
data_all_var$Bildung <- 
  if("Bildungsabschluss" == "(Noch) kein Abschluss") {
    print ("niedrig");
  } else if ("Bildungsabschluss" == "Hauptschulabschluss"){
    print ("niedrig");
  } else if ("Bildungsabschluss" == "Realschulabschluss"){
    print ("niedrid");
  } else if ("Bildungsabschluss" == "Abitur"){
      print ("mittel");
  } else if ("Bildungsabschluss" == "Hochschulabschluss (Bachelor oder Master)"){
    print ("hoch");
  } else if ("Bildungsabschluss" == "Promotion"){
    print ("hoch");
  } else {
    print ("Keine Angabe");
  }

#Religion Zugehörigkeit
data_all_var$Religion_Zug <-
  if("Religion" == "Christentum"){
    print ("Ja");
  } else if ("Religion" == "Islam"){
    print ("Ja");
  } else if ("Religion" == "Judentum"){
    print ("Ja");
  } else {
    print ("Nein")
  }
# Was machen wir mit sonstige?

#PLZ - Die Null wird nicht mit angezeigt müssen vorher die Daten noch anpassen
data_all_var$Ost-West <-
  if("PLZ" = xxx){
    print ("Ost")
  } else {
    print ("West")
  }

#Beziehung Gruppen, würde ich nicht machen