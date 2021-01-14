Ost_West_test <- data %>% mutate(Ost_West = case_when(PLZ <= 19 ~ 'Osten',
                                                      PLZ >= 20 & PLZ <= 38 ~ 'Westen',
                                                      PLZ == 39 ~ 'Osten',
                                                      PLZ >= 40 & PLZ <= 97 ~ 'Westen',
                                                      PLZ >= 98 ~ 'Osten'))

Altersgruppe <- data %>% mutate(Age_Range = case_when(Alter >= 45 ~ 'hohes Alter',
                                                      Alter >= 30  & Alter <= 44 ~ 'mittleres Alter',
                                                      Alter <= 29 ~ 'niedriges Alter'))

Bildung_test <- data %>% mutate(Bildungsgruppe = case_when(Bildungsabschluss == "(Noch) kein Abschluss" ~ 'niedrige Bildung',
                                                           Bildungsabschluss == "Hauptschulabschluss" ~ 'niedrige Bildung',
                                                           Bildungsabschluss == "Realschulabschluss" ~ 'niedrige Bildung',
                                                           Bildungsabschluss == "Abitur" ~ 'mittlere Bildung',
                                                           Bildungsabschluss == "Hochschulabschluss (Bachelor oder Master)" ~ 'hohe Bildung',
                                                           Bildungsabschluss == "Promotion" ~ 'hohe Bildung'))

###machen die Einkommengruppen Sinn? die erste Zeile funktioniert nicht -.-
Einkommen_test <- data %>% mutate(Einkommensgruppe = case_when(Nettoeinkommen == "0 € - 1000 €" ~ 'niedriges Einkommen',
                                                               Nettoeinkommen == "1001 € - 2000€" ~ 'niedriges Einkommen',
                                                               Nettoeinkommen == "2001 € - 3000 €" ~ 'mittleres Einkommen',
                                                               Nettoeinkommen == "3001 € - 4000 €" ~ 'mittleres Einkommen',
                                                               Nettoeinkommen == "4001 € - 5000 €" ~ 'hohes Einkommen',
                                                               Nettoeinkommen == "Mehr als 5000 €" ~ 'hohes Einkommen'))

Alkohol_test <- data %>% mutate(Alkoholgruppe = case_when(`Alkohol Konsum` == "Nein" ~ 'kein Konsum',
                                                          `Alkohol Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedriger Konsum',
                                                          `Alkohol Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedriger Konsum',
                                                          `Alkohol Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoher Konsum',
                                                          `Alkohol Konsum` == "Ja, mehrmals pro Woche" ~ 'hoher Konsum',
                                                          `Alkohol Konsum` == "Ja, täglich" ~ 'hoher Konsum'))

Zigaretten_test <- data %>% mutate(Zigarettengruppe = case_when(`Zigaretten Konsum` == "Nein" ~ 'kein Konsum',
                                                          `Zigaretten Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedriger Konsum',
                                                          `Zigaretten Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedriger Konsum',
                                                          `Zigaretten Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoher Konsum',
                                                          `Zigaretten Konsum` == "Ja, mehrmals pro Woche" ~ 'hoher Konsum',
                                                          `Zigaretten Konsum` == "Ja, täglich" ~ 'hoher Konsum'))

Drogen_test <- data %>% mutate(Drogengruppe = case_when(`Drogen Konsum` == "Nein" ~ 'kein Konsum',
                                                        `Drogen Konsum` == "Ja, mindestens einmal im Jahr" ~ 'niedriger Konsum',
                                                        `Drogen Konsum` == "Ja, mindestens einmal im Monat" ~ 'niedriger Konsum',
                                                        `Drogen Konsum` == "Ja, mindestens einmal pro Woche" ~ 'hoher Konsum',
                                                        `Drogen Konsum` == "Ja, mehrmals pro Woche" ~ 'hoher Konsum',
                                                        `Drogen Konsum` == "Ja, täglich" ~ 'hoher Konsum'))