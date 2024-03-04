# Aufsetzen der Arbeitsumgebung ------------------------------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(tidylog)
library(ggplot2)

# Daten einlesen ---------------------------------------------------------------
daten <- read_excel("Auswertung_2018-2023.xlsx", col_types = "text", na = c("-", "")) # einlesen in Text-Format
# Anzahl Datensätze: 79272

# Formate Ã¤ndern und Spaltennamen vereinheitlichen -----------------------------
df <- daten %>% 
  rename(
    # Spaltennamen der Zeitstempel vereinheitlichen
    datum_Annahme = AnnahmeD,
    zeit_Annahme = Annahme,
    zeit_Alarm = Alarm,
    zeit_S3 = S3,
    zeit_S4 = S4,
    zeit_S7 = S7,
    zeit_S8 = S8,
    zeit_S1 = S1,
    zeit_S2 = S2
  ) %>% 
  # Excel-Zeitformate werden zu numerischen Werten umgewandelt
  # Diese Werte müssen an das Format JJJJ-MM-DD HH:MM:SS angepasst werden
  mutate_at(vars(starts_with("zeit_")), # alle Zeitstempel
            function(x)
              format(as.POSIXct(as.numeric(x) * 86400, origin="1899-12-30", tz="UTC"), "%H:%M:%S")) %>% 
  mutate(
    datum_Annahme = format(as.POSIXct(as.numeric(datum_Annahme) * 86400, origin="1899-12-30", tz="UTC"), "%Y-%m-%d"),
    # die Zeiten müssen dem Datum zugeordnet werden
    # und werden anschließend in das Zeitformat POSIXct umgewandelt (Zeitformat in R)
    t_Annahme = if_else(
      is.na(zeit_Annahme),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_Annahme, sep = " ")) 
    ),
    t_Alarm = if_else(
      is.na(zeit_Alarm),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_Alarm, sep = " ")) 
    ),
    t_S3 = if_else(
      is.na(zeit_S3),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_S3, sep = " ")) 
    ),
    t_S4 = if_else(
      is.na(zeit_S4),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_S4, sep = " ")) 
    ),
    t_S7 = if_else(
      is.na(zeit_S7),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_S7, sep = " ")) 
    ),
    t_S8 = if_else(
      is.na(zeit_S8),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_S8, sep = " ")) 
    ),
    t_S1 = if_else(
      is.na(zeit_S1),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_S1, sep = " ")) 
    ),
    t_S2 = if_else(
      is.na(zeit_S2),
      as.POSIXct(NA),
      ymd_hms(paste(datum_Annahme, zeit_S2, sep = " ")) 
    )
  )

# Da es bei der manuellen Zusammensetzung von Datum + Zeitstempel dazu kommen könnte,
# dass sich in der Reihenfolge das Datum ändert,
# beispielsweise, wenn der Einsatz zwischen 23:59:59 und 00:00:00 stattfindet,
# muss eine Prüfung und Anderung durchgeführt werden

df1 <- df %>%
  # Prüfung beinhaltet:
  # Prüfung, ob der Zeitstempel zeitlich nach dem vorherigen liegt
  # wenn ja, dann wird ein Tag (24 Std.) addiert
  mutate(t_Alarm = if_else(t_Annahme > t_Alarm & !is.na(t_Annahme) & !is.na(t_Alarm), 
                           t_Alarm + (24 * 60 * 60), 
                           t_Alarm)) %>%
  mutate(t_S3 = if_else(is.na(t_Alarm), 
                        if_else(is.na(t_Annahme), 
                                t_S3, 
                                if_else(t_Annahme > t_S3 & !is.na(t_S3), 
                                        t_S3 + (24 * 60 * 60), 
                                        t_S3)), 
                        if_else(t_Alarm > t_S3 & !is.na(t_S3), 
                                t_S3 + (24 * 60 * 60), 
                                t_S3))) %>%
  mutate(t_S4 = if_else(is.na(t_S3),
                        if_else(is.na(t_Alarm),
                                if_else(t_Annahme > t_S4 & !is.na(t_S4), 
                                        t_S4 + (24 * 60 * 60), 
                                        t_S4),
                                if_else(t_Alarm > t_S4 & !is.na(t_S4), 
                                        t_S4 + (24 * 60 * 60), 
                                        t_S4)),
                        if_else(t_S3 > t_S4 & !is.na(t_S4), 
                                t_S4 + (24 * 60 * 60), 
                                t_S4))) %>%
  mutate(t_S7 = if_else(is.na(t_S4),
                        if_else(is.na(t_S3),
                                if_else(t_Alarm > t_S7 & !is.na(t_S7) & !is.na(t_Alarm), 
                                        t_S7 + (24 * 60 * 60), 
                                        t_S7),
                                if_else(t_S3 > t_S7 & !is.na(t_S7) & !is.na(t_S3), 
                                        t_S7 + (24 * 60 * 60), 
                                        t_S7)),
                        if_else(t_S4 > t_S7 & !is.na(t_S7) & !is.na(t_S4), 
                                t_S7 + (24 * 60 * 60), 
                                t_S7))) %>%
  mutate(t_S8 = if_else(is.na(t_S7),
                        if_else(is.na(t_S4),
                                if_else(t_S3 > t_S8 & !is.na(t_S8) & !is.na(t_S3), 
                                        t_S8 + (24 * 60 * 60), 
                                        t_S8),
                                if_else(t_S4 > t_S8 & !is.na(t_S8) & !is.na(t_S4), 
                                        t_S8 + (24 * 60 * 60), 
                                        t_S8)),
                        if_else(t_S7 > t_S8 & !is.na(t_S8) & !is.na(t_S7), 
                                t_S8 + (24 * 60 * 60), 
                                t_S8))) %>%
  mutate(t_S1 = if_else(is.na(t_S8),
                        if_else(is.na(t_S7),
                                if_else(t_S4 > t_S1 & !is.na(t_S1) & !is.na(t_S4), 
                                        t_S1 + (24 * 60 * 60), 
                                        t_S1),
                                if_else(t_S7 > t_S1 & !is.na(t_S1) & !is.na(t_S7), 
                                        t_S1 + (24 * 60 * 60), 
                                        t_S1)),
                        if_else(t_S8 > t_S1 & !is.na(t_S1) & !is.na(t_S8), 
                                t_S1 + (24 * 60 * 60), 
                                t_S1))) %>%
  mutate(t_S2 = if_else(is.na(t_S1),
                        if_else(is.na(t_S8),
                                if_else(t_S4 > t_S2 & !is.na(t_S2) & !is.na(t_S4), 
                                        t_S2 + (24 * 60 * 60), 
                                        t_S2),
                                if_else(t_S8 > t_S2 & !is.na(t_S2) & !is.na(t_S8),  
                                        t_S2 + (24 * 60 * 60), 
                                        t_S2)),
                        if_else(t_S1 > t_S2 & !is.na(t_S2) & !is.na(t_S1), 
                                t_S2 + (24 * 60 * 60), 
                                t_S2))) %>%
  ungroup()

# Datensatz filtern und vorbereiten --------------------------------------------
# Es erfolgt die Vorbereitung des Datensatzes anhand von festgelegten Filterschritten
# Zu diesen gehÃ¶ren das Filtern von Duplikaten in den Einsatzdaten
# BerÃ¼cksichtigung ausschließlich plausibler Zeitstempel in der Reihenfolge der prÃ¤klinischen Notfallversorgung
# BerÃ¼cksichtung der Einsatzfahrten der Notfallrettung (Krankentransportfahrten werden entfernt)

## Duplikate markieren ---------------------------------------------------------
logische_duplikate_merkmale <- c("EinsatzNr", "t_Alarm", "Einsatzmittel") # logisches Duplikat anhand gleicher Angabe Ã¼ber Einsatznummer, Alarmierungszeitpunkt, Funkrufname bestimmen

# wenn Duplikat vorliegt, dann TRUE, ansonsten FALSE
df1$b_DUPLIKAT <- duplicated(df1) | duplicated(df1[, logische_duplikate_merkmale]) # vollstÃ¤ndige / logische Duplikate markieren

## PlausibilitÃ¤t der Zeitstempel prÃ¼fen ----------------------------------------
df1 <- df1 %>% 
  mutate(
    # wenn Zeitstempel plausibel ist, dann TRUE, ansonsten FALSE
    b_zeit_plausibel = case_when(
      t_Annahme > t_Alarm ~ FALSE,
      t_Alarm > t_S3 ~ FALSE,
      t_Alarm > t_S4 ~ FALSE,
      t_S3 > t_S4 ~ FALSE,
      t_S4 > t_S7 ~ FALSE,
      t_S7 > t_S8 ~ FALSE,
      t_S4 > t_S1 ~ FALSE,
      t_S8 > t_S1 ~ FALSE,
      t_Alarm > t_S1 ~ FALSE,
      t_Annahme > t_S1 ~ FALSE,
      t_S1 > t_S2 ~ FALSE,
      TRUE ~ TRUE))

## Einsatzart der Notfallrettung kennzeichnen ----------------------------------
stichworte <- read.csv2("Anhang-1_Zuordnung_Einsatzstichworte.csv") # Abgleich-Liste über die Zuordnung der Einsatzstichworte zu Notfallrettung / Sonstiges
kein_notfalleinsatz <- stichworte$Stichwort[stichworte$b_notfalleinsatz == FALSE]
  
df1 <- df1 %>% 
  mutate(
    # wenn Einsatzstichwort nicht in der Liste ist, dann TRUE, ansonsten FALSE
    b_notfalleinsatz = if_else(
      Stichwort %in% kein_notfalleinsatz,
      FALSE,
      TRUE
    )
  )

## Datengrundlage filtern ------------------------------------------------------

df_gefiltert <- df1 %>% 
  mutate(
    ZEITSTEMPEL = coalesce(t_Alarm, t_S3, t_S4, t_S7, t_S8, t_Annahme, t_S2, t_S1), # Nutzung des nächsten verfügbaren Zeitstempel
    ZEIT = round(ymd_hms(ZEITSTEMPEL), units = "hours"),
  ) %>% 
  filter(
    b_DUPLIKAT == FALSE,
    b_zeit_plausibel == TRUE,
    b_notfalleinsatz == TRUE,
    ZEIT >= dmy_hms("01.01.2018 00:00:00") & ZEIT <= dmy_hms("30.04.2023 23:59:59") & !is.na(ZEIT)
  )

## Umwandeln zu einer Zeitreihe ------------------------------------------------
hilfsgeruest <- tibble(
  # ein HilfsfristgerÃ¼st Ã¼ber den Dokumentationszeitraum erstellen
  # damit Zeitreihe angefügt werden kann
  ZEIT = seq(dmy_hms("01.01.2018 00:00:00"), dmy_hms("30.04.2023 23:59:59"), "hours")
  )

zeitreihe <- df_gefiltert %>% 
  # Erstellung der Zeitreihe
  # Einsatznachfrage nach Stundenintervall
  group_by(
    ZEIT
  ) %>% 
  summarise(
    Anzahl = n()
  ) %>% 
  ungroup()

h1 <- hilfsgeruest %>% 
  left_join(zeitreihe,
            by = "ZEIT"
            )

sum(is.na(h1$Anzahl)) / nrow(h1) # 44,4 % fehlend

# Zeitreihe vervollständigen ---------------------------------------------------
h1$Anzahl[is.na(h1$Anzahl)]<- 0 # Fehlende Daten mit 0 ersetzen

h1 <- h1 %>% 
  rename(
    ZEIT = ZEIT_4H 
  ) %>% 
  # Zeitliche Informationen anfÃ¼gen
  mutate(
    MONAT = month(ZEIT),
    WOCHENTAG = wday(ZEIT, week_start = 1), # Wochenstart = Montag # numerische Tage
    STUNDE = hour(ZEIT)
  ) 

einsatz.zeitreihe <- h1

write.csv2(einsatz.zeitreihe, "Einsatzzeitreihe_Notfallrettung_LSTM.csv", row.names = F) # Export als CSV-Datei

# Weitere Verarbeitung erfolgt in Python