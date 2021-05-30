###################################################################################################
###################################################################################################
# Diese Datei lädt die Daten, die für die Wahlanalyse nötig sind. Dies muss einmal auf jedem benutzen
# Rechner durchgeführt werden. Die Daten werden im Ordner /data gespeichert.
###################################################################################################
###################################################################################################

###################################################################################################
# Settings and loading of user written functions 
###################################################################################################
forcereload_setting = FALSE # Wenn dieser Wert auf TRUE gesetzt wird, lädt das Skript in jedem Fall alle Daten neu ein.
source(here("R/functions.R")) # Selbstgeschriebene Funktionen

###################################################################################################
# Packages needed
###################################################################################################
suppressPackageStartupMessages(library(tidyverse)) # Tidying data
suppressPackageStartupMessages(library(readxl)) # Reading excelfiles
suppressPackageStartupMessages(library(sf)) # Reading Shapefiles
suppressPackageStartupMessages(library(areal)) # Mix incongruent data
suppressPackageStartupMessages(library(here)) # Find correct place to load 

###################################################################################################
# Daten von Wahlen-Berlin.de 
###################################################################################################
df_wahlen_2016_AGH <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_EE_WB_AH2016.xlsx", wahlart = "AGH", datenname = "df_wahlen_2016_AGH", forcereload = forcereload_setting)

df_wahlen_2011_AGH <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_AB2011.xlsx", wahlart = "AGH", datenname = "df_wahlen_2011_AGH", forcereload = forcereload_setting)

df_wahlen_2019_EU <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_EU2019.xlsx", wahlart = "EU", datenname = "df_wahlen_2019_EU", forcereload = forcereload_setting)

df_wahlen_2014_EU <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_EU2014.xlsx", wahlart = "EU", datenname = "df_wahlen_2014_EU", forcereload = forcereload_setting)

df_wahlen_2009_EU <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_EU2009.xlsx", wahlart = "EU", datenname = "df_wahlen_2009_EU", forcereload = forcereload_setting)

df_wahlen_2017_BU <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_EE_WB_BU2017.xlsx", wahlart = "BU", datenname = "df_wahlen_2017_BU", forcereload = forcereload_setting)

df_wahlen_2013_BU <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_BU2013.xlsx", wahlart = "BU", datenname = "df_wahlen_2013_BU", forcereload = forcereload_setting)

df_wahlen_2009_BU <- read_wahlenberlin(quelle = "https://www.statistik-berlin-brandenburg.de/publikationen/Dowmies/DL_BE_BU2009.xlsx", wahlart = "BU", datenname = "df_wahlen_2009_BU", forcereload = forcereload_setting)


###################################################################################################
# Open Data Berlin
###################################################################################################

# Wahlbezirke 
# https://daten.berlin.de/datensaetze/geometrien-der-wahlbezirke-f%C3%BCr-die-wahl-zum-abgeordnetenhaus-von-berlin-und-zu-den
df_wahlbezirke <- read_opendataberlin(quelle = "https://www.statistik-berlin-brandenburg.de/opendata/RBS_OD_UWB_AGH_09_2016.zip", datenname = "df_wahlbezirke", zieldatei = "UWB.shp", forcereload = forcereload_setting)

# Wohndauer
# https://daten.berlin.de/datensaetze/einwohnerinnen-und-einwohner-berlin-lor-planungsr%C3%A4umen-nach-wohndauer-am-31122019 
df_wohndauer <- read_opendataberlin(quelle = "https://www.statistik-berlin-brandenburg.de/opendata/WHNDAUER2019_Matrix.csv", datenname = "df_wohndauer", forcereload = forcereload_setting)

# LOR
# https://daten.berlin.de/datensaetze/lebensweltlich-orientierte-r%C3%A4ume-lor-berlin
df_lor <- read_opendataberlin(quelle = "http://www.stadtentwicklung.berlin.de/planen/basisdaten_stadtentwicklung/lor/download/LOR_SHP_EPSG_25833.zip", datenname = "df_lor", zieldatei = "Planungsraum_EPSG_25833.shp", forcereload = forcereload_setting)

# Demographie
df_demographie <- read_opendataberlin(quelle = "https://www.statistik-berlin-brandenburg.de/opendata/EWR201812E_Matrix.csv", datenname = "df_demographie", forcereload = forcereload_setting)

# Wohnlage
# https://daten.berlin.de/datensaetze/einwohnerinnen-und-einwohner-nach-wohnlagen-den-lor-planungsr%C3%A4umen-am-31122019
df_wohnlage <- read_opendataberlin(quelle = "https://www.statistik-berlin-brandenburg.de/opendata/WHNLAGE2019_Matrix.csv", datenname = "df_wohnlage", forcereload = forcereload_setting)


tf = tempfile(fileext = ".xlsx")
curl::curl_download("https://www.statistik-berlin-brandenburg.de/publikationen/dowmies/DL_BE_EU2019_Strukturdaten.xlsx", tf)
df_strukturdaten <- readxl::read_excel(tf, sheet = "Strukturdaten") %>% mutate(wahlbezirk = paste0(Bezirksnummer, Wahlbezirk)) %>% select(-Wahlbezirk) %>% janitor::clean_names()

###################################################################################################
# Umwandlung von Datensätzen 
###################################################################################################
df_wohnqualitaet <- df_wohnlage %>% 
  mutate(gesamt = WLEINFoL + WLEINFmL + WLMIToL + WLMITmL + WLGUToL + WLGUTmL) %>% 
  mutate(mitlärm = WLEINFmL + WLMITmL + WLGUTmL) %>% 
  mutate(lärmanteil = 100 * mitlärm / gesamt) %>% 
  mutate(gutewohnlage = WLGUToL + WLGUTmL) %>% 
  mutate(gutewohnlage = 100 * gutewohnlage / gesamt) %>% 
  mutate(mittlerewohnlage = WLMIToL + WLMITmL) %>% 
  mutate(mittlerewohnlage = 100 * mittlerewohnlage / gesamt) %>% 
  mutate(einfachewohnlage = WLEINFoL + WLEINFmL) %>% 
  mutate(einfachewohnlage = 100 * einfachewohnlage / gesamt) %>% 
  select(RAUMID, lärmanteil, gutewohnlage, mittlerewohnlage, einfachewohnlage)

df_demographie <- df_demographie %>% 
  select(-c(E_E,E_EM,E_EW, BEZ, PGR, BZR, PLR, STADTRAUM, ZEIT, E_EU1, E_E1U6, E_E6U15, E_E15U18, E_E18U25, E_E25U55, E_E55U65, E_E65U80, E_E80U110 )) %>% 
  pivot_longer(cols = starts_with("E_")) %>% 
  separate(name, c(NA, NA, "number"), sep = "E") %>% 
  separate(number, c("min", "max")) %>% 
  mutate(max = as.numeric(max)) %>% 
  mutate(min = as.numeric(min)) %>% 
  mutate(alter = (min + max)/2) %>% 
  mutate(gewicht = alter * value) %>% 
  group_by(RAUMID) %>% 
  summarise(altersmittelwert = round(sum(gewicht)/sum(value)),1)  

df_lor <- df_lor %>%  
  st_set_crs(., 25833) %>% 
  rename(RAUMID = SCHLUESSEL)

###################################################################################################
# Datensätze zusammenfügen 
###################################################################################################

# Prepare Wahlbezirke
df_wahlbezirke <- df_wahlbezirke %>% 
  mutate(wahlbezirk = paste0(BEZ, UWB)) %>% 
  select(wahlbezirk)

# Prepare Wahl
parteienfilter <- c("spd", "grune", "cdu", "fdp", "af_d", "die_linke", "die_partei")
df_wahlen <- rbind(df_wahlen_2011_AGH, 
                   df_wahlen_2016_AGH,
                   df_wahlen_2009_BU,
                   df_wahlen_2009_EU,
                   df_wahlen_2014_EU,
                   df_wahlen_2017_BU,
                   df_wahlen_2019_EU,
                   df_wahlen_2013_BU) %>% 
  mutate(wahlbezirk = paste0(bezirksnummer, wahlbezirk)) %>% 
  mutate(stimmanteil = round(100*stimmen/gultige_stimmen,1)) %>% 
  filter(str_detect(wahlbezirksart, "^B", negate = TRUE)) %>% 
  filter(partei %in% parteienfilter) %>% 
  select(wahlbezirk, partei, wahl, stimmanteil, jahr)

rm(df_wahlen_2011_AGH, df_wahlen_2016_AGH, df_wahlen_2009_BU, df_wahlen_2009_EU, df_wahlen_2014_EU, df_wahlen_2017_BU, df_wahlen_2019_EU, df_wahlen_2013_BU)

# Prepare LOR
df_lor <- df_lor %>% 
  left_join(df_wohndauer, by = "RAUMID") %>% 
  left_join(df_demographie, by = "RAUMID")  %>% 
  left_join(df_wohnqualitaet, by = "RAUMID")  %>% 
  select(RAUMID, PDAU10, PDAU5, altersmittelwert, lärmanteil, gutewohnlage, mittlerewohnlage, einfachewohnlage)


# Combine Wahlbezirke + LOR
df_wahlbezirke_lor <- aw_interpolate(df_wahlbezirke, tid = wahlbezirk, source = df_lor, sid = RAUMID, weight = "sum", output = "sf", intensive = c("altersmittelwert", "lärmanteil", "gutewohnlage", "mittlerewohnlage", "einfachewohnlage"))

# Combine Wahlbezirke_LOR + Wahlen
df <- df_wahlbezirke_lor %>% full_join(df_wahlen %>% filter(partei == "grune"), by = "wahlbezirk")

# Prepare Bundestagswahl 2017-Daten
df_bu <- df %>% filter(wahl == "BU2_2017")


# Combine Strukturdaten
df_strukturdaten <- df_bu %>% left_join(df_strukturdaten, by="wahlbezirk")

###################################################################################################
# Datensätze speichern  
###################################################################################################

# Dataset für Wahlkarten
df_grune_pankow <- df %>% filter(partei == "grune") %>% filter(str_detect(wahlbezirk, "^03"))
saveRDS(df_grune_pankow, file=here("data/df_grune_pankow.rds"))
saveRDS(df_wahlen, file=here("data/df_wahlen.rds"))

# Dataset für Zusammenhänge 1
saveRDS(df_bu, file=here("data/df_bu.rds"))

# Dataset für Zusammenhänge 2
df_alter <- df_wahlbezirke_lor %>% select(wahlbezirk, altersmittelwert) %>% mutate(altersmittelwert = round(altersmittelwert,2))
saveRDS(df_alter, file=here("data/df_alter.rds"))

# Dataset für Zusammenhänge
saveRDS(df_strukturdaten, file=here("data/df_strukturdaten.rds"))

# Dataset für Zusammenhänge 3 Wohnlage
df_wohnqualitaet <- df %>% filter(partei == "grune") %>% select(wahlbezirk, lärmanteil, gutewohnlage, mittlerewohnlage, einfachewohnlage, partei, wahl, stimmanteil)
saveRDS(df_wohnqualitaet, file=here("data/df_wohnqualitaet.rds"))

# Save big dataframe into data
saveRDS(df, file=here("data/df.rds"))









