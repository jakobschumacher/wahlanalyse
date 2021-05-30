
read_opendataberlin <- function(quelle, datenname, zieldatei = NULL, forcereload = FALSE){
  place <- paste0(here(), "/data/", datenname, ".rds")
  if (file.exists(place) & !forcereload){
    print(paste("Reading", datenname, "from memory"))
    assign(datenname, readRDS(place))
  } else {
    q <- quelle
    z <- zieldatei
    print(paste("Reading", datenname, "from", q))
    # Reading 
    if(str_sub(q, - 3, - 1) == "csv") { 
      assign(datenname, read_csv2(q))
    } else if (str_sub(q, - 3, - 1) == "zip"){
      temp <- tempfile() 
      download.file(q,temp)
      unzip(temp, exdir = tempdir())
      # Reading with the sf package
      assign(datenname, st_read(paste0(tempdir(), "/", z)))
    }
    # Save into data
    saveRDS(get(datenname), file=place)
    get(datenname)
  } 
}


read_wahlenberlin <- function(quelle, wahlart, datenname, forcereload = FALSE) {
  place <- paste0(here(), "/data/", datenname, ".rds")
  q <- quelle
  jahreszahl <- str_sub(q, - 9, - 6)
  t <- wahlart
  if (file.exists(place) & !forcereload){
    print(paste("Reading", datenname, "from memory"))
    assign(datenname, readRDS(place)) 
    data <- get(datenname)
    
    
  } else {
    print(paste("Reading", jahreszahl, "from", q)) 
    temp <- tempfile() 
    download.file(q,temp)
    
    
    if(t == "AGH") {
      data <- read_xlsx(temp, sheet="BVV") %>% 
        bind_rows(read_xlsx(temp, sheet="Erststimme")) %>%
        bind_rows(read_xlsx(temp, sheet="Zweitstimme")) %>%
        janitor::clean_names() %>% 
        mutate(stimmart = paste0(t, stimmart)) %>% 
        select(-starts_with("EB")) # Alle Einzelbewerber werden herausgefiltert
    }  else if (t == "EU") {
      sheetname = tail(excel_sheets(temp),1) 
      data <- read_xlsx(temp, sheet=sheetname) %>% 
        janitor::clean_names() %>% 
        select(-starts_with("EB")) # Alle Einzelbewerber werden herausgefiltert
    } else if (t == "BU") {
      sheetname1 = tail(excel_sheets(temp),1) 
      sheetname2 = tail(excel_sheets(temp),2)[1] 
      data <- read_xlsx(temp, sheet=sheetname1) %>% 
        bind_rows(read_xlsx(temp, sheet=sheetname2)) %>%
        janitor::clean_names() %>%
        mutate(stimmart = paste0(t, stimmart)) %>% 
        select(-starts_with("EB")) # Alle Einzelbewerber werden herausgefiltert
    }
    
    startnumber <- match("gultige_stimmen", names(data)) + 1 # Finde die erste Partei in den Variablennamen
    data <- data %>%
      pivot_longer(all_of(startnumber):last_col(), names_to="partei", values_to="stimmen") %>% # Pivot des Datensatzes mit Tidyr
      mutate(jahr = as.Date(paste0("01-01-", all_of(jahreszahl)), format = "%d-%m-%Y")) 
    
    data <- data %>%
      mutate(bezirksname = ifelse(bezirksname == "Hellersdorf"|bezirksname == "Marzahn", "Marzahn-Hellersdorf", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Friedrichshain"|bezirksname == "Kreuzberg", "Friedrichshain-Kreuzberg", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Charlottenburg"|bezirksname == "Wilmersdorf", "Charlottenburg-Wilmersdorf", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Köpenick"|bezirksname == "Treptow", "Treptow-Köpenick", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Steglitz"|bezirksname == "Zehlendorf", "Steglitz-Zehlendorf", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Schöneberg"|bezirksname == "Tempelhof", "Tempelhof-Schöneberg", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Hohenschönhausen", "Lichtenberg", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Weißensee"|bezirksname == "Prenzlauer Berg", "Pankow", bezirksname)) %>% 
      mutate(bezirksname = ifelse(bezirksname == "Tiergarten"|bezirksname == "Wedding", "Mitte", bezirksname))
    
    
    data <- data %>% 
      select(wahlbezirk, bezirksname, bezirksnummer, partei, stimmen, wahlberechtigte_insgesamt, gultige_stimmen, jahr, stimmart, wahlbezirksart )
    
    
    data <- data %>% 
      mutate(wahl = paste0(stimmart, "_", jahreszahl ))
    
  }
  
  # Save into data
  saveRDS(data, file=place)
  data
} 

