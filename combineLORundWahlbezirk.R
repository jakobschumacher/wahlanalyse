library(sf)
library(tidyverse)
library(tmap)


# Reading Wahlbezirke
wahlbezirke = st_read("geo/UWB.shp") %>% mutate(wb = paste0(BEZ, UWB))

# Reading LOR
lor = st_read("geo/lor/LOR_Planungsräume__Berlin.shp") %>% 
  st_transform(., 25833)# Setting the correct crs as the objekt wahlbezirke

# Reading Wohndauerdata from https://daten.berlin.de/datensaetze/einwohnerinnen-und-einwohner-berlin-lor-planungsr%C3%A4umen-nach-wohndauer-am-31122019 
wohndauer <- read_csv2("data/WHNDAUER2019_Matrix.csv") %>% 
  mutate(spatial_na = paste0(BEZ, PGR, BZR, PLR))

# Combine LOR and Wohndauer an aggregate incongruent data
lor <- lor %>% left_join(wohndauer, by = "spatial_na") 
df_geo = st_interpolate_aw(lor[, c("PDAU10", "PDAU5")], wahlbezirke, extensive = FALSE) %>% 
  st_join(wahlbezirke)
  
# Graphical check
wb_map <- tm_shape(agg_aw) + tm_borders() + tm_fill(col = "PDAU10")
lor_map <- tm_shape(lor) + tm_borders() + tm_fill(col = "PDAU10")
tmap_arrange(wb_map, lor_map)

# Correlation
wahl_df <- df_complete %>% 
  filter(Parteien == "fdp") %>% 
  filter(stimmart == "BVV") %>% # Hier erfolgt die Filterung nach der Wahlart
  filter(year > as.Date("01-01-2015", format="%d-%m-%Y")) %>% 
  mutate(wb = paste0(bezirksnummer, wahlbezirk)) %>% 
  group_by(wb) %>% 
  summarise(anzahl = 100*sum(Stimmen)/sum(gultige_stimmen), .groups = 'drop') 

df <- df_geo %>% left_join(wahl_df, by = "wb")


# Multivariables Model (noch ganz rudimentär)
model <- glm(df$anzahl ~ df$PDAU10 + df$PDAU5)
summary(model)
plot(df$PDAU10, df$anzahl)
abline(glm(df$anzahl ~ df$PDAU10))
