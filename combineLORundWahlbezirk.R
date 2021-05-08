library(sf)
library(ows4R)
library(httr)
library(tidyverse)


wahlbezirke = st_read("geo/UWB.shp")
plot(wahlbezirke)
st_is_longlat(wahlbezirke)

# Reading WFS from Open Data Berlin. See: https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/ for tutorial
wfs_bwk = "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_pgr_2021"
wfs = WFSClient$new(wfs_bwk, serviceVersion = "2.0.0")
lor = wfs$getFeatures("fis:s_lor_pgr_2021")

plot(lor)
st_is_longlat(lor)
lor_geo = st_set_crs(lor, 4326)
st_is_longlat(lor_geo)


kreuzberg = lor %>% 
  filter(PGR_NAME == "Kreuzberg Süd")

wahlbezirke[kreuzberg,]
