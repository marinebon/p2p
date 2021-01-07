if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  # obis
  robis,
  # tidyverse
  dplyr, readr,
  # utilities
  glue, here)


# paths
sites_csv <- here("data/sites.csv")
dir_obis  <- here("data/obis")

# data
sites <- read_csv(sites_csv, col_types=cols())

# TODO: iterate over all sites
site <- sites %>% 
  slice(1)

obis_csv <- glue("{dir_obis}/obis_{site$id}.csv")

## query OBIS records
# total = occurrence(areaid = site$area_code, taxonid = c(51, 1806, 882, 3))
# setwd("~/p2p/data/obis")
# sel_area <- site$area_code
# filename <- glue("obis_{sel_area}.csv")
# total <- read.csv(filename)

if (site$area_code > 0){
  total <- occurrence(areaid = polygon, taxonid = c(51, 1806, 882, 3))
} else if(site$area_code == -9999){
  # Patagonia and CA Current systems where there's so much data
  val <- .25
  lat <- site$lat
  lon <- site$lon
  latmin <- lat - val
  latmax <- lat + val
  lonmin <- lon - val
  lonmax <- lon + val
  polygon <- glue("POLYGON (({lonmin} {latmin}, {lonmin} {latmax}, {lonmax} {latmax}, {lonmax} {latmin}, {lonmin} {latmin}))")
  options(show.error.messages = FALSE)
  #total <- try(occurrence(taxonid = c(51, 1806, 882, 3), geometry = polygon))
  total <- occurrence(taxonid = c(51, 1806, 882, 3), geometry = polygon)
  # Peter Provoost seems to have fixed this issue:
  # if("try-error" %in% class(total))
  #   total <- occurrence(areaid = 40003, taxonid = c(51, 1806, 882, 3))
}

write_csv(total, obis_csv)

# res <- system("Rscript scripts/obis_download.R")
