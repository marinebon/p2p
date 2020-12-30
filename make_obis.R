# library(tidyverse)
# library(robis)
# library(here)
# library(glue)
# 
# poly_tbl <- c(40014, 40061, 40015, 40016, 40013, 40011, 40003, 21000, 20001)
# 
# for (i in 1:length(poly_tbl))
# {
#   area_code <- poly_tbl[i]
#   
#   ## read data from OBIS
#   ## Molluscs
#   mollusc = occurrence(areaid = area_code, taxonid = 51, enddepth = 500) 
#   ## Merge the data frames
#   #subset data (year and phylum)
#   sub_mollusc <- data.frame(mollusc$date_year, mollusc$phylum) 
#   names(sub_mollusc)[names(sub_mollusc) == "mollusc.date_year"] <- "year"
#   names(sub_mollusc)[names(sub_mollusc) == "mollusc.phylum"] <- "phylum"
#   
#   # ## Ehinoderms
#   # echino = occurrence(areaid = site$area_code, taxonid = 1806, enddepth = 500)
#   # ## Annelida
#   # anne = occurrence(areaid = site$area_code, taxonid = 882, enddepth = 500)
#   # ## Platae
#   # plant = occurrence(areaid = site$area_code, taxonid = 3, enddepth = 500)
#   
#   # sub_echino <- data.frame(echino$date_year, echino$phylum)
#   # names(sub_echino)[names(sub_echino) == "echino.date_year"] <- "year"
#   # names(sub_echino)[names(sub_echino) == "echino.phylum"] <- "phylum"
#   # 
#   # sub_anne <- data.frame(anne$date_year, anne$phylum)
#   # names(sub_anne)[names(sub_anne) == "anne.date_year"] <- "year"
#   # names(sub_anne)[names(sub_anne) == "anne.phylum"] <- "phylum"
#   # 
#   # sub_plant <- data.frame(plant$date_year, plant$phylum)
#   # names(sub_plant)[names(sub_plant) == "plant.date_year"] <- "year"
#   # names(sub_plant)[names(sub_plant) == "plant.phylum"] <- "phylum"
#   # 
#   # total <- bind_rows(sub_mollusc, sub_echino, sub_anne, sub_plant)
#   
#   csv <- here(glue("data/obis/obis_{area_code}.csv"))
#   write.csv(sub_mollusc, file = csv)
# }
