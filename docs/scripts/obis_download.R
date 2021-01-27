# Goals: 
# 1) see visibility of data contributions to OBIS
#   - breakdown by contributor
# 2) see records of significance for site
#   - Ecuador: using EEZ records
#   - Fernando: using World Heritage Sites
#   - where to show. Melissa Miner sent list of regions, but not in mregions. MEOWs regions.
#   - very data limited for some sites, like in Chukchi Sea

# TODO:
# 1. investigate the latest version of api.obis.org (v3) with the POST (vs GET) method: /occurrence,  especially faster methods like 
#   - eg use grid to get approximation: https://api.obis.org/v3/occurrence/grid/10?taxonid=51


# libraries ----
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  # obis (latest version directly from OBIS Github repo)
  iobis/robis,
  # spatial
  rmapshaper, sf,
  # tidyverse
  dplyr, readr, purrr, tidyr,
  # utilities
  glue, here, tictoc)

# TODO: try parallelizing, but prob not multiple cores with free tier

# custom functions ----

occurrence_count <- function (scientificname = NULL, taxonid = NULL, datasetid = NULL, 
          nodeid = NULL, areaid = NULL, startdate = NULL, enddate = NULL, 
          startdepth = NULL, enddepth = NULL, geometry = NULL, measurementtype = NULL, 
          measurementtypeid = NULL, measurementvalue = NULL, measurementvalueid = NULL, 
          measurementunit = NULL, measurementunitid = NULL, redlist = NULL, 
          hab = NULL, mof = NULL, absence = NULL, event = NULL, dropped = NULL, 
          flags = NULL, exclude = NULL, fields = NULL, verbose = FALSE) {
  
  handle_vector   <- robis:::handle_vector
  handle_logical  <- robis:::handle_logical
  handle_fields   <- robis:::handle_fields
  handle_date     <- robis:::handle_date
  http_request    <- robis:::http_request
  stop_for_status <- httr::stop_for_status
  content         <- httr::content
  fromJSON        <- jsonlite::fromJSON
  
  query <- list(scientificname = handle_vector(scientificname), 
                taxonid = handle_vector(taxonid), datasetid = handle_vector(datasetid), 
                nodeid = handle_vector(nodeid), areaid = handle_vector(areaid), 
                startdate = handle_date(startdate), enddate = handle_date(enddate), 
                startdepth = startdepth, enddepth = enddepth, geometry = geometry, 
                measurementtype = measurementtype, measurementtypeid = measurementtypeid, 
                measurementvalue = measurementvalue, measurementvalueid = measurementvalueid, 
                measurementunit = measurementunit, measurementunitid = measurementunitid, 
                redlist = handle_logical(redlist), hab = handle_logical(hab), 
                mof = handle_logical(mof), absence = absence, event = event, 
                dropped = dropped, flags = handle_vector(flags), exclude = handle_vector(exclude), 
                fields = handle_fields(fields))
  result <- http_request("GET", "metrics/logusage", c(query, list(agent = "robis")))
  if (verbose) {
    log_request(result)
  }
  result <- http_request(
    "GET", "occurrence", 
    c(query, 
      list(after = "-1", size = 1)))
  if (verbose) {
    log_request(result)
  }
  stop_for_status(result)
  text <- content(result, "text", encoding = "UTF-8")
  res <- fromJSON(text, simplifyVector = TRUE)
  total <- res$total
  
  total
}

get_bbox <- function(lon, lat, dd = 0.25){
  latmin <- lat - dd
  latmax <- lat + dd
  lonmin <- lon - dd
  lonmax <- lon + dd
  bbox <- glue("POLYGON (({lonmin} {latmin}, {lonmin} {latmax}, {lonmax} {latmax}, {lonmax} {latmin}, {lonmin} {latmin}))")
  bbox
}

# paths & variables ----
sites_csv      <- here("data/sites.csv")
dir_obis       <- here("data/obis")
obis_data_csv  <- here("data/obis/obis_data.csv")
obis_sites_geo <- here("data/obis/obis_sites.geojson")

n_025_min      <- 100   # min # of OBIS records for 1/4 degree bounding box to go to 1 dd bbox
n_100_min      <- 100   # min # of OBIS records for 1   degree bounding box to go to 5 dd bbox
n_wkt_char_max <- 1000  # max # of characters for bounding box in wkt format to work with robis::occurrence()

obis_flds <- c(
  "country",
  "date_year",
  "scientificNameID", "scientificName", "aphiaID", "phylum", "kingdom",
  "decimalLatitude", "decimalLongitude",
  "flags",
  "institutionID","collectionCode")


# get OBIS counts across sites ----

sites <- read_csv(sites_csv, col_types=cols())


# get OBIS records for 1/4 decimal degree bounding box
tic("fetch # of OBIS occurrences per 0.25 deg box for all sites")
sites <- sites %>% 
  arrange(id) %>% 
  mutate(
    bbox_025 = map2_chr(lon, lat, get_bbox, dd = 0.25),
    n_025    = map2_int(bbox_025, id, function(geom = .x, id = .y){
      #browser()
      n <- occurrence_count(taxonid = c(51, 1806, 882, 3), geometry = geom)
      message(glue("n_025 for {id}: {n}"))
      n
    }))
toc()

tic("fetch # of OBIS occurrences per 1 & 5 deg box for data poor sites")

# if too few OBIS records, get 1 decimal degree bounding box
sites_100 <- sites %>% 
  filter(n_025 < n_025_min) %>% 
  mutate(
    bbox_100 = map2_chr(lon, lat, get_bbox, dd = 1.00),
    n_100    = map2_int(bbox_100, id, function(geom = .x, id = .y){
      n <- occurrence_count(taxonid = c(51, 1806, 882, 3), geometry = geom)
      message(glue("n_100 for {id}: {n}"))
      n
    }))

# if still too few OBIS records, get 5 decimal degree bounding box
sites_500 <- sites_100 %>% 
  filter(n_100 < n_100_min) %>% 
  mutate(
    bbox_500 = map2_chr(lon, lat, get_bbox, dd = 5.00),
    n_500    = map2_int(bbox_500, id, function(geom = .x, id = .y){
      n <- occurrence_count(taxonid = c(51, 1806, 882, 3), geometry = geom)
      message(glue("n_500 for {id}: {n}"))
      n
    }))

# setup sites based on bounding box with minimum # of OBIS records
sites <- sites %>% 
  left_join(
    sites_100 %>% 
      select(id, bbox_100, n_100),
    by = "id") %>% 
  left_join(
    sites_500 %>% 
      select(id, bbox_500, n_500),
    by = "id") %>% 
  mutate(
    bbox_dd = case_when(
      !is.na(bbox_500) ~ 5,
      !is.na(bbox_100) ~ 1,
      T ~ 0.25),
    bbox = case_when(
      bbox_dd == 5 ~ bbox_500,
      bbox_dd == 1 ~ bbox_100,
      T ~ bbox_025))
rm(sites_100, sites_500)

toc()

# convert to spatial
sites <- st_as_sf(
  sites,
  wkt = "bbox",
  crs = 4326,
  remove = F)
st_write(sites, obis_sites_geo, delete_dsn = T)

# union polygons to capture overlapping
sites_ply <- st_union(sites)

sites_plys <- tibble(
  # extract individual polygons
  geom = st_cast(sites_ply, "POLYGON")) %>% 
  st_as_sf(
    sf_column_name = "geom",
    crs = 4326) %>% 
  mutate(
    wkt = st_as_text(geom),
    wkt_nchar = nchar(wkt)) %>% 
  arrange(desc(wkt_nchar))

# map sites
# library(leaflet)
# leaflet(sites) %>%
# leaflet(sites_ply) %>%
# leaflet(sites_plys) %>%
#   addProviderTiles(providers$Esri.OceanBasemap) %>%
#   addPolygons()
# 
# show how big polygon in CA simplified ----
# ply_b <- sites_plys[1,]
# 
# ply_b_bs <- ply_b %>%
#   st_buffer(0.1) %>%
#   ms_simplify(keep = 0.05, snap_interval = 0.01)
# 
# leaflet(ply_b) %>%
#   addProviderTiles(providers$Esri.OceanBasemap) %>%
#   addPolygons() %>%
#   addPolygons(data = ply_b_bs, color="red")
# 
# st_as_text(ply_b$geom)    %>% nchar() # 4206
# st_as_text(ply_b_bs$geom) %>% nchar() # 911

# simplify polygons exceeding n_wkt_char_max
sites_plys <- sites_plys %>% 
  filter(wkt_nchar <= n_wkt_char_max) %>% 
  rbind(
    sites_plys %>% 
      filter(wkt_nchar > n_wkt_char_max) %>% 
      st_buffer(0.1) %>% 
      ms_simplify(keep = 0.05, snap_interval = 0.01) %>% 
      mutate(
        wkt = st_as_text(geom),
        wkt_nchar = nchar(wkt))) %>% 
  arrange(desc(wkt_nchar))

# remove existing obis_sites_plys_*.csv
unlink(list.files(here("data/obis"), "^obis_sites_plys_[0-9]+\\.csv$"))

tic("iterate over sites_plys")
for (i in 1:nrow(sites_plys)) { # i=1
# for (i in 13:nrow(sites_plys)) { # i=12 # DEBUG
  
  ply       <- sites_plys$wkt[i]
  wkt_nchar <- sites_plys$wkt_nchar[i]
  occs_csv  <- glue("{dir_obis}/obis_sites_plys_{sprintf('%02d', i)}.csv")

  tic(glue("\nsite {i} of {nrow(sites_plys)} sites_plys (nchar={wkt_nchar})"))
  
  occs <- try(occurrence(taxonid = c(51, 1806, 882, 3), geometry = ply, fields = obis_flds)) # 1st ply: 99.702 sec elapsed
  
  toc()
  
  # try by directly using api.obis.org ----
  # tic()
  # library(httr) # https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
  # 
  # tic("page 10K")
  # res <- GET("https://api.obis.org/v3/occurrence", query = list(
  #   taxonid  = "51,1806,882,3",
  #   geometry = ply,
  #   size     = 10000))
  # 
  # res_p <- jsonlite::fromJSON(content(res, "text")) %>% 
  #   .[['results']] %>% tibble() # , simplifyVector = FALSE)
  # 
  # toc()
  # ORIGINAL: site 1 of 43 sites_plys (nchar=911): 99.702 sec elapsed
  # NEW API: 75695 * 9.064/10000 = 68.60995 sec to do it
  
  if ("try-error" %in% class(occs)){
    message(glue("  ERROR: {total}"))
    next()
  }
  
  write_csv(occs, occs_csv)
  # occs <- read_csv(here("data/obis/obis_sites_plys_01.csv")) # %>% names()

}
toc()

# combine all sites plys csvs into one
obis_sites_plys_csvs <- list.files(here("data/obis"), "^obis_sites_plys_[0-9]+\\.csv$", full.names = T)
obis_sites_plys_csvs %>% 
  map_dfr(read_csv, col_types = cols()) %>% 
  write_csv(obis_data_csv)
  # TODO: Deal with parsing failure(s) by explicitly setting col_types() per field in flds
  # Warning: 1 parsing failure.
  #  row           col           expected actual                                                       file
  # 1680 institutionID 1/0/T/F/TRUE/FALSE   ISDM '/Users/bbest/github/p2p/data/obis/obis_sites_plys_38.csv'
unlink(obis_sites_plys_csvs)
