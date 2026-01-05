# The script identifies which sites are missing from your existing obis_sites.geojson and only downloads data for 
# those "new" locations. It then automatically combines this new data with the existing records from obis_data.csv.gz, 
# ensuring that your dataset grows without re-downloading thousands of existing records.

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

# custom functions ----
occurrence_count <- function (taxonid = NULL, geometry = NULL) {
  # Simplified wrapper for checking if data exists for a bounding box
  query <- list(taxonid = paste(taxonid, collapse = ","), geometry = geometry, after = "-1", size = 1)
  result <- robis:::http_request("GET", "occurrence", query)
  httr::stop_for_status(result)
  res <- jsonlite::fromJSON(httr::content(result, "text", encoding = "UTF-8"), simplifyVector = TRUE)
  return(res$total)
}

get_bbox <- function(lon, lat, dd = 0.25){
  latmin <- lat - dd; latmax <- lat + dd
  lonmin <- lon - dd; lonmax <- lon + dd
  glue("POLYGON (({lonmin} {latmin}, {lonmin} {latmax}, {lonmax} {latmax}, {lonmax} {latmin}, {lonmin} {latmin}))")
}

# paths & variables ----
sites_csv      <- here("data/sites.csv")
dir_obis       <- here("data/obis")
# We now target the .gz file directly for both reading and writing
obis_data_gz   <- here("data/obis/obis_data.csv.gz")
obis_sites_geo <- here("data/obis/obis_sites.geojson")

obis_flds <- c("country", "date_year", "scientificNameID", "scientificName", 
               "aphiaID", "phylum", "kingdom", "decimalLatitude", 
               "decimalLongitude", "flags", "institutionID", "collectionCode")

# 1. Determine which sites are "New" ----
sites_all <- read_csv(sites_csv, col_types = cols())

if (file.exists(obis_sites_geo)) {
  existing_sites <- read_sf(obis_sites_geo)
  new_site_ids <- setdiff(sites_all$id, existing_sites$id)
} else {
  new_site_ids <- sites_all$id
  existing_sites <- NULL
}

if (length(new_site_ids) == 0) {
  message("No new sites found. Everything is up to date.")
  if (!interactive()) quit(save = "no")
}

message(glue("Found {length(new_site_ids)} new sites to process..."))

# 2. Process New Sites Only ----
sites_new <- sites_all %>% filter(id %in% new_site_ids)

tic("Processing new sites")
sites_new <- sites_new %>% 
  mutate(
    bbox_025 = map2_chr(lon, lat, get_bbox, dd = 0.25),
    n_025    = map2_int(bbox_025, id, ~occurrence_count(taxonid = c(51, 1806, 882, 3), geometry = .x))
  )

# Recursive expansion for data-poor sites
sites_new <- sites_new %>%
  mutate(
    bbox_dd = case_when(n_025 < 100 ~ 1, TRUE ~ 0.25),
    # FIX: Use pmap_chr to correctly map over the bbox_dd column per site
    bbox    = pmap_chr(list(lon, lat, bbox_dd), get_bbox)
  )

# Final spatial conversion for new sites
sites_new_sf <- st_as_sf(sites_new, wkt = "bbox", crs = 4326, remove = FALSE)
toc()

# 3. Download Occurrences for New Bounding Boxes ----
new_plys <- st_union(sites_new_sf) %>% 
  st_cast("POLYGON") %>% 
  st_as_sf(crs = 4326) %>%
  mutate(wkt = st_as_text(x))

new_occs_list <- list()
for (i in 1:nrow(new_plys)) {
  message(glue("Downloading occurrences for new polygon {i} of {nrow(new_plys)}..."))
  occs <- try(occurrence(taxonid = c(51, 1806, 882, 3), 
                         geometry = new_plys$wkt[i], 
                         fields = obis_flds))
  if (!inherits(occs, "try-error")) new_occs_list[[i]] <- occs
}

new_occs <- bind_rows(new_occs_list)

# 4. Merge and Update Files with Internal Compression ----

# Update occurrences data (Append and compress)
if (file.exists(obis_data_gz)) {
  message("Reading existing compressed data for merging...")
  old_occs <- read_csv(obis_data_gz, col_types = cols())
  combined_occs <- bind_rows(old_occs, new_occs) %>% distinct()
} else {
  combined_occs <- new_occs
}

# write_csv automatically handles GZIP compression if the filename ends in .gz
message("Saving merged data with GZIP compression...")
write_csv(combined_occs, obis_data_gz) 

# Update sites geojson
if (!is.null(existing_sites)) {
  combined_sites <- bind_rows(existing_sites, sites_new_sf)
} else {
  combined_sites <- sites_new_sf
}
st_write(combined_sites, obis_sites_geo, delete_dsn = TRUE)

message("Incremental update and compression complete.")