library(tidyverse)
library(here)
library(glue)
library(mregions)
library(countrycode)
library(RCurl)
library(jsonlite)
library(listviewer)

# paths
sites_csv <- here("data/sites.csv")

# update sites.csv with country codes for prefixing site names ----

# google maps api key
gkey_txt <- "~/private/google-maps-api-key_bdbest-csuci-invite.txt"
gkey <- readLines(gkey_txt)

sites <- read_csv(sites_csv, encoding="UTF-8")

get_country <- function(lon, lat){

  name <- filter(sites, longitude==lon, latitude==lat) %>% pull(name)
  row <- which(sites$name == name)
  
  url_pfx <- "https://maps.googleapis.com/maps/api/geocode/json"
  url <- glue("{url_pfx}?latlng={lat},{lon}&key={gkey}")
  
  cat(glue("{str_pad(row, 2, pad = '0')}: {name}\n  {url}\n\n"))
  
  r <- mr_rev_geo_code(lat, lon)
  # View(r)
  
  country <- filter(r, placeType == "EEZ") %>% pull(preferredGazetteerName)

  if (length(country)==0){
    json <- fromJSON(getURL(url))
    # listviewer::jsonedit(json)
    
    country <- str_split(json$plus_code$compound_code, ", ", simplify=T) %>% as.vector() %>% tail(1)
  }
  
  if (length(country)==0){
    country = NA
  } else {
    country <- country %>% 
      str_replace("Exclusive Economic Zone", "") %>% 
      str_replace("USVI", "United States Virgin Islands") %>% 
      str_replace("\\((.*)\\)", "") %>% 
      str_trim() %>% 
      countrycode('country.name', 'iso3c') %>% 
      str_to_lower()
  }
  
  cat("  country: ", country, "\n")
  country
}

get_gmapurl <- function(lon, lat){
  glue("http://maps.google.com/maps?q={lat},{lon}&ll={lat},{lon}&z=6")
}

sites <- sites %>% 
  mutate(
    country = map2_chr(longitude, latitude, get_country),
    gmapurl = map2_chr(longitude, latitude, get_gmapurl))
#View(sites)

write_csv(sites, sites_csv)

# extract org from name ----
sites <- read.csv(sites_csv)

sites <- sites %>% 
  mutate(
    org   = str_replace(name, "(.*)\\((.*)\\)", "\\2"),
    name = str_replace(name, "(.*)\\((.*)\\)", "\\1")) %>% 
  select(
    id=site, org, name, country, lat=latitude, lon=longitude, gmapurl)
#View(sites)

write_csv(sites, sites_csv)

# make site pages ----
sites <- read.csv(sites_csv)
View(sites)

site <- sites %>% slice(1)
glue(.open = "{{", .close = "}}", '
  ---
  title: "{{site$name}}"
  ---
    
  Country: {{str_to_upper(str_to_upper(site$country))}}
  Organization: {{site$org}}

  ```{r setup, include=FALSE}
  knitr::opts_chunk$set(echo = TRUE)
  ```
  ')


