library(tidyverse)
library(here)
library(glue)

sites_csv <- here("data/sites.csv")

sites <- read_csv(sites_csv, col_types=cols()) %>% 
  arrange(id)

make_site <- function(id, name){
  # show message of progress
  i_row <- sprintf("%02d", which(id == sites$id))
  message(glue("\n{i_row} of {nrow(sites)} sites\n   id: {id}\n  name: {name}"))
  
  # render html
  rmarkdown::render(
    input       = "site_template.Rmd",
    params      = list(
      site_name = name,
      site_id   = id),
    output_file = glue("docs/z_{id}.html"))
}

# walk through all sites to render html
sites %>% 
  slice(113:nrow(sites)) %>% # DEBUG
  # TODO: handle ERDDAP timeout
  # slice(113) %>% 
  #   label: unnamed-chunk-1
  # Quitting from lines 17-39 (site_template.Rmd) 
  # Error in curl::curl_fetch_memory(x$url$url, handle = x$url$handle) : 
  #   Timeout was reached: [upwell.pfeg.noaa.gov] Operation timed out after 10005 milliseconds with 0 out of 0 bytes received
  select(id, name) %>% 
  pwalk(make_site)
