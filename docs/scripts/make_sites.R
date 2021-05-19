if (! require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, glue, here, fs, purrr, readr, rmarkdown)

setwd(here())

sites_csv <- here("data/sites.csv")

sites <- read_csv(sites_csv, col_types=cols()) %>% 
  arrange(id)

make_site <- function(id, name, ...){
  # show message of progress
  i_row <- sprintf("%02d", which(id == sites$id))
  message(glue("\n{i_row} of {nrow(sites)} sites\n   id: {id}\n  name: {name}"))
  html <- glue("z_{id}.html")
  
  # render html
  rmarkdown::render(
    input       = here("_site_template.Rmd"),
    params      = list(
      site_name = name,
      site_id   = id),
    output_file = html,
    output_dir  = "docs")
}

# walk through all sites to render html
sites %>% 
  # render only sites with in situ temperature # DEBUG
  inner_join(
    tibble(
      csv = list.files(here("data/temperature_in-situ"), "csv$"),
      id = fs::path_ext_remove(csv)),
    by = "id") %>%
  # slice(134:nrow(sites)) %>% # DEBUG
  # slice(1:3) %>% # DEBUG
  # TODO: handle ERDDAP timeout
  # slice(113) %>% 
  #   label: unnamed-chunk-1
  # Quitting from lines 17-39 (site_template.Rmd) 
  # Error in curl::curl_fetch_memory(x$url$url, handle = x$url$handle) : 
  #   Timeout was reached: [upwell.pfeg.noaa.gov] Operation timed out after 10005 milliseconds with 0 out of 0 bytes received
  # render only updated in situ temperature non-MARINe p2p sites # DEBUG
  # filter(id %in% c(
  #   "arg-puertomadryn3", "bra-arraialdocabo-fortaleza", 
  #   "bra-costadasalgas-gramute","usa-fknms.csv")) %>% 
  select(id, name) %>% 
  pwalk(make_site)
