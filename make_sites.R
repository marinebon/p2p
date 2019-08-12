library(tidyverse)
library(here)
library(glue)

sites_csv <- here("data/sites.csv")

sites <- read_csv(sites_csv) #%>% 
  #filter(id == "bra-saintpeterandsaintpaularchipelago-enseada")

make_site <- function(site_id){
  site_name <- sites %>% 
    filter(id == site_id) %>% 
    pull(name)
  rmarkdown::render(
    input       = "site_template.Rmd",
    params      = list(
      site_name = site_name,
      site_id   = site_id),
    output_file = glue("docs/z_{site_id}.html"))
}

walk(sites$id[1], make_site)
