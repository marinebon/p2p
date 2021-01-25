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
  select(id, name) %>% 
  pwalk(make_site)
