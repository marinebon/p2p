if (! require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, glue, here, fs, purrr, readr, rmarkdown)

setwd(here())

sites_csv <- here("data/sites.csv")

# Load sites and ensure they are ordered for predictable logging
sites <- read_csv(sites_csv, col_types=cols()) %>% 
  arrange(id)

make_site <- function(id, name, ...){
  # show message of progress
  i_row <- sprintf("%02d", which(id == sites$id))
  message(glue("\n{i_row} of {nrow(sites)} sites\n   id: {id}\n  name: {name}"))
  html <- glue("z_{id}.html")
  
  # Render the biodiversity-only template
  # Changed input to _site_template_obis.Rmd to resolve 403 Forbidden satellite data errors
  rmarkdown::render(
    input       = here("_site_template_obis.Rmd"),
    params      = list(
      site_name = name,
      site_id   = id),
    output_file = html,
    output_dir  = "docs")
}

# Process all sites
# We remove the previous 'DEBUG' filters to ensure the entire site is rebuilt with OBIS data
sites %>% 
  select(id, name) %>% 
  pwalk(make_site)