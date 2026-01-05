# Check for existing HTML files in the docs folder and only generate pages for new sites.

if (! require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, glue, here, fs, purrr, readr, rmarkdown)

setwd(here())

sites_csv <- here("data/sites.csv")
dir_docs  <- here("docs")

# Load sites
sites_all <- read_csv(sites_csv, col_types=cols()) %>% 
  arrange(id)

# 1. Identify which sites already have HTML files ----
# We look for files matching the pattern "z_{id}.html"
existing_htmls <- list.files(dir_docs, "^z_.*\\.html$")
existing_ids   <- existing_htmls %>% 
  str_replace("^z_", "") %>% 
  str_replace("\\.html$", "")

# 2. Filter for new sites only ----
sites_new <- sites_all %>% 
  filter(!id %in% existing_ids)

if (nrow(sites_new) == 0) {
  message("All site pages already exist in /docs. Nothing to generate.")
  if (!interactive()) quit(save = "no")
}

message(glue("Found {nrow(sites_new)} new sites to generate..."))

make_site <- function(id, name, ...){
  # show message of progress relative to the total number of sites
  i_row <- sprintf("%02d", which(id == sites_all$id))
  message(glue("\n{i_row} of {nrow(sites_all)} sites\n   id: {id}\n  name: {name}"))
  html <- glue("z_{id}.html")
  
  # Render the biodiversity-only template
  rmarkdown::render(
    input       = here("_site_template_obis.Rmd"),
    params      = list(
      site_name = name,
      site_id   = id),
    output_file = html,
    output_dir  = "docs")
}

# 3. Walk through only the new sites ----
sites_new %>% 
  select(id, name) %>% 
  pwalk(make_site)

message("Incremental site generation complete.")