# PROBLEM: rmarkdown::render_site() deletes entire docs/ folder, so lose all the indiv site pages.
# SOLUTION: copy *.Rmd into docs_pages/, render_site() there, copy into docs/

if (! require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, here, fs, rmarkdown)

setwd(here())
dir_pages <- here("docs_pages") # directory to stage copy of Rmarkdown website files
rmds      <- list.files(pattern="^[^_].*Rmd$") %>% 
  setdiff(c("_site_template.Rmd"))
other     <- c("_site.yml", "_includes", "data", "images", "protocols")

if (dir_exists(dir_pages))
  dir_delete(dir_pages)
dir_create(dir_pages)

sapply(
  c(rmds, other),
  function(p){
    p_new <- path(dir_pages, p)
    if (is_dir(p)){
      dir_copy(p, p_new)
    } else {
      file_copy(p, p_new)
    }})

setwd(dir_pages)
rmarkdown::render_site()
setwd(here())
dir_copy(path(dir_pages, "docs"), path(here(), "docs"), overwrite = T)
dir_delete(dir_pages)
