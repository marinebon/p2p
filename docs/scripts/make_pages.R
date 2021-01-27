# PROBLEM: rmarkdown::render_site() deletes entire docs/ folder, so lose all the indiv site pages.
# SOLUTION: list all Rmds and render them into html individually
sapply(list.files(pattern="^[^_].*Rmd$"), rmarkdown::render, output_dir = "docs")
