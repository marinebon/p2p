# p2p
Pole to Pole website

This website was built as an [R Markdown website](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html).

## Building Website

- The easiest way to generate the website is clicking on the **Build Website** menu button from the **Build** pane in RStudio after opening this repository's project by double-clicking on `p2p.Rproj`. All the content files (\*.Rmd, \*.md, \*.html) are rendered and output to the **`docs/`** folder, where it is served to [marinebon.github.io/p2p](https://marinebon.github.io/p2p) via [Github Pages](https://pages.github.com).

- **WARNING**: Do _**NOT**_ place any content in the **`docs/`** since it gets wiped clean before rebuilding the website in its place.

To build from command shell cd to the project & run:
* R   : `rmarkdown::render_site()`
* bash: `Rscript -e 'rmarkdown::render_site()'`

### Installing Dependencies
If you don't have the required packages you will need to install them:
`cd` into the project directory then execute the following R to install dependencies automatically from the `DESCRIPTION` file.

```R
install.packages('devtools')
devtools.install('./')
```

## Resources

- [Markdown cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)

- Jekyll themes:
  - [Phlow/feeling-responsive](https://github.com/Phlow/feeling-responsive), [demo](http://phlow.github.io/feeling-responsive/)
