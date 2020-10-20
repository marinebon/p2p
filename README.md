# p2p
***Marine Biodiversity Observation Network Pole to Pole of the Americas ([MBON Pole to Pole](https://marinebon.org/p2p/)) website***

[![Build Status](https://travis-ci.org/marinebon/p2p.svg?branch=master)](https://travis-ci.org/marinebon/p2p)

This website was built as an [R Markdown website](https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html).

## Add a Site

Add entry to [`data/sites.csv`](https://github.com/marinebon/p2p/blob/master/data/sites.csv). Be sure to use a unique `id`. Please do not add more than 10 entries at a time.

Then build site (or check into Github and Travis will build it for you). A new marker will be populated into the home page map via `rmarkdown::render_site()` rendering [`index.Rmd`](https://github.com/marinebon/p2p/blob/master/index.Rmd) and a new site page will be generated via `source("make_sites.R")` (see [`make_sites.R`](https://github.com/marinebon/p2p/blob/master/index.Rmd))) rendering a [parameterized Rmarkdown](https://bookdown.org/yihui/rmarkdown/parameterized-reports.html) using [`site_template.Rmd`](https://github.com/marinebon/p2p/blob/master/site_template.Rmd).

For notes about finding country given lat/lon (ie reverse geocoding), creating Google Maps URL (`gmapurl`; optional, not a necessary field), check out code in [`prep.R`](https://github.com/marinebon/p2p/blob/master/prep.R).

- **WARNING**: If you change the coordinates of a site (ie lat/lon in `sites.csv`), please delete the associated site's file in `data/sst`, so the timeseries plot becomes representative of the data for that coordinate. Also, do not add more than 10 sites to the `sites.csv` at once to avoid Tracis CI to stop running due to time limit.

## Add Pictures to a Site

Associate URL of a photo to `id` of a site in a new row into 
[site_photos - Google Sheets](https://docs.google.com/spreadsheets/d/1-1rIIiH9OV1C7vPzAH4R_PpC1fFj3ZoA6sV8J28Loxc/edit#gid=292028887).

To use a photo stored in Google Drive, right-click on the file > **Get sharable link**, for instance

* sharable link: `https://drive.google.com/open?id=18lW657Las2jUrDeA06nZ77Smd27Es_N3` <br/>
or
* shareble link: `https://drive.google.com/file/d/1luySMHkWAOi-inyhURmarcii_1fXfbmW/view?usp=sharing`

And replace the beginning of the URL up to `id=` or `/d/` with (also delete `/view?usp=sharing`): 

* `http://drive.google.com/uc?export=view&id=`

So here's the usable URL:

* `http://drive.google.com/uc?export=view&id=18lW657Las2jUrDeA06nZ77Smd27Es_N3` <br/>
or
* `http://drive.google.com/uc?export=view&id=1luySMHkWAOi-inyhURmarcii_1fXfbmW`

The website will get rebuilt weekly (see [travis-ci.org/marinebon/p2p/settings](https://travis-ci.org/marinebon/p2p/settings)). If you want the website to rebuild sooner, simply make a modification to a file in the github site, like to the [`README.md`](https://github.com/marinebon/p2p/edit/master/README.md), and commit the change to fire off a website rebuild with Travis: [travis-ci.org/marinebon/p2p](https://travis-ci.org/marinebon/p2p).

Photos are now stored in Enrique's GDrive and should eventually be moved to a dedicated drive.

### Old info:

So far as a test, we only have some pics from these two sites:

- [Site: Isla Gorgona - La Mancora](https://marinebon.github.io/p2p/z_col-islagorgona-lamancora.html)
- [Site: Costa das Algas - Gramuté](https://marinebon.github.io/p2p/z_bra-costadasalgas-gramute.html)

[Flickr](http://flickr.com/) seems to have the most options for tagging and creating albums or collections (ie BB's [mbon-p2p | Flickr](https://www.flickr.com/photos/bbest/albums/72157705442437201)). You can also [get the URL of a Flickr photo](https://help.flickr.com/en_us/get-the-url-of-a-flickr-photo-S1Hnnmjym)
with a given size (eg "Medium (640 × 480)").

## Building Website

- The easiest way to generate the website is clicking on the **Build Website** menu button from the **Build** pane in RStudio after opening this repository's project by double-clicking on `p2p.Rproj`. All the content files (\*.Rmd, \*.md, \*.html) are rendered and output to the **`docs/`** folder, where it is served to [marinebon.github.io/p2p](https://marinebon.github.io/p2p) via [Github Pages](https://pages.github.com).

- **WARNING**: Do _**NOT**_ place any content in the **`docs/`** since it gets wiped clean before rebuilding the website in its place.

To build from command shell cd to the project & run:

- R   : `rmarkdown::render_site(); source("make_sites.R")`
- bash: `Rscript -e 'rmarkdown::render_site(); source("make_sites.R")'`

### Installing Dependencies

If you don't have the required packages you will need to install them:
`cd` into the project directory then execute the following R to install dependencies automatically from the `DESCRIPTION` file.

```R
install.packages('devtools')
devtools.install('./')
```

For Travis to work, add package to `Imports:` in `DESCRIPTION`. For packages not on CRAN, also add to `Remotes:` to install via Github.

## Resources

- [Markdown cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)

- Jekyll themes:
  - [Phlow/feeling-responsive](https://github.com/Phlow/feeling-responsive), [demo](http://phlow.github.io/feeling-responsive/)
  
- End -

  
