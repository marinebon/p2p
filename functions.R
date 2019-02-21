library(tidyverse) 
library(here) 
library(glue) 
library(rerddap)  # install.packages("rerddap")
library(raster)
library(leaflet)
library(dygraphs) # install.packages("dygraphs")
library(xts)
library(rerddap)
#library(plotdap)
select <- dplyr::select
addLegend <- leaflet::addLegend


grid_to_raster <- function (grid, var) {
  # original: plotdap:::get_raster
  # grid <- sst_grid
  #library(magrittr)
  
  times <- grid$summary$dim$time$vals
  lats <- grid$summary$dim$latitude$vals
  lons <- grid$summary$dim$longitude$vals
  ylim <- range(lats, na.rm = TRUE)
  xlim <- range(lons, na.rm = TRUE)
  ext <- raster::extent(xlim[1], xlim[2], ylim[1], ylim[2])
  r <- if (length(times) > 1) {
    d <- dplyr::arrange(grid$data, time, desc(lat), lon)
    b <- raster::brick(nl = length(times), nrows = length(lats), 
                       ncols = length(lons))
    raster::values(b) <- lazyeval::f_eval(var, d)
    raster::setExtent(b, ext)
  }
  else {
    d <- dplyr::arrange(grid$data, desc(lat), lon)
    r <- raster::raster(nrows = length(lats), ncols = length(lons), 
                        #ext = ext, vals = lazyeval::f_eval(var, d)) # plotdap:::get_raster
                        ext = ext, vals = d[,var])
  }
  #browser()
  #names(r) <- make.names(unique(grid$data$time) %||% "")
  r
}

get_dates <- function(info){
  info$alldata$time %>% 
    filter(attribute_name=="actual_range") %>% 
    pull(value) %>% 
    str_split(", ", simplify = T) %>% 
    as.numeric() %>% 
    as.POSIXct(origin = "1970-01-01", tz = "GMT")
}

get_box <- function(lon, lat, cells_wide){
  w <- cells_wide * 0.01 / 2
  box <- list(
    lon = c(round(lon, 2) - w, round(lon, 2) + w), 
    lat = c(round(lat, 2) - w, round(lat, 2) + w))
}

get_raster <- function(info, lon, lat, date="last", field="sst"){
  g <- griddap(
    info, longitude = lon, latitude = lat, 
    time = c(date, date), fields = field)
  grid_to_raster(g, "sst") %>% 
    leaflet::projectRasterForLeaflet(method="ngb")
}

map_raster <- function(r, site_lon, site_lat, site_label, title){
  pal <- colorNumeric(colors$temperature, values(r), na.color = "transparent")
  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap) %>%
    #addProviderTiles(providers$Stamen.TonerLite) %>%
    #addProviderTiles(providers$Stamen.TonerLabels) %>%
    addRasterImage(r, colors = pal, opacity = 0.8, project=F) %>%
    addMarkers(lng = site_lon, lat = site_lat, label = site_label) %>%
    addLegend(pal = pal, values = values(r), title = title)
}

get_timeseries <- function(info, lon, lat, csv, field="sst"){
  
  dates  <- get_dates(info)
  
  if (file.exists(csv)){
    d_prev <- read_csv(csv) %>% 
      arrange(date)
    start_date <- read_csv(csv) %>% 
      tail(1) %>% 
      pull(date) %>% 
      as.POSIXct()
  } else {
    start_date <- dates[1]
  }
  
  v <- griddap(
    info,
    longitude = c(lon, lon), latitude = c(lat, lat), 
    time = c(start_date, dates[2]), fields = field)
  
  d_now <- v$data %>%
    as_tibble() %>%
    mutate(
      date = lubridate::as_date(time, "%Y-%m-%dT00:00:00Z")) %>%
    select(date, sst) %>%
    arrange(date)
  
  if (file.exists(csv)){
    d <- bind_rows(d_prev, d_now) %>% 
      filter(!duplicated(date))
  } else {
    d <- d_now
  }
  
  d %>% 
    write_csv(csv)
  d
}

plot_timeseries <- function(d, title="SST", color="red"){
  xts(select(d, -date), order.by=d$date) %>% 
    dygraph(main=title) %>%
    dyOptions(
      colors = color,
      fillGraph = TRUE, fillAlpha = 0.4) %>% 
    dyRangeSelector()
}
