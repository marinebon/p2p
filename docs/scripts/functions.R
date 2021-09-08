# libraries ----
if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  bsplus, caTools, dygraphs, fs, glue, here, iobis/robis, knitr, 
  leafem, leaflet, lubridate, mapview, 
  raster, rerddap, sf, stringr, tidyverse, purrr, yaml, xts)

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

# convert sf to st_point; add sf geometry list column & coord ref sys
xy2pt <- function(x, y){
  st_point(c(x, y)) %>% 
    st_sfc(crs = 4326)
}

# combine data files for MARINe sites
dataCombiner <- function(data_site_zone) {
  # message("reading in sites")
  
  # store site and zone names
  if ("site" %in% colnames(data_site_zone)) {
    site <- unique(data_site_zone$site) 
  }
  if ("zone" %in% colnames(data_site_zone)) {
    zone <- unique(data_site_zone$zone)
  }
  
  # read temp file corresponding to each path name 
  if (file_ext(data_site_zone$path) == "csv") {
    temp_data <- bind_rows(lapply(data_site_zone$path, read_csv, col_types = cols())) %>% 
      rename(Temp_C = sst) %>% 
      mutate(time   = parse_date_time(date, "y-m-d")) %>% 
      select(-date)
    
  } else {
    temp_data <- bind_rows(lapply(data_site_zone$path, read_tsv)) %>% 
      mutate(time = parse_date_time(Time_GMT, "m/d/y H:M")) %>%
      select(-Time_GMT)
  }
  
  temp_data <- temp_data %>% 
    drop_na() %>% 
    group_by(time) %>% 
    summarize(Temp_C = mean(Temp_C)) %>% 
    mutate(
      site = if("site" %in% colnames(data_site_zone)) site else NA,
      zone = if("zone" %in% colnames(data_site_zone)) zone else NA) %>% 
    relocate(site)
}

dailyQuantilesData <- function(data) {
  data %>% 
    mutate(
      day = floor_date(time, unit = "day")) %>%
    group_by(day) %>%
    distinct(day, .keep_all = T) %>% 
    mutate(
      temp_c_q10 = quantile(Temp_C, 0.1),
      temp_c_q90 = quantile(Temp_C, 0.9),
      temp_c_avg = mean(Temp_C),
      temp_c_min = min(Temp_C),
      temp_c_max = max(Temp_C)) %>% 
    select(-time, -Temp_C) %>% 
    gather("metric", "Temp_C", c(-1, -2, -3)) %>% 
    select(-zone, zone)
}

# read in smoothed csv and convert to xts for dygraphs
get_xts <- function(path) {
  d_smoothed <- read_csv(path) %>%
    mutate(
      day = parse_date_time(day, "ymd")) %>%
    mutate_at(vars(-1), funs(as.numeric))
  x_smoothed <- xts(select(d_smoothed, -day), order.by = d_smoothed$day)
  return(x_smoothed)
}

plot_insitutemp <- function(temp_csv, meta_yml, main = "Daily Temperature", ylab="ºC", ...) {
  # TODO: consider colors for non-MARINe zones in dygraph plot
  
  # MARINe:
  #   temp_csv <- "/Users/bbest/github/p2p/data/temperature_in-situ/usa-bml.csv"
  #   meta_yml <- "/Users/bbest/github/p2p/data/temperature_in-situ/usa-bml_meta.yml"
  # non-MARINe, wierd zones:
  #   temp_csv <- "/Users/bbest/github/p2p/data/temperature_in-situ/arg-puertomadryn3.csv"
  #   meta_yml <- "/Users/bbest/github/p2p/data/temperature_in-situ/arg-puertomadryn3_meta.yml"
  # non-MARINe:
  #   temp_csv <- "/Users/bbest/github/p2p/data/temperature_in-situ/bra-arraialdocabo-fortaleza.csv"
  #   meta_yml <- "/Users/bbest/github/p2p/data/temperature_in-situ/bra-arraialdocabo-fortaleza_meta.yml"
  # main = "Daily Temperature"; ylab="ºC"

  m <- read_yaml(meta_yml)
  d <- read_csv(temp_csv, col_types = cols())
  x <- xts(select(d, -day), order.by = d$day)

  is_MARINe <- str_detect(m$org, "MARINe")
  
  if (is_MARINe){
    zones <- setdiff(names(d), "day") 
    pal   <- c("#3D2C9A", "#3E98C5", "#4A9A78", "#F7BD33", "#D74B00")
  } else {
    zones <- setdiff(names(d), "day") %>% 
      str_replace_all(c("temp_c_avg_","temp_c_min_","temp_c_max_"), "") %>% 
      unique()
    mins  <- glue("temp_c_min_{zones}")
    avgs  <- glue("temp_c_avg_{zones}")
    maxes <- glue("temp_c_max_{zones}")
  }

  # MARINE: no min/max ribbon
  if (is_MARINe) {
    zone_colors <- setNames(pal, zones) 
    
    # plot
    p <- x %>% 
      dygraph(
        main = main, 
        ylab = ylab, ...) %>%
      dyHighlight(
        highlightCircleSize = 5, 
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE) %>%
      dyOptions(
        colors                 = as.character(zone_colors[names(x)]),
        connectSeparatedPoints = FALSE) %>% 
      dyOptions(
        fillGraph = FALSE, 
        fillAlpha = 0.4) %>%
      dyRangeSelector()
    
  } else {
    p <- x %>% 
      dygraph(
        main = main, 
        #ylab = ylab, ...) %>%
        ylab = ylab) %>%
      dySeries(
        c(mins[1], avgs[1], maxes[1]), 
        label = glue("{zones[1]}")) %>% 
      dySeries(
        c(mins[2], avgs[2], maxes[2]),
        label = glue("{zones[2]}")) %>% 
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = TRUE) %>%
      dyOptions(
        connectSeparatedPoints = FALSE) %>% 
      dyOptions(
        fillGraph = FALSE, fillAlpha = 0.4) %>%
      dyRangeSelector()
  }
  m$day_min <- min(d$day)
  m$day_max <- max(d$day)
  m$zones   <- zones
  attr(p, "meta") <- m
  p
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
    addProviderTiles(providers$Esri.OceanBasemap, group="Color") %>%
    addProviderTiles(providers$Stamen.TonerLite, group="B&W") %>%
    #addProviderTiles(providers$Stamen.TonerLabels) %>%
    addRasterImage(r, colors = pal, opacity = 0.8, project=F, group="CHL") %>%
    addMarkers(lng = site_lon, lat = site_lat, label = site_label) %>%
    addLegend(pal = pal, values = values(r), title = title, position="bottomright") %>%
    addLayersControl(
      baseGroups = c("Color", "B&W"),
      overlayGroups = c("SST"),
      options = layersControlOptions(collapsed = T))
}

get_raster_2 <- function(info, lon, lat, date="last", field="chlor_a"){
  g_2 <- griddap(
    info, longitude = lon, latitude = lat,
    time = c(date, date), fields = field)
  grid_to_raster(g_2, "chlor_a") %>%
    leaflet::projectRasterForLeaflet(method="ngb")
}

map_raster_2 <- function(r, site_lon, site_lat, site_label, title){
  pal <- colorNumeric(colors$temperature, values(r), na.color = "transparent")

  leaflet() %>%
    addProviderTiles(providers$Esri.OceanBasemap, group="Color") %>%
    addProviderTiles(providers$Stamen.TonerLite, group="B&W") %>%
    #addProviderTiles(providers$Stamen.TonerLabels) %>%
    addRasterImage(r, colors = pal, opacity = 0.8, project=F, group="CHL") %>%
    addMarkers(lng = site_lon, lat = site_lat, label = site_label) %>%
    addLegend(pal = pal, values = values(r), title = title, position="bottomright") %>%
    addLayersControl(
      baseGroups = c("Color", "B&W"),
      overlayGroups = c("CHL"),
      options = layersControlOptions(collapsed = T))
}

get_timeseries <- function(info, lon, lat, csv, field="sst"){

  # info = chl; lon=site$lon; lat=site$lat; csv=csv; field="chlor_a"

  dates  <- get_dates(info)
  
  if (file.exists(csv)){
    d_prev <- read_csv(csv, col_types=cols()) %>%
      arrange(date)
    start_date <- read_csv(csv, col_types=cols()) %>%
      tail(1) %>%
      pull(date) %>%
      as.POSIXct()
  } else {
    start_date <- dates[1]
  }
  
  # TODO: handle non-responsive ERDDAP server
  # griddap <- function (x, ..., fields = "all", stride = 1, fmt = "nc", url = eurl(),
  #                              store = disk(), read = TRUE, callopts = list())
  # {
  #   calls <- names(sapply(match.call(), deparse))[-1]
  #   calls_vec <- "ncdf" %in% calls
  #   if (any(calls_vec)) {
  #     stop("The parameter ncdf has been removed. We use ncdf4 package now internally",
  #          call. = FALSE)
  #   }
  #   x <- as.info(x, url)
  #   dimargs <- list(...)
  #   rerddap:::check_dims(dimargs, x)
  #   rerddap:::check_lat_text(dimargs)
  #   rerddap:::check_lon_text(dimargs)
  #   dimargs <- rerddap:::fix_dims(dimargs, .info = x)
  #   rerddap:::check_lon_data_range(dimargs, x)
  #   rerddap:::check_lat_data_range(dimargs, x)
  #   d <- attr(x, "datasetid")
  #   var <- rerddap:::field_handler(fields, x$variables$variable_name)
  #   dims <- rerddap:::dimvars(x)
  #   store <- rerddap:::toggle_store(fmt, store)
  #   if (all(var == "none")) {
  #     args <- paste0(sapply(dims, function(y) {
  #       rerddap:::parse_args(x, y, stride, dimargs, wname = TRUE)
  #     }), collapse = ",")
  #   }
  #   else {
  #     pargs <- sapply(dims, function(y) rerddap:::parse_args(x, y, stride,
  #                                                  dimargs))
  #     args <- paste0(lapply(var, function(y) {
  #       paste0(y, paste0(pargs, collapse = ""))
  #     }), collapse = ",")
  #   }
  #   fmt <- match.arg(fmt, c("nc", "csv"))
  #   resp <- erd_up_GET(url = sprintf("%sgriddap/%s.%s", url,
  #                                    d, fmt), dset = d, args = args, store = store, fmt = fmt,
  #                      callopts)
  #   loc <- if (store$store == "disk")
  #     resp
  #   else "memory"
  #   outclasses <- switch(fmt, nc = c("griddap_nc", "nc", "list"),
  #                        csv = c("griddap_csv", "csv", "data.frame"))
  #   read <- toggle_read(read, store)
  #   structure(read_all(resp, fmt, read), class = outclasses,
  #             datasetid = d, path = loc, url = url_build(sprintf("%sgriddap/%s.%s",
  #                                                                url, d, fmt), args))
  # }
  # 
  # erd_up_GET <- function (url, dset, args, store, fmt, callopts){
  #   if (length(args) > 0)
  #     url <- sprintf("%s?%s", url, args)
  #   cli <- crul::HttpClient$new(url = url, opts = callopts)
  #   if (store$store == "disk") {
  #     key <- rerddap:::gen_key(url, args, fmt)
  #     if (file.exists(file.path(store$path, key))) {
  #       file.path(store$path, key)
  #     }
  #     else {
  #       dir.create(store$path, showWarnings = FALSE, recursive = TRUE)
  #       if (!store$overwrite) {
  #         stop("overwrite was `FALSE`, see ?disk")
  #       }
  #       res <- cli$get(disk = file.path(store$path, key))
  # 
  #       browser()
  #       rerddap:::err_handle(res, store, key)
  #       res$content
  #     }
  #   }
  #   else {
  #     res <- cli$get()
  # 
  #     browser()
  #     rerddap:::err_handle(res, store, key)
  #     res
  #   }
  # }
  # 

  v <- try(griddap(
    info,
    longitude = c(lon, lon), latitude = c(lat, lat),
    time = c(start_date, dates[2]), fields = field))

  d_now <- v$data %>%
    as_tibble() %>%
    mutate(
      date = lubridate::as_date(time, format = "%Y-%m-%dT00:00:00Z")) %>%
    select(date, field) %>%
    arrange(date)

  if (file.exists(csv)){
    d <- bind_rows(d_prev, d_now) %>%
      filter(!duplicated(all_of(date)))
  } else {
    d <- d_now
  }

  d %>%
    write_csv(csv)
  d
}

plot_timeseries <- function(d, title="SST", color="red", dyRangeSelector=T, ...){
  p <- xts(select(d, -date), order.by=d$date) %>%
    dygraph(main=title, ...) %>%
    dyOptions(
      colors = color,
      fillGraph = TRUE, fillAlpha = 0.4)
  if (dyRangeSelector){
    p <- p %>%
      dyRangeSelector()
  }
  p
}

map_obis_pts <- function(pts, ply){
  if (nrow(pts) == 0){
    print("No data")
  } else if (nrow(pts)  > 0){
    leaflet(pts) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolygons(
        data = ply, color = "blue") %>% 
      addCircleMarkers(
        fillOpacity = 0.7, color = "yellow", radius = 7, stroke = F,
        clusterOptions = markerClusterOptions())
  }
}

plot_obis_hist <- function(df, x, y){
  g <- ggplot() +
    geom_histogram(data = df, aes(x = {{x}}, fill = {{y}}), binwidth = 3) +
    scale_fill_brewer(palette = "Spectral") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    theme(axis.text.x = element_text(size=14, angle=0),
          axis.text.y = element_text(size=14, angle=0))
  g
}
