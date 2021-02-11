if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  bsplus, dygraphs, glue, here, iobis/robis, knitr, leafem, leaflet, mapview, raster, rerddap, tidyverse, xts)

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

  v <- griddap(
    info,
    longitude = c(lon, lon), latitude = c(lat, lat),
    time = c(start_date, dates[2]), fields = field)

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
