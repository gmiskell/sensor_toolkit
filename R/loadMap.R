#' A mapping function. It is set up to work in a near real-time manner, where (multiple) daily files are created and saved to a location. Plots use the last seven days.
#'
#' This function gives a map with summary pop-ups for a set of observations.
#' @param x This is either the file (default) or the location of a list of files to upload (if multuple.files is set to `TRUE`).
#' @param set.date This is the latest date for the examined sample. Defaults to `Sys.time()`.
#' @param obs This is the column under observation. Should be a numeric value. Can be multiple columns (e.g. "c("colA","colB")).
#' @param date This is the date column (set up as 'YYYY-MM-DD HH:MM:SS').
#' @param time.zone This is the time zone of the location (e.g. `Pacific/Auckland`).
#' @param id This is an identifier column (e.g. site location name).
#' @param tier This is an additional identifier column, if needed (e.g. different measurement techniques).
#' @param multiple.files An option if multiple files saved locally are to be plotted. Can be useful if there are a few daily files saved that need combining. This option is useful if the function is to run continuously.
#' @param lat Latitude.
#' @param lon Longitude.
#' @param dest This is the location where the interactive map will be saved on the local drive.
#' @param statistic This is the choice for statistic to be presented. Options are `latest` (default), `mean`, `median`, `sd`, `min`, and `max`.
#' @param leaflet.title This is some descriptor of the leaflet, if needed.
#' @export
#' @examples
#' loadMap()


loadMap <- function(x, multiple.files = FALSE, set.date = Sys.time(), obs, time.zone, id, tier = NA, lat, lon, dest, statistic = 'latest', leaflet.title = 'Leaflet Map') {
  
  list.of.packages <- c("lubridate","htmlwidgets","tidyverse");
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
  if(length(new.packages)) install.packages(new.packages);
  lapply(list.of.packages, library, character.only = TRUE);
  
  # install the development version of leaflet
  devtools::install_github('rstudio/leaflet');
  
  # load the data
  if(multiple.files == F){
    data = x;
  };
  
  if(multiple.files == T){
    first.date <- ymd_hms(set.date, tz = time.zone);
    last.date <- first.date - 60*60*24*7; # a week prior to first.date
    date.list <- seq(as.Date(last.date, tz = time.zone), as.Date(first.date, tz = time.zone), by = 'days');
    date.list2 <- paste(date.list, collapse = '|');
    file.list <- list.files(x, pattern = date.list2, full.names = T);
    data <- do(file.list, fread, sep = ',');
  }
  
  # data set up
  data$date <- ymd_hms(data$date);
  data$date <- force_tz(data$date, tz = time.zone);
  if(is.na(tier)) {data$tier <- NA};
  
  # find selected statistic 
  # if median
  if(statistic == 'median'){
    data.sum <- data %>%
      group_by(site, tier) %>%
      summarise_all(median, na.rm = T) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(round(., 2))) 
  };
  # if minimum
  if(statistic == 'min'){
    data.sum <- data %>%
      group_by(site, tier) %>%
      summarise_all(min, na.rm = T) %>%
      ungroup()
  };
  # If maximum
  if(statistic == 'max'){
    data.sum <- data %>%
      group_by(site, tier) %>%
      summarise_all(max, na.rm = T) %>%
      ungroup()
  };
  # if mean
  if(statistic == 'mean'){
    data.sum <- data %>%
      group_by(site, tier) %>%
      summarise_all(mean, na.rm = T) %>%
      ungroup() %>%
      mutate_if(is.numeric, funs(round(., 2)))
  };
  # if standard deviation
  if(statistic == 'sd'){
    data.sum <- data %>%
      group_by(site, tier) %>%
      summarise_all(median, na.rm = T) %>%
      ungroup()
  };
  # if latest concentration
  if(statistic == 'latest'){
    data.sum <- data %>%
      group_by(site) %>%
      filter(date == max(date)) %>%
      mutate(date = force_tz(date, tzone = time.zone)) %>%
      ungroup()
  };
  
  # set up data for leaflet projection
  data.merge <- data.sum %>%
    filter(lat != 'NaN' & lat != 0) %>%
    mutate(lat = jitter(lat, amount = 0.0005), 
           lon = jitter(lon, amount = 0.0005)) %>%
    mutate(instrument.status = ifelse(date > last.date - hours(12), 'Online', 'Offline'));
  
  # make concentration values NA if they did not happen recently (if using latest statistic)
  if(statistic == 'latest'){
    data.merge <- data.merge %>%
      mutate_at(vars(one_of(obs)), funs(ifelse(instrument.status == 'Online', ., NA)))
  };
  
  # create map
  # set map style
  pal <- leaflet::colorNumeric(c("green","yellow","red"), domain = NULL);
  
  leaflet.map <- leaflet(data.merge) %>%
    addTiles(group = 'OSM') %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addTerminator(group = 'daylight') %>%
    addLayersControl(overlayGroups = 'daylight');
  
  for(i in obs){
    leaflet.map <- leaflet.map %>% 
      addCircleMarkers(data = data.merge, ~lon, ~lat, label = as.formula(paste0("~as.character(round(",i,", 2))")), color = as.formula(paste0("~pal(",i,")")), group = as.character(i)) %>% 
      addLegend("bottomleft", group = i, pal = pal, values = as.formula(paste0("~", i)))
  };
  
  leaflet.map <- leaflet.map %>% addLayersControl(baseGroups = as.character(set.date), overlayGroups = obs, options = layersControlOptions(collapsed = F));
  
  # save html map to a location ready for pushing
  saveWidget(leaflet.map, selfcontained = T, file = paste0(dest, leaflet.title, ".html"));  
};
