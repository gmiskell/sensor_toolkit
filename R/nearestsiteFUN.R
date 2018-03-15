#' This function gives the proxy type of nearest site data.
#'
#' @param obs This is the column being examined.
#' @param group This is the grouping variable.
#' @param lat Latitude column.
#' @param lon Longitude column.
#' @export
#' @examples
#' nearestsiteFUN()
  
nearestsiteFUN <- function(x, obs, group, lat, lon){

  # install and load required packages
  list.of.packages <- c("dplyr");
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
  if(length(new.packages)) install.packages(new.packages);
  library(dplyr);

  # define variables
  x <- as.data.frame(x);
  x$obs <- x[, obs]; x$group <- x[, group]; x$lat <- x[, lat]; x$lon <- x[, lon];
  
  # find closest sites
  y <- x %>%
    select(group, lat, lon) %>%
    unique();
	  
  d <- as.matrix(dist(cbind(y$lon, y$lat)));
  d <- data.frame(d);
  colnames(d) <- c(as.character(d$group), 'group');	
  min.d <- apply(d, 1, function(x) order(x, decreasing = F)[2]);
  newdata <- cbind(y, y[min.d,], apply(d, 1, function(x) sort(x, decreasing=F)[2]));
  colnames(newdata) <- c(colnames(y), 'n.group', 'n.lat', 'n.lon', 'distance');
  newdata <- newdata[,c(1, 4:7)];

  # combine the data with appropriate proxy
  df3 <- join(x, newdata, by = 'group');
  x$nearest.proxy <- x$obs;
  x$obs <- NULL;
	
  finaldata <- left_join(df3, x, by = c('date', 'n.group' = 'group'));
  finaldata <- finaldata %>%
    select(date, group, obs, nearest.proxy);
	
}


