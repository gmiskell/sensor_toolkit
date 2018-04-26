#' Find the Nearest n Sites.
#'
#' A function for finding the closest n sites to a location.
#' @param id Name of the location (as character string).
#' @param lat Latitude column.
#' @param lon Longitude column.
#' @param n Number of nearest sites to find.
#' @export
#' @examples
#' nearestNSitesFUN()


nearestNSitesFUN <- function(x, id, lat, lon, n, weighted = F){

  library(stats);library(sp);library(geosphere);library(rgeos);library(tidyverse)
  output.file <- NULL

  x$id <- x[, id]; x$lat <- x[, lat]; x$lon <- x[, lon]

  x <- x %>%
    dplyr::select(id, lat, lon) %>%
    na.omit()
  rownames(x) <- seq(length = nrow(x))

  sp.x = x
  coordinates(sp.x) <- ~lon+lat

  d <- gDistance(sp.x, byid = T)
  min.distance <- apply(d, 2, function(x) order(x, decreasing = F)[2:(n+1)])

  for(i in 1:n){

    distance.i <- x[min.distance[i,],]
    combined.df <- cbind(x, distance.i)
    dist.min <- cbind(order = min.distance[i,], site = seq(length(min.distance[i,])))
    combined.df$dist <- d[dist.min]
    combined.df$order <- i
    colnames(combined.df) <- c(colnames(x), "n_id", "n_lat", "n_lon", "dist", "order")

    if(exists('output.file')){
      output.file <- rbind(output.file, combined.df)
      output.file <- unique(output.file)
    } else {
      output.file <- combined.df
    }
  }
  return(output.file)
}
