#' Find All Sites Within n Radius.
#'
#' A function for finding all sites within a radius around a location.
#' @param id Name of the location (as character string).
#' @param lat Latitude column.
#' @param lon Longitude column.
#' @param n Distance radius (in degrees). Kilometre distance can be found using the latitude of the measurement locaition.
#' @param as.list Option to unite the selected sites into a string (can be useful for later selection).
#' @export
#' @examples
#' sitesInNKMFUN()

sitesInNKMFUN <- function(x, id, lat, lon, n, as.list = F){
  
  library(stats); library(sp); library(rgeos); library(tidyverse)
  
  x$id <- x[[id]]; x$lat <- x[[lat]]; x$lon <- x[[lon]]
  
  x <- x %>%
    dplyr::select(id, lat, lon) %>%
    unique() %>%
    na.omit() %>%
    mutate(id = as.character(id))
  
  sp.x = x
  coordinates(sp.x) <- ~lon+lat
  
  n.distance <- gWithinDistance(sp.x, byid = T, dist = n)
  
  x.dist <- cbind(x, n.distance)
  colnames(x.dist) <- c(colnames(x), x$id)
  
  x.gather <- x.dist %>%
    gather(n_id, in_n, -id, -lat, -lon) %>%
    filter(id != n_id & in_n == TRUE) %>%
    dplyr::select(-in_n) %>%
    arrange(id)
  
  to.return <- x.gather
  
  if(as.list == TRUE){
    
    x.spread <- x.gather %>%
      spread(n_id, n_id) %>%
      mutate(n_id = apply(.[,-c(1:3)], 1, paste, collapse = "|")) %>%
      dplyr::select(id, lat, lon, n_id)
    
    to.return <- x.spread
  }
  
  return(to.return)
}
