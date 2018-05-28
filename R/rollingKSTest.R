#' This function gives the rolling KS function
#'
#' @param obs This is the column being examined.
#' @param proxy This is the comparison variable
#' @param window This defines the size of the window to sample data from 
#' @export
#' @examples
#' rollingKStest()

rollingKStest <- function(z, obs, proxy, window = 1440){
  
  library(zoo);library(dplyr);

  # define variables
  z <- as.data.frame(z);
  z$obs <- z[[obs]]; z$proxy <- z[[proxy]];
  
  if(length(z$obs) > window){
    
    z <- z %>%
      dplyr::arrange(date);
    
    # save date column
    date <- z[, 'date'];
    
    # turn data into data frame
    y <- data.frame(z$obs, z$proxy);
  
    # convert df into zoo classes used in R for rolling functions
    y.zoo <-zoo(y);
    colnames(y.zoo) <- c('r', 's');
  
	# create custom KS function to deal with missing data
	
	ks.dev.p <- function(x, y){
  
  min.length <- min(c(length(na.omit(x)),length(na.omit(y))));
  
  if(min.length > 0.5 * length(x)){
    p.value <- suppressWarnings(ks.test(x, y)$p.value);
  } else {
    p.value <- NA;
  }
  return(p.value);
};

ks.dev.s <- function(x, y){
  
  min.length <- min(c(length(na.omit(x)),length(na.omit(y))));
  
  if(min.length > 0.5 * length(x)){
    statistic <- suppressWarnings(ks.test(x, y)$statistic);
  } else {
    statistic <- NA;
  };
  return(statistic);
};
    # running ks functions
    p.value <- rollapply(y.zoo, function (x) ks.dev.p(x[, 'r'], x[, 's']), 
	  by.column = F, fill = NA, align = 'right', width = window);
    statistic <- rollapply(y.zoo, function (x) ks.dev.s(x[, 'r'], x[, 's']), 
	  by.column = F, fill = NA, align = 'right', width = window);
  
    p.value <- data.frame(p.value);
    statistic <- data.frame(statistic);
  
    # make results into df and tidy
    model <- cbind(p.value, statistic);
    names(model) <- c('p.value', 'statistic');
    model <- data.frame(lapply(model, function(x) round(x, 6)));
    model <- cbind(date, model);
  
    # join ks results to the data and return
    z <- left_join(z, model, by = 'date');
    } else {	
	
	z$p.value <- NA;
	z$statistic <- NA;
 };
};