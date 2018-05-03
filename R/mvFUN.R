#' A running mean-variance function.
#'
#' @param obs The data under investigation.
#' @param date A required column with the format 'YYYY-MM-DD HH:MM:SS'.
#' @param proxy The comparison data.
#' @param all.data This decides whether to use all provided data (all.data - `TRUE`), or to use data from the latest day (`FALSE`), which makes the function possible to run as a schedule for real-time updating. Defaults to `TRUE`.
#' @param window.length This defines the length of the sampled window, defined by row number. Default is 72 (three days of hourly data).
#' @param intercept.theta This sets the test threshold on whether to flag the data for the intercept. Defaults to `NA` (no flags given).
#' @param slope.theta This sets the test threshold on whether to flag the data for the slope. Defaults to `NA` (no flags given).
#' @param tau This sets the day threshold on whether to flag the data given consistent theta flags. Defaults to `NA` (no flags given).
#' @param accum This is if the data before the `n` sample size should still perform the statistics with a reduced sample size.
#' @param truncate This is is subsets of the data should be derived, by using only values above or below the mean to derive the mean and variance.
#' @param trun.direction This the side of the mean to take for the sample, if truncate is `TRUE`. Options are 'under' (below the sample mean) or 'over' (above the sample mean).
#' @export
#' @examples
#' mvFUN()


mvFUN <- function(x, proxy = 'comparison.value', obs = 'obs', date = 'date', window.length = 72, slope.theta = NA, intercept.theta = NA, tau = NA, accum = FALSE, truncate = FALSE, all.data = TRUE, trun.direction = 'over'){
	
	# install and load required packages
	list.of.packages <- c("stats","lubridate","zoo","raster","data.table","tidyverse");
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
	if(length(new.packages)) install.packages(new.packages);
	lapply(list.of.packages, library, character.only = T);
	
	# define selected variables
  # use `data.table` package to deal with large datasets
	x <- as.data.table(x);
	x$date <- x[, date];
	x[, date := ymd_hms(date)];
    
  # clause on type of analysis to be run
	if (all.data == TRUE){
		date.start = min(x$date, na.rm = T);
		date.end = max(x$date, na.rm = T);
	} else {
		date.start = Sys.time()-60*60*24*7;
		date.end = Sys.time();
		date.start <- ymd_hms(date.start); date.end <- ymd_hms(date.end);
	};
	
	x <- x[date %within% interval(date.start, date.end)];
	x$obs <- x[,..obs];
	x$proxy <- x[,..proxy];
	x <- x[order(date)];
	date <- x[,..date];
	
	y <- data.frame(x[, obs], x[, proxy]);
	y.zoo <- zoo(y);
	colnames(y.zoo) <- c('s','r');
	
	# create custom MV functions
	mv.dev.i <- function(x, y){
		min.length <- min(c(length(na.omit(x)), length(na.omit(y))))
		if(min.length > 0.25 * window.length){
			if(truncate == TRUE){
				if(trun.direction == 'over'){
					x <- ifelse(x > mean(x, na.rm = T), x, NA);
          y <- ifelse(y > mean(y, na.rm = T), y, NA);
				} else {
          x <- ifelse(x < mean(x, na.rm = T), x, NA);
          y <- ifelse(y < mean(y, na.rm = T), y, NA);
        }
			}
			intercept <- mean(x, na.rm = T) - mean(y, na.rm = T) * sqrt((var(x, na.rm = T)) / var(y, na.rm = T))
		} else {
			intercept <- NA
		}
		return(intercept)
	};
  
  mv.dev.s <- function(x, y){
		min.length <- min(c(length(na.omit(x)), nrow(length(y))))
		if(min.length > 0.25 * window.length){
      if(truncate == TRUE){
        if(trun.direction == 'over'){
          x <- ifelse(x > mean(x, na.rm = T), x, NA);
          y <- ifelse(y > mean(y, na.rm = T), y, NA);
				} else {
          x <- ifelse(x < mean(x, na.rm = T), x, NA);
          y <- ifelse(y < mean(y, na.rm = T), y, NA);
        }
      }
      slope <- sqrt((var(x, na.rm = T)) / var(y, na.rm = T))
		} else {
      slope <- NA
    }
    return(slope)
  };
	
	slope <- rollapply(y.zoo, function(x) mv.dev.s(x[, 's'], x[, 'r']), width = window.length, by.column = F, fill = NA, align = 'right');
  
  intercept <- rollapply(y.zoo, function(x) mv.dev.i(x[, 's'], x[, 'r']), width = window.length, by.column = F, fill = NA, align = 'right');

  if(accum == TRUE){
		intercept.start <- mean(w$obs[1:window], na.rm = T) - mean(w$proxy[1:window], na.rm = T) * sqrt((var(w$obs[1:window], na.rm = T)) / var(w$proxy[1:window], na.rm = T));
    slope.start <- sqrt((var(w$obs[1:window], na.rm = T)) / var(w$proxy[1:window], na.rm = T));
    intercept <- c(rep(intercept.start,window - 1), intercept[window:length(intercept)]);
    slope <- c(rep(slope.start,window - 1), slope[window:length(slope)]);
	}
  
  mv_value <- data.frame(slope);
  mv_value$test <- 'mv_slope';
  colnames(mv_value)[1] <- 'mv_value';
  slope <- cbind(date, mv_value);
  
 # use theta and tau thresholds if present
    if(!is.na(slope.theta)){
			mv.slope <- setDT(slope)[, warning := ifelse(mv_value > 1 + slope.theta, 1, 0)];
			mv.slope <- mv.slope[, warning := ifelse(mv_value < 1 - slope.theta, 1, warning)]
			mv.slope <- mv.slope[, warning := ifelse(is.na(warning), 0, warning)];
			if(!is.na(tau)){
				# if else clause on whether the data are less than theta, becomes 1 if true, and 0 otherwise
				# this looks at running means and is for tau, the length of time for alarms
				mv.slope <- mv.slope[, alarm := movingFun(warning, n = tau, type = 'to', fun = mean, na.rm = T)];
        mv.slope <- mv.slope[, alarm := ifelse(is.na(alarm), 0, alarm)];
			} else {
				mv.slope <- slope;
        mv.slope$alarm <- NA;
      }; 
    } else {
      mv.slope <- slope;
      mv.slope$warning <- NA;
      mv.slope$alarm <- NA;
    };
    
  mv_value <- data.frame(intercept);
  mv_value$test <- 'mv_intercept';
  colnames(mv_value)[1] <- 'mv_value';
  intercept <- cbind(date, mv_value);
												 
	# use theta and tau thresholds if present
	if(!is.na(intercept.theta)){
		mv.int <- setDT(intercept)[, warning := ifelse(mv_value > intercept.theta, 1, 0)];
		mv.int <- mv.int[, warning := ifelse(mv_value < -intercept.theta, 1, warning)];
		mv.int <- mv.int[, warning := ifelse(is.na(warning), 0, warning)];
      if(!is.na(tau)){
      # if else clause on whether the data are less than theta, becomes 1 if true, and 0 otherwise
      # this looks at running means and is for tau, the length of time for alarms
				mv.int <- mv.int[, alarm := movingFun(warning, n = tau, type = 'to', fun = mean, na.rm = T)];
        mv.int <- mv.int[, alarm := ifelse(is.na(alarm), 0, alarm)];
      } else {
        mv.int <- intercept;
        mv.int$alarm <- NA;
      }; 
	} else {
      mv.int <- intercept;
      mv.int$warning <- NA;
      mv.int$alarm <- NA;
	};
  
  z <- bind_rows(mv.int, mv.slope); 
  b <- full_join(x, z, by = 'date');

  # gather variables of interest and return
  mv.x <- b %>% dplyr::select(date, test = test, statistic = mv_value, warning, alarm);
	mv.x;
};
