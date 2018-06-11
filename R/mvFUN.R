#' Running Mean-Variance Test.
#'
#' This function gives the running MV test for an observation, relative to a proxy from some other observation/s.
#' @param obs The data under investigation.
#' @param date A required column with the format 'YYYY-MM-DD HH:MM:SS'.
#' @param proxy The comparison data.
#' @param all.data This decides whether to use all provided data (all.data - `TRUE`), or to use data from the latest day (`FALSE`), which makes the function possible to run as a schedule for real-time updating. Defaults to `TRUE`.
#' @param intercept.theta This sets the test threshold on whether to flag the data for the intercept. Defaults to `NA` (no flags given).
#' @param slope.theta This sets the test threshold on whether to flag the data for the slope. Defaults to `NA` (no flags given).
#' @param tau This sets the day threshold on whether to flag the data given consistent theta flags. Defaults to `NA` (no flags given).
#' @param window.length This defines the length of the sampled window, defined by row number. Default is 72 (three days of hourly data).
#' @param truncate This is is subsets of the data should be derived, by using only values above or below the mean to derive the mean and variance.
#' @param trun.direction This the side of the mean to take for the sample, if truncate is `TRUE`. Options are 'under' (below the sample mean) or 'over' (above the sample mean).
#' @export
#' @examples
#' mvFUN()

mvFUN <- function(x, obs, date, proxy, all.data = TRUE, intercept.theta = NA, slope.theta = NA, tau = NA, window.length = 72, truncate = FALSE, trun.direction = 'over'){
	
	# install and load required packages
	list.of.packages <- c("lubridate", "zoo", "raster", "tidyverse");
	lapply(list.of.packages, library, character.only = T);
	
	# define selected variables
	x$date <- x[[date]]; x$obs <- x[[obs]]; x$proxy <- x[[proxy]];
	x$date <- ymd_hms(x$date);
    
	# clause on type of analysis to be run
	if (all.data == TRUE){
		date.start = min(x$date, na.rm = T);
		date.end = max(x$date, na.rm = T);
	} else {
		date.start = now()-60*60*24*7;
		date.end = now();
		date.start <- ymd_hms(date.start); date.end <- ymd_hms(date.end);
	};
	
	x <- x %>% filter(date %within% interval(date.start, date.end)) %>% arrange(date);
	date <- x[[date]];
	
	y <- x %>% dplyr::select(obs, proxy);
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

	mv.slope <- data.frame(slope);
	colnames(mv.slope)[1] <- 'mv_value';
	mv.slope$test <- 'mv_slope';
	mv.slope <- cbind(date, mv.slope)

	mv.intercept <- data.frame(intercept);
	colnames(mv.intercept)[1] <- 'mv_value';
	mv.intercept$test <- 'mv_intercept';
	mv.intercept <- cbind(date, mv.intercept);
  
	# use theta and tau thresholds if present
	if(!is.na(slope.theta)){
		mv.slope$warning <- ifelse(mv.slope$mv_value > 1 + slope.theta, 1, 0);
		mv.slope$warning <- ifelse(mv.slope$mv_value < 1 - slope.theta, 1, mv.slope$warning)
		mv.slope$warning <- ifelse(is.na(mv.slope$warning), 0, mv.slope$warning);
		if(!is.na(tau)){
		# if else clause on whether the data are less than theta, becomes 1 if true, and 0 otherwise
		# this looks at running means and is for tau, the length of time for alarms
			mv.slope$alarm <- movingFun(mv.slope$warning, n = tau, type = 'to', fun = mean, na.rm = T);
			mv.slope$alarm <- ifelse(is.na(mv.slope$alarm), 0, mv.slope$alarm);
		} else {
			mv.slope <- mv.slope$mv_slope;
			mv.slope$alarm <- NA;
		}; 
	} else {
		mv.slope$warning <- NA;
		mv.slope$alarm <- NA;
	};
    
	# use theta and tau thresholds if present
	if(!is.na(intercept.theta)){
		mv.intercept$warning <- ifelse(mv.intercept$mv_value > intercept.theta, 	1, 0);
		mv.intercept$warning <- ifelse(mv.intercept$mv_value < -intercept.theta, 1, 	mv.intercept$warning);
		mv.intercept$warning <- ifelse(is.na(mv.intercept$warning), 0, mv.intercept$warning);
		if(!is.na(tau)){
			mv.intercept$alarm <- movingFun(mv.intercept$warning, n = tau, type = 'to', fun = 	mean, na.rm = T);
			mv.intercept$alarm <- ifelse(is.na(mv.intercept$alarm), 0, mv.intercept$alarm);
		} else {
			mv.intercept <- mv.intercept$mv_intercept;
			mv.intercept$alarm <- NA;
		}; 
	} else {
		mv.intercept$warning <- NA;
		mv.intercept$alarm <- NA;
	};
  
	z <- bind_rows(mv.intercept, mv.slope); 
	b <- full_join(x, z, by = 'date');

	# gather variables of interest and return
	mv.x <- b %>% dplyr::select(date, test = test, statistic = mv_value, 	warning, alarm);
	mv.x;
};
