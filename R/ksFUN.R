#' Running Kolmogorov-Smirnov (KS) Two-Sample Test.
#'
#' This function gives the running KS test for an observation, relative to a proxy from some other observation/s.
#' @param obs The data under investigation.
#' @param date A required column with the format 'YYYY-MM-DD HH:MM:SS'.
#' @param proxy The comparison data.
#' @param all.data This decides whether to use all provided data (all.data - `TRUE`), or to run the function on live data (`FALSE`), which makes the function possible to run as a schedule for real-time updating. Defaults is `TRUE`.
#' @param theta This sets the test threshold on whether to flag the data. Defaults to `NA` (no flags given).
#' @param tau This sets the day threshold on whether to flag the data given consistent theta flags. Defaults to `NA` (no flags given).
#' @param window.length This defines the length of the sampled window, defined by row number. Default is 72 (three days of hourly data).
#' @export
#' @examples
#' ksFUN()


ksFUN <- function(x, obs, date, proxy, all.data = TRUE, theta = NA, tau = NA, window.length = 72){
	
	# install and load required packages
	list.of.packages <- c("raster","lubridate","data.table","tidyverse");
	lapply(list.of.packages, library, character.only = T);
	
	# define selected variables
	# use `data.table` package to deal with large datasets
	x <- as.data.table(x);
	x$date <- x[[date]];
	setDT(x)[, date := ymd_hms(date)];
	
	# clause on type of analysis to be run
	if (all.data == TRUE){
		date.start = min(x$date, na.rm = T);
		date.end = max(x$date, na.rm = T);
	} else {
		date.start = Sys.time()-60*60*24*7; # a week prior from Sys.time
		date.end = Sys.time();
		date.start <- ymd_hms(date.start); date.end <- ymd_hms(date.end);
	};
	
	# assign variable names and filter data to the set dates
	x <- x[date %within% interval(date.start, date.end)];
	x$proxy <- x[[proxy]];
	x$obs <- x[[obs]];
	x <- x[order(date)];
	
	# select the required columns
	x <- x[, list(date, obs, proxy)];
	
	# remove any repeating and empty rows
	x <- unique.data.frame(x);
	x <- na.omit(x);
	
	# generate running KS test results - see separate rollingKStest function in toolkit
	if(length(x$obs) > window.length){
		ks.x <- rollingKStest(x, obs = 'obs', proxy = 'proxy', window = window.length);
	} else {
		ks.x <- cbind(x, p.value = rep(NA, length(x[[1]])), statistic = rep(NA, length(x[[1]])));
	}
	ks.x <- as.data.table(ks.x);
	
	# use theta and tau thresholds, if set
	if(!is.na(theta)){
		ks.x <- setDT(ks.x)[, warning := ifelse(p.value < theta, 1, 0)];
		ks.x <- ks.x[, warning := ifelse(is.na(warning), 0, warning)];
		if(!is.na(tau)){
			# if else clause on whether the data are less than theta, becomes 1 if true, and 0 otherwise
			# this looks at running means and is for tau, the length of time for alarms
			ks.x <- ks.x[, alarm := movingFun(warning, n = tau, type = 'to', fun = mean, na.rm = T)];
			ks.x <- ks.x[, alarm := ifelse(is.na(alarm), 0, alarm)];
		} else {
			ks.x$alarm <- NA;
		}; 
	} else {
		ks.x$warning <- NA;
		ks.x$alarm <- NA;
	};
	
	# gather variables of interest and return the final product
	ks.x <- ks.x[, list(date, test = 'ks test', statistic = p.value, warning, alarm)];
	ks.x;
};
