#' Deriving the Network Median.

#' The network median is found for each iteration. Values are removed, one at a time, across the iteration, with the removal specified by the group option. To run this over a time-series the group would be the date. The final product is a column of the network median that can be used as a proxy in other analyses.
#' @param obs The data to derive the network median from.
#' @param group An identifying variable.
#' @param id This is an option if some column is required to remain in the final output (e.g. site). Default is `NA`.
#' @param by.group This option is if the iterations need to happen over the group (default, `TRUE`). `FALSE` uses all values.
#' @param statistic This is the statistic to summarise the data. Default is `median`. Other options are `mean` or `sd`.
#' @export
#' @examples
#' networkmedianFUN()

networkmedianFUN <- function(x, group, obs, by.group = T, id = NA, statistic = median){

	list.of.packages <- c("lubridate","tidyverse");
	lapply(list.of.packages, library, character.only = T);

	# define variables
	x$obs <- x[[obs]];
	x$group <- x[[group]];
	if(!is.na(id)) x$id <- x[[id]];

	# filter data to that of interest
	z <- x %>% dplyr::select(group, obs);
	if(!is.na(id)) z.id <- x %>% dplyr::select(group, id, obs);

	net.day.FUN <- function(z){
		if(length(z$obs) > 1){
			network.proxy <- unlist(sapply(seq(1, nrow(z)), function(i){
				test = z[-i,];
				network.proxy <- statistic(test$obs, na.rm = T);
				return(network.proxy);
			}))
		} else {
			network.proxy <- NA;
			network.proxy <- as.numeric(network.proxy);
		}
		z <- cbind(z, network.proxy);
		z <- unique(z);
	};

	net.FUN <- function(z){
		group.list <- as.vector(unique(z$group));
		output = data.frame(group = as.character(),
		network.proxy = as.numeric());
		for(i in group.list){
			if(length(z$obs) > 1){
				selected.group = i;
				group.stat <- data.frame(group = selected.group,
				network.proxy = statistic(z$obs[z$group != selected.group], na.rm = T))
				output = bind_rows(output, group.stat);
			}
		}
		x <- full_join(x, output);
		x <- unique(x);
	}

	if(by.group == T){
		Z.data <- z %>% group_by(group) %>% do(net.day.FUN(.));
	}

	if(by.group == F){
		Z.data <- net.FUN(z)
	}

	if(!is.na(id)) Z.data <- left_join(Z.data, z.id, by = c("group", "obs"));
  
	return(Z.data);
};
