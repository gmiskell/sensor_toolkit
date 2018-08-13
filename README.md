# Sensor Toolkit
## A collection of R scripts to help in managing an air quality sensor network.

The sensor toolkit offers a collection of R scripts that can be used to help in managing a low-cost air pollution sensor network. Most analysis here is set to run in a "real time" framework, with use of rolling metrics.

### Set up.
    
    library(devtools)
    # load the dev sensortoolkit from GitHub
    install_github('gmiskell/sensortoolkit')
    library(sensortoolkit)
    
Some example data, `example.RData` has been included for demonstration purposes. Note, the pollution data (`pol`) is deliberately changed to a random value for data propriety reasons.

    load('~/example.RData')   
	example
	# A tibble: 22,362 x 5
	   date                site        lat   lon   pol
	   <dttm>              <chr>     <dbl> <dbl> <dbl>
	 1 2017-09-10 00:00:00 AQY-AA011 -43.5  173.  6.20
	 2 2017-09-11 00:00:00 AQY-AA011 -43.5  173.  3.19
	 3 2017-09-12 00:00:00 AQY-AA011 -43.5  173.  5.34
	 4 2017-09-13 00:00:00 AQY-AA011 -43.5  173.  5.56
	 5 2017-09-14 00:00:00 AQY-AA011 -43.5  173.  1.53
	 6 2017-09-15 00:00:00 AQY-AA011 -43.5  173.  3.72
	 7 2017-09-16 00:00:00 AQY-AA011 -43.5  173.  3.09
	 8 2017-09-17 00:00:00 AQY-AA011 -43.5  173.  4.28
	 9 2017-09-18 00:00:00 AQY-AA011 -43.5  173.  5.31
	10 2017-09-19 00:00:00 AQY-AA011 -43.5  173.  3.98
	# ... with 22,352 more rows
    
### Proxy options.

Proxies, a signal from an independent site or number of sites to the observation with similar statistical properties, was introduced in this [paper](https://www.researchgate.net/publication/286479082_Data_Verification_Tools_for_Minimizing_Management_Costs_of_Dense_Air-Quality_Monitoring_Networks). In brief, the paper aimed to minimise costs, both of expenditure and maintenance, for an air quality network. It presented a simple statistical framework that does not need extensive training datasets for automated verification of the reliability of data. Data are from a network of both low-cost and regulatory instruments measuring ground-level ozone, a pollutant with a relatively regular spatiotemporal profile, although there can be significant small-scale variations. The framework takes advantage of these characteristics to detect calibration drift in low-cost sensors by using running tests and using a suitably selected comparison dataset, or ‘proxy’. Land use was found to be a good determinant for a proxy, and drift was noted within one-week, on average. Results here were also used to correct sensor drift, with results in this [paper](https://www.researchgate.net/publication/323595039_Solution_to_the_Problem_of_Calibration_of_Low-Cost_Air_Quality_Measurement_Sensors_in_Networks) <br> 
Here, the provided code uses **only** low-cost sensor data - nearby regulatory data has been found to be a good proxy if the data is available.

#### sensornetworkmedianFUN

    networkmedian_example <- example %>%
      do(networkmedianFUN(., obs = 'pol', group = 'date', id = 'site')) %>%
      rename(date = group, site = id)
    networkmedian_example
	# A tibble: 22,362 x 4
	# Groups:   date [1,246]
	   date                  obs network.proxy site     
	   <dttm>              <dbl>         <dbl> <chr>    
	 1 2017-09-09 11:00:00  5.19          5.46 AQY-AA011
	 2 2017-09-09 11:00:00  1.95          5.46 AQY-AA012
	 3 2017-09-09 11:00:00  1.95          5.46 AQY-AA013
	 4 2017-09-09 11:00:00  4.87          5.46 AQY-AA014
	 5 2017-09-09 11:00:00  3.93          5.46 AQY-AA015
	 6 2017-09-09 11:00:00  5.51          5.19 AQY-AA016
	 7 2017-09-09 11:00:00  5.53          5.19 AQY-AA017
	 8 2017-09-09 11:00:00  3.99          5.46 AQY-AA018
	 9 2017-09-09 11:00:00  6.05          5.19 AQY-AA019
	10 2017-09-09 11:00:00  3.79          5.46 AQY-AA020
	# ... with 22,352 more rows

#### nearestsiteFUN

    nearestsite_example <- example %>% 
      do(nearestsiteFUN(., group = 'site', obs = 'pol', lat = 'lat', lon = 'lon')) %>% 
      rename(site = group)
    as.tibble(nearestsite_example)
	# A tibble: 22,362 x 4
	   date                site        obs nearest.proxy
	   <dttm>              <chr>     <dbl>         <dbl>
	 1 2017-09-10 00:00:00 AQY-AA011  6.20          9.40
	 2 2017-09-11 00:00:00 AQY-AA011  3.19          4.98
	 3 2017-09-12 00:00:00 AQY-AA011  5.34          5.09
	 4 2017-09-13 00:00:00 AQY-AA011  5.56          7.70
	 5 2017-09-14 00:00:00 AQY-AA011  1.53          3.24
	 6 2017-09-15 00:00:00 AQY-AA011  3.72          2.71
	 7 2017-09-16 00:00:00 AQY-AA011  3.09          7.07
	 8 2017-09-17 00:00:00 AQY-AA011  4.28          2.85
	 9 2017-09-18 00:00:00 AQY-AA011  5.31          1.22
	10 2017-09-19 00:00:00 AQY-AA011  3.98          5.95
	# ... with 22,352 more rows
	
#### nearestNSitesFUN

    nearestn_example <- example %>% 
      do(nearestNSitesFUN(., id = 'site', lat = 'lat', lon = 'lon', n = 3)) %>%
      rename(site = id)
    as.tibble(nearestn_example)
	# A tibble: 60 x 8
	   site        lat   lon n_id      n_lat n_lon  dist order
	 * <chr>     <dbl> <dbl> <chr>     <dbl> <dbl> <dbl> <int>
	 1 AQY-AA011 -43.5  173. AQY-AA026 -43.5  173. 1775.     1
	 2 AQY-AA012 -43.5  173. AQY-AA014 -43.5  173. 2276.     1
	 3 AQY-AA013 -43.5  173. AQY-AA016 -43.5  173.  260.     1
	 4 AQY-AA014 -43.5  173. AQY-AA024 -43.5  173. 2256.     1
	 5 AQY-AA015 -43.5  173. AQY-AA027 -43.5  173. 3594.     1
	 6 AQY-AA016 -43.5  173. AQY-AA013 -43.5  173.  260.     1
	 7 AQY-AA017 -43.5  173. AQY-AA030 -43.5  173. 2252.     1
	 8 AQY-AA018 -43.6  173. AQY-AA022 -43.6  173. 1726.     1
	 9 AQY-AA019 -43.6  173. AQY-AA029 -43.6  173. 2468.     1
	10 AQY-AA020 -43.5  173. AQY-AA028 -43.5  173. 1494.     1
	# ... with 50 more rows
  
#### sitesInNKMFUN
This proxy function also only identifies the locations within a specified radius. Following identification, one may choose to use the `networkmedianFUN` and group by `site` and `n_id`. A note on the parameter, `n`, which defines the size of the radius. It is calculated using degrees, which change relative to where on the globe the measurements are positioned. An online calculator can be useful in finding the correct `n` [here](https://www.nhc.noaa.gov/gccalc.shtml). Here, our dataset is around 40 degrees south. Entering 40.95 and 41 into the latitude options (a difference in degrees of 0.05) with similar longitude value is a 6 km distance, and so those locations within 6 km will be identified.

    nearestkm_example <- example %>% 
      do(sitesInNKMFUN(., id = 'site', lat = 'lat', lon = 'lon', n = 0.05)) %>%
      rename(site = id)
	as.tibble(nearestkm_example)
	# A tibble: 140 x 4
	   site        lat   lon n_id     
	   <chr>     <dbl> <dbl> <chr>    
	 1 AQY-AA011 -43.5  173. AQY-AA012
	 2 AQY-AA011 -43.5  173. AQY-AA013
	 3 AQY-AA011 -43.5  173. AQY-AA014
	 4 AQY-AA011 -43.5  173. AQY-AA016
	 5 AQY-AA011 -43.5  173. AQY-AA026
	 6 AQY-AA012 -43.5  173. AQY-AA011
	 7 AQY-AA012 -43.5  173. AQY-AA013
	 8 AQY-AA012 -43.5  173. AQY-AA014
	 9 AQY-AA012 -43.5  173. AQY-AA016
	10 AQY-AA012 -43.5  173. AQY-AA017
	# ... with 130 more rows

#### ksFUN - A running two-sided Kolmogorov-Smirnov test.

    ks_example <- networkmedian_example %>% 
      group_by(site) %>% 
      do(ksFUN(., obs = 'obs', date = 'date', proxy = 'network.proxy'))
    |==============================================================|100% ~0 s remaining     
    as.tibble(ks_example)
    # A tibble: 22,349 x 6
    # Groups:   site [20]
            site                date    test statistic warning alarm
           <chr>              <dttm>   <chr>     <dbl>   <lgl> <lgl>
     1 AQY-AA011 2017-09-09 11:00:00 ks test        NA      NA    NA
     2 AQY-AA011 2017-09-09 12:00:00 ks test        NA      NA    NA
     3 AQY-AA011 2017-09-09 13:00:00 ks test        NA      NA    NA
     4 AQY-AA011 2017-09-09 14:00:00 ks test        NA      NA    NA
     5 AQY-AA011 2017-09-09 15:00:00 ks test        NA      NA    NA
     6 AQY-AA011 2017-09-09 16:00:00 ks test        NA      NA    NA
     7 AQY-AA011 2017-09-09 17:00:00 ks test        NA      NA    NA
     8 AQY-AA011 2017-09-09 18:00:00 ks test        NA      NA    NA
     9 AQY-AA011 2017-09-09 19:00:00 ks test        NA      NA    NA
    10 AQY-AA011 2017-09-09 20:00:00 ks test        NA      NA    NA
    # ... with 22,339 more rows
    ggplot(ks_example, aes(date, statistic)) + geom_line() + facet_wrap(~site)

![](https://github.com/gmiskell/sensortoolkit/blob/master/ks_example.png)

#### mvFUN - A running custom Mean-Variance comparison test.

    mv_example <- networkmedian_example %>% 
      group_by(site) %>% 
      do(mvFUN(., obs = 'obs', date = 'date', proxy = 'network.proxy')) %>% 
      spread(test, statistic) # if using warning and alarms keep data in long format
    |==============================================================|100% ~0 s remaining     
    as.tibble(mv_example)
    # A tibble: 22,362 x 6
    # Groups:   site [20]
            site                date warning alarm mv_intercept mv_slope
     *     <chr>              <dttm>   <lgl> <lgl>        <dbl>    <dbl>
     1 AQY-AA011 2017-09-09 11:00:00      NA    NA           NA       NA
     2 AQY-AA011 2017-09-09 12:00:00      NA    NA           NA       NA
     3 AQY-AA011 2017-09-09 13:00:00      NA    NA           NA       NA
     4 AQY-AA011 2017-09-09 14:00:00      NA    NA           NA       NA
     5 AQY-AA011 2017-09-09 15:00:00      NA    NA           NA       NA
     6 AQY-AA011 2017-09-09 16:00:00      NA    NA           NA       NA
     7 AQY-AA011 2017-09-09 17:00:00      NA    NA           NA       NA
     8 AQY-AA011 2017-09-09 18:00:00      NA    NA           NA       NA
     9 AQY-AA011 2017-09-09 19:00:00      NA    NA           NA       NA
    10 AQY-AA011 2017-09-09 20:00:00      NA    NA           NA       NA
    # ... with 22,352 more rows
    ggplot(mv_example, aes(date, mv_intercept)) + geom_line() + facet_wrap(~site)
    
![](https://github.com/gmiskell/sensortoolkit/blob/master/mv_example.png)

#### loadMap - A script for creating and saving a HTML leaflet map.

    loadMap(example, set.date = '2017-10-15', obs = 'pol', time.zone = 'Pacific/Auckland', id = 'site', lat = 'lat', lon = 'lon', dest = '')

#### shiny_display - A script for creating an interactive shiny app.

    shiny_display(as.data.frame(example), cols.to.display = 'pol')

![](https://github.com/gmiskell/sensortoolkit/blob/master/shiny_example.png)
