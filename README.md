# Sensor Toolkit
## A collection of R scripts to help in managing an air quality sensor network.

The sensor toolkit offers a collection of R scripts that can be used to help in managing a low-cost air pollution sensor network. Most analysis here is set to run in a "real time" framework, with use of running metrics.

### Set up.
    
    library(devtools)
    # load the dev sensortoolkit from GitHub
    install_github('gmiskell/sensortoolkit')
    library(sensortoolkit)
    
Some example data, `example.RData` has been included for demonstration purposes. Note, the pollution data (`pol`) is deliberately changed by a random value for data propriety reasons.

    load('~/example.RData')   
    as.tibble(example)
    # A tibble: 22,362 x 5
       date                site        pol   lat   lon
       <dttm>              <chr>     <dbl> <dbl> <dbl>
     1 2017-09-10 00:00:00 AQY-AA011 11.5  -43.5  173.
     2 2017-09-11 00:00:00 AQY-AA011 22.9  -43.5  173.
     3 2017-09-12 00:00:00 AQY-AA011  5.44 -43.5  173.
     4 2017-09-13 00:00:00 AQY-AA011  5.56 -43.5  173.
     5 2017-09-14 00:00:00 AQY-AA011  2.8  -43.5  173.
     6 2017-09-15 00:00:00 AQY-AA011 37.7  -43.5  173.
     7 2017-09-16 00:00:00 AQY-AA011 23.6  -43.5  173.
     8 2017-09-17 00:00:00 AQY-AA011  4.85 -43.5  173.
     9 2017-09-18 00:00:00 AQY-AA011 18.9  -43.5  173.
    10 2017-09-19 00:00:00 AQY-AA011 36.6  -43.5  173.
    # ... with 22,352 more rows
   
### Proxy options.
Proxies, a signal from an independent site or number of sites to the observation with similar statistical properties, was introduced in papers [here](https://www.researchgate.net/publication/286479082_Data_Verification_Tools_for_Minimizing_Management_Costs_of_Dense_Air-Quality_Monitoring_Networks). In brief, ... Here, the provided code uses **only** sensor data - nearby regulatory data has been found to be a good proxy if the data is available.

#### sensornetworkmedianFUN

    networkmedian.example <- example %>%
      do(networkmedianFUN(., obs = 'pol', group = 'date', id = 'site')) %>%
      rename(date = group, site = id)
    as.tibble(networkmedian.example)
    # A tibble: 22,362 x 4
       date                  obs network.proxy site     
       <dttm>              <dbl>         <dbl> <chr>    
     1 2017-09-10 00:00:00 11.5           14.8 AQY-AA011
     2 2017-09-10 00:00:00 17.5           14.8 AQY-AA012
     3 2017-09-10 00:00:00 21.2           14.8 AQY-AA013
     4 2017-09-10 00:00:00  9.65          14.8 AQY-AA014
     5 2017-09-10 00:00:00 24.8           14.8 AQY-AA015
     6 2017-09-10 00:00:00 22.9           14.8 AQY-AA016
     7 2017-09-10 00:00:00 14.8           14.8 AQY-AA017
     8 2017-09-10 00:00:00  7.39          14.8 AQY-AA018
     9 2017-09-10 00:00:00 30.9           14.8 AQY-AA019
    10 2017-09-10 00:00:00  5.61          14.8 AQY-AA020
    # ... with 22,352 more rows

#### nearestsiteFUN

    nearestsite.example = example %>% 
      do(nearestsiteFUN(., group = 'site', obs = 'pol', lat = 'lat', lon = 'lon')) %>% 
      rename(site = group)
    as.tibble(nearestsite.example)
    # A tibble: 22,362 x 4
       date                site        obs nearest.proxy
       <dttm>              <chr>     <dbl>         <dbl>
     1 2017-09-10 00:00:00 AQY-AA011 11.5          16.5 
     2 2017-09-11 00:00:00 AQY-AA011 22.9          27   
     3 2017-09-12 00:00:00 AQY-AA011  5.44          6.61
     4 2017-09-13 00:00:00 AQY-AA011  5.56          8.92
     5 2017-09-14 00:00:00 AQY-AA011  2.8           4.83
     6 2017-09-15 00:00:00 AQY-AA011 37.7          35.9 
     7 2017-09-16 00:00:00 AQY-AA011 23.6          36.0 
     8 2017-09-17 00:00:00 AQY-AA011  4.85          4.8 
     9 2017-09-18 00:00:00 AQY-AA011 18.9          18.0 
    10 2017-09-19 00:00:00 AQY-AA011 36.6          41.3 
    # ... with 22,352 more rows

#### nearestNSitesFUN

    nearestn.example = example %>% 
      do(nearestNSitesFUN(., id = 'site', lat = 'lat', lon = 'lon', n = 3)) %>%
      rename(site = id)
    as.tibble(nearestn.example)
       site        lat   lon n_id      n_lat n_lon    dist order
     * <chr>     <dbl> <dbl> <chr>     <dbl> <dbl>   <dbl> <int>
     1 AQY-AA011 -43.5  173. AQY-AA026 -43.5  173. 0.0160      1
     2 AQY-AA012 -43.5  173. AQY-AA014 -43.5  173. 0.0202      1
     3 AQY-AA013 -43.5  173. AQY-AA016 -43.5  173. 0.00224     1
     4 AQY-AA014 -43.5  173. AQY-AA012 -43.5  173. 0.0202      1
     5 AQY-AA015 -43.5  173. AQY-AA027 -43.5  173. 0.0332      1
     6 AQY-AA016 -43.5  173. AQY-AA013 -43.5  173. 0.00224     1
     7 AQY-AA017 -43.5  173. AQY-AA023 -43.5  173. 0.0225      1
     8 AQY-AA018 -43.6  173. AQY-AA022 -43.6  173. 0.0172      1
     9 AQY-AA019 -43.6  173. AQY-AA020 -43.5  173. 0.0283      1
    10 AQY-AA020 -43.5  173. AQY-AA028 -43.5  173. 0.0190      1
    # ... with 50 more rows
  
#### sitesInNKMFUN
This proxy function also only identifies the locations within a specified radius. Following identification, one may choose to use the `networkmedianFUN` and group by `site` and `n_id`. A note on the parameter, `n`, which defines the size of the radius. It is calculated using degrees, which change relative to where on the globe the measurements are positioned. An online calculator can be useful in finding the correct `n` [here](https://www.nhc.noaa.gov/gccalc.shtml). The inputs that will change the distance will be the latitude options (as longitude remains constant). Here, our dataset is around 40 degrees south. Entering 40.95 and 41 into the latitude options (a difference in degrees of 0.05) is a 6 km distance, and so those locations within 6 km will be identified.

    nearestkm.example = example %>% 
      do(sitesInNKMFUN(., id = 'site', lat = 'lat', lon = 'lon', n = 0.05)) %>%
      rename(site = id)
    as.tibble(nearestkm.example)
       site        lat   lon n_id     
       <chr>     <dbl> <dbl> <chr>    
     1 AQY-AA011 -43.5  173. AQY-AA012
     2 AQY-AA011 -43.5  173. AQY-AA013
     3 AQY-AA011 -43.5  173. AQY-AA016
     4 AQY-AA011 -43.5  173. AQY-AA026
     5 AQY-AA012 -43.5  173. AQY-AA011
     6 AQY-AA012 -43.5  173. AQY-AA013
     7 AQY-AA012 -43.5  173. AQY-AA014
     8 AQY-AA012 -43.5  173. AQY-AA016
     9 AQY-AA012 -43.5  173. AQY-AA018
    10 AQY-AA012 -43.5  173. AQY-AA024
    # ... with 126 more rows

#### ksFUN - A running two-sided Kolmogorov-Smirnov test.

    ks.example = networkmedian.example %>% 
      group_by(site) %>% 
      do(ksFUN(., obs = 'obs', date = 'date', proxy = 'network.proxy'))
    |==============================================================|100% ~0 s remaining     
    as.tibble(ks.example)
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
    ggplot(ks.example, aes(date, statistic)) + geom_line() + facet_wrap(~site)

[](ks_example.tiff)

#### mvFUN - A running custom Mean-Variance comparison test.

    mv.example = networkmedian.example %>% 
      group_by(site) %>% 
      do(mvFUN(., obs = 'obs', date = 'date', proxy = 'network.proxy')) %>% 
      spread(test, statistic) # if using warning and alarms keep data in long format
    |==============================================================|100% ~0 s remaining     
    as.tibble(mv.example)
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
    ggplot(mv.example, aes(date, mv_intercept)) + geom_line() + facet_wrap(~site)
    
[](mv_example.tiff)

#### loadMap - A script for creating and saving a HTML leaflet map.

    loadMap(example, set.date = '2017-10-15', obs = 'pol', time.zone = 'Pacific/Auckland', id = 'site', lat = 'lat', lon = 'lon', dest = '')

#### shiny_display - A script for creating an interactive shiny app.

    shiny_display(as.data.frame(example), cols.to.display = 'pol')

[](shiny_example.tiff)
