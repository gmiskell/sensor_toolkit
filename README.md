# Sensor Toolkit
## A collection of R scripts to help in managing an air quality sensor network.

The sensor toolkit offers a collection of R scripts that can be used to help in managing a low-cost air pollution sensor network. Most analysis here is set to run in a "real time" framework, with use of running metrics.

### Set up.
    
    library(devtools)
    # load the dev sensortoolkit from GitHub
    install_github('gmiskell/sensortoolkit')
    library(sensortoolkit)
    
Some example data, `example.RData` has been included for demonstration purposes. Note, the pollution data is deliberately changed by a random value for data propriety reasons.

    load('~/example.RData')
    as.tibble(example)
    # A tibble: 22,362 x 7
             date      site         O3     GSEOx       lat      lon   random
           <dttm>     <chr>      <dbl>     <dbl>     <dbl>    <dbl>    <dbl>
     1 2017-09-10 AQY-AA011  5.2616667 12.288333 -43.50591 172.5714 6.201248
     2 2017-09-11 AQY-AA011 19.7216667 12.888333 -43.50591 172.5714 3.186662
     3 2017-09-12 AQY-AA011  0.1016667  5.925000 -43.50591 172.5714 5.342526
     4 2017-09-13 AQY-AA011  0.0000000  6.133333 -43.50591 172.5714 5.562104
     5 2017-09-14 AQY-AA011  1.2683333  8.301667 -43.50591 172.5714 1.528833
     6 2017-09-15 AQY-AA011 34.0100000 30.333333 -43.50591 172.5714 3.722929
     7 2017-09-16 AQY-AA011 20.4933333 25.458333 -43.50591 172.5714 3.089015
     8 2017-09-17 AQY-AA011  0.5683333  9.291667 -43.50591 172.5714 4.284960
     9 2017-09-18 AQY-AA011 13.6033333 13.055000 -43.50591 172.5714 5.313567
    10 2017-09-19 AQY-AA011 32.6366667 23.226667 -43.50591 172.5714 3.981459
    # ... with 22,352 more rows
   
### Proxy options.
Proxies, a signal from an independent site or number of sites to the observation with similar statistical properties, was introduced in papers [here](https://www.researchgate.net/publication/286479082_Data_Verification_Tools_for_Minimizing_Management_Costs_of_Dense_Air-Quality_Monitoring_Networks). In brief, ... Here, the provided code uses **only** sensor data - nearby regulatory data has been found to be a good proxy if the data is available.

#### sensornetworkmedianFUN

    networkmedian.example <- example %>%
      do(networkmedianFUN(., obs = 'O3', group = 'date', id = 'site')) %>%
      rename(date = group, site = id)
    as.tibble(networkmedian.example)
    # A tibble: 22,362 x 4
             date       obs network.proxy      site
           <dttm>     <dbl>         <dbl>     <chr>
     1 2017-09-10  5.261667      8.598333 AQY-AA011
     2 2017-09-10 14.033333      8.495000 AQY-AA012
     3 2017-09-10 17.275000      8.495000 AQY-AA013
     4 2017-09-10  4.703333      8.598333 AQY-AA014
     5 2017-09-10 18.873333      8.495000 AQY-AA015
     6 2017-09-10 17.845000      8.495000 AQY-AA016
     7 2017-09-10  9.105000      8.495000 AQY-AA017
     8 2017-09-10  2.040000      8.598333 AQY-AA018
     9 2017-09-10 22.291667      8.495000 AQY-AA019
    10 2017-09-10  2.143333      8.598333 AQY-AA020
    # ... with 22,352 more rows
    
#### nearestsiteFUN

    nearestsite.example = example %>% 
      do(nearestsiteFUN(., group = 'site', obs = 'O3', lat = 'lat', lon = 'lon')) %>% 
      rename(site = group)
    as.tibble(nearestsite.example)
    # A tibble: 22,362 x 4
             date      site        obs nearest.proxy
           <dttm>     <chr>      <dbl>         <dbl>
     1 2017-09-10 AQY-AA011  5.2616667      7.145000
     2 2017-09-11 AQY-AA011 19.7216667     22.026667
     3 2017-09-12 AQY-AA011  0.1016667      1.523333
     4 2017-09-13 AQY-AA011  0.0000000      1.215000
     5 2017-09-14 AQY-AA011  1.2683333      1.588333
     6 2017-09-15 AQY-AA011 34.0100000     33.211667
     7 2017-09-16 AQY-AA011 20.4933333     28.936667
     8 2017-09-17 AQY-AA011  0.5683333      1.951667
     9 2017-09-18 AQY-AA011 13.6033333     16.820000
    10 2017-09-19 AQY-AA011 32.6366667     35.360000
    # ... with 22,352 more rows

#### nearestNSitesFUN

    nearestn.example = example %>% 
      do(nearestNSitesFUN(., id = 'site', lat = 'lat', lon = 'lon', n = 3)) %>%
      rename(site = id)
    as.tibble(nearestn.example)
    # A tibble: 60 x 8
              id       lat      lon      n_id     n_lat    n_lon        dist order
     *     <chr>     <dbl>    <dbl>     <chr>     <dbl>    <dbl>       <dbl> <int>
     1 AQY-AA011 -43.50591 172.5714 AQY-AA026 -43.52188 172.5711 0.015977306     1
     2 AQY-AA012 -43.51475 172.6155 AQY-AA014 -43.53507 172.6120 0.020624126     1
     3 AQY-AA013 -43.48968 172.5997 AQY-AA016 -43.48750 172.5985 0.002480305     1
     4 AQY-AA014 -43.53507 172.6120 AQY-AA012 -43.51475 172.6155 0.020624126     1
     5 AQY-AA015 -43.48628 172.7124 AQY-AA027 -43.51794 172.7033 0.032942839     1
     6 AQY-AA016 -43.48750 172.5985 AQY-AA013 -43.48968 172.5997 0.002480305     1
     7 AQY-AA017 -43.50142 172.6626 AQY-AA023 -43.52244 172.6709 0.022604363     1
     8 AQY-AA018 -43.55881 172.6118 AQY-AA022 -43.57251 172.6219 0.017015747     1
     9 AQY-AA019 -43.55764 172.7117 AQY-AA020 -43.53906 172.6908 0.027910804     1
    10 AQY-AA020 -43.53906 172.6908 AQY-AA028 -43.54010 172.6724 0.018459489     1
    # ... with 50 more rows
  
#### sitesInNKMFUN
This proxy function also only identifies the locations within a specified radius. Following identification, one may choose to use the `networkmedianFUN` and group by `site` and `n_id`. A note on the parameter, `n`, which defines the size of the radius. It is calculated using degrees, which change relative to where on the globe the measurements are positioned. An online calculator can be useful in finding the correct `n` [here](https://www.nhc.noaa.gov/gccalc.shtml). The inputs that will change the distance will be the latitude options (as longitude remains constant). Here, our dataset is around 40 degrees south. Entering 40.95 and 41 into the latitude options (a difference in degrees of 0.05) is a 6 km distance, and so those locations within 6 km will be identified.

    nearestkm.example = example %>% 
      do(sitesInNKMFUN(., id = 'site', lat = 'lat', lon = 'lon', n = 0.05)) %>%
      rename(site = id)
    as.tibble(nearestkm.example)
    # A tibble: 140 x 4
            site       lat      lon      n_id
           <chr>     <dbl>    <dbl>     <chr>
     1 AQY-AA011 -43.50591 172.5714 AQY-AA012
     2 AQY-AA011 -43.50591 172.5714 AQY-AA013
     3 AQY-AA011 -43.50591 172.5714 AQY-AA014
     4 AQY-AA011 -43.50591 172.5714 AQY-AA016
     5 AQY-AA011 -43.50591 172.5714 AQY-AA026
     6 AQY-AA012 -43.51475 172.6155 AQY-AA011
     7 AQY-AA012 -43.51475 172.6155 AQY-AA013
     8 AQY-AA012 -43.51475 172.6155 AQY-AA014
     9 AQY-AA012 -43.51475 172.6155 AQY-AA016
    10 AQY-AA012 -43.51475 172.6155 AQY-AA017
    # ... with 130 more rows

#### ksFUN - A running two-sided Kolmogorov-Smirnov test.

    ks.example = network.example %>% 
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
    ggplot(ks.example, aes(date, mv_intercept)) + geom_line() + facet_wrap(~site)

#### mvFUN - A running custom Mean-Variance comparison test.

    mv.example = network.example %>% 
      group_by(site) %>% 
      do(mvFUN(., obs = 'obs', date = 'date', proxy = 'network.proxy')) %>% 
      spread(test, statistic)
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
    
#### loadMap - A script for creating and saving a HTML leaflet map.

    loadMap(example, set.date = '2017-10-15', obs = 'O3', time.zone = 'Pacific/Auckland', id = 'site', lat = 'lat', lon = 'lon', dest = '')

Map [here](http://christchurch_pm.droppages.com/latest+map)

#### shiny_display - A script for creating an interactive shiny app.

    shiny_display(example, cols.to.display = 'O3')
