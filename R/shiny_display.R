#' A function to plot and map air quality data.

#' This function ... Written by Dr. Kyle Alberti (kyle.alberti@aeroqual.com).
#' @param x A csv file of the data. The data are set to wide format.
#' @param time.zone A string identifying the time zone for the data. Here, the default is `Pacific/Auckland`.
#' @param data.frequency A string identifying the averaging time of the data. Here, the default is `60 min`.
#' @param dashboard.title A string identifying the title that will appear on the dashboard. Here, `AQY deployment`.
#' @param cols.to.display A list of strings in x that are to be plotted. Here, the default is `c('O3","PM2.5","NO2")`.
#' @param site.col The name of the column in x that identifies the units or the sites. Here, the default is `Serial`.
#' @param lat.col Latitude column.
#' @param long.col Longitude column.
#' @export
#' @examples
#' appFUN()


appFUN <- function(x, time.zone = 'Pacific/Auckland', data.frequency = '60 min', dashboard.title = 'AQY deployment', cols.to.display = c("O3","PM2.5","NO2"), site.col = 'Serial', lat.col = 'LAT', long.col = 'LONG'){

  # install and load required packages
	list.of.packages <- c("shinyjs","shiny","xts","leaflet","raster","gstat","shinydashboard","sp","dygraphs","dplyr");
	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
	if(length(new.packages)) install.packages(new.packages);
	lapply(list.of.packages, library, character.only = T);
  require(rgdal);

  x$date <- as.POSIXct(x$date, format = "%Y-%m-%d %H:%M", tz = time.zone)
  date <- seq(x$date[1], x$date[nrow(x)], by = data.frequency)
  x$Serial <- x[, site.col]
  x$LAT <- x[, lat.col]
  x$LONG <- x[, long.col]
  serialList <- unique(x$Serial)
  lat.view = mean(x$LAT, na.rm = T)
  long.view = mean(x$LONG, na.rm = T)

  ui <- dashboardPage(
    dashboardHeader(title = dashboard.title,
                    titleWidth = 450),
    dashboardSidebar(sidebarMenu(id = "tab",
                                 menuItem("Time Series", tabName = "ts", icon = icon("line-chart", lib = "font-awesome")),
                                 menuItem("Map", icon = icon("map"), tabName = "map")),
                     conditionalPanel(condition = "input.tab == 'map'",
                     div(style = "display:inline-block;vertical-align:top;margin-bottom: -20px;width: 220px;margin-top: -10px;",
                     sliderInput("time", "Display Date", min(date), max(date), value = min(date), step = (60*60), animate = T, timezone = "+1300")),
                     div(style = "display:inline-block;vertical-align:top;margin-left: 50px;margin-top: -10px;",
                         actionButton('backButton', label = icon("step-backward", lib = "font-awesome"))),
                     div(style = "display:inline-block;vertical-align:top;margin-top: -10px;",
                         actionButton('forwardButton', label = icon("step-forward", lib = "font-awesome"))),
                     div(style = "display:inline-block;vertical-align:top; width: 155px; margin-right:-20px; margin-top: -10px;",
                         textInput("timein", "Date Input", min(x$date))),
                     div(style = "display:inline-block;vertical-align:top; width: 30px; margin-top: 21px;",
                         actionButton('goButton', 'Submit')),
                     div(style = "display:inline-block;vertical-align:top; width: 100px; margin-left: 10px; margin-top: -10px;",
                         selectInput("pol", "Pollutant 1", cols.to.display, selected = cols.to.display[1])),
                     div(style = "display:inline-block;vertical-align:top; width: 100px;margin-top: -10px;",
                         selectInput("pol2", "Pollutant 2", cols.to.display, selected = cols.to.display[length(cols.to.display)])),
                     div(style="display:inline-block;vertical-align:top; width: 100px; margin-left: 10px;margin-top: -10px;",
                         sliderInput("max1", "Pol1 Max", 0, ceiling(max(x[, cols.to.display[1]], na.rm = T)), value = ceiling(max(x[, cols.to.display[1]], na.rm = T)), step = ceiling(sd(x[, cols.to.display[1]], na.rm = T)/5 * 5))),
                     div(style = "display:inline-block;vertical-align:top; width: 100px;margin-top: -10px;",
                         sliderInput("max2", "Pol2 Max", 0, ceiling(max(x[, cols.to.display[length(cols.to.display)]], na.rm = T)), value = ceiling(max(x[, cols.to.display[length(cols.to.display)]], na.rm = T)), step = ceiling(sd(x[, cols.to.display[length(cols.to.display)]], na.rm = T)/5 * 5))),
                     div(style = "display:inline-block;vertical-align:top; width: 200px; margin-top: -10px;",
                         sliderInput("htopc", "Heatmap Opacity", 0, 100, value = 50, step = 1))),
                     conditionalPanel(condition = "input.tab == 'ts'",
                                    selectInput("Serial1","Instrument 1", serialList, selected = serialList[1]),
                                    selectInput("Serial2","Instrument 2", serialList, selected = serialList[2]), tags$style(".checkbox { /* checkbox is a div class*/
                 margin-bottom: 3px; /*set the margin, so boxes don't overlap*/
                 }"),
                                    checkboxGroupInput(inputId = "pollutant",
                                                       label = "Pollutants",
                                                       choices = cols.to.display,
                                                       selected = cols.to.display[1]))),

  dashboardBody(
    tabItems(tabItem(tabName = "map",
                     fluidRow(tags$head(tags$style(HTML('.skin-blue .main-header .logo {
                                          background-color: #3c8dbc;}
                                          .skin-blue .main-header .logo:hover {
                                          background-color: #3c8dbc;}'))),
                              tags$style(type = "text/css", "#myMap {height: calc(98vh - 80px) !important;}"),
                              tags$style(type = "text/css", " .leaflet-fade-anim .leaflet-tile, .leaflet-fade-anim .leaflet-popup, .leaflet-fade-anim .leaflet-marker-icon, .leaflet-fade-anim .leaflet-pane, .leaflet-fade-anim .leaflet-tile-loaded {opacity: 1 !important;}"), #.leaflet-container, .leaflet-overlay-pane
                              tags$style(type = "text/css", " .leaflet-fade-anim .leaflet-tile, .leaflet-fade-anim .leaflet-popup, .leaflet-fade-anim .leaflet-marker-icon, .leaflet-fade-anim .leaflet-pane, .leaflet-fade-anim .leaflet-tile-loaded {transition: none !important;}"),
                              tags$style(type = "text/css","  .imageOverlay {opacity: 1.0 !important; }"),
                              tags$style(type = "text/css", " .recalculating { opacity: 1.0 !important; }"),
                              box(leafletOutput("myMap")),
                              tags$style(type = "text/css", "#myMap2 {height: calc(98vh - 80px) !important;}"),
                              tags$style(type = "text/css", " .leaflet-fade-anim .leaflet-tile, .leaflet-fade-anim .leaflet-popup, .leaflet-fade-anim .leaflet-marker-icon, .leaflet-fade-anim .leaflet-pane, .leaflet-container .leaflet-overlay-pane {opacity: 1 !important;}"),
                              tags$style(type = "text/css", " .leaflet-fade-anim .leaflet-tile, .leaflet-fade-anim .leaflet-popup, .leaflet-fade-anim .leaflet-marker-icon, .leaflet-fade-anim .leaflet-pane, .leaflet-container .leaflet-overlay-pane {transition: none !important;}"),
                              tags$style(type = "text/css","  .imageOverlay {opacity: 1.0 !important; }"),
                              tags$style(type = "text/css", " .recalculating { opacity: 1.0 !important; }"),
                              box(leafletOutput("myMap2")))),
             tabItem(tabName = "ts",
                     tags$style(type = "text/css", "#plot1 {height: calc(50vh - 40px) !important;}"),
                     tags$style(type = "text/css", "#plot2 {height: calc(50vh - 40px) !important;}"),
                     dygraphOutput("plot1"),
                     dygraphOutput("plot2")))),
  useShinyjs()
  )

server <- function(input, output, session) {
  pal <- reactive({
    colorNumeric(palette = c("navy", "blue", "cyan","yellow", "red", "red4"), domain = c(0, input$max1), na.color = '#00000000')
  })
  pal2 <- reactive({
    colorNumeric(palette = c("navy", "blue", "cyan","yellow", "red", "red4"), domain = c(0, input$max2), na.color = '#00000000')
  })
  points <- reactive({
    pol <- x[x$date == .POSIXct(input$time, tz = time.zone), c("date", "Serial", input$pol, "LAT", "LONG")]
    colnames(pol)[3] <- "pol"
    pol[is.na(pol$pol), "pol"] <- -1000
    pol[pol$pol > input$max1, "pol"] <- input$max1
    return(pol)
  })
  points2 <- reactive({
    pol2 <- x[x$date == .POSIXct(input$time, tz = time.zone), c("date", "Serial", input$pol2, "LAT", "LONG")]
    colnames(pol2)[3] <- "pol2"
    pol2[is.na(pol2$pol2), "pol2"] <- -1000
    pol2[pol2$pol2 > input$max2, "pol2"] <- input$max2
    return(pol2)
  })
  r <- reactive({
    if(is.null(input$myMap_bounds)){
      #west, east, north, south, div1, div2
      bound <- c(min(x$LONG, na.rm = T), max(x$LONG, na.rm = T), max(x$LAT, na.rm = T), min(x$LAT, na.rm = T), 0.005, 0.005)
    } else {
      #west, east, north, south, div1, div2
      bound <- c(input$myMap_bounds$west, input$myMap_bounds$east,
                 input$myMap_bounds$north, input$myMap_bounds$south,
                 (input$myMap_bounds$east - input$myMap_bounds$west) / 100,
                 (input$myMap_bounds$north - input$myMap_bounds$south) / 100)
    }
    x_Mapspace <- x[x$date == .POSIXct(input$time, tz = time.zone), c("date", "Serial", input$pol, "LAT", "LONG")]
    colnames(x_Mapspace)[3] <- "pol"
    x_Mapspace <- x_Mapspace[!is.na(x_Mapspace$date) & !is.na(x_Mapspace$LAT) & !is.na(x_Mapspace$LONG),]
    x_Mapspace[x_Mapspace$pol > input$max1 & !is.na(x_Mapspace$pol), "pol"] <- input$max1
    coordinates(x_Mapspace) = ~LONG+LAT

    # divide interpolation area into grid
    grd <- expand.grid(x = seq(from = bound[1], to = bound[2], by = bound[5]), y = seq(from = bound[4], to = bound[3], by = bound[6])) # expand points to grid
    coordinates(grd) <- ~x + y
    gridded(grd) <- TRUE
    x_Mapspace[is.na(x_Mapspace$pol), 'pol'] <- -1000

    idw.pol <- idw(formula = pol ~ 1, locations = x_Mapspace, newdata = grd)  # apply idw model for the data
    crs(idw.pol) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ras <- raster(idw.pol)
    return(ras)
  })

  r2 <- reactive({
    if(is.null(input$myMap_bounds)){
      #west, east, north, south, div1, div2
      bound <- c(min(x$LONG, na.rm = T), max(x$LONG, na.rm = T), max(x$LAT, na.rm = T), min(x$LAT, na.rm = T), 0.005, 0.005)
    } else {
      #west, east, north, south, div1, div2
      bound <- c(input$myMap_bounds$west, input$myMap_bounds$east,
                 input$myMap_bounds$north, input$myMap_bounds$south,
                 (input$myMap_bounds$east - input$myMap_bounds$west) / 100,
                 (input$myMap_bounds$north - input$myMap_bounds$south) / 100)
    }

    x_Mapspace2 <- x[x$date == .POSIXct(input$time, tz = time.zone), c("date", "Serial", input$pol2, "LAT", "LONG")]
    colnames(x_Mapspace2)[3] <- "pol2"
    x_Mapspace2 <- x_Mapspace2[!is.na(x_Mapspace2$date) & !is.na(x_Mapspace2$LAT) & !is.na(x_Mapspace2$LONG),]
    x_Mapspace2[x_Mapspace2$pol2 > input$max2 & !is.na(x_Mapspace2$pol2), "pol2"] <- input$max2
    coordinates(x_Mapspace2) = ~LONG+LAT

    # divide interpolation area into grid
    grd <- expand.grid(x = seq(from = bound[1], to = bound[2], by = bound[5]), y = seq(from = bound[4], to = bound[3], by = bound[6]))  # expand points to grid
    coordinates(grd) <- ~x + y
    gridded(grd) <- TRUE
    x_Mapspace2[is.na(x_Mapspace2$pol2), 'pol2'] <- -1000

    idw.pol2 <- idw(formula = pol2 ~ 1, locations = x_Mapspace2, newdata = grd)  # apply idw model for the data
    crs(idw.pol2) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ras2<-raster(idw.pol2)
    return(ras2)
  })

  coo <- reactive({
    if(is.null(input$myMap_bounds)){
      return(c(34.02, 34.02, -117.35, -117.35, 11))
    } else {
      return(c(input$myMap_bounds$north, input$myMap_bounds$south, input$myMap_bounds$east, input$myMap_bounds$west, input$myMap_zoom))
    }
  })

  output$myMap <- renderLeaflet({
    Sys.sleep(1)
    leaflet() %>%
      addTiles(group = "OSM") %>%
      addProviderTiles('Stamen.Terrain', group = "Terrain") %>%
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles('CartoDB.DarkMatter', group = "Dark") %>%
      setView(lat = lat.view, lng = long.view, zoom = 12) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial = F)) %>%
      addLayersControl(baseGroups = c("Terrain", "OSM", "Satellite", "Dark"),
                       overlayGroups = c("Heatmap"),
                       options = layersControlOptions(collapsed = T, autoZIndex = F)) %>%
      htmlwidgets::onRender("
                            function(el, x) {
                            this.on('baselayerchange', function(e) {
                            e.layer.bringToBack();
                            })
                            }
                            ")

  })

  output$myMap2 <- renderLeaflet({
    Sys.sleep(1)
    leaflet() %>%
      addTiles(group="OSM") %>%
      addProviderTiles('Stamen.Terrain', group = "Terrain") %>%
      addProviderTiles('Esri.WorldImagery', group = "Satellite") %>%
      addProviderTiles('CartoDB.DarkMatter', group = "Dark") %>%
      setView(lat = lat.view, lng = long.view, zoom = 12) %>%
      addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial = F)) %>%
      addLayersControl(baseGroups = c("Terrain", "OSM", "Satellite", "Dark"),
                       overlayGroups = c("Heatmap"),
                       options = layersControlOptions(collapsed = T, autoZIndex = F)) %>%
      htmlwidgets::onRender("
                            function(el, x) {
                            this.on('baselayerchange', function(e) {
                            e.layer.bringToBack();
                            })
                            }
                            ")
  })

  observe({

    leafletProxy("myMap") %>%
      clearPopups() %>%
      addRasterImage(r(), opacity = input$htopc/100, colors = pal(), layerId = "Heatmap", group = "Heatmap") %>%
      removeMarker(layerId = setdiff(serialList, unique(points()[,'Serial']))) %>%
      addCircleMarkers(data = points(), ~LONG, ~LAT, color = ~pal()(pol), radius = 3, fill = T, popup =~as.character(pol), fillOpacity = 1, label =~Serial, layerId =~Serial, opacity = 1) %>%
      addLegend(pal = pal(), values = seq(0, input$max1, 10), layerId = "legend", title = input$pol, opacity = 0.8)

    leafletProxy("myMap2") %>%
      setView(lng = (coo()[3]+coo()[4])/2, lat = (coo()[1]+coo()[2])/2, zoom = coo()[5]) %>%
      clearPopups() %>%
      addRasterImage(r2(), opacity = input$htopc/100, colors = pal2(), layerId = "Heatmap2", group = "Heatmap2") %>%
      removeMarker(layerId = setdiff(serialList, unique(points()[,'Serial']))) %>%
      addCircleMarkers(data = points2(), ~LONG, ~LAT, color = ~pal2()(pol2), radius = 3, fill = T, popup =~as.character(pol2), fillOpacity = 1, label =~Serial, layerId =~Serial, opacity = 1) %>%
      addLegend(pal = pal2(), values = seq(0, input$max2, 10), layerId = "legend", title = input$pol2, opacity = 0.8)
  })
  observeEvent(input$goButton,{
    updateSliderInput(session, "time", value = as.POSIXct(input$timein, format = "%Y-%m-%d %H:%M", tz = time.zone))
  })
  observeEvent(input$forwardButton,{
    tval <- match(input$time, date) + 1
    updateSliderInput(session, "time", value = as.POSIXct(date[tval], format = "%Y-%m-%d %H:%M", tz = time.zone))
  })
  observeEvent(input$backButton,{
    tval<- match(input$time, date) - 1
    updateSliderInput(session, "time", value = as.POSIXct(date[tval], format = "%Y-%m-%d %H:%M", tz = time.zone))
  })

  active.data1 <- reactive({
    active.data1 <- x %>%
      filter(Serial == input$Serial1) %>%
      filter(!is.na(date)) %>%
      dplyr::select(date, input$pollutant)
    active.data1 <- xts(active.data1[,-c(1)], order.by = active.data1$date)
    return(active.data1)
  })

  active.data2 <- reactive({
    active.data2 <- x %>%
      filter(Serial == input$Serial2) %>%
      filter(!is.na(date)) %>%
      dplyr::select(date,input$pollutant)
    active.data2 <- xts(active.data2[,-c(1)], order.by = active.data2$date)
    return(active.data2)
  })

  output$plot1 <- renderDygraph({
    if("DP" %in% input$pollutant){
      dygraph(active.data1(), group = "aq",main = input$Serial1) %>%
        dyRangeSelector() %>%
        dyAxis(name = "y", label = "Concentration") %>%
        dyAxis(name = "y2", label = "Dew Point", valueRange = c(-15, 15)) %>%
        dySeries("DP", axis = ('y2'))%>%
        dyOptions(drawAxesAtZer = T, colors = RColorBrewer::brewer.pal(length(input$pollutant), "Dark2"))
    } else {
      dygraph(active.data1(), group = "aq", main = input$Serial1) %>%
        dyRangeSelector() %>%
        dyAxis(name = "y", label = "Concentration") %>%
        dyOptions(drawAxesAtZer = T, colors = RColorBrewer::brewer.pal(length(input$pollutant), "Dark2"))
    }
  })

  output$plot2 <- renderDygraph({
    if("DP" %in% input$pollutant){
      dygraph(active.data2(), group = "aq", main = input$Serial2) %>%
        dyRangeSelector() %>%
        dyAxis(name = "y", label = "Concentration") %>%
        dyAxis(name = "y2", label = "Dew Point", valueRange = c(-15, 15)) %>%
        dySeries("DP", axis = ('y2')) %>%
        dyOptions(drawAxesAtZer = T, colors = RColorBrewer::brewer.pal(length(input$pollutant), "Dark2"))
    } else {
      dygraph(active.data2(), group = "aq", main = input$Serial2) %>%
        dyRangeSelector() %>%
        dyAxis(name = "y", label = "Concentration") %>%
        dyOptions(drawAxesAtZero = T, colors = RColorBrewer::brewer.pal(length(input$pollutant), "Dark2"))
    }
  })

}
shinyApp(ui, server)
}
