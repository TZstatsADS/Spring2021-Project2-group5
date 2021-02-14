
# References
# https://github.com/gadenbuie/tweet-conf-dash
source("global-v2.R")

if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(RSocrata)) devtools::install_github("Chicago/RSocrata")
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(googleway)) install.packages("googleway", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if(!require(highcharter)) install.packages("highcharter")

borough_vars <- c("ALL BOROUGHS" = "",
                  "BRONX" = "Bronx",
                  "BROOKLYN" = "Brooklyn",
                  "MANHATTAN" = "Manhattan",
                  "QUEENS" = "Queens", 
                  "STATEN ISLAND" = "Staten Island"
)
freemeals_icons <- awesomeIcons( #https://www.rdocumentation.org/packages/leaflet/versions/2.0.3/topics/awesomeIcons
    #icon library fontawesome: https://fontawesome.com/icons?d=gallery&q=food&m=free
    markerColor = "orange",
    text = fa("pizza-slice")
)
adultexerciseequip_icons <- awesomeIcons(
  markerColor = "blue",
  text = fa("dumbbell")
)
playgrounds_icons <- awesomeIcons(
    markerColor = "green",
    text = fa("child")
)
dogruns_icons <- awesomeIcons(
    markerColor = "yellow",
    text = fa("dog")
)
skateparks_icons <- awesomeIcons(
  markerColor = "navy",
  text = fa("skating")
)
atheleticfac_icons <- awesomeIcons(
  markerColor = "red",
  text = fa("futbol")
)


my_icons <- makeIcon(
    iconUrl = "https://image.flaticon.com/icons/png/512/45/45332.png",
    iconWidth = 508, iconHeight = 508,
    iconAnchorX = 250, iconAnchorY = 250
)
#https://image.flaticon.com/icons/png/512/501/501649.png

#===============================================Shiny UI=========================================================
ui <- fluidPage(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;" class="active" href="#">COVID-19 In New York City</a>'), id="nav",
               windowTitle = "COVID-19 in New York City",
               
               # tab 1 (section 1)
               tabPanel('Introduction', icon = icon("home"),
                        div(class='coverDiv',
                            #tags$head(includeCSS('styles.css')), # custom styles
                            titlePanel("Introduction to Covid-19 in New York City"),
                            fluidRow(
                                # Value Boxes
                                valueBoxOutput(outputId = "TCasesBox",width = 3),
                                valueBoxOutput(outputId = "NewCasesBox",width = 3),
                                valueBoxOutput(outputId = "HospitalizedBox",width = 3),
                                valueBoxOutput(outputId = "TDeathsBox",width = 3),
                                valueBoxOutput(outputId = "NewDeathsBox",width = 3),
                                valueBoxOutput(outputId = "FatalityBox",width = 3)),
                            fluidRow(
                                span(tags$i(h6(paste0("Last Update: ", nyc_latest$date_of_interest[1]))))
                            ),
                            sidebarLayout(position = "left",
                                          sidebarPanel(
                                              h2("NYC Neighborhoods"),
                                              
                                              span(tags$i(h6("Reported cases are subject to significant variation in reporting organizations.")), style="color:#045a8d"),
                                              
                                              # select from drop-down lists
                                              selectInput("select_borough", 
                                                          label = "Borough", 
                                                          choices = borough_vars, 
                                                          selected = borough_vars[1]),
                                              h4("Select the topic(s) to see trends over time:", align = "left"),
                                              checkboxInput("casesummary", label = "Cases", value = TRUE),
                                              checkboxInput("deathsummary", label = "Deaths", value = FALSE),
                                              checkboxInput("hospsummary", label = "Hospitalization", value = FALSE)
                                              # select from slider
                                              #dateRangeInput("plot_date", "Select date range:",
                                              #               start  = as.Date.character(data_by_day$date_of_interest[1],format="%m/%d/%Y"),
                                              #               end    = as.Date.character(nyc_latest$date_of_interest[1],format="%m/%d/%Y"),
                                              #               min    = as.Date.character(data_by_day$date_of_interest[1],format="%m/%d/%Y"),
                                              #               max    = as.Date.character(nyc_latest$date_of_interest[1],format="%m/%d/%Y"),
                                              #               format = "mm/dd/yyyy",
                                              #               separator = " to ")
                                             
                                          ),
                                          mainPanel(
                                              div(tags$style(".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }"),
                                                  fluidRow(
                                                      highchartOutput("ts1",
                                                          width = "100%",
                                                          height = "600px"
                                                      )
                                                  )
                                              ))
                            )
                        )
               ),
               # tab 2 (map)
               tabPanel("Resources mapper", icon = icon("map-marker-alt"),
                        div(class="outer",
                            #tags$head(includeCSS("styles.css")),
                            #map
                            leafletOutput("mymap", width="100%", height=550),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 25, width = 300, fixed=FALSE,
                                          draggable = TRUE, height = "auto",
                                          
                                          h2("NYC Neighborhoods"),
                                          
                                          span(tags$i(h6("Reported cases are subject to significant variation in reporting organizations.")), style="color:#045a8d"),
                                          
                                          # select from drop-down lists
                                          selectInput("plot_borough", 
                                                      label = "Borough", 
                                                      choices = borough_vars, 
                                                      selected = borough_vars[2]),
                                          h3("Select covid-19 related resources:", align="left"),
                                          helpText(""),
                                          h4("FOODS", align = "left"),
                                          checkboxInput("freemeals", "Free Meals"),
                                          h4("HEALTH", align = "left"),
                                          checkboxInput("testing", label = "Covid Testing", value = FALSE),
                                          checkboxInput("hospitals", label = "Hospitals", value = FALSE),
                                           
                                          h4("LEISURE", align = "left"),
                                          checkboxInput("atheleticfac", label = "Atheletic Facilities", value = FALSE),
                                          checkboxInput("dogruns", label = "Dog Runs", value = FALSE),
                                          checkboxInput("adultexerciseequip", label = "Exercise Equipments", value = FALSE),
                                          checkboxInput("playgrounds", label = "Playgrounds", value = FALSE),
                                          checkboxInput("skateparks", label = "Skate Parks", value = FALSE),
                                          style = "opacity: 0.80")
                            )
                        ),
               # tab 3
               tabPanel('Mobility', icon = icon("luggage-cart")),
               # tab 4
               tabPanel('About', icon = icon("info"))
               ))

#===============================================Shiny SERVER=====================================================
server <- function(input, output,session) {
    #=========================
    #======== tab 1 ==========
    #=========================
    output$TCasesBox = renderValueBox({
        valueBox(0,
                 subtitle = "Cases in Total",
                 color = "purple",
                 icon = icon("lungs-virus", lib = "font-awesome"))
    })
    output$NewCasesBox = renderValueBox({
        valueBox(nyc_latest$CASE_COUNT[1],
                 subtitle = "New Cases",
                 color = "orange",
                 icon = icon("stethoscope", lib = "font-awesome"))
    })
    output$HospitalizedBox = renderValueBox({
        valueBox(nyc_latest$HOSPITALIZED_COUNT[1],
                 subtitle = "Hospitalized",
                 color = "green",
                 icon = icon("hospital", lib = "font-awesome"))
    })
    output$TDeathsBox = renderValueBox({
        valueBox(value = 0,
                 subtitle = "Deaths in Total",
                 color = "maroon",
                 icon = icon("virus-slash", lib = "font-awesome"))
    })
    output$NewDeathsBox = renderValueBox({
        valueBox(nyc_latest$DEATH_COUNT[1],
                 subtitle = "New Deaths",
                 color = "teal",
                 icon = icon("frown", lib = "font-awesome"))
    })
    output$FatalityBox = renderValueBox({
        valueBox(0,
                 subtitle = "7-Day Average/Fatality (%)",
                 color = "fuchsia",
                 icon = icon("calculator", lib = "font-awesome"))
    })
    
    # ================================ line plot =========================================================
    data_by_day$date_of_interest <-
        as.Date(data_by_day$date_of_interest,format="%m/%d/%Y")
    
    # select subset by borough
    SubsetBorough <- function(ts_borough){
        if (ts_borough == ""){
            data <- data_by_day[,c(1:11)]
        } else if (ts_borough == "Bronx"){
            data <- data_by_day[,c(1,12:21)]
        } else if (ts_borough == "Brooklyn"){
            data <- data_by_day[,c(1,22:31)]
        } else if (ts_borough == "Manhattan"){
            data <- data_by_day[,c(1,32:41)]
        } else if (ts_borough == "Queens"){
            data <- data_by_day[,c(1,42:51)]
        } else if (ts_borough == "Staten Island"){
            data <- data_by_day[,c(1,52:61)]
        }
        return(data)
    }
    
    # Interactive time series plot with highcharter
    observe({
        # Render highchart outcome
        output$ts1 <- renderHighchart({
                # subset data and make it tidy
                data_by_day_sub <- SubsetBorough(input$select_borough) %>%
                    tidyr::pivot_longer(
                        cols = -date_of_interest, 
                        names_to = "line_var", 
                        values_to = "value") %>% #names_prefix = "fight_" 
                    dplyr::mutate(line_var = as.factor(line_var))
                # custom title of plot
                ts_title <- ifelse(input$select_borough == "", "New York City", input$select_borough)

                # ---------------filter variables--------------------------------------------------
                # If no selection, generate a dataframe of zeros to avoid errors
                if (!input$casesummary & !input$deathsummary & !input$hospsummary){
                    data_by_day_filter <- data_by_day_sub
                    data_by_day_filter$value <- 0
                } else {
                    if (input$casesummary) {
                        df1 <- data_by_day_sub %>%
                            dplyr::filter(stringr::str_detect(line_var,"CASE_COUNT"))
                    } else {df1 <- data.frame()}
                    if (input$deathsummary) {
                        df2 <- data_by_day_sub %>%
                            dplyr::filter(stringr::str_detect(line_var,"DEATH_COUNT"))
                    } else {df2 <- data.frame()}
                    if (input$hospsummary) {
                        df3 <- data_by_day_sub %>%
                            dplyr::filter(stringr::str_detect(line_var,"HOSP"))
                    } else {df3 <- data.frame()}
                    # aggregated dataframe for plot
                    data_by_day_filter = rbind(df1, df2, df3)
                }
                
                # ------------------------- plot -------------------------------------------------
                hchart(data_by_day_filter, "line",
                           hcaes(x = date_of_interest, y = value, group = line_var)) %>%
                    hc_chart(zoomType = "x") %>%
                    #hc_colors(c("#0015BC", "#FF0000")) %>% need one color for each variable
                    hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
                    hc_xAxis(title = list(text = "Date"),
                             labels = list(format = '{value:%b %d %y}')) %>%
                    hc_yAxis(title = list(text = "Count"),
                             tickInterval = 400,
                             max = max(data_by_day_filter$value)) %>%
                    hc_title(text = paste0("<b>Covid-19 Summary for ",ts_title, ", NY by Date</b>")) %>%
                    hc_subtitle(text = "Click and drag in the plot area to zoom in on a time span") %>%
                    hc_plotOptions(area = list(lineWidth = 0.5)) %>% 
                    hc_exporting(enabled = TRUE)
                }, quoted = TRUE)
        })

    #=========================
    #======== tab 2 ==========
    #=========================
    # NYC basemap
    output$mymap <- renderLeaflet({ 
        leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
            htmlwidgets::onRender(
                "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
            ) %>%
            addTiles() %>%
            setView(lng = -73.935242, lat = 40.730610, zoom = 11)
    })
    
    # --------------------------- hospitals & testing (google map) -------------------------------
    # read the data
    df <- readRDS(file="../data/processed_data.Rda")
    
    # create reactive components
    df_react_hospitals <- reactive({ 
      df1 = df[df$category == 'hospital', ]
      data = df1[df1$borough == input$plot_borough, ]
    })
    
    df_react_test_sites <- reactive({ 
      df1 = df[df$category == 'covid testing site', ]
      data = df1[df1$borough == input$plot_borough, ]
    })
    
    # -------------------------- park closure status---------------------------------------------
    FilterPoints <- function(ds) {
        result <- ds %>% dplyr::filter(borough == input$plot_borough)
        return(result)
    }
    FreeMeals_reactive <- reactive({FilterPoints(FreeMeals)})
    AdultExerciseEquip_reactive <- reactive({FilterPoints(AdultExerciseEquip)})
    Playgrounds_reactive <- reactive({FilterPoints(Playgrounds)})
    #Playgrounds_reactive <- reactive({
    #   Playgrounds %>% dplyr::filter(borough == input$plot_borough)
    #})
    
    FilterPolygons <- function(geo) {
        result <- subset(geo, borough == input$plot_borough)
        return(result)
    }
    AtheleticFac_reactive <- reactive({FilterPolygons(AtheleticFac_geo)})
    DogRuns_reactive <- reactive({FilterPolygons(DogRuns_geo)})
    SkateParks_reactive <- reactive({FilterPolygons(SkateParks_geo)})
    
    # observer
    observe({
      # --------------------------- hospitals & testing (google map) -------------------------------
      proxy <- leafletProxy("mymap", data = df)
      
      # format (closed, no data, open)
      palette_testing = c("#9c5151", "#ff8f8f", "red")
      palette_hospital = c("#4d688c", "#69bbff", "blue")
      
      # color pallets for open / close
      color1 <- colorFactor(palette=palette_testing, df$open_now)
      color2 <- colorFactor(palette=palette_hospital, df$open_now)
      
      proxy %>% clearControls()
      
      # clear the map
      leafletProxy("mymap", data = df) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addTiles() %>%
        fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
      
      # output data related to hospitals
      if (input$hospitals){
        leafletProxy("mymap", data = df_react_hospitals()) %>%
          clearShapes() %>%
          addTiles() %>% 
          addCircleMarkers(
            ~lon, 
            ~lat, 
            radius=15, 
            color = ~color2(open_now),
            label = paste(df_react_hospitals()$formatted_address, ', ', df_react_hospitals()$name),
            popup = paste(
              "<b>Name:</b>", df_react_hospitals()$name, "<br>",
              "<b>Address:</b>", df_react_hospitals()$formatted_address, "<br>",
              "<b>Rating:</b>", df_react_hospitals()$rating, "<br>",
              "<b>Open now:</b>", df_react_hospitals()$open_now, "<br>")) %>%
          addLegend("bottomright",
                    pal = color2,
                    values = df$open_now,
                    title = "Status",
                    opacity = 1)
      }
      # output data related to covid testing sites
      if (input$testing){
        leafletProxy("mymap", data = df_react_test_sites()) %>%
          clearShapes() %>%
          addTiles() %>% 
          addCircleMarkers(~lon, ~lat, radius=10,
            color = ~color1(open_now),
            label = paste(df_react_test_sites()$formatted_address, ', ', df_react_test_sites()$name),
            popup = paste(
              "<b>Name:</b>", df_react_test_sites()$name, "<br>",
              "<b>Address:</b>", df_react_test_sites()$formatted_address, "<br>",
              "<b>Rating:</b>", df_react_test_sites()$rating, "<br>",
              "<b>Open now:</b>", df_react_test_sites()$open_now, "<br>")) %>%
          addLegend("bottomright",
                    pal = color1,
                    values = df$open_now,
                    title = "Status",
                    opacity = 1)
      }
      # -------------------------- park closure status---------------------------------------------
      if (input$freemeals) {
        leafletProxy("mymap", data = FreeMeals_reactive()) %>%
            addAwesomeMarkers(~longitude, ~latitude, 
                              icon=freemeals_icons, label=~schoolname, popup=~content)
      }
      if (input$adultexerciseequip) {
          leafletProxy("mymap", data = AdultExerciseEquip_reactive()) %>%
            addAwesomeMarkers(~longitude, ~latitude, 
                              icon=adultexerciseequip_icons, label=~SiteName, popup=~content)
      }
      if (input$playgrounds) {
          leafletProxy("mymap", data = Playgrounds_reactive()) %>%
            addAwesomeMarkers(~longitude, ~latitude, 
                              icon=playgrounds_icons, label=~name, popup=~content)
      }
      if (input$dogruns) {
          leafletProxy("mymap",data=DogRuns_reactive()) %>% 
            addPolygons(color="yellow",stroke = TRUE, weight = 8,
                        label = ~DogRuns_reactive()@data$content,
                          #paste(
                          #DogRuns_geo@data$propertyname, ", ", DogRuns_geo@data$borough, ", NY<br>",
                          #"<b>Status:</b>", DogRuns_geo@data$status, "<br>"),
                        highlight = highlightOptions(weight = 15,
                                                     color = "blue",
                                                     bringToFront = TRUE))# %>
      }
      if (input$skateparks) {
          leafletProxy("mymap",data=SkateParks_reactive()) %>% 
            addPolygons(color="navy",stroke = TRUE, weight = 8,
                        label = ~SkateParks_reactive()@data$content,
                        #paste(
                        #DogRuns_geo@data$propertyname, ", ", DogRuns_geo@data$borough, ", NY<br>",
                        #"<b>Status:</b>", DogRuns_geo@data$status, "<br>"),#
                        highlight = highlightOptions(weight = 15,
                                                     color = "blue",
                                                     bringToFront = TRUE))# %>%
      }
      if (input$atheleticfac) {
        leafletProxy("mymap",data=AtheleticFac_reactive()) %>% 
          addPolygons(color="red",stroke = TRUE, weight = 8,
                      label = ~AtheleticFac_reactive()@data$content,
                      #paste(
                      #DogRuns_geo@data$propertyname, ", ", DogRuns_geo@data$borough, ", NY<br>",
                      #"<b>Status:</b>", DogRuns_geo@data$status, "<br>"),#
                      highlight = highlightOptions(weight = 15,
                                                   color = "blue",
                                                   bringToFront = TRUE))# %>%
          #addPopups(-122.327298, 47.597131, content,
          #          options = popupOptions(closeButton = TRUE)
          #)
          #clusterOptions = markerClusterOptions(clusterClass = "cluster2",
          #                                      minimumClusterSize = 10))
          
    #=========================
    #======== tab 3 ==========
    #=========================
          
      }
    })
}




shiny::shinyApp(ui, server)
