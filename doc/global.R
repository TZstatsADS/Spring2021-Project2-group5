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


# ==================================================== Load data ==========================================================
#nyc health data
data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
data_by_day <- data_by_day %>% rename_all(funs(str_replace_all(., "_7DAY_AVG", "(7DAY_AVG)")))
nyc_latest <- data_by_day %>% tail(1)
nyc_yesterday<- data_by_day[nrow(data_by_day)-1,]

#JHU covid19 data in US
#latest<- format(Sys.Date(),format="%m-%d-%Y")
#Readjhu <- function (latest) {
#    jhu_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", latest, ".csv")
#    jhu_data <- read.csv(jhu_url, stringsAsFactors = FALSE)
#}
#options(show.error.messages = FALSE)
#while (class(try(Readjhu(latest))) == "try-error") {# Date of today is not updated yet
#    date<- as.Date(latest, format="%m-%d-%Y")
#    latest<- format(date-1,format="%m-%d-%Y") # Use date of yesterday instead
#    options(show.error.messages = FALSE)
#    jhu_data <- try(Readjhu(latest),silent=TRUE)
#}
#jhu_ny <- jhu_data[jhu_data$Province_State == "New York",]

# mobility data
data_NT <- read.csv("../output/mobility_data_clean.csv")
data_NT$date_of_interest <- as.Date(data_NT$date,format="%Y-%m-%d")
data_NT <- data_NT %>% rename_all(funs(str_replace_all(., "_percent_change_from_baseline", "")))

# Park Closure Status datasets
# Geo Points
FreeMeals <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/output/FreeMeals_clean.csv?token=AKNEFHWNBDDICOIDN3COTOLAGP46M", stringsAsFactors = FALSE)
Playgrounds <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/output/Playgrounds_clean.csv?token=AKNEFHVALHT4IM4AR24F6PLAGP5FU", stringsAsFactors = FALSE)
AdultExerciseEquip <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/output/AdultExerciseEquip_clean.csv?token=AKNEFHUQUMDJNCU4YOLAUELAGP43K", stringsAsFactors = FALSE)
# Geo Polygons
AtheleticFac_geo <- geojsonio::geojson_read("../output/AtheleticFac_geo_clean.geojson", what = "sp")
SkateParks_geo <- geojsonio::geojson_read("../output/SkateParks_geo_clean.geojson", what = "sp")
DogRuns_geo <- geojsonio::geojson_read("../output/DogRuns_geo_clean.geojson", what = "sp")

