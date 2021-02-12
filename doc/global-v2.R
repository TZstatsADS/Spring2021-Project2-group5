if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(RSocrata)) devtools::install_github("Chicago/RSocrata")
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
#if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
#if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
#if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
#if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
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
#if(!require(geojsonR)) install.packages("geojsonR", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(googleway)) install.packages("googleway", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")





# ==================================================== Load data ==========================================================
#nyc health data
data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
nyc_latest <- data_by_day %>% tail(1)
nyc_yesterday<- data_by_day[nrow(data_by_day)-1,]

#JHU covid19 data in US
latest<- format(Sys.Date(),format="%m-%d-%Y")
Readjhu <- function (latest) {
    jhu_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", latest, ".csv")
    jhu_data <- read.csv(jhu_url, stringsAsFactors = FALSE)
}
#jhu_data <- try(Readjhu(latest))
while (class(try(Readjhu(latest))) == "try-error") {# Date of today is not updated yet
    date<- as.Date(latest, format="%m-%d-%Y")
    latest<- format(date-1,format="%m-%d-%Y")     # Use date of yesterday instead
    jhu_data <- try(Readjhu(latest))
}
jhu_ny <- jhu_data[jhu_data$Province_State == "New York",]


# -------------------------------------------------------------------------------------------------------------------------
# NYC Open Data datasets 
#dt<- readr::read_csv(here::here("./data/freemeals.csv"))
app_token <- "TCudJYL2zt35zEIi5qEwV0UwM" #app token
# Free Meals: Use library(RSocrata) 
FreeMeals <- read.socrata("https://data.cityofnewyork.us/resource/sp4a-vevi.json", app_token=app_token) 

# Park Closure Status Datasets
# Points
Playgrounds <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/park_closure_status/Parks_Closure_Status_Due_to_COVID-19__Playgrounds.csv?token=AKNEFHSYBW3NY35EPL5PMFDAFVUNI", stringsAsFactors = FALSE)
AdultExerciseEquip <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/park_closure_status/Parks_Closure_Status_Due_to_COVID-19__Adult_Exercise_Equipment.csv?token=AKNEFHUQ5IJHSCD6WA6DICDAFVUPW", stringsAsFactors = FALSE)
# Polygons
AtheleticFac_geo <- geojsonio::geojson_read("../data/park_closure_status/Parks Closure Status Due to COVID-19_ Athletic Facilities.geojson", what = "sp")
SkateParks_geo <- geojsonio::geojson_read("../data/park_closure_status/Parks Closure Status Due to COVID-19_ Skate Parks.geojson", what = "sp")
DogRuns_geo <- geojsonio::geojson_read("../data/park_closure_status/Parks Closure Status Due to COVID-19_ Dog Runs.geojson", what = "sp")

#AtheleticFac_geo <- geojsonio::geojson_read("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/park_closure_status/Parks%20Closure%20Status%20Due%20to%20COVID-19_%20Athletic%20Facilities.geojson?token=AKNEFHSWSNHUBFEBNVPJE3DAFVUSQ")#, what = "sp")
#SkateParks_geo <- geojsonio::geojson_read("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/park_closure_status/Parks%20Closure%20Status%20Due%20to%20COVID-19_%20Skate%20Parks.geojson?token=AKNEFHSEGTL5EX334MQQJQLAFVTPE", what = "sp")
#DogRuns_geo <- geojsonio::geojson_read("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/park_closure_status/Parks%20Closure%20Status%20Due%20to%20COVID-19_%20Dog%20Runs.geojson?token=AKNEFHR3B5GOQH7PCFWCCR3AFVTKW", what = "sp")
#AtheleticFac <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/Parks_Closure_Status_Due_to_COVID-19__Athletic_Facilities.csv?token=AKNEFHUFHED3VHKCWDUWA33AEZEH4", stringsAsFactors = FALSE)
#DogRuns <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/Parks_Closure_Status_Due_to_COVID-19__Dog_Runs.csv?token=AKNEFHUUINBSTIBGLVQY54DAEZESC", stringsAsFactors = FALSE)
#SkateParks <- read.csv("https://raw.githubusercontent.com/TZstatsADS/Spring2021-Project2-group5/master/data/Parks_Closure_Status_Due_to_COVID-19__Skate_Parks.csv?token=AKNEFHWEHT7LPA6LOLTOZLTAEZER4", stringsAsFactors = FALSE)

# ==================================================== data processing ====================================================

#------------------------------------Goal: combine all map datasets into A SINGLE DATAFRAME------------------------------
dfList <- list("FreeMeals"=FreeMeals, "AdultExerciseEquip"=AdultExerciseEquip, "Playgrounds"=Playgrounds,                
               "AtheleticFac"=AtheleticFac_geo@data, "DogRuns"=DogRuns_geo@data, "SkateParks"=SkateParks_geo@data)
# Add a common column in each dataframe specifying which dataset it belongs to
for (i in 1:length(dfList)) {
    dfList[[i]]["button"] <- names(dfList)[i] # returns the name of the dataframe
}

# Recode borough names for PARK CLOSURE STATUS datasets
dfList$FreeMeals <- dfList$FreeMeals %>% dplyr::rename(borough = city)
#patterns<- c("B","X","Q","M","R")
#replacements<- c("Brooklyn","Bronx","Queens","Manhattan","Staten Island")
for (i in 2:length(dfList)) {
    dfList[[i]] <- dfList[[i]] %>%
        # rename variable
        dplyr::rename_all(funs(stringr::str_replace_all(., "B", "b"))) %>%
        # Replace patterns with replacements
        dplyr::mutate(borough = ifelse(borough=='B',"Brooklyn",ifelse(borough=='X',"Bronx",
                                        ifelse(borough=='Q',"Queens",ifelse(borough=='M',"Manhattan",
                                                ifelse(borough=='R',"Staten Island",NA))))))
    }
# popup content on the map
dfList$FreeMeals["content"] <- paste(sep = "<br/>", # html style of `next line`
                            paste0(dfList$FreeMeals$siteaddress, ", ",dfList$FreeMeals$city, ", NY ",dfList$FreeMeals$zip), #full address
                            paste0("<b>Kosher meal type: <b/>",dfList$FreeMeals$koshermealtype),
                            paste0("<b>Accessibility: <b/>",dfList$FreeMeals$accessibility))
dfList$AdultExerciseEquip["content"] <- paste(sep = "<br/>",
                            paste0(dfList$AdultExerciseEquip$PropName,", ",dfList$AdultExerciseEquip$borough,", NY"), #full address
                            paste0("<b>Status: <b/>",dfList$AdultExerciseEquip$Status),
                            paste0("<b>Features: <b/>", dfList$AdultExerciseEquip$FeatureType))
dfList$Playgrounds["content"] <- paste(sep = "<br/>",
                            paste0(dfList$Playgrounds$location,", ",dfList$Playgrounds$borough,", NY"), #full address
                            paste0("<b>Status: <b/>",dfList$Playgrounds$Status),
                            paste0("<b>Accessibility: <b/>", dfList$Playgrounds$accessibilityLevel))
dfList$AtheleticFac["content"] <- paste(sep = "<br/>",
                            paste0(dfList$AtheleticFac$propertyname,", ",dfList$AtheleticFac$borough,", NY"), #full address
                            paste0("<b>Status: <b/>",dfList$AtheleticFac$status),
                            paste0("<b>Primary sport: <b/>", dfList$AtheleticFac$primarysport),
                            paste0("<b>Surface type: <b/>", dfList$AtheleticFac$surfacetype))
dfList$DogRuns["content"] <- paste(sep = "<br/>",
                            paste0(dfList$DogRuns$propertyname,", ",dfList$DogRuns$borough,", NY"), #full address
                            paste0("<b>Status: <b/>",dfList$DogRuns$status))
dfList$SkateParks["content"] <- paste(sep = "<br/>",
                            paste0(dfList$SkateParks$propertyname,", ",dfList$SkateParks$borough,", NY"), #full address
                            paste0("<b>Status: <b/>",dfList$SkateParks$status))

# Extract longitude and latitude
ExtractCoords <- function(df) {
    df$point <- gsub("POINT |[()]", "", df$point)
    df <- df %>%
        tidyr::separate(point, c("longitude", "latitude"), " ")
    return(df)
}
dfList$Playgrounds <- ExtractCoords(dfList$Playgrounds)
dfList$AdultExerciseEquip <- ExtractCoords(dfList$AdultExerciseEquip)


options(digits=6)
for (i in 1:3) {
dfList[[i]] <- dfList[[i]] %>%
    dplyr::mutate(longitude = as.numeric(longitude)) %>%
    dplyr::mutate(latitude = as.numeric(latitude))
}
# ------------------------------------------------------
# Update datasets in the list to the global environment
list2env(dfList,.GlobalEnv)

AtheleticFac_geo@data <- AtheleticFac
DogRuns_geo@data <- DogRuns
SkateParks_geo@data <- SkateParks

