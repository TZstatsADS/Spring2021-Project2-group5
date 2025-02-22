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
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggmap)) install.packages("ggmap", repos = "http://cran.us.r-project.org")
if(!require(googleway)) install.packages("googleway", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")


api_key <- "INSERT YOUR API KEY HERE"
register_google(api_key)

# DATA PROCESSING
# get covid testing data
covid_testing_data_bk <- google_places(
  search_string = "covid testing sites in Brooklyn",
  key=api_key
)
covid_testing_data_bk_res <- data.frame(covid_testing_data_bk$results)
covid_testing_data_bk_res$borough <- 'Brooklyn'
    
covid_testing_data_qn <- google_places(
    search_string = "covid testing sites in Queens",
    key=api_key
)
covid_testing_data_qn_res <- data.frame(covid_testing_data_qn$results)
covid_testing_data_qn_res$borough <- 'Queens'


covid_testing_data_mh <- google_places(
    search_string = "covid testing sites in Manhattan",
    key=api_key
)
covid_testing_data_mh_res <- data.frame(covid_testing_data_mh$results)
covid_testing_data_mh_res$borough <- 'Manhattan'

covid_testing_data_br <- google_places(
    search_string = "covid testing sites in Bronx",
    key=api_key
)
covid_testing_data_br_res <- data.frame(covid_testing_data_br$results)
covid_testing_data_br_res$borough <- 'Bronx'

covid_testing_data_sti <- google_places(
    search_string = "covid testing sites in Staten Island",
    key=api_key
)
covid_testing_data_sti_res <- data.frame(covid_testing_data_sti$results)
covid_testing_data_sti_res$borough <- 'Staten Island'

covid_testing_data <- full_join(covid_testing_data_sti_res,covid_testing_data_mh_res)
covid_testing_data <- full_join(covid_testing_data, covid_testing_data_qn_res)
covid_testing_data <- full_join(covid_testing_data, covid_testing_data_bk_res)
covid_testing_data <- full_join(covid_testing_data, covid_testing_data_br_res)

# process covid data
list_covid_testing_geo <- geocode(covid_testing_data[, 2])
covid_testing_data$lon <- list_covid_testing_geo$lon
covid_testing_data$lat <- list_covid_testing_geo$lat
covid_testing_data <- covid_testing_data[which(covid_testing_data$business_status == 'OPERATIONAL'), ]
covid_testing_data$category <- 'covid testing site'


#---------------------HOSPITALS---------------------------------------------

# get hospitals data
hospitals_data_bk <- google_places(
  search_string = "hospitals in brooklyn",
  key=api_key
)
hospitals_data_bk_res <- data.frame(hospitals_data_bk$results)
hospitals_data_bk_res$borough <- 'Brooklyn'

hospitals_data_qn <- google_places(
    search_string = "hospitals in queens",
    key=api_key
)
hospitals_data_qn_res <- data.frame(hospitals_data_qn$results)
hospitals_data_qn_res$borough <- 'Queens'

hospitals_data_br <- google_places(
    search_string = "hospitals in bronx",
    key=api_key
)
hospitals_data_br_res <- data.frame(hospitals_data_br$results)
hospitals_data_br_res$borough <- 'Bronx'

hospitals_data_mh <- google_places(
    search_string = "hospitals in Manhattan",
    key=api_key
)
hospitals_data_mh_res <- data.frame(hospitals_data_mh$results)
hospitals_data_mh_res$borough <- 'Manhattan'

hospitals_data_sti <- google_places(
    search_string = "hospitals in staten island",
    key=api_key
)
hospitals_data_sti_res <- data.frame(hospitals_data_sti$results)
hospitals_data_sti_res$borough <- 'Staten Island'

hospitals_data <- full_join(hospitals_data_sti_res,hospitals_data_mh_res)
hospitals_data <- full_join(hospitals_data, hospitals_data_qn_res)
hospitals_data <- full_join(hospitals_data, hospitals_data_bk_res)
hospitals_data <- full_join(hospitals_data, hospitals_data_br_res)

# process hospitals data
hospitals_data_geo <- geocode(hospitals_data[, 2])
hospitals_data$lon <- hospitals_data_geo$lon
hospitals_data$lat <- hospitals_data_geo$lat
hospitals_data$category <- 'hospital'

# merge two dataframes
df <- full_join(hospitals_data, covid_testing_data)
df <- na.omit(df, cols="lon")

# update to ints
df$open_now[!is.na(df$opening_hours['open_now']) & df$opening_hours['open_now'] == 'TRUE'] <- 'Open'
df$open_now[!is.na(df$opening_hours['open_now']) & df$opening_hours['open_now'] == 'FALSE'] <- 'Closed'
df$open_now[is.na(df$opening_hours['open_now'])]  <- 'No Data'

saveRDS(
  df,
  "../output/processed_data.Rda"
)

#--------------------mobility data---------------------------------------------------------------------
mobility_data <- read.csv("../data/2020_US_Region_Mobility_Report.csv")

data_NT <- subset(mobility_data, sub_region_1 == "New York")
#data_NT$month <-months(as.Date(data_NT$date))
data_NT <- data_NT[,c("sub_region_2",
                      "date",
                      "retail_and_recreation_percent_change_from_baseline",
                      "grocery_and_pharmacy_percent_change_from_baseline",
                      "parks_percent_change_from_baseline",
                      "transit_stations_percent_change_from_baseline",
                      "workplaces_percent_change_from_baseline",
                      "residential_percent_change_from_baseline")]
data_NT <- data_NT %>% rename_all(funs(str_replace_all(., "_percent_change_from_baseline", "")))
data_NT["sub_region_2"][is.na(data_NT["sub_region_2"])] <- "ALL"
COUNTIES<- c("ALL","Bronx County", "Kings County", "New York County", "Queens County","Richmond County")
data_NT <- data_NT %>% dplyr::filter(sub_region_2 %in% COUNTIES)
write.csv(data_NT, file = here::here("app","output", "mobility_data_clean.csv"),row.names=FALSE)

# ---------------------- Park Closure Status Data -----------------------------------------
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
                            paste0("<b>Kosher meal type: </b>",dfList$FreeMeals$koshermealtype),
                            paste0("<b>Accessibility: </b>",dfList$FreeMeals$accessibility))
dfList$AdultExerciseEquip["content"] <- paste(sep = "<br/>",
                            paste0(dfList$AdultExerciseEquip$PropName,", ",dfList$AdultExerciseEquip$borough,", NY"), #full address
                            paste0("<b>Status: </b>",dfList$AdultExerciseEquip$Status),
                            paste0("<b>Features: </b>", dfList$AdultExerciseEquip$FeatureType))
dfList$Playgrounds["content"] <- paste(sep = "<br/>",
                            paste0(dfList$Playgrounds$location,", ",dfList$Playgrounds$borough,", NY"), #full address
                            paste0("<b>Status: </b>",dfList$Playgrounds$Status),
                            paste0("<b>Accessibility: </b>", dfList$Playgrounds$accessibilityLevel))
dfList$AtheleticFac["content"] <- paste(sep = "<br/>",
                            paste0(dfList$AtheleticFac$propertyname,", ",dfList$AtheleticFac$borough,", NY"), #full address
                            paste0("<b>Status: </b>",dfList$AtheleticFac$status),
                            paste0("<b>Primary sport: </b>", dfList$AtheleticFac$primarysport),
                            paste0("<b>Surface type: </b>", dfList$AtheleticFac$surfacetype))
dfList$DogRuns["content"] <- paste(sep = "<br/>",
                            paste0(dfList$DogRuns$propertyname,", ",dfList$DogRuns$borough,", NY"), #full address
                            paste0("<b>Status: </b>",dfList$DogRuns$status))
dfList$SkateParks["content"] <- paste(sep = "<br/>",
                            paste0(dfList$SkateParks$propertyname,", ",dfList$SkateParks$borough,", NY"), #full address
                            paste0("<b>Status: </b>",dfList$SkateParks$status))

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

# ------------Extract points from polygons--------------
PtsFromPolygons <- function(geo) {
    lng_list <- list()
    lat_list <- list()
    for (i in 1:length(geo@polygons)){
        lng_list[i] <- as.numeric(geo@polygons[[i]]@labpt[1])
        lat_list[i] <- as.numeric(geo@polygons[[i]]@labpt[2])
    }
    geo@data$longitude <- unlist(lng_list)
    geo@data$latitude <- unlist(lat_list)
    return(geo)
}
SkateParks_geo <- PtsFromPolygons(SkateParks_geo)
AtheleticFac_geo <- PtsFromPolygons(AtheleticFac_geo)
DogRuns_geo <- PtsFromPolygons(DogRuns_geo)

write.csv(AdultExerciseEquip, file = here::here("output", "AdultExerciseEquip_clean.csv"),row.names=FALSE)
write.csv(FreeMeals, file = here::here("output", "FreeMeals_clean.csv"),row.names=FALSE)
write.csv(Playgrounds, file = here::here("output", "Playgrounds_clean.csv"),row.names=FALSE)
geojson_write(input=DogRuns_geo, lat='latitude', lon='longitude',
              geometry='polygons', file=here::here("output", "DogRuns_geo_clean.geojson"))
geojson_write(input=SkateParks_geo, lat='latitude', lon='longitude',
              geometry='polygons', file=here::here("output", "SkateParks_geo_clean.geojson"))
geojson_write(input=AtheleticFac_geo, lat='latitude', lon='longitude',
              geometry='polygons', file=here::here("output", "AtheleticFac_geo_clean.geojson"))



