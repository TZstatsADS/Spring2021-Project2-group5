library(googleway)
library(ggmap)

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
  "processed_data.Rda"
)
