# ================================ Notes by Chloe: =================================================================
# This R script would need to be divided into three files: 1) global.R   2) ui.R     3) server.R


## Code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
# https://nycdatascience.com/blog/student-works/the-most-dangerous-intersections-in-nyc/
# https://github.com/yanghua23/shinyapp_NYC_Motor_Vehicle_Collision
# https://github.com/eparker12/nCoV_tracker

# ================================== Packages (Put all packages used under this section) ===========================
# ===============Note: when using functions, specify the package like 'dplyr::select()' to avoid conflicts==========

# load required packages
install_github("Chicago/RSocrata")
library(RSocrata)

#if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
#if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
#if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
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
#if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")



# ================================= Parameter/variable Definitions====================================================
borough_vars <- c("All boroughs" = "",
    "Bronx" = "bronx",
    "Brooklyn" = "brooklyn",
    "Manhattan" = "manhattan",
    "Queens" = "queens", 
    "Staten Island" = "staten_island"
)

zip_vars <- c(10001, 10002)

# set mapping color
covid_col = "#cc4c02"
covid_other_col = "#662506"

# ================================= Get data with updates ==========================================================
# ================Note: Instead of getting updates using scripts, read data directly from target github repo========
#nyc health data
data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
latest <- data_by_day %>% tail(1)
yesterday<- data_by_day[nrow(data_by_day)-1,]

#JHU covid19 data in US
latest<- format(Sys.Date(),format="%m-%d-%Y")
Readjhu <- function (latest) {
    jhu_url <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/", latest, ".csv")
    jhu_data <- read.csv(jhu_url, stringsAsFactors = FALSE)
}
jhu_data <- try(Readjhu(latest))
while (class(jhu_data) == "try-error") {# Date of today is not updated yet
    date<- as.Date(latest, format="%m-%d-%Y")
    latest<- format(date-1,format="%m-%d-%Y")     # Use date of yesterday instead
    jhu_data <- try(Readjhu(latest))
}
jhu_ny <- jhu_data[jhu_data$Province_State == "New York",]


# NYC Open Data datasets (or use datasets uploaded to github)
#dt<- readr::read_csv(here::here("./data/freemeals.csv"))
app_token <- "TCudJYL2zt35zEIi5qEwV0UwM" #app token
# Use library(RSocrata)
freemeals_js <- "https://data.cityofnewyork.us/resource/sp4a-vevi.json" #Basic endpoint
FreeMeals <- read.socrata(freemeals_js, app_token=app_token) 
# NYC Park Closure Status
playgrounds_js <- "https://data.cityofnewyork.us/resource/a4qt-mpr5.json" #Basic endpoint
Playgrounds <- read.socrata(playgrounds_js, app_token=app_token) #+ "?borough=bronx") 
atheleticfac_js <- "https://data.cityofnewyork.us/resource/g3xg-qtbc.json" #Basic endpoint
AtheleticFac <- read.socrata(atheleticfac_js, app_token=app_token)
dogruns_js<- "https://data.cityofnewyork.us/resource/wswf-9pts.json" #Basic endpoint
DogRuns <- read.socrata(dogruns_js, app_token=app_token)
adultexerciseequip_js<- "https://data.cityofnewyork.us/resource/tkzt-zfpz.json" #Basic endpoint
AdultExerciseEquip <- read.socrata(adultexerciseequip_js, app_token=app_token)
skateparks_js<- "https://data.cityofnewyork.us/resource/pvvr-75zk.json" #Basic endpoint
SkateParks <- read.socrata(skateparks_js, app_token=app_token)
comfortstations_js<- "https://data.cityofnewyork.us/resource/i5n2-q8ck.json" #Basic endpoint
ComfortStations <- read.socrata(comfortstations_js, app_token=app_token)
# ...


# Google covid-19 mobility data 



# update data with automated script
#source("jhu_data_daily_cases.R") # option to update daily cases
#source("jhu_data_weekly_cases.R") # run locally to update numbers, but not live on Rstudio server /Users/epp11/Dropbox (VERG)/GitHub/nCoV_tracker/app.R(to avoid possible errors on auto-updates)
#source("ny_data_us.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)
# import data
#cv_cases = read.csv("input_data/coronavirus.csv")
#countries = read.csv("input_data/countries_codes_and_coordinates.csv")
#worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
#country_geoms = read.csv("input_data/country_geoms.csv")
#cv_states = read.csv("input_data/coronavirus_states.csv")


# ============================================ Plots ==================================================================
# =================================== Need to change ALL ==============================================================
# ================================= These code should work in R notebook/markdown first ===============================


# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
    plot_df = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        ylab("Cumulative cases") +  xlab("Date") + theme_bw() + 
        scale_colour_manual(values=c(covid_col)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
    plot_df_new = subset(cv_aggregated, date<=plot_date)
    g1 = ggplot(plot_df_new, aes(x = date, y = new, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
        # geom_bar(position="stack", stat="identity") + 
        ylab("New cases (weekly)") + xlab("Date") + theme_bw() + 
        scale_colour_manual(values=c(covid_col)) +
        scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
              plot.margin = margin(5, 12, 5, 5))
    g1
}

# test function
#cumulative_plot(cv_aggregated, current_date)
#new_cases_plot(cv_aggregated, current_date)


# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date) {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
            xlim(c(plot_start_date,(current_date+5))) + xlab("Date")
    }
    
    if (start_point=="Week of 100th confirmed case") {
        cv_cases = subset(cv_cases, weeks_since_case100>0)
        g = ggplot(cv_cases, aes(x = weeks_since_case100, y = new_outcome, fill = region, group = 1,
                                 text = paste0("Week ",weeks_since_case100, "\n", region, ": ",new_outcome)))+
            xlab("Weeks since 100th confirmed case") #+ xlim(c(plot_start_date,(current_date+5))) 
    }
    
    if (start_point=="Week of 10th death") {
        cv_cases = subset(cv_cases, weeks_since_death10>0)
        g = ggplot(cv_cases, aes(x = weeks_since_death10, y = new_outcome, fill = region, group = 1,
                                 text = paste0("Week ",weeks_since_death10, "\n", region, ": ",new_outcome))) +
            xlab("Weeks since 10th death") #+ xlim(c(plot_start_date,(current_date+5))) 
    }
    
    g1 = g +
        geom_bar(position="stack", stat="identity") + 
        ylab("New (weekly)") + theme_bw() + 
        scale_fill_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date) {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
            xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
    }
    
    if (start_point=="Week of 100th confirmed case") {
        cv_cases = subset(cv_cases, weeks_since_case100>0)
        g = ggplot(cv_cases, aes(x = weeks_since_case100, y = outcome, colour = region, group = 1,
                                 text = paste0("Week ", weeks_since_case100,"\n", region, ": ",outcome))) +
            xlab("Weeks since 100th confirmed case")
    }
    
    if (start_point=="Week of 10th death") {
        cv_cases = subset(cv_cases, weeks_since_death10>0)
        g = ggplot(cv_cases, aes(x = weeks_since_death10, y = outcome, colour = region, group = 1,
                                 text = paste0("Week ", weeks_since_death10,"\n", region, ": ",outcome))) +
            xlab("Weeks since 10th death")
    }
    
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
        ylab("Cumulative") + theme_bw() + 
        scale_colour_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date)  {
    if (start_point=="Date") {
        g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                                 text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
            xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
    }
    
    if (start_point=="Week of 100th confirmed case") {
        cv_cases = subset(cv_cases, weeks_since_case100>0)
        g = ggplot(cv_cases, aes(x = weeks_since_case100, y = outcome, colour = region, group = 1,
                                 text = paste0("Week ",weeks_since_case100, "\n", region, ": ",outcome))) +
            xlab("Weeks since 100th confirmed case")
    }
    
    if (start_point=="Week of 10th death") {
        cv_cases = subset(cv_cases, weeks_since_death10>0)
        g = ggplot(cv_cases, aes(x = weeks_since_death10, y = outcome, colour = region, group = 1,
                                 text = paste0("Week ",weeks_since_death10, "\n", region, ": ",outcome))) +
            xlab("Weeks since 10th death")
    }
    
    g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
        ylab("Cumulative (log10)") + theme_bw() +
        scale_y_continuous(trans="log10") +
        scale_colour_manual(values=country_cols) +
        theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
    ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to render plotly of epidemic comparison depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
    epi_comp$outcome = epi_comp[,comparison] 
    epi_comp = epi_comp[order(epi_comp$outcome),]
    epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
    
    p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
        ylab("N") + xlab("") + theme_bw() + 
        scale_fill_manual(values=c("2019-COVID"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
        theme(legend.position = "")
    
    if(comparison == "cfr") { p1 = p1 + ylab("%") }
    if(comparison == "deaths") { p1 = p1 + scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
    if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e8), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
    ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}



# ========================= DATA PROCESSING: COVID-19 and Map =====================================================
# ========================= Need to change ALL=====================================================================

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
    cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$cases_per_million = as.numeric(format(round(cv_cases$cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_cases_per_million = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
cv_cases$deaths_per_million = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_deaths_per_million = as.numeric(format(round(cv_cases$new_deaths/(cv_cases$population/1000000),1),nsmall=1))

# add variable for weeks since 100th case and 10th death
cv_cases$weeks_since_case100 = cv_cases$weeks_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
    country_name = as.character(unique(cv_cases$country))[i]
    country_db = subset(cv_cases, country==country_name)
    country_db$weeks_since_case100[country_db$cases>=100] = 0:(sum(country_db$cases>=100)-1)
    country_db$weeks_since_death10[country_db$deaths>=10] = 0:(sum(country_db$deaths>=10)-1)
    cv_cases$weeks_since_case100[cv_cases$country==country_name] = country_db$weeks_since_case100
    cv_cases$weeks_since_death10[cv_cases$country==country_name] = country_db$weeks_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset of state data for today's data
if (any(grepl("/", cv_states$date))) { 
    cv_states$date = format(as.Date(cv_states$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_states$date = as.Date(cv_states$date, format="%Y-%m-%d") }
cv_states_today = subset(cv_states, date==max(cv_states$date))

# create subset for countries with at least 1000 cases
cv_today_reduced = subset(cv_today, cases>=1000)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                cases_per_million, new_cases_per_million,
                                deaths_per_million, new_deaths_per_million,
                                weeks_since_case100, weeks_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for weeks since 100th case and 10th death
cv_cases_continent$weeks_since_case100 = cv_cases_continent$weeks_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
    continent_name = as.character(unique(cv_cases_continent$continent))[i]
    continent_db = subset(cv_cases_continent, continent==continent_name)
    continent_db$weeks_since_case100[continent_db$cases>=100] = 0:(sum(continent_db$cases>=100)-1)
    continent_db$weeks_since_death10[continent_db$deaths>=10] = 0:(sum(continent_db$deaths>=10)-1)
    cv_cases_continent$weeks_since_case100[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_case100
    cv_cases_continent$weeks_since_death10[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_death10
}

# add continent populations
cv_cases_continent$pop = NA
cv_cases_continent$pop[cv_cases_continent$continent=="Africa"] = 1.2e9
cv_cases_continent$pop[cv_cases_continent$continent=="Asia"] = 4.5e9
cv_cases_continent$pop[cv_cases_continent$continent=="Europe"] = 7.4e8
cv_cases_continent$pop[cv_cases_continent$continent=="North America"] = 5.8e8
cv_cases_continent$pop[cv_cases_continent$continent=="Oceania"] = 3.8e7
cv_cases_continent$pop[cv_cases_continent$continent=="South America"] = 4.2e8

# add normalised counts
cv_cases_continent$cases_per_million =  as.numeric(format(round(cv_cases_continent$cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_cases_per_million =  as.numeric(format(round(cv_cases_continent$new_cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$deaths_per_million =  as.numeric(format(round(cv_cases_continent$deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_deaths_per_million =  as.numeric(format(round(cv_cases_continent$new_deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$weeks_since_case100 = cv_cases_global$weeks_since_death10 = 0:(nrow(cv_cases_global)-1)

# add normalised counts
cv_cases_global$pop = 7.6e9
cv_cases_global$cases_per_million =  as.numeric(format(round(cv_cases_global$cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_cases_per_million =  as.numeric(format(round(cv_cases_global$new_cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$deaths_per_million =  as.numeric(format(round(cv_cases_global$deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_deaths_per_million =  as.numeric(format(round(cv_cases_global$new_deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$cases_per_million, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
    addTiles() %>% 
    addLayersControl(
        position = "bottomright",
        overlayGroups = c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
    hideGroup(c("2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola")) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    fitBounds(~-100,-60,~60,70) %>%
    addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths_per_million,
              title = "<small>Deaths per million</small>") 

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 7 days
for (i in 1:nrow(cv_aggregated)) { 
    if (i==1) { cv_aggregated$new[i] = 0 }
    if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)), as.character(unique(cv_states$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names


# ================================== SHINY ui.R ======================================================================
ui <- bootstrapPage(
    #tags$head(includeHTML("gtag.html")),
    navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 In New York City</a>'), id="nav",
               windowTitle = "COVID-19 in New York City",
               
               # tab 1 (section 1)
               tabPanel('Introduction',
                        div(
                            class='coverDiv',
                            tags$head(includeCSS('styles.css')), # custom styles
                            
                            fluidRow(
                                # Dynamic valueBoxes
                                valueBoxOutput("TCasesBox"),
                                
                                valueBoxOutput("NewCasesBox"),
                                
                                valueBoxOutput("HospitalizedBox"),
                                
                                valueBoxOutput("TDeathsBox"),
                                
                                valueBoxOutput("NewDeathsBox")
                            ),
                            
                            absolutePanel(fixed = TRUE, draggable = FALSE, 
                                          top = 100, left = 50, right = 'auto', bottom = 'auto',
                                          width = 550, height = 'auto', 
                                          
                                          div( class = 'coverTextDiv',
                                               
                                               h1('New York City Covid-19 Visualization'),
                                               h3('...'),
                                               h3('...'),
                                               h3('...'),
                                               h3('....'),
                                               h3('The work done so far includes an interactive map tool and some preliminary data analysis result, to start with...'),
                                               br()
                                          )
                                          
                            )
                            
                            # Another way to add image:
                            #img(src = "airbnb_overview.jpg", height = 600, weight =700, align="center")
                            #use HTML tag functions to center the image
                            #https://stackoverflow.com/questions/34663099/how-to-center-an-image-in-a-shiny-app
                            #HTML('<center><img src="NYC_cover_image.jpg", height = 600, weight =700 ></center>')
                            
                        )
               ),
               # tab 2 (map)
               tabPanel("NYC covid-19 resources mapper",
                        div(class="outer",
                            tags$head(includeCSS("styles.css")),
                            #map
                            leafletOutput("mymap", width="100%", height="100%"),
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 75, left = 55, width = 250, fixed=TRUE,
                                          draggable = TRUE, height = "auto",
                                          
                                          h2("NYC Neighborhoods"),
                                          
                                          span(tags$i(h6("Reported cases are subject to significant variation in reporting organizations.")), style="color:#045a8d"),
                                          
                                          # select from drop-down lists
                                          selectInput("plot_borough", 
                                                      label = "Borough", 
                                                      choices = borough_vars, selected = "BOROUGH_SELECTED"),
                                          selectInput("plot_zip",
                                                      label = "Zip Code",
                                                      choices = zip_vars, selected = "ZIP_SELECTED"),
                                          
                                          # select from slider
                                          #sliderTextInput("plot_date",
                                          #               label = h5("Select mapping date"),
                                          #               choices = format(unique(cv_cases$date), "%d %b %y"),
                                          #               selected = format(current_date, "%d %b %y"),
                                          #               grid = FALSE,
                                          #               animate=animationOptions(interval = 3000, loop = FALSE)),
                                          
                                          #h3(textOutput("reactive_case_count"), align = "right"),
                                          #h4(textOutput("reactive_death_count"), align = "right"),
                                          #h6(textOutput("clean_date_reactive"), align = "right"),
                                          #h6(textOutput("reactive_country_count"), align = "right"),
                                          #plotOutput("epi_curve", height="130px", width="100%"),
                                          #plotOutput("cumulative_plot", height="130px", width="100%"),
                                          
                                          h3("Select covid-19 related resources:", align="left"),
                                          helpText(""),
                                          h4("FOODS", align = "left"),
                                          checkboxInput("freemeals", "Free Meals"),
                                          h4("HEALTH", align = "left"),
                                          fluidRow (
                                              checkboxInput("vaccenters", "Vaccination Centers"),
                                              checkboxInput("bedsavailable", "Beds Available")
                                          ),
                                          h4("LEISURE", align = "left"),
                                          fluidRow (
                                              checkboxInput("playgrounds", "Playgrounds"),
                                              checkboxInput("atheleticfac", "Atheletic Facilities")
                                          ),
                                          fluidRow (
                                              checkboxInput("dogruns", "Dog Runs"),
                                              checkboxInput("comfortstations", "Comfort Stations")
                                          ),
                                          fluidRow (
                                              checkboxInput("skateparks", "Skate Parks"),
                                              checkboxInput("exerciseequip", "Exercise Equipments")
                                          )

                                          #radioButtons("vehicle", "Show Just One Vehicle", vars3, selected = ''),
                                         
                            )
                            
                            #             absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                            #                           tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                            
                            #             absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                            #                           actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                            #                                        onclick = sprintf("window.open('%s')", 
                            #                                                          "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                            
                            
                        )
               ),
               # tab 3 (optional)
               tabPanel("Data",
                        numericInput("maxrows", "Rows to show", 25),
                        verbatimTextOutput("rawtable"),
                        downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                        "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                           "Johns Hopkins Center for Systems Science and Engineering.")
               ),
               # tab 4 (optional)
               tabPanel("About this site",
                        tags$div(
                            tags$h4("Last update"), 
                            h6(paste0(update)),
                            "This site is updated once daily. There are several other excellent COVID mapping tools available, including those run by", 
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "the WHO,"),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University,"),"and",
                            tags$a(href="https://ourworldindata.org/coronavirus-data-explorer?zoomToSelection=true&time=2020-03-01..latest&country=IND~USA~GBR~CAN~DEU~FRA&region=World&casesMetric=true&interval=smoothed&perCapita=true&smoothing=7&pickerMetric=total_cases&pickerSort=desc", "Our World in Data."),
                            "Our aim is to complement these resources with several interactive features, including the timeline function and the ability to overlay past outbreaks.",
                            
                            tags$br(),tags$br(),tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$br(),tags$br(),tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$br(),tags$br(),tags$h4("Sources"),
                            tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                            " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                            tags$b("US state-level case data: "), tags$a(href="https://github.com/nytimes/covid-19-data", "New York Times github page,"),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://github.com/martynafford/natural-earth-geojson", "Martyn Afford's Github repository"),
                            tags$br(),tags$br(),tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$br(),tags$br(),tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                        )
               )
               
    )          
)





# ========================================= SHINY server.R =============================================================
# ===================================== need to change ALL =============================================================

server = function(input, output, session) {
    
    # covid tab 
    formatted_date = reactive({
        format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
    })
    
    output$clean_date_reactive <- renderText({
        format(as.POSIXct(formatted_date()),"%d %B %Y")
    })
    
    reactive_db = reactive({
        cv_cases %>% filter(date == formatted_date())
    })
    
    reactive_db_last7d = reactive({
        cv_cases %>% filter(date == formatted_date() & new_cases>0)
    })
    
    reactive_db_large = reactive({
        large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
        large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
        large_countries
    })
    
    reactive_db_large_last7d = reactive({
        large_countries = reactive_db_last7d() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
        large_countries = large_countries[order(large_countries$alpha3),]
        large_countries
    })
    
    reactive_polygons = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
    })
    
    reactive_polygons_last7d = reactive({
        worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last7d()$alpha3, ]
    })
    
    output$reactive_case_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
    })
    
    output$reactive_death_count <- renderText({
        paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
    })
    
    output$reactive_country_count <- renderText({
        paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
    })
    
    output$reactive_new_cases_7d <- renderText({
        paste0(round((cv_aggregated %>% filter(date == formatted_date() & region=="Global"))$new/7,0), " 7-day average")
    })
    
    output$mymap <- renderLeaflet({ 
        basemap
    })
    
    observeEvent(input$plot_date, {
        leafletProxy("mymap") %>% 
            clearMarkers() %>%
            clearShapes() %>%
            
            addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5.5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                             label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$cases_per_million, reactive_db()$deaths_per_million) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%  
            
            addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deaths_per_million)) %>%
            
            addCircleMarkers(data = reactive_db_last7d(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5.5), 
                             fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                             label = sprintf("<strong>%s (7-day average)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db_last7d()$country, round(reactive_db_last7d()$new_cases/7,0), round(reactive_db_last7d()$new_deaths/7,0), round(reactive_db_last7d()$new_cases_per_million/7,1), round(reactive_db_last7d()$new_deaths_per_million/7,1)) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                             fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
                             label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per million: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$cases_per_million) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4), 
                             fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                             label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                                 textsize = "15px", direction = "auto")) %>%
            
            addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                             fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                             label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                             labelOptions = labelOptions(
                                 style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                                 textsize = "15px", direction = "auto"))
    })
    
    output$cumulative_plot <- renderPlot({
        cumulative_plot(cv_aggregated, formatted_date())
    })
    
    output$epi_curve <- renderPlot({
        new_cases_plot(cv_aggregated, formatted_date())
    })
    
    
    

    
    # add note for cfr
    output$epi_notes_3 <- renderText({
        if(input$comparison_metric=="cfr") { 
            paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
        }
    })
    
    # update region selections
    observeEvent(input$level_select, {
        if (input$level_select=="Global") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = "Global", selected = "Global")
        }
        
        if (input$level_select=="Continent") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                              selected = c("Africa", "Asia", "Europe", "North America", "South America"))
        }
        
        if (input$level_select=="US state") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_states_today[order(-cv_states_today$cases),]$state), 
                              selected = as.character(cv_states_today[order(-cv_states_today$cases),]$state)[1:10])
        }
        
        if (input$level_select=="Country") {
            updatePickerInput(session = session, inputId = "region_select", 
                              choices = as.character(cv_today_reduced[order(-cv_today_reduced$cases),]$country), 
                              selected = as.character(cv_states_today[order(-cv_states_today$cases),]$state)[1:10])
        }
    }, ignoreInit = TRUE)
    
    # create dataframe with selected countries
    country_reactive_db = reactive({
        if (input$level_select=="Global") { 
            db = cv_cases_global
            db$region = db$global_level
        }
        if (input$level_select=="Continent") { 
            db = cv_cases_continent 
            db$region = db$continent
        }
        if (input$level_select=="Country") { 
            db = cv_cases
            db$region = db$country
        }
        if (input$level_select=="US state") { 
            db = cv_states
            db$region = db$state
        }
        
        if (input$outcome_select=="Cases (total)") { 
            db$outcome = db$cases
            db$new_outcome = db$new_cases
        }
        
        if (input$outcome_select=="Deaths (total)") { 
            db$outcome = db$deaths 
            db$new_outcome = db$new_deaths 
        }
        
        if (input$outcome_select=="Cases per million") { 
            db$outcome = db$cases_per_million 
            db$new_outcome = db$new_cases_per_million 
        }
        
        if (input$outcome_select=="Deaths per million") { 
            db$outcome = db$deaths_per_million 
            db$new_outcome = db$new_deaths_per_million 
        }
        
        db %>% filter(region %in% input$region_select)
    })
    
    # country-specific plots
    output$country_plot <- renderPlotly({
        country_cases_plot(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative <- renderPlotly({
        country_cases_cumulative(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # country-specific plots
    output$country_plot_cumulative_log <- renderPlotly({
        country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date, input$minimum_date)
    })
    
    # output to download data
    output$downloadCsv <- downloadHandler(
        filename = function() {
            paste("COVID_data_", cv_today$date[1], ".csv", sep="")
        },
        content = function(file) {
            cv_cases_sub = cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                                 cases_per_million, new_cases_per_million, deaths_per_million, new_deaths_per_million))
            names(cv_cases_sub) = c("country", "date", "cumulative_cases", "new_cases_past_week", "cumulative_deaths", "new_deaths_past_week",
                                    "cumulative_cases_per_million", "new_cases_per_million_past_week", "cumulative_deaths_per_million", "new_deaths_per_million_past_week")
            write.csv(cv_cases_sub, file)
        }
    )
    
    output$rawtable <- renderPrint({
        cv_cases_sub = cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                             cases_per_million, new_cases_per_million, deaths_per_million, new_deaths_per_million))
        names(cv_cases_sub) = c("country", "date", "cumulative_cases", "new_cases_past_week", "cumulative_deaths", "new_deaths_past_week",
                                "cumulative_cases_per_million", "new_cases_per_million_past_week", "cumulative_deaths_per_million", "new_deaths_per_million_past_week")
        orig <- options(width = 1000)
        print(tail(cv_cases_sub, input$maxrows), row.names = FALSE)
        options(orig)
    })
    
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")

