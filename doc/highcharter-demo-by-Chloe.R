# Highcharter DEMO
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")
if(!require(highcharter)) devtools::install_github("jbkunst/highcharter")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")


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


# ================================== convert to tidy data ===========================================
# Convert format
data_by_day$date_of_interest <-
    as_date(data_by_day$date_of_interest)

# select subset by borough (do similar in shiny)
plot_borough <- "BX"

data_by_day_sub <- data_by_day[,c(1,12:21)] %>%
    tidyr::pivot_longer(
        cols = -date_of_interest, 
        names_to = "line_var", 
        values_to = "value") %>% #names_prefix = "fight_" 
    dplyr::mutate(line_var = as.factor(data_by_day_sub$line_var)) # This is tidy

# ================================ line plot =========================================================
# Interactive time series plot with highcharter
ts1 <- data_by_day_sub %>%
    hchart("line",hcaes(x = date_of_interest, y = value, 
                        group = line_var)) %>%
    hc_chart(zoomType = "x") %>%
    #hc_colors(c("#0015BC", "#FF0000")) %>% need one color for each variable
    hc_legend(align = "center", verticalAlign = "bottom",layout = "horizontal") %>%
    hc_xAxis(title = list(text = "Date"),
             labels = list(format = '{value:%b %d %y}')) %>%
    hc_yAxis(title = list(text = "Case count"),
             tickInterval = 400,
             max = 1600) %>%
    hc_title(text = "<b>Covid-19 Summary for Bronx, NY by Date</b>") %>%
    hc_subtitle(text = "Click and drag in the plot area to zoom in on a time span") %>%
    hc_plotOptions(area = list(lineWidth = 0.5)) %>% 
    hc_exporting(enabled = TRUE)

ts1

#=====================the following annotations are optional========================================
ts1 %>%
    hc_add_annotation(
        labelOptions = list(
            backgroundColor = 'rgba(0,0,0,0.4)',
            verticalAlign = 'top',
            y = 15
        ),
        labels = list(
            list(point = list(xAxis = 0, yAxis = 0,
                              x = datetime_to_timestamp(as.Date("2020/02/29")),
                              y = 2200000), text = "Feb.29 - Biden Won South Carolina Primary"),
            list(point = list(xAxis = 0, yAxis = 0,
                              x = datetime_to_timestamp(as.Date("2020/03/04")),
                              y = 3700000), text = "Mar.4 - Michael Bloomberg dropped out and endorsed Biden"),
            list(point = list(xAxis = 0, yAxis = 0,
                              x = datetime_to_timestamp(as.Date("2020/08/11")),
                              y = 6900000), text = "Aug.11 - Biden names Kamala Harris as VP nominee"),
            list(point = list(xAxis = 0, yAxis = 0,
                              x = datetime_to_timestamp(as.Date("2020/08/20")),
                              y = 13500000), text = "Aug.20 - Biden Accepted Democratic Nomination"),
            list(point = list(xAxis = 0, yAxis = 0,
                              x = datetime_to_timestamp(as.Date("2020/08/27")),
                              y = 4860000), text = "Aug.27 - Trump Accepted Republican Nomination"),
            list(point = list(xAxis = 0, yAxis = 0,
                              x = datetime_to_timestamp(as.Date("2020/09/29")),
                              y = 11500000), text = "Sep.29 - First Presidential Debate")
        )
    )

ts1
```