
library(tidyverse)
library(dplyr)
library(sf)


# Statewide cumulative and daily cases ------------------------------------

# Read in NYT data from online .csv file
nyt <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

idaho <- filter(nyt, state == "Idaho")

# Cumulative (done)
idaho_cumulative <- filter(nyt, state == "Idaho") %>%
  group_by(date) %>%
  summarise(cum_cases = sum(cases),
            cum_deaths = sum(deaths))

# Daily
idaho_daily <- idaho_cumulative %>%
  mutate(daily_cases = cum_cases - lag(cum_cases),
         daily_deaths = cum_deaths - lag(cum_deaths)) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
  rename(DATE = date,
         New_Cases = daily_cases,
         New_Deaths = daily_deaths,
         Cumulative_Cases = cum_cases,
         Cumulative_Deaths = cum_deaths)

# County-level shapefile --------------------------------------------------

# Uplodad Idaho counties shapefile
#id_counties <- st_read("/Users/carsonmk/Desktop/Box Sync/HCRI/COVID-19/Idaho_COVID-19_BSU_HCRI/HCRIShapefiles/COVID-19Counties_simpl/COVID_19_counties.shp")
id_counties <- st_read("/Users/brittanybrand/Desktop/HCRI 2020/Idaho_COVID-19/Idaho_Counties_Shapefile/ID_counties.shp")

# Select most recent date of NYT county-level data
recent_date <- idaho %>%
  slice(which.max(date))

# Filter only most recent date observations
recent_counties <- filter(idaho, date == recent_date$date)

# Import health districts by count
#h_districts <- read_csv("/Users/carsonmk/Desktop/Box Sync/HCRI/COVID-19/Idaho_COVID-19/County_Health_Districts.csv")
h_districts <- read_csv("/Users/brittanybrand/Desktop/HCRI 2020/Idaho_COVID-19/County_Health_Districts.csv")

# Merge cases by county with spatial county file
counties_shapefile <- full_join(id_counties, recent_counties,
                                by = c("NAME" = "county")) %>%
  left_join(h_districts, by = c("NAME" = "County"))


# Make final data outputs -------------------------------------------------

# Save .csv
#write_csv(idaho_daily, "/Users/carsonmk/Desktop/Box Sync/HCRI/COVID-19/Idaho_COVID-19/ID_COVID_CUMULATIVE_BY_DATE.csv")
write_csv(idaho_daily, "/Users/brittanybrand/Desktop/HCRI 2020/Idaho_COVID-19/ID_COVID_CUMULATIVE_BY_DATE.csv")

# Save shapefile for use in ArcMap, UPDATE DATE IN FILE TITLE
#st_write(counties_shapefile, "/Users/carsonmk/Desktop/Box Sync/HCRI/COVID-19/Idaho_COVID-19/3_26_21_cumulativecases.shp")
st_write(counties_shapefile, "/Users/brittanybrand/Desktop/HCRI 2020/Idaho_COVID-19/7_2_21_cumulativecases.shp")

