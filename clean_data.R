library(tidyverse)
library(dplyr)
library(reshape)

#gets the country-level stuff from JHU and US state-level from NYT
#last_date = how many days after initial date of 10th death to look at
get_data <- function(last_date) {

  #get data and clean it up
  d <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    select(-Lat, -Long) %>% melt(id = c("Province.State", "Country.Region")) %>%
    arrange(Country.Region, Province.State) %>%
    dplyr::rename(
      date = variable,
      country = Country.Region,
      state = Province.State
    ) %>%
    mutate(
      date = substring(date, 2),
      date = as.Date(date, format = "%m.%d.%y")
    )
  
  #aggregate for the countries with dis-aggregated data
  #and get days since 10th death
  agg <- d %>% group_by(country, date) %>%
    summarize(deaths = sum(value)) %>%
    dplyr::rename(
      state = country
    ) %>%
    mutate(type = 'world')
  
  
  #US data
  d <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
    mutate(
      date = as.Date(date)
    ) %>%
    arrange(state, date) %>%
    select(date, state, deaths) %>%
    mutate(type = 'us')
  
  #bind
  data <- bind_rows(agg, d) %>%
    mutate(d10 = ifelse(deaths >= 10, 1, 0)) %>%
    filter(d10 == 1) %>% ungroup() %>%
    group_by(state) %>%
    mutate(min_date = min(date),
           days_10 = as.numeric(date - min_date)) %>%
    filter(days_10 <= last_date) %>%
    mutate(last = ifelse(row_number() == n(), 1, 0)) %>% ungroup()
  
  return(data)
  
}


#gets the county-level stuff from NYT
get_counties <- function() {
  d <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) %>%
    mutate(
      date = as.Date(date)
    ) %>%
    arrange(state, county, date) %>%
    select(date, state, county, deaths) %>%
    mutate(type = 'county')
  
  data <-  d %>%
    mutate(d10 = ifelse(deaths >= 10, 1, 0)) %>%
    filter(d10 == 1) %>% ungroup() %>%
    group_by(state, county) %>%
    mutate(min_date = min(date),
           days_10 = as.numeric(date - min_date)) %>%
    filter(days_10 <= last_date) %>%
    mutate(last = ifelse(row_number() == n(), 1, 0)) %>% ungroup()
  
}







