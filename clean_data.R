library(tidyverse)
library(dplyr)
library(reshape)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(jcolors)
library(scales)

#gets the country-level stuff from JHU and US state- and county- level from NYT
#last_date = how many days after initial date of 10th death to look at
get_data <- function(last_date) {

  #get data and clean it up: jhu
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
  agg <- d %>% group_by(country, date) %>%
    summarize(deaths = sum(value)) %>%
    dplyr::rename(
      state = country
    ) %>%
    mutate(type = 'world')
  
  #state US data from nyt
  d1 <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
    mutate(
      date = as.Date(date)
    ) %>%
    arrange(state, date) %>%
    select(date, state, deaths) %>%
    mutate(type = 'us')
  
  #county-level US data from nyt
  d2 <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")) %>%
    mutate(
      date = as.Date(date)
    ) %>%
    arrange(state, county, date) %>%
    mutate(state = paste0(county, ", ", state)) %>%
    select(date, state, deaths) %>%
    mutate(type = 'county')
  
  #bind together
  data <- bind_rows(agg, d1, d2) %>%
    mutate(d10 = ifelse(deaths >= 10, 1, 0)) %>%
    filter(d10 == 1) %>% ungroup() %>%
    group_by(state) %>%
    mutate(min_date = min(date),
           days_10 = as.numeric(date - min_date)) %>%
    filter(days_10 <= last_date) %>%
    mutate(last = ifelse(row_number() == n(), 1, 0)) %>% ungroup()
  
  return(data)
  
}


#take data and keep the top n highest states/countries/counties plus any extras specified
keep_top <- function(data, outcome, n, extras) {

  #passing variable names as arguments is impossibly hard so just do this badly
  if (outcome == "deaths") {
    d <- data %>% filter(last==1) %>% arrange(-deaths) %>%
      filter(row_number() <= n | state %in% extras) %>%
      select(state)
  } else{
    d <- data %>% filter(last==1) %>% arrange(-hospitalized) %>%
      filter(row_number() <= n | state %in% extras) %>%
      select(state)
  }

  data <- data %>%
    inner_join(d, by="state")
  
  return(data)
  
}


make_figure <- function(data_set, source) {
  
  if (source == 'nyt') {
    cap = "@nytimes https://github.com/nytimes/covid-19-data"
  } else {
    cap = "@JHUSystems https://github.com/CSSEGISandData/COVID-19"
  }
  
  g <- 
    data_set %>% 
    ggplot(aes(x=days_10, y=deaths, group = state, color = state, label = state)) +
    geom_point(size=4, shape=16) +
    geom_line(size = 2) +
    theme_minimal() +
    labs(x = "Days since 10th death",
         y = "Total deaths",
         caption = paste("Figure: @benbbaldwin | Data:", cap),
         title = "COVID-19 Tracker: Mortality Progression Since 10th Death") +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text.x=element_text(hjust=0.5)
    )   +
    scale_color_jcolors(palette="pal8") +
    scale_y_continuous(breaks=pretty_breaks(n = 5)) +
    geom_text_repel(
      data = filter(data_set, last == 1 & deaths > .5 * max(data_set$deaths)),
      color = "black",
      nudge_x = -1,
      nudge_y = -0,
      size = 5,
    ) +
    geom_text_repel(
      data = filter(data_set, last == 1 & deaths <= .5 * max(data_set$deaths)),
      color = "black",
      nudge_x = -1,
      nudge_y = .08 * max(data_set$deaths),
      size = 5,
    ) +
    annotate("text",x=4, y= .8 * max(data_set$deaths), label = paste0("Most recent update:\n",month(max(data_set$date)),"-",day(max(data_set$date))), color="red", size=5)
  
  return(g)
  
}



