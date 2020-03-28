library(tidyverse)
library(dplyr)
library(reshape)
library(ggplot2)
library(ggrepel)
library(jcolors)

#the countries we want to look at
states <- c('Washington','California','Georgia','New York','Louisiana','New Jersey','Michigan', 'Florida','Illinois', 'Massachusetts')
last_date <- 40 #how many days after 10 days you want to go

#get data and clean it up
d <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  arrange(state, date)


recent = max(d$date)

# get days since 10th death
agg <- d %>% 
  mutate(d10 = ifelse(deaths >= 10, 1, 0)) %>%
  filter(d10 == 1) %>% ungroup() %>%
  group_by(state) %>%
  mutate(min_date = min(date),
         days_10 = as.numeric(date - min_date)) %>%
  mutate(last = ifelse(row_number() == n(), 1, 0))

agg%>%filter(last==1) %>% arrange(-deaths)

plot <- agg %>%
  filter(state %in% states)

  
#make the graphic
plot %>% 
  ggplot(aes(x=days_10, y=deaths, group = state, color = state, label = state)) +
  geom_point(size=4, shape=16) +
  geom_line(size = 2) +
  theme_minimal() +
  labs(x = "Days since 10th death",
       y = "Total deaths",
       caption = "Figure: @benbbaldwin | Data: @NYT https://github.com/nytimes/covid-19-data",
       title = "COVID-19 Tracker: Mortality Progression Since 10th Death") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5)
  )   +
  scale_color_jcolors(palette="pal8") +
  geom_text_repel(
    data = filter(plot, last == 1),
    color = "black",
    nudge_x = -2,
    nudge_y = 10,
    size = 5,
  ) +
  annotate("text",x=3, y= 350, label = paste0("Most recent update:\n",month(recent),"-",day(recent)), color="red", size=5)
  

ggsave("deaths_since_10_states.png", dpi=800)



