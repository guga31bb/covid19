library(ggplot2)
library(ggrepel)
library(jcolors)
#library(patchwork)
source("clean_data.R")

#get_data is called from clean_data.R
#40 is how many days to keep after 10th death
agg <- get_data(40) 

agg_us <- agg %>%
  filter(type == 'us')
agg_world <- agg %>%
  filter(type == 'world') 
agg_county <- get_counties()

recent_us = max(agg_us$date)
recent_world = max(agg_world$date)
recent_county = max(agg_county$date)

#check which states have the highest death count
#keep the top 10 to show in plot. and also south korea 
top_10_us <- agg_us %>% filter(last==1) %>% arrange(-deaths) %>%
  filter(row_number() <= 10) %>%
  select(state)

top_10_world <- agg_world %>% filter(last==1) %>% arrange(-deaths) %>%
  filter(row_number() <= 10 | state %in% c("Korea, South")) %>%
  select(state)

top_10_county <- agg_county %>%filter(last==1) %>% arrange(-deaths) %>%
  filter(row_number() <= 5) %>%
  select(state, county)

plot_us <- agg_us %>%
  inner_join(top_10_us, by="state")

plot_world <- agg_world %>%
  inner_join(top_10_world, by="state")

plot_county <- agg_county %>%
  inner_join(top_10_county, by=c("state", "county")) %>%
  mutate(country = paste0(county, ", ", state))

#make the graphic
u <- 
  plot_us %>% 
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
    data = filter(plot_us, last == 1 & deaths > 400),
    color = "black",
    nudge_x = -1,
    nudge_y = -0,
    size = 5,
  ) +
  geom_text_repel(
    data = filter(plot_us, last == 1 & deaths < 400),
    color = "black",
    nudge_x = -2,
    nudge_y = 50,
    size = 5,
  ) +
  annotate("text",x=3, y= 350, label = paste0("Most recent update:\n",month(recent_us),"-",day(recent_us)), color="red", size=5)
  
u

ggsave("figures/deaths_since_10_states.png", dpi=700, width = 16, height = 8)



#make the graphic: world
w <- 
  plot_world %>% 
  ggplot(aes(x=days_10, y=deaths, group = state, color = state, label = state)) +
  geom_point(size=4, shape=16) +
  geom_line(size = 2) +
  theme_minimal() +
  labs(x = "Days since 10th death",
       y = "Total deaths",
       caption = "Figure: @benbbaldwin | Data: @JHU https://github.com/CSSEGISandData/COVID-19",
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
    data = filter(plot_world, last == 1 & deaths > 5000),
    color = "black",
    nudge_x = -1,
    nudge_y = -0,
    size = 5,
  ) +
  geom_text_repel(
    data = filter(plot_world, last == 1 & deaths < 5000),
    color = "black",
    nudge_x = -2,
    nudge_y = 600,
    size = 5,
  ) +
  annotate("text",x=5, y= 8000, label = paste0("Most recent update:\n",month(recent_world),"-",day(recent_world)), color="red", size=5)

w

ggsave("figures/deaths_since_10.png", dpi=700, width = 16, height = 8)


#make the graphic: us counties
c <- 
  plot_county %>% 
  ggplot(aes(x=days_10, y=deaths, group = country, color = country, label = country)) +
  geom_point(size=4, shape=16) +
  geom_line(size = 2) +
  theme_minimal() +
  labs(x = "Days since 10th death",
       y = "Total deaths",
       caption = "Figure: @benbbaldwin | Data: @JHU https://github.com/CSSEGISandData/COVID-19",
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
    data = filter(plot_county, last == 1 & deaths > 100),
    color = "black",
    nudge_x = -3,
    nudge_y = -0,
    size = 5,
  ) +
  geom_text_repel(
    data = filter(plot_county, last == 1 & deaths < 100),
    color = "black",
    nudge_x = 1,
    nudge_y = 50,
    size = 5,
  ) +
  annotate("text",x=4, y= 300, label = paste0("Most recent update:\n",month(recent_county),"-",day(recent_county)), color="red", size=5)

c

ggsave("figures/deaths_since_10_county.png", dpi=700, width = 16, height = 8)



#u / w

