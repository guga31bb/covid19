source("clean_data.R")

#get_data is called from clean_data.R
#argument is how many days to keep after 10th death
all <- get_data(40) 

#check which states/countries/counties have the highest death count
#keep the top X to show in plot. and also south korea for the country one
u <- all %>% filter(type == 'us') %>%
  keep_top(10) %>%
  make_figure('nyt')
w <- all %>% filter(type == 'world') %>%
  keep_top(10) %>%
  make_figure('jhu')
c <- all %>% filter(type == 'county') %>%
  keep_top(6) %>%
  make_figure('nyt')

#last nyt update
all %>% filter(type =='us') %>% select(date) %>% arrange(date) %>% tail(1)
#last jhu update
all %>% filter(type =='world') %>% select(date) %>% arrange(date) %>% tail(1)

u
ggsave("figures/deaths_since_10_states.png", dpi=700, width = 16, height = 8)

c
ggsave("figures/deaths_since_10_county.png", dpi=700, width = 16, height = 8)

w
ggsave("figures/deaths_since_10.png", dpi=700, width = 16, height = 8)
