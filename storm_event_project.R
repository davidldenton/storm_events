library(rvest)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(plotly)
library(maps)
library(scales)

url <- ("https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/")

file_paths <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(header = FALSE) %>%
  extract2(1) %>%
  mutate(path = paste0(url, X2), 
         year = str_sub(str_extract(X2, "d[0-9]{4}"), 2, 5)) %>%
  filter(str_detect(X2, "details"), year >= 1996 & year <= year(Sys.Date() - years(1))) %>%
  pull(path)

master_dat <- map_df(file_paths, read_csv, col_types = cols(.default = "c"))

percent_func <- function(x, digits = 1, format = 'f') {
  paste0(formatC(100 * x, format = format, digits = digits), "%")
}


dat <- master_dat %>% 
  select(YEAR, STATE, EVENT_ID) %>%
  filter(STATE %in% toupper(state.name)) %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  group_by(STATE, YEAR) %>%
  summarise(STORM_COUNT = n()) %>%
  ungroup() %>%
  group_by(STATE) %>%
  mutate(FLAG = ifelse(YEAR %in% sort(unique(YEAR))[1:round(length(unique(YEAR))/2,0)],
                       'avg_annual_storms1','avg_annual_storms2')) %>%
  group_by(STATE, FLAG) %>%
  summarise(avg_storm_count = sum(STORM_COUNT)/n_distinct(YEAR)) %>%
  spread(FLAG, avg_storm_count, fill = 0) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  mutate(pct_delta = avg_annual_storms2/avg_annual_storms1 - 1, pct_delta_disp = percent_func(pct_delta),
         state_name = state.name[match(STATE, toupper(state.name))],
         region = tolower(state_name),
         hover = paste(paste0(state_name, ':'), pct_delta_disp, sep = ' '))

us <- map_data("state")

gg <- ggplot() + 
  geom_map(data = us, map = us, aes(x = long, y = lat, map_id = region), fill = "#ffffff", 
           color = "#ffffff", size = 0.15) +
  geom_map(data = dat, map = us, aes(fill = pct_delta, map_id = region, text = hover), 
           color = "#ffffff", size = 0.15) +
  scale_fill_gradientn(colours = c("red", "yellow", "white", "lightblue", "darkblue"), 
                       values = scales::rescale(c(min(dat$pct_delta), -.01, 0, .01, max(dat$pct_delta))), 
                       space = "Lab", na.value = "grey50", guide = "colourbar", name = '% change') + 
  coord_fixed(1.3) + 
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), 
        axis.text = element_blank())

ggp <- ggplotly(gg, tooltip = c("text"), width = 500, height = '100%')