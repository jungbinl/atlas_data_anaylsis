library(mapproj)
library(ggiraphExtra)
library(readxl)
library(dplyr)
library(tidyverse)
library(plotly)
library(sf)
library(dplyr)
library(wordcloud)
library(showtext)
library(ggnewscale)
library(ggtext)

# election result(city)
data <- read_excel("election_result.xlsx") 

View(data)

data <- rename(data, state = 시도명, city = 선거구명, county = 법정읍면동명, place = 투표구명, candidate = 후보자, votes = 득표수)
data <- data %>% filter(!candidate %in% c("선거인수", "투표수", "무효 투표수", "기권자수")) %>% mutate(state = ifelse(state == "전북특별자치도", "전라북도", state))
data <- data %>% group_by(state, city, candidate) %>% summarise(votes = sum(votes))
data <- data %>% group_by(state, city) %>% filter(votes == max(votes))

data <- data %>%
  mutate(candidate = case_when(
    str_detect(candidate, "국민의힘") ~ "국민의힘",
    str_detect(candidate, "더불어민주당") ~ "더불어민주당",
    TRUE ~ "무소속"
  ))

data <- data %>%
  mutate(city = str_replace(city, "(시|군|구).*", "\\1"))

data <- data %>% group_by(state, city, candidate) %>% summarise(votes = sum(votes))
data_per_city <- data %>% group_by(state, city) %>% filter(votes == max(votes))

#election result(state) with map
data_per_state <- data_per_city %>% group_by(state, candidate) %>% summarise(votes = sum(votes))
data_per_state <- data_per_state %>% group_by(state) %>% summarise(votes = min(votes) / max(votes) * 100, candidate = candidate[which.max(votes)] )

data_per_state <- data_per_state %>% 
  mutate(color = case_when(
    str_detect(candidate, "국민의힘") ~ rgb(1, 0, 0, votes / 100),
    str_detect(candidate, "더불어민주당") ~ rgb(0, 0, 1, 1),        
    TRUE ~ rgb(1, 1, 1, 1)                                          
  ))

map <- st_read("ctprvn.shp")

map$CTP_KOR_NM <- iconv(map$CTP_KOR_NM,
                    from = 'CP949',
                    to = 'UTF-8',
                    sub = NA,
                    mark = T,
                    toRaw = F)

# merge and visualization
map <- map %>%
  left_join(data_per_state, by = c("CTP_KOR_NM" = "state"))

# font
font_add_google(name = "Black Han Sans", family = "a")
showtext_auto()

# map
p <- ggplot(map) +
  geom_sf(aes(fill = color,
              text = paste0("State: ", CTP_KOR_NM, "\nCandidate: ", candidate)), color = "black") +
  scale_fill_identity(guide = "legend",
                      breaks = c("red", "blue", "white"),
                      labels = c("국민의힘", "더불어민주당", "무소속"),
                      name = "정당") + 
  labs(title = "2024 South Korean National Assembly Election Results", subtitle = "<span style='color:red'>■</span> : People Power Party &nbsp;&nbsp;
                <span style='color:blue'>■</span> : Democratic Party &nbsp;&nbsp;
                <span style='color:gray'>■</span> : Independent", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.position = "left",
        plot.title = element_text(family = "a", 
                                  face = "bold",  
                                  size = 16,  
                                  hjust = 0.5),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5),
        panel.grid = element_blank())

# map with interactive
ggplotly(p, tooltip = "text")
