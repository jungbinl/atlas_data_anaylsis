---
title: "Analysis of the 2024 South Korean National Election"
author: "Jungbin Lee"
date: "election_result.xlsx"
output:
  html_document
---

## Summary

This document is based on data from the 2024 South Korean National Assembly election and includes results visualized on a map showing the winning parties by region.

### Packages Loading

#### Load necessary packages for map visualization, data manipulation, and interactive plots
   - library(mapproj) -> Map projections
   - library(ggiraphExtra)  -> Interactive map generation
   - library(readxl) -> Reading Excel files
   - library(dplyr)         -> Data manipulation
   - library(tidyverse)     -> Data wrangling and visualization
   - library(plotly)        -> Interactive plotting
   - library(sf)            -> Handling spatial data
   - library(wordcloud)     -> Wordcloud generation (not used here)
   - library(showtext)      -> Korean font support
   - library(ggnewscale)    -> Extending ggplot color scales
   - library(ggtext)        -> Markdown text in ggplot

### Read election results from the Excel file
data <- read_excel("election_result.xlsx") 

### Rename columns to English and remove irrelevant rows like total voters or invalid votes
data <- rename(data, state = 시도명, city = 선거구명, county = 법정읍면동명, place = 투표구명, candidate = 후보자, votes = 득표수)

data <- data %>% 
  filter(!candidate %in% c("선거인수", "투표수", "무효 투표수", "기권자수")) %>% 
  mutate(state = ifelse(state == "전북특별자치도", "전라북도", state))

### Sum votes by state, city, and candidate
data <- data %>% group_by(state, city, candidate) %>% summarise(votes = sum(votes))

### Filter only the candidate with the maximum votes per city
data <- data %>% group_by(state, city) %>% filter(votes == max(votes))

### Simplify candidate names to party names: People Power Party, Democratic Party, or Independent
data <- data %>%
  mutate(candidate = case_when(
    str_detect(candidate, "국민의힘") ~ "People Power Party",
    str_detect(candidate, "더불어민주당") ~ "Democratic Party",
    TRUE ~ "Independent"
  ))

### Clean city names by removing suffixes like 시, 군, 구
data <- data %>%
  mutate(city = str_replace(city, "(시|군|구).*", "\\1"))

### Group again and sum votes to avoid duplicates
data <- data %>% group_by(state, city, candidate) %>% summarise(votes = sum(votes))
data_per_city <- data %>% group_by(state, city) %>% filter(votes == max(votes))

### Aggregate votes by state and candidate
data_per_state <- data_per_city %>% group_by(state, candidate) %>% summarise(votes = sum(votes))

### Determine winning party per state by comparing vote ratios
data_per_state <- data_per_state %>% 
  group_by(state) %>% 
  summarise(votes = min(votes) / max(votes) * 100, candidate = candidate[which.max(votes)] )

### Assign colors based on the party with transparency proportional to vote share
data_per_state <- data_per_state %>% 
  mutate(color = case_when(
    str_detect(candidate, "People Power Party") ~ rgb(1, 0, 0, votes / 100),  # Red
    str_detect(candidate, "Democratic Party") ~ rgb(0, 0, 1, 1),              # Blue
    TRUE ~ rgb(0.7, 0.7, 0.7, 1)                                             # Gray for Independent
  ))

### Map Loading and Merging

### Load South Korean province boundaries shapefile and convert encoding
map <- st_read("ctprvn.shp")
map$CTP_KOR_NM <- iconv(map$CTP_KOR_NM, from = 'CP949', to = 'UTF-8', sub = NA, mark = TRUE, toRaw = FALSE)

### Join election data with the spatial map data by province name
map <- map %>%
  left_join(data_per_state, by = c("CTP_KOR_NM" = "state"))

### Add Google font 'Black Han Sans' for Korean characters and enable showtext
font_add_google(name = "Black Han Sans", family = "a")
showtext_auto()

### Plot election results by province using ggplot2
The map below shows the winning party by province:

```{r plot-map, echo=FALSE, message=FALSE, warning=FALSE, fig.width=8, fig.height=6}

print(p)

### Interactive Map with Plotly

ggplotly(p, tooltip = "text")

## Results Description
  - This report aggregates the 2024 South Korean National Assembly election results and visualizes the winning party by province on a map.

  - The People Power Party is represented in red, the Democratic Party in blue, and Independents in gray.

  - The interactive map allows users to hover over provinces to see detailed information about the winning party.

  - This visualization helps in understanding regional political trends and election outcomes at a glance.

