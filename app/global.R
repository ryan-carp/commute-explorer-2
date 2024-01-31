################################################################################
# Entrypoint of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:30:40
################################################################################

library(tidyr)
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)
library(DT)
library(plotly)
library(zoo)

cra_color = "#4e79a7"
msl_color = "#d27dcf"

barrier_red = "#e05759"
rs_blue = "#4d78a5"



#setwd("C:/Users/BRAKSAX/Documents/site-engagement/ASCENT app")


heatmap <-             read.csv("~/R/commute-explorer-2/heatmap.csv") %>% 
  filter(!is.na(STUDY_NUM)) %>% 
  filter(year > 2000) %>% 
  mutate(Date = as.Date(paste0(year,"-",month,"-","01"),"%Y-%b-%d"))
glmm_window_results <- read.csv("~/R/commute-explorer-2/glmm_window_results.csv")
glmm_decay_results <-  read.csv("~/R/commute-explorer-2/glmm_decay_output.csv")
glmm_recruitment   <-  read.csv("~/R/commute-explorer-2/recruitment_glmm_df.csv")
ascent_heatmap     <-  read.csv("~/R/commute-explorer-2/ascent_heatmap_final.csv",check.names = F)

min_msl_df = heatmap %>% filter(event=="MSL") %>%
  filter(cnt>0) %>% 
  group_by(STUDY_NUM)%>%
  summarise(min_msl = min(Date)) %>% 
  ungroup()

min_cra_df = heatmap %>% filter(event=="CRA") %>%
  filter(cnt>0) %>% 
  group_by(STUDY_NUM)%>%
  summarise(min_cra = min(Date)) %>% 
  ungroup()

min_screens_df = heatmap %>% filter(event=="screens") %>%
  filter(cnt>0) %>% 
  group_by(STUDY_NUM)%>%
  summarise(min_screen = min(Date)) %>% 
  ungroup()

print(heatmap)

max_date_df = heatmap %>% 
  group_by(STUDY_NUM) %>%
  #summarise(max_date=min(as.Date(LAST_SUBJECT_SCREENED_DATE),
  #                    Sys.Date(),max(Date,na.rm=T),na.rm=T
  #       )
  #) %>%
  summarise(max_date = Sys.Date()) %>% 
  ungroup()


min_date = min_msl_df %>% full_join(min_cra_df) %>% full_join(min_screens_df) %>%
  group_by(STUDY_NUM) %>% 
  mutate(`Min Date` = max(min_msl,min_cra,min_screen,na.rm=T)) %>%
  select(STUDY_NUM,`Min Date`) %>% 
  ungroup()

swimlane_input = min_msl_df %>% 
  full_join(min_cra_df) %>% 
  full_join(min_screens_df) %>%
  full_join(max_date_df) %>% 
  group_by(STUDY_NUM) %>%
  mutate(min_date = min(min_msl,min_cra,min_screen,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = !c(STUDY_NUM,min_date),names_to = "Event",values_to = "Date")

min_date = min_msl_df %>% full_join(min_cra_df) %>% full_join(min_screens_df) %>%
  group_by(STUDY_NUM) %>% 
  mutate(`Min Date` = max(min_msl,min_cra,min_screen,na.rm=T)) %>%
  select(STUDY_NUM,`Min Date`) %>% 
  ungroup()

swimlane_input = min_msl_df %>% 
  full_join(min_cra_df) %>% 
  full_join(min_screens_df) %>%
  full_join(max_date_df) %>% 
  group_by(STUDY_NUM) %>%
  mutate(min_date = min(min_msl,min_cra,min_screen,na.rm=T)) %>% 
  ungroup() %>% 
  pivot_longer(cols = !c(STUDY_NUM,min_date),names_to = "Event",values_to = "Date")


library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(echarts4r)
library(glue)
library(sf)
library(mapboxer)
library(reactable)
library(shinyWidgets)



# Config ------------------------------------------------------------------

MAPBOX_TOKEN <- "pk.eyJ1IjoiYWFyb242NjYiLCJhIjoiY2o2M2NmZmJ4MWc0ZDJxbnI3dmZ5OXB2MSJ9.T3trJ5Tu66Kw-w-1ELpzBA"
F_DATA_MODEL <- "data/data-model.rds"



# Colors and themes -------------------------------------------------------

ECHARTS_THEME <- "auritus"

COLOR_BLUE <- "#00a2eb"
COLOR_GREEN <- "#adb514"
COLOR_ORANGE <- "#fd9f02"
COLOR_PINK <- "#ce2c78"
COLOR_RED <- "#d32d05"
COLOR_PURPLE <- "#7522b8"
COLOR_GREY <- "#ffffffaa"
COLOR_DARK_GREY <- "#888888aa"
COLOR_WHITE <- "#eee"


# Data --------------------------------------------------------------------

# load data model
data_model <- readRDS(F_DATA_MODEL)

# extract model components
SF_SHAPE <- data_model$shape
D_COMMUTE <- data_model$d_commute
D_LOOKUP <- data_model$d_lookup
D_LOOKUP_REGION <- data_model$d_lookup_region
MAP_SRC <- as_mapbox_source(SF_SHAPE)



# App state ---------------------------------------------------------------

# We implement a state machine to capture the views of the app. The application 
# state is stored in a reactive list which is shared across all Shiny modules.

# app states
STATE_NOTHING_SELECTED <- 1
STATE_MB_SELECTED <- 2
STATE_BUCKET_SELECTED <- 3

# initial state values
INITIAL_DIRECTION <- "depart"
INITIAL_REGION <- "Auckland Region"
INITIAL_DATA_SOURCE <- "work"

# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")
source("utils/app-logic.R")



# Modules -----------------------------------------------------------------

source("modules/mod_commute_map.R")
source("modules/mod_commute_mode.R")
source("modules/mod_commute_table.R")
source("modules/mod_commute_filter.R")
