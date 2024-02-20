################################################################################
# Entrypoint of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:30:40
################################################################################

library(shiny)
library(bslib)
library(dplyr)
library(bsicons)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggthemes)
library(rsconnect)

cra_color = "#4e79a7"
msl_color = "#d27dcf"

barrier_red = "#e05759"
rs_blue = "#4d78a5"

heatmap <- read.csv("heatmap.csv") %>% 
  filter(!is.na(STUDY_NUM)) %>% 
  filter(year > 2000) %>% 
  mutate(Date = as.Date(paste0(year,"-",month,"-","01"),"%Y-%b-%d"))
glmm_window_results <- read.csv("glmm_window_results.csv")
glmm_decay_results <-  read.csv("glmm_decay_output.csv")
glmm_recruitment   <-  read.csv("recruitment_glmm_df.csv")
ascent_heatmap     <-  read.csv("ascent_heatmap_final.csv",check.names = F)

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



# Make layout elements
studies = c(unique(ascent_heatmap$STUDY_NUM))
CRM_func = c("CRA", "MSL")
topics = c("Barrier", "Recruitment Strategy")

sidebar_content <-
  list(
    selectInput("studies",
                "Select Study(s)",
                choices = studies,
                selected = "M18-969",
                multiple  = FALSE),
    selectInput("crm_func",
                "Select CRM Function(s)",
                choices = CRM_func,
                selected = "CRA",
                multiple  = TRUE),
    selectInput("topics",
                "Select Topic(s)",
                choices = topics,
                selected = "Barrier",
                multiple  = TRUE)
  )


