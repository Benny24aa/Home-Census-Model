library(shinyWidgets)
library(zoo)
library(leaflet)
library(sf)
library(shinyjs)
library(rsconnect)
library(curl)
library(httr)
library(openssl)
library(shinycssloaders)
library(ISOweek)
library(janitor)
library(sf)
library(shiny)
library(glue)
library(DBI)
library(odbc)
library(Metrics)
library(jsonlite)
library(phsopendata)
library(tidyr)
library(dplyr)


Population_Projections <- get_resource(res_id = "7a9e74c9-8746-488b-8fba-0fad7c7866ea") %>% 
  mutate(
    `65-74`    = rowSums(select(., Age65:Age74), na.rm = TRUE),
    `75-84`  = rowSums(select(., Age75:Age84), na.rm = TRUE),
    `85 plus`  = rowSums(select(., Age85:Age90plus), na.rm = TRUE)
  ) %>% 
  select(-starts_with("Age")) %>% 
  pivot_longer(
    cols = c(`65-74`, `75-84`, `85 plus`), #### The limitation here is we can't break the data up in 95 +, so the age bracket will be 85 + and aggregate others row together to account for this
    names_to = "Age",
    values_to = "Population"
  ) %>% 
  select(Year, Sex, Age, Population)


Care_Home_Census_Health_Characteristics <- get_resource(res_id = "92ebf3df-2af4-4d73-9397-f5d6a6778da7")

Care_Home_Census_Demographic_Characteristics <- get_resource(res_id = "39d2b480-2990-46a2-bd58-96aac41a032a")
