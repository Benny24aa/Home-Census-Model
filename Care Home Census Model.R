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



Demographic_Characteristics <- get_resource(res_id = "39d2b480-2990-46a2-bd58-96aac41a032a")%>% 
  dplyr::filter(
    MainClientGroup == "Older People Aged 65 and Older", # Only looking at care home for older people
    Sector == "All Sectors",
    CA == "S92000003",
    stringr::str_detect(KeyStatistic, "Long Stay Residents Aged")
  ) %>% 
  dplyr::mutate(
    AgeGroup = stringr::str_extract(KeyStatistic, "\\d{2}-\\d{2}|95 and Older"),
    Sex = dplyr::case_when(
      stringr::str_detect(KeyStatistic, "Male") ~ "Male",
      stringr::str_detect(KeyStatistic, "Female") ~ "Female"
    )
  ) %>% 
  dplyr::filter(AgeGroup != "18-64") %>% 
  select(Date, Value, AgeGroup, Sex) %>% 
  dplyr::mutate(
    Year = substr(as.character(Date), 1, 4))


Residents <- Demographic_Characteristics %>%
  mutate(
    Age = case_when(
      AgeGroup %in% c("85-94", "95 and Older") ~ "85 plus",
      TRUE ~ AgeGroup
    )
  ) %>%
  group_by(Year, Age, Sex) %>%
  summarise(Residents = sum(Value)) %>% 
  mutate(Year = as.numeric(Year))


Utilisation <- Residents %>%
  left_join(Population_Projections, by = c("Year", "Age", "Sex"))
 


Utilisation <- Utilisation %>%
  mutate(
    UtilisationRate = Residents / Population
  ) %>% 
  ungroup()

LatestRates <- Utilisation %>%
  filter(Year == "2022")%>%
  select(Age, Sex, UtilisationRate)

Forecast <- Population_Projections %>%
  left_join(LatestRates, by = c("Age", "Sex")) %>%
  mutate(
    ProjectedResidents = Population * UtilisationRate
  ) %>% 
  filter(Sex != "All")

library(plotly)

p1 <- Forecast %>%
  filter(Age == "65-74") %>%
  plot_ly(
    x = ~Year,
    y = ~ProjectedResidents,
    color = ~Sex,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
  layout(
    title = "Projected Care Home Residents (Age 65–74)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Projected Residents")
  )

p1

p2 <- Forecast %>%
  filter(Age == "75-84") %>%
  plot_ly(
    x = ~Year,
    y = ~ProjectedResidents,
    color = ~Sex,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
  layout(
    title = "Projected Care Home Residents (Age 75–84)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Projected Residents")
  )

p2

p3 <- Forecast %>%
  filter(Age == "85 plus") %>%
  plot_ly(
    x = ~Year,
    y = ~ProjectedResidents,
    color = ~Sex,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
  layout(
    title = "Projected Care Home Residents (Age 85+)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Projected Residents")
  )

p3


actuals <- Residents %>% 
  group_by(Year)%>%
  summarise(
   Residents = sum(Residents, na.rm = TRUE)
  )

Forecast_All <- Forecast %>%
  group_by(Year) %>%
  summarise(
    ProjectedResidents = sum(ProjectedResidents, na.rm = TRUE)
  )

p_total <- plot_ly(
  Forecast_All,
  x = ~Year,
  y = ~ProjectedResidents,
  type = "scatter",
  mode = "lines+markers"
) %>%
  layout(
    title = "Projected Demand for Care Home Residents (Age 65+)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Projected Residents")
  )

p_total

Forecast_Validate <- Forecast_All %>% 
  left_join(actuals, by = "Year")

Forecast_Plot <- Forecast_Validate %>%
  select(Year, ProjectedResidents, Residents) %>%
  pivot_longer(
    cols = c(ProjectedResidents, Residents),
    names_to = "Series",
    values_to = "Value"
  ) %>%
  mutate(
    Series = recode(Series,
                    "Residents" = "Actual",
                    "ProjectedResidents" = "Forecast")
  )

library(plotly)

plot_ly(
  Forecast_Plot,
  x = ~Year,
  y = ~Value,
  color = ~Series,
  type = "scatter",
  mode = "lines+markers"
) %>%
  layout(
    title = "Actual vs Forecast Care Home Residents (Age 65+)",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Residents")
  )


Registered_Places <- get_resource(res_id = "04958b74-a351-4dc0-b8e4-cbc369372804") %>%
  filter(
    KeyStatistic == "Number of Registered Places",
    MainClientGroup == "Older People Aged 65 and Older",
    Sector == "All Sectors",
    CA == "S92000003"
  ) %>%
  mutate(
    Year = as.numeric(substr(as.character(Date),1,4))
  ) %>%
  select(Year, Value) %>%
  rename(RegisteredPlaces = Value)

CurrentPlaces <- Registered_Places %>%
  filter(Year == 2022) %>%
  pull(RegisteredPlaces)