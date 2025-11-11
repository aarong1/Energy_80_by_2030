# source: all ireland adequacy assesment

library(tidyverse)
library(echarts4r)

renewables <- tibble::tibble(
year_end = c("2025","2026","2027","2028","2029","2030","2031","2032","2033","2034"),
wind = c(1435, 1485, 1535, 1635, 1810, 2485, 2660, 2835, 3010, 3185),
Solar_PV = c(250,300,350,400,450,500,550,600,650,700),
Biomass_biogas = c(250,300,350,400,450,500,550,600,650,700),
dunno = c(79,79,79,79,79,79,79,79,79,79),
Renewable_chp = c(3,3,3,3,3,3,3,3,3,3),
hydro = c(6,6,6,6,6,6,6,6,6,6)
)

# Convert year_end to numeric for proper plotting
renewables <- renewables %>%
  mutate(year_end = as.numeric(year_end))

# Display the data
print("Renewables Capacity Data:")
print(renewables)

# 1. Stacked Area Chart - All Renewable Sources
(stacked_chart <- renewables %>%
  e_charts(year_end) %>%
  #e_area(Renewable_chp, name = "Renewable CHP", stack = "renewable") %>%
  #e_area(hydro, name = "Hydro", stack = "renewable") %>%
  e_area(wind, name = "Wind", stack = "renewable") %>%
  e_area(Solar_PV, name = "Solar PV", stack = "renewable") %>%
  e_area(Biomass_biogas, name = "Biomass/Biogas", stack = "renewable") %>%
  e_area(dunno, name = "Other", stack = "renewable") %>%
    e_title("Renewable Energy Capacity Trends by Source",
            subtext = 'Not obvious: HYDRO and CHP constant at 6 MW and 3 MW repectively',
            link = 'https://www.gov.uk/government/statistics/solar-photovoltaics-deployment'
    ) %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
  e_tooltip(trigger = "axis", backgroundColor = 'white') %>%
    #e_labels(formatter= '{a} {@[1]}') |> 
  e_theme("infographic") |> 
    e_x_axis(type = 'category')
)
# Display stacked chart
stacked_chart

# 2. Line Chart - Individual Renewable Sources
line_chart <- renewables %>%
  e_charts(year_end) %>%
  e_line(wind, name = "Wind") %>%
  e_line(Solar_PV, name = "Solar PV") %>%
  e_line(Biomass_biogas, name = "Biomass/Biogas") %>%
  e_line(dunno, name = "Other") %>%
  e_line(Renewable_chp, name = "Renewable CHP") %>%
  e_line(hydro, name = "Hydro") %>%
  e_title("Renewable Energy Capacity Trends by Source",
          subtext = 'Not obvious: hydro and chp constant at 6MW and 3MW',
            link = 'https://www.gov.uk/government/statistics/solar-photovoltaics-deployment'
            ) %>%
  e_legend(orient = "horizontal", bottom = 0) %>%
  e_tooltip(trigger = "axis", backgroundColor='white') %>%
  e_theme("green") |> 
  e_x_axis(type = 'category')

# Display line chart
line_chart

# 3. Wind Focus Chart (since it's the dominant source)
wind_chart <- renewables %>%
  e_charts(year_end) %>%
  e_line(wind, name = "Wind Capacity", symbol = "circle", symbolSize = 8) %>%
  e_title("Wind Energy Capacity Growth") %>%
  e_tooltip(trigger = "axis") %>%
  e_theme("green") |>
  e_y_axis(name = 'GW') |> 
  e_x_axis(type = 'category')

# Display wind chart
wind_chart

# 4. Total Renewable Capacity
renewables_with_total <- renewables %>%
  mutate(total_renewable = wind + Solar_PV + Biomass_biogas + dunno + Renewable_chp + hydro)

total_chart <- renewables_with_total %>%
  e_charts(year_end) %>%
  e_line(total_renewable, name = "Total Renewable Capacity", symbol = "circle", symbolSize = 8) %>%
  e_title("Total Renewable Energy Capacity") %>%
  e_tooltip(trigger = "axis",backgroundColor = 'white') %>%
  e_theme("green") |> 
  e_axis(type = 'category')

# Display total chart
total_chart

# Summary statistics
print("\nSummary Statistics:")
print(paste("Total renewable capacity in 2025:", sum(renewables[1, 2:7]), "MW"))
print(paste("Total renewable capacity in 2034:", sum(renewables[10, 2:7]), "MW"))
print(paste("Growth from 2025 to 2034:", 
            round((sum(renewables[10, 2:7]) - sum(renewables[1, 2:7])) / sum(renewables[1, 2:7]) * 100, 1), "%"))



