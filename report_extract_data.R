# Extract and structure energy data into data frames
library(tidyverse)
library(echarts4r)

# Based on the data structure, it appears to contain TER (Total Energy Requirement) data
# for Ireland, Northern Ireland, and All-Island with growth percentages

# Main energy consumption data (TER values and growth rates)
calendar_year_ter_mwhr <- tibble(
  year = 2023:2034,
  calendar_yr_ter_ireland_ter = c(34.0, 35.1, 35.7, 36.6, 37.4, 38.0, 38.6, 39.2, 39.9, 40.6, 41.3, 42.0),
  calendar_yr_ter_ireland_growth = c(NA, 3.0, 1.9, 2.4, 2.2, 1.6, 1.6, 1.5, 1.8, 1.8, 1.7, 1.7),
  calendar_yr_ter_ni_ter = c(8.09, 7.95, 8.02, 8.14, 8.34, 8.53, 8.72, 8.92, 9.13, 9.34, 9.56, 9.77),
  calendar_yr_ter_ni_growth = c(NA, -1.7, 0.8, 1.5, 2.4, 2.3, 2.3, 2.2, 2.3, 2.3, 2.3, 2.3),
  calendar_yr_ter_island_ter = c(42.1, 43.0, 43.7, 44.7, 45.7, 46.5, 47.3, 48.1, 49.0, 49.9, 50.9, 51.8),
  calendar_yr_ter_island_growth = c(NA, 2.1, 1.7, 2.3, 2.2, 1.8, 1.7, 1.6, 1.9, 1.9, 1.9, 1.8)
)

# Additional data points (appears to be additional metrics)
ter_peak_gw <- tibble(
  year = 2023:2034,
  ter_peak_ireland = c(5.75, 5.77, 5.85, 5.93, 6.00, 6.04, 6.06, 6.09, 6.14, 6.19, 6.25, 6.31),
  ter_peak_ni = c(1.6, 1.49, 1.49, 1.53, 1.57, 1.61, 1.64, 1.66, 1.70, 1.73, 1.76, 1.79),
  ter_peak_island = c(7.35, 7.26, 7.39, 7.50, 7.61, 7.69, 7.74, 7.78, 7.85, 7.92, 7.98, 8.06),
)
# Final data points
transmission_peak_gw <- tibble(
  year = 2023:2034,
  transmission_peak_ireland = c(5.64, 5.65, 5.74, 5.82, 5.88, 5.93, 5.95, 5.98, 6.03, 6.08, 6.14, 6.20),
  transmission_peak_ni = c(1.57, 1.45, 1.46, 1.49, 1.53, 1.57, 1.60, 1.63, 1.66, 1.69, 1.72, 1.75),
  transmission_peak_island = c(7.21, 7.11, 7.24, 7.35, 7.45, 7.54, 7.59, 7.64, 7.71, 7.76, 7.84, 7.90)
)

# Create Low scenario dataset
low_scenario_data <- calendar_year_ter_mwhr %>%
  left_join(ter_peak_gw, by = "year") %>%
  left_join(transmission_peak_gw, by = "year") %>%
  mutate(scenario = "Low")

# HIGH SCENARIO DATA
# High scenario TER data (from the additional raw data)
high_calendar_year_ter_mwhr <- tibble(
  year = 2023:2034,
  calendar_yr_ter_ireland_ter = c(34.0, 36.4, 39.2, 41.9, 44.1, 45.7, 47.2, 48.5, 49.8, 50.9, 51.9, 53.0),
  calendar_yr_ter_ireland_growth = c(NA, 6.8, 7.8, 6.9, 5.2, 3.7, 3.3, 2.8, 2.6, 2.2, 2.0, 2.0),
  calendar_yr_ter_ni_ter = c(8.10, 8.53, 8.68, 8.94, 9.49, 9.81, 10.15, 10.49, 10.84, 11.29, 11.61, 11.92),
  calendar_yr_ter_ni_growth = c(NA, 5.2, 1.8, 3.0, 6.2, 3.3, 3.5, 3.3, 3.4, 4.1, 2.8, 2.7),
  calendar_yr_ter_island_ter = c(42.1, 44.9, 47.9, 50.8, 53.5, 55.5, 57.4, 59.0, 60.7, 62.2, 63.6, 64.9),
  calendar_yr_ter_island_growth = c(NA, 6.5, 6.6, 6.2, 5.4, 3.6, 3.4, 2.9, 2.8, 2.5, 2.2, 2.1)
)

# High scenario peak data
high_ter_peak_gw <- tibble(
  year = 2023:2034,
  ter_peak_ireland = c(5.76, 6.08, 6.39, 6.67, 6.87, 7.03, 7.15, 7.25, 7.36, 7.45, 7.56, 7.66),
  ter_peak_ni = c(1.60, 1.71, 1.73, 1.78, 1.87, 1.92, 1.98, 2.02, 2.08, 2.14, 2.19, 2.24),
  ter_peak_island = c(7.36, 7.79, 8.06, 8.39, 8.67, 8.89, 9.05, 9.17, 9.30, 9.43, 9.56, 9.70)
)

# High scenario transmission peak data
high_transmission_peak_gw <- tibble(
  year = 2023:2034,
  transmission_peak_ireland = c(5.64, 5.97, 6.28, 6.56, 6.75, 6.92, 7.03, 7.14, 7.25, 7.34, 7.44, 7.55),
  transmission_peak_ni = c(1.57, 1.67, 1.69, 1.75, 1.83, 1.89, 1.94, 1.98, 2.04, 2.11, 2.16, 2.21),
  transmission_peak_island = c(7.21, 7.65, 7.90, 8.23, 8.52, 8.74, 8.89, 9.03, 9.16, 9.27, 9.42, 9.55)
)

# Create High scenario dataset
high_scenario_data <- high_calendar_year_ter_mwhr %>%
  left_join(high_ter_peak_gw, by = "year") %>%
  left_join(high_transmission_peak_gw, by = "year") %>%
  mutate(scenario = "High")

# Combine both scenarios
all_scenarios_data <- bind_rows(low_scenario_data, high_scenario_data)

# MEDIAN SCENARIO DATA
# Median scenario TER data (from the additional raw data)
median_calendar_year_ter_mwhr <- tibble(
  year = 2023:2034,
  calendar_yr_ter_ireland_ter = c(34.0, 35.8, 37.7, 39.5, 41.1, 42.4, 43.5, 44.7, 46.0, 47.2, 48.4, 49.5),
  calendar_yr_ter_ireland_growth = c(NA, 5.3, 5.2, 4.7, 4.1, 3.1, 2.8, 2.6, 2.9, 2.7, 2.5, 2.4),
  calendar_yr_ter_ni_ter = c(8.09, 8.21, 8.33, 8.57, 9.05, 9.31, 9.58, 9.83, 10.11, 10.47, 10.71, 10.94),
  calendar_yr_ter_ni_growth = c(NA, 1.6, 1.4, 2.8, 5.7, 2.9, 2.9, 2.5, 2.9, 3.5, 2.3, 2.2),
  calendar_yr_ter_island_ter = c(42.1, 44.1, 46.1, 48.1, 50.2, 51.7, 53.1, 54.5, 56.1, 57.7, 59.1, 60.5),
  calendar_yr_ter_island_growth = c(NA, 4.6, 4.5, 4.4, 4.4, 3.0, 2.8, 2.6, 2.9, 2.8, 2.5, 2.3)
)

# Median scenario peak data
median_ter_peak_gw <- tibble(
  year = 2023:2034,
  ter_peak_ireland = c(5.76, 5.95, 6.13, 6.32, 6.46, 6.57, 6.64, 6.72, 6.83, 6.92, 7.03, 7.12),
  ter_peak_ni = c(1.60, 1.59, 1.61, 1.67, 1.75, 1.79, 1.84, 1.87, 1.91, 1.95, 1.99, 2.04),
  ter_peak_island = c(7.36, 7.54, 7.72, 7.97, 8.19, 8.33, 8.45, 8.53, 8.66, 8.79, 8.90, 9.03)
)

# Median scenario transmission peak data
median_transmission_peak_gw <- tibble(
  year = 2023:2034,
  transmission_peak_ireland = c(5.64, 5.83, 6.02, 6.21, 6.35, 6.46, 6.53, 6.61, 6.71, 6.81, 6.91, 7.01),
  transmission_peak_ni = c(1.57, 1.56, 1.57, 1.63, 1.71, 1.76, 1.80, 1.84, 1.88, 1.91, 1.96, 2.00),
  transmission_peak_island = c(7.21, 7.39, 7.57, 7.82, 8.04, 8.18, 8.30, 8.39, 8.52, 8.63, 8.76, 8.88)
)

# Create Median scenario dataset
median_scenario_data <- median_calendar_year_ter_mwhr %>%
  left_join(median_ter_peak_gw, by = "year") %>%
  left_join(median_transmission_peak_gw, by = "year") %>%
  mutate(scenario = "Median")

# Combine all three scenarios
all_scenarios_data <- bind_rows(low_scenario_data, high_scenario_data, median_scenario_data)

# Display the data
print("Low Scenario Data:")
print(low_scenario_data)
print("\nHigh Scenario Data:")
print(high_scenario_data)
print("\nMedian Scenario Data:")
print(median_scenario_data)




# Create ECharts4R visualizations with minimal styling

# 1. TER Comparison Chart - All Three Scenarios
ter_chart <- all_scenarios_data %>%
  select(year, scenario, calendar_yr_ter_ireland_ter, calendar_yr_ter_ni_ter, calendar_yr_ter_island_ter) %>%
  pivot_longer(cols = starts_with("calendar"), names_to = "region", values_to = "ter") %>%
  mutate(
    region = case_when(
      region == "calendar_yr_ter_ireland_ter" ~ "Ireland",
      region == "calendar_yr_ter_ni_ter" ~ "Northern Ireland",
      region == "calendar_yr_ter_island_ter" ~ "All-Island"
    ),
    series_name = paste(region, scenario, sep = " - ")
  ) %>%
  filter(region == 'Northern Ireland') |> 
  group_by(series_name) %>%
  e_charts(year) %>%
  e_line(ter, bind = series_name) %>%
  e_title("Total Energy Requirement (TER) - Low vs Median vs High Scenarios") %>%
  e_legend(orient = "horizontal", bottom = 0) %>%
  e_tooltip(trigger = "axis") %>%
  e_theme("infographic") |> 
  e_x_axis(type = 'category')

# Display TER chart
ter_chart

# 2. Peak Demand Chart - All Three Scenarios
(peak_chart <- all_scenarios_data %>%
  select(year, scenario, ter_peak_ireland, ter_peak_ni, ter_peak_island) %>%
  pivot_longer(cols = starts_with("ter_peak"), names_to = "region", values_to = "peak_demand") %>%
  mutate(
    region = case_when(
      region == "ter_peak_ireland" ~ "Ireland",
      region == "ter_peak_ni" ~ "Northern Ireland", 
      region == "ter_peak_island" ~ "All-Island"
    ),
    series_name = paste(region, scenario, sep = " - ")
  ) %>% 
    filter(region == 'Northern Ireland') %>%  # Remove NA values
    
  group_by(series_name) %>%
  e_charts(year) %>%
  e_x_axis(type= 'category',min = '2023') |>
  e_line(peak_demand, bind = series_name, shape = 'circle',SymbolSize = 0) %>%
  e_title("Peak Demand (GW) - Low vs Median vs High Scenarios") %>%
  e_legend(orient = "horizontal", bottom = 0) %>%
  e_tooltip(trigger = "axis",  backgroundColor= "white") %>%
  e_theme("green"))

# Display peak chart
peak_chart

# 3. Growth Rates Comparison - All Three Scenarios
(growth_chart <- all_scenarios_data %>%
  select(year, scenario, calendar_yr_ter_ireland_growth, calendar_yr_ter_ni_growth, calendar_yr_ter_island_growth) %>%
  filter(year > 2023) %>%  # Remove NA values
    
  mutate(as.Date(year)) %>%
  pivot_longer(cols = ends_with("_growth"), names_to = "region", values_to = "growth_rate") %>%
  mutate(
    region = case_when(
      region == "calendar_yr_ter_ireland_growth" ~ "Ireland",
      region == "calendar_yr_ter_ni_growth" ~ "Northern Ireland",
      region == "calendar_yr_ter_island_growth" ~ "All-Island"
    ),
    series_name = paste(region, scenario, sep = " - ")
  ) %>%
  filter(region == 'Northern Ireland') %>%  # Remove NA values
  group_by(series_name) %>%
  e_charts(year) %>%
  e_x_axis(type= 'category',min = '2023') |> 
  e_line(growth_rate, bind = series_name) %>%
  e_title("TER Growth Rates (%) - Low vs Median vs High Scenarios") %>%
  e_legend(orient = "horizontal", bottom = 0) %>%
  e_tooltip(trigger = "axis", backgroundColor= "white")
 %>%
  e_theme("green"))

# Display growth chart
growth_chart

# 4. Scenario Comparison for All-Island Only (simplified view)
(
all_island_comparison <- all_scenarios_data %>%
  select(year, scenario, calendar_yr_ter_island_ter) %>%
  pivot_wider(names_from = scenario, values_from = calendar_yr_ter_island_ter) %>%
  e_charts(year) %>%
  e_line(Low, name = "Low Scenario") %>%
  e_line(Median, name = "Median Scenario") %>%
  e_line(High, name = "High Scenario") %>%
  e_title("All-Island TER Scenarios Comparison") %>%
  e_x_axis(type= 'category',min = '2023') |> 
  e_legend(orient = "horizontal", bottom = 0) %>%
  e_tooltip(trigger = "axis") %>%
  e_theme("minimal")
)
# Display All-Island comparison
all_island_comparison

