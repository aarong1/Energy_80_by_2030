# Energy Data Visualizations in R using ECharts4R
# Minimal styling implementation

library(tidyverse)
library(echarts4r)

# Chart 1: Generation by Type (June 2025)
render_generation_by_type <- function() {
  generation_data <- tibble(
    type = c("Wind", "Biogas", "Biomass", "Solar PV", "Landfill gas", "Other"),
    percentage = c(81.9, 6.8, 5.2, 3.6, 1.6, 0.9),
    volume = c(2561, 214, 161, 112, 49, 29)
  )
  
 ( chart <- generation_data %>%
    e_charts(type) %>%
    e_bar(volume, name = "Volume Generated (GWh)", y_index = 0) %>%
    e_line(percentage, name = "Percentage Contribution", y_index = 1) %>%
    e_y_axis(index = 0, name = "GWh") %>%
    e_y_axis(index = 1, name = "%", max = 100) %>%
    e_title("Generation by Type (June 2025)") %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
    e_tooltip(trigger = "axis") %>%
    e_theme("minimal") %>%
    e_color(c("#184d36", "#e6b800")))
  
  return(chart)
}

# Chart 2: Monthly Electricity Consumption and Renewable Generation (2015-2025)
render_consumption_trends <- function() {
  
  consumption_data <- read.csv('./data/ecrg-figure-3-june-2025.csv')
  
  names(consumption_data) <- c('date','total_consumption','renewable','wind','nonwind')
  
 ( 
  chart <- consumption_data %>%
    e_charts(date) %>%
    e_scatter(total_consumption, name = "Total Consumption", smooth = TRUE) %>%
    e_scatter(renewable, name = "Renewable", smooth = T) %>%
    # e_line(wind, name = "Wind", smooth = TRUE) %>%
    # e_line(nonwind, name = "Non-wind", smooth = TRUE) %>%
    e_title("12 month Electricity Consumption and Renewable Generation (2015–2025)",
            subtext = 'Rolling 12 month volume to March 2025') %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
    e_tooltip(trigger = "axis") %>%
    e_theme("minimal") %>%
    e_color(c("#184d36", "#2bd78d", "#6ad1e3", "#e3c36a")) %>%
    e_y_axis(name = "GWh")) |> 
    e_grid(top='25%')
  
  return(chart)
}

consumption_data <- read.csv('./data/ecrg-figure-3-june-2025.csv')

names(consumption_data) <- c('date','total_consumption','renewable','wind','nonwind')

consumption_data <- consumption_data |> 
  mutate(x = as.numeric(as.Date( consumption_data$date)))

pred <- lm(data = consumption_data,formula = total_consumption ~ x )

plot(as.Date(consumption_data$x), predict.lm(pred),xlim = c(16000, 24000 ),ylim = c(0, 8000 ))

lines(as.Date(consumption_data$x),consumption_data$total_consumption)

l = last(as.numeric(as.Date( consumption_data$date)))

new_x = seq.int(from = l, by = 5, length.out = 500)
# as.Date(seq.int(from = l, by = 5, length.out = 500))
lines(x = as.Date(new_x) ,
  predict.lm(pred,
                 newdata = data.frame(x = new_x ) 
 )
)


# Chart 3: Renewable Generation as % of Consumption (2015-2017 Sample)
render_renewable_percentage <- function() {
  percentage_data <- tibble(
    date = c("2015-03", "2015-06", "2015-12", "2016-03", "2016-06", 
             "2016-12", "2017-03", "2017-05"),
    pct_renewable = c(27.6, 22.2, 38.4, 22.5, 16.2, 36.3, 31.7, 30.9),
    wind_gwh = c(180.7, 118.0, 247.9, 124.4, 66.4, 227.0, 186.7, 143.2),
    nonwind_gwh = c(14.6, 13.9, 22.2, 29.8, 28.2, 32.0, 32.6, 44.6),
    total_renewable = c(195.2, 131.9, 270.1, 154.2, 94.6, 259.0, 219.3, 187.7),
    total_consumption = c(706.6, 593.6, 703.8, 685.3, 582.0, 714.3, 691.7, 607.1)
  )
  
  (chart <- percentage_data %>%
    e_charts(date) %>%
    e_bar(wind_gwh, name = "Wind", stack = "renewable", y_index = 0) %>%
    e_bar(nonwind_gwh, name = "Non-wind", stack = "renewable", y_index = 0) %>%
    e_line(total_renewable, name = "Total Renewable", y_index = 0, 
           lineStyle = list(type = "dashed")) %>%
    e_line(total_consumption, name = "Total Consumption", y_index = 0) %>%
    e_line(pct_renewable, name = "% Renewable", y_index = 1) %>%
    e_y_axis(index = 0, name = "GWh") %>%
    e_y_axis(index = 1, name = "% Renewable", max = 100) %>%
    e_title("Monthly Renewable Generation as % of Consumption (2015–2017 Sample)") %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
    e_tooltip(trigger = "axis") %>%
    e_theme("minimal") %>%
    e_color(c("#6ad1e3", "#e3c36a", "#2bd78d", "#184d36", "#e6b800"))
  )
  return(chart)
}

# Chart 4: Renewables Capacity Forecast (2025-2034)
render_renewables_forecast <- function() {
  renewables_data <- tibble(
    year = 2025:2034,
    wind = c(1435, 1485, 1535, 1635, 1810, 2485, 2660, 2835, 3010, 3185),
    solar_pv = c(250, 300, 350, 400, 450, 500, 550, 600, 650, 700),
    biomass_biogas = c(250, 300, 350, 400, 450, 500, 550, 600, 650, 700),
    other = c(79, 79, 79, 79, 79, 79, 79, 79, 79, 79),
    renewable_chp = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3),
    hydro = c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6)
  )
  
  (
  chart <- renewables_data |> 
    rowwise() |> 
    mutate(total = sum(c_across(-year))) |> 
    e_charts(year) %>%
    e_area(wind, name = "Wind", stack = "renewable") %>%
    e_area(solar_pv, name = "Solar PV", stack = "renewable") %>%
    e_area(biomass_biogas, name = "Biomass/Biogas", stack = "renewable") %>%
    e_area(other, name = "Other", stack = "renewable") %>%
    e_area(renewable_chp, name = "Renewable CHP", stack = "renewable") %>%
    e_area(hydro, name = "Hydro", stack = "renewable") %>%
    e_title("Renewable Energy Capacity by Source (2025-2034)") %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
    e_tooltip(trigger = "axis") %>%
    e_theme("minimal") %>%
    e_color(c("#6ad1e3", "#e6b800", "#2bd78d", "#e3c36a", "#d7b7a3", "#a7c7e7")) %>%
    e_y_axis(name = "MW") |> 
    e_x_axis(type = "category")
  )
  return(chart)
}

# Chart 5: Energy Demand Scenarios Comparison
render_demand_scenarios <- function() {
  scenarios_data <- tibble(
    year = 2023:2034,
    low_scenario = c(42.1, 43.0, 43.7, 44.7, 45.7, 46.5, 47.3, 48.1, 49.0, 49.9, 50.9, 51.8),
    median_scenario = c(42.1, 44.1, 46.1, 48.1, 50.2, 51.7, 53.1, 54.5, 56.1, 57.7, 59.1, 60.5),
    high_scenario = c(42.1, 44.9, 47.9, 50.8, 53.5, 55.5, 57.4, 59.0, 60.7, 62.2, 63.6, 64.9)
  )
  
 ( chart <- scenarios_data %>%
    e_charts(year) %>%
    e_line(low_scenario, name = "Low Scenario", smooth = TRUE) %>%
    e_line(median_scenario, name = "Median Scenario", smooth = TRUE, 
           lineStyle = list(width = 3)) %>%
    e_line(high_scenario, name = "High Scenario", smooth = TRUE) %>%
    e_title("Total Energy Requirement - All Scenarios Comparison") %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
    e_tooltip(trigger = "axis") %>%
    e_theme("minimal") %>%
    e_color(c("#2bd78d", "#184d36", "#e6b800")) %>%
    e_y_axis(name = "TWh") |> 
    e_x_axis(type = 'category')
  )
  return(chart)
}

# Chart 6: Peak Demand Scenarios Comparison
render_peak_demand_scenarios <- function() {
  peak_data <- tibble(
    year = 2023:2034,
    low_ireland = c(5.75, 5.77, 5.85, 5.93, 6.00, 6.04, 6.06, 6.09, 6.14, 6.19, 6.25, 6.31),
    median_ireland = c(5.76, 5.95, 6.13, 6.32, 6.46, 6.57, 6.64, 6.72, 6.83, 6.92, 7.03, 7.12),
    high_ireland = c(5.76, 6.08, 6.39, 6.67, 6.87, 7.03, 7.15, 7.25, 7.36, 7.45, 7.56, 7.66),
    low_all_island = c(7.35, 7.26, 7.39, 7.50, 7.61, 7.69, 7.74, 7.78, 7.85, 7.92, 7.98, 8.06),
    median_all_island = c(7.36, 7.54, 7.72, 7.97, 8.19, 8.33, 8.45, 8.53, 8.66, 8.79, 8.90, 9.03),
    high_all_island = c(7.36, 7.79, 8.06, 8.39, 8.67, 8.89, 9.05, 9.17, 9.30, 9.43, 9.56, 9.70)
  )
  
 ( chart <- peak_data %>%
    e_charts(year) %>%
    e_line(low_ireland, name = "Ireland - Low", smooth = TRUE, linetype = "dashed") %>%
    e_line(median_ireland, name = "Ireland - Median", smooth = TRUE) %>%
    e_line(high_ireland, name = "Ireland - High", smooth = TRUE, linetype = "dashed") %>%
    e_line(low_all_island, name = "All-Island - Low", smooth = TRUE, linetype = "dashed") %>%
    e_line(median_all_island, name = "All-Island - Median", smooth = TRUE, 
           lineStyle = list(width = 3)) %>%
    e_x_axis(type = 'category') |> 
    e_line(high_all_island, name = "All-Island - High", smooth = TRUE, linetype = "dashed") %>%
    e_title("Peak Demand Scenarios (GW)") %>%
    e_legend(orient = "horizontal", bottom = 0) %>%
    e_tooltip(trigger = "axis") %>%
    e_theme("minimal") %>%
    e_color(c("#a7c7e7", "#184d36", "#e3c36a", "#6ad1e3", "#2bd78d", "#e6b800")) %>%
    e_y_axis(name = "GW") 
    )
  
  return(chart)
}

# Display all charts
print("=== Energy Data Visualizations ===")
print("")

print("Chart 1: Generation by Type (June 2025)")
chart1 <- render_generation_by_type()
chart1

print("Chart 2: Monthly Electricity Consumption and Renewable Generation (2015–2025)")
chart2 <- render_consumption_trends()
chart2

print("Chart 3: Monthly Renewable Generation as % of Consumption (2015–2017 Sample)")
chart3 <- render_renewable_percentage()
chart3

print("Chart 4: Renewable Energy Capacity by Source (2025-2034)")
chart4 <- render_renewables_forecast()
chart4

print("Chart 5: Total Energy Requirement - All Scenarios Comparison")
chart5 <- render_demand_scenarios()
chart5

print("Chart 6: Peak Demand Scenarios (GW)")
chart6 <- render_peak_demand_scenarios()
chart6

print("=== Summary Statistics ===")
print(paste("Renewable capacity growth 2025-2034:", 
            round(((sum(c(3185, 700, 700, 79, 3, 6)) - sum(c(1435, 250, 250, 79, 3, 6))) / 
                   sum(c(1435, 250, 250, 79, 3, 6))) * 100, 1), "%"))
print(paste("Wind dominance in 2034:", round(3185 / sum(c(3185, 700, 700, 79, 3, 6)) * 100, 1), "%"))
print(paste("All-Island TER range 2034 (Low-High):", "51.8 - 64.9 TWh"))

