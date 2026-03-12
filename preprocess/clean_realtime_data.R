
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

df2014_2015 <- read_excel("./data/System-Data-Qtr-Hourly-2014-2015.xlsx")
df2016_2017 <- read_excel("./data/System-Data-Qtr-Hourly-2016-2017.xlsx")
df2018_2019 <- read_excel("./data/System-Data-Qtr-Hourly-2018-2019.xlsx")
df2020_2021 <- read_excel("./data/System-Data-Qtr-Hourly-2020-2021.xlsx")
df2022_2023 <- read_excel("./data/System-Data-Qtr-Hourly-2022-2023_0.xlsx")
df2024 <- read_excel("./data/System_Data_Qtr_Hourly_2024.xlsx")
df2025 <- read_excel("./data/System-Data-Qtr-Hourly-2025-v10.xlsx")



snsp <- bind_rows(
  df2014_2015 %>% select(DateTime, SNSP),
  df2016_2017 %>% select(DateTime, SNSP),
  df2018_2019 %>% select(DateTime, SNSP),
  df2020_2021 %>% select(DateTime, SNSP),
  df2022_2023 %>% select(DateTime, SNSP),
  df2024 %>% select(DateTime, SNSP),
  df2025 %>% select(DateTime, SNSP)
)
snsp[is.na(snsp)] <- 0

timestamp <- snsp$DateTime


daily_median_snsp <- snsp %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(median_SNSP = median(SNSP), .groups = "drop")


monthly_median_snsp <- snsp %>%
  mutate(month_start = floor_date(timestamp, unit = "month")) %>%
  group_by(month_start) %>%
  summarise(median_SNSP = median(SNSP), .groups = "drop")

system_gen <- bind_rows(
  df2014_2015 %>% select(DateTime, `NI Generation`),
  df2016_2017 %>% select(DateTime, `NI Generation`),
  df2018_2019 %>% select(DateTime, `NI Generation`),
  df2020_2021 %>% select(DateTime, `NI Generation`),
  df2022_2023 %>% select(DateTime, `NI Generation`),
  df2024 %>% select(DateTime, `NI Generation`),
  df2025 %>% select(DateTime, `NI Generation`)
)
system_gen[is.na(system_gen)] <- 0

daily_sum_system_gen <- system_gen %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(sum_system_gen = (sum(`NI Generation`))/4, .groups = "drop")

demand <- bind_rows(
  df2014_2015 %>% select(DateTime,`NI Demand`),
  df2016_2017 %>% select(DateTime, `NI Demand`),
  df2018_2019 %>% select(DateTime, `NI Demand`),
  df2020_2021 %>% select(DateTime, `NI Demand`),
  df2022_2023 %>% select(DateTime, `NI Demand`),
  df2024 %>% select(DateTime, `NI Demand`),
  df2025 %>% select(DateTime, `NI Demand`)
)
demand[is.na(demand)] <- 0


daily_sum_demand <- demand %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>%
  summarise(sum_demand = (sum(`NI Demand`))/4, .groups = "drop")


wind <- bind_rows(
  df2014_2015 %>% select(DateTime, `NI Wind Generation`),
  df2016_2017 %>% select(DateTime, `NI Wind Generation`),
  df2018_2019 %>% select(DateTime, `NI Wind Generation`),
  df2020_2021 %>% select(DateTime, `NI Wind Generation`),
  df2022_2023 %>% select(DateTime, `NI Wind Generation`),
  df2024 %>% select(DateTime, `NI Wind Generation`),
  df2025 %>% select(DateTime, `NI Wind Generation`)
)
wind[is.na(wind)] <- 0

daily_sum_wind <- wind %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>%
  summarise(sum_wind = (sum(`NI Wind Generation`))/4, .groups = "drop")


solar <- bind_rows(
  df2014_2015 %>% select(DateTime),
  df2016_2017 %>% select(DateTime),
  df2018_2019 %>% select(DateTime, `NI Solar Generation`),
  df2020_2021 %>% select(DateTime, `NI Solar Generation`),
  df2022_2023 %>% select(DateTime, `NI Solar Generation`),
  df2024 %>% select(DateTime, `NI Solar Generation`),
  df2025 %>% select(DateTime, `NI Solar Generation`)
)
solar[is.na(solar)] <- 0

daily_sum_solar <- solar %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>%
  summarise(sum_solar = (sum(`NI Solar Generation`))/4, .groups = "drop")

moyle <- bind_rows(
  df2014_2015 %>% select(DateTime),
  df2016_2017 %>% select(DateTime),
  df2018_2019 %>% select(DateTime),
  df2020_2021 %>% select(DateTime),
  df2022_2023 %>% select(DateTime, `Moyle I/C`),
  df2024 %>% select(DateTime, `Moyle I/C`),
  df2025 %>% select(DateTime, `Moyle I/C`)
)

moyle_split_abs <- moyle %>%
  mutate(
    import = if_else(moyle$`Moyle I/C` > 0,  moyle$`Moyle I/C`, NA_real_),
    export = if_else(moyle$`Moyle I/C` < 0, -(moyle$`Moyle I/C`), NA_real_))  # absolute magnitude for export
import = moyle_split_abs[,3]
import <- data.frame(timestamp, import)
export = moyle_split_abs[,4]
export <- data.frame(timestamp, export)
import[is.na(import)] <- 0
export[is.na(export)] <- 0

daily_sum_import <- import %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(sum_import = (sum(import))/4, .groups = "drop")

daily_sum_export <- export %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(sum_export = (sum(export))/4, .groups = "drop")

daily_time <- daily_median_snsp[,1]

potential_ratio <- data.frame(daily_time, (daily_sum_solar[,2]+daily_sum_wind[,2])/daily_sum_demand[,2])
curtailment_res <- data.frame(daily_time, ((daily_median_snsp [,2]*(daily_sum_demand[,2] + daily_sum_export[,2])) - daily_sum_import[,2]))
# curtailment_res <- pmax(curtailment_res, 0)
curtailment_ratio <- data.frame(daily_time, curtailment_res[,2]/daily_sum_demand[,2])



res <- wind[,2]+solar[,2]
res <- data.frame(timestamp, res)

daily_sum_res <- res %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(sum_res = (sum(NI.Wind.Generation))/4, .groups = "drop")

monthly_sum_cur_res <- curtailment_res %>%
  mutate(month_start = floor_date(date, unit = "month")) %>%
  group_by(month_start) %>%
  summarise(sum_cur_res = sum(median_SNSP), .groups = "drop")

monthly_sum_pot_res <- res %>%
  mutate(month_start = floor_date(timestamp, unit = "month")) %>%
  group_by(month_start) %>%
  summarise(sum_pot_res = sum(NI.Wind.Generation), .groups = "drop")

fossil_fuel <- daily_sum_system_gen[,2] - daily_sum_res[,2]
fossil_fuel <- data.frame(daily_time, fossil_fuel)

avai_wind <- bind_rows(
  df2014_2015 %>% select(DateTime, `NI Wind Availability`),
  df2016_2017 %>% select(DateTime, `NI Wind Availability`),
  df2018_2019 %>% select(DateTime, `NI Wind Availability`),
  df2020_2021 %>% select(DateTime, `NI Wind Availability`),
  df2022_2023 %>% select(DateTime, `NI Wind Availability`),
  df2024 %>% select(DateTime, `NI Wind Availability`),
  df2025 %>% select(DateTime, `NI Wind Availability`)
)
avai_wind[is.na(avai_wind)] <- 0

daily_sum_avai_wind <- avai_wind %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>%
  summarise(sum_avai_wind = (sum(`NI Wind Availability`))/4, .groups = "drop")

avai_solar <- bind_rows(
  df2014_2015 %>% select(DateTime),
  df2016_2017 %>% select(DateTime),
  df2018_2019 %>% select(DateTime, `NI Solar Availability`),
  df2020_2021 %>% select(DateTime, `NI Solar Availability`),
  df2022_2023 %>% select(DateTime, `NI Solar Availability`),
  df2024 %>% select(DateTime, `NI Solar Availability`),
  df2025 %>% select(DateTime, `NI Solar Availability`)
)
avai_solar[is.na(avai_solar)] <- 0

daily_sum_avai_solar <- avai_solar %>%
  mutate(date = as.Date(DateTime)) %>%
  group_by(date) %>%
  summarise(sum_avai_solar = (sum(`NI Solar Availability`))/4, .groups = "drop")

# avai_res <- daily_sum_avai_wind[,2]+ daily_sum_avai_solar[,2]
# avai_res <- data.frame(daily_time, avai_res)

combined_df <- daily_median_snsp %>%
  left_join(daily_sum_demand, by = "date") %>%
  left_join(daily_sum_export, by = "date") %>%
  left_join(daily_sum_import, by = "date") %>%
  left_join(daily_sum_system_gen, by = "date") %>%
  left_join(daily_sum_res, by = "date") %>%
  left_join(daily_sum_solar, by = "date") %>%
  left_join(daily_sum_wind, by = "date") %>%
  left_join(fossil_fuel, by = "date") %>%
  # left_join(curtailment_res, by = "date") %>%
  left_join(daily_sum_avai_solar, by = "date") %>%
  left_join(daily_sum_avai_wind, by = "date")
 
  
              
save(daily_median_snsp, monthly_median_snsp, combined_df,
     file = "./data/inputs.rda", compress = "xz")


  

