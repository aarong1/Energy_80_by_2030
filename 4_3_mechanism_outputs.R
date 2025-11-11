target_date = '2030-01-01'
end_date = end_date




# By end date ----
current_projects_outcome_target_date <- current_projects_outcome |> 
  filter(year(finished) == year(target_date)) |> 
  pull(mean) |> sum()

forward_projects_outcome_target_date <- forward_projects_outcome |> 
  filter(year(finished) == year(target_date)) |> 
  pull(mean) |> sum()

current_projects_outcome_cumulative_target_date <- current_projects_outcome_cumulative |> 
  filter(finished == target_date) |> 
  pull(mean) |>  sum()

forward_projects_outcome_cumulative_target_date <- forward_projects_outcome_cumulative |> 
  filter(finished == target_date) |> 
  pull(mean) |>  sum()

# By Target Date ----

current_projects_outcome_end_date <- current_projects_outcome |> 
  filter(year(finished) == year(end_date)) |> 
  pull(mean) |> sum()

forward_projects_outcome_end_date <- forward_projects_outcome |> 
  filter(year(finished) == year(end_date)) |> 
  pull(mean) |> sum()


current_projects_outcome_cumulative_end_date <- current_projects_outcome_cumulative |> 
  filter(finished == end_date) |> 
  pull(mean) |> sum()

forward_projects_outcome_cumulative_end_date <- forward_projects_outcome_cumulative|> 
  filter(finished == end_date) |> 
  pull(mean) |> sum()

offshore_wind_df <- forward_projects_outcome_cumulative |> select(finished) |> 
  mutate(mean = ifelse(finished > offshore_wind_date, offshore_wind_capacity/12, 0)) 

offshore_wind_target_date <- offshore_wind_df|> 
  filter(year(finished) == year(target_date)) |> 
  pull(mean) |> sum()

offshore_wind_end_date <- offshore_wind_df|> 
  filter(finished == end_date) |> 
  pull(mean) |> sum()

## target_date ----

existing_res_target_date <- existing_res_end_date <- 3961

TER_target_date  <- TER_end_date <- 9000


total_new_res_target_date = (forward_projects_outcome_cumulative_target_date + current_projects_outcome_cumulative_target_date)
total_new_res_end_date = (forward_projects_outcome_cumulative_target_date + current_projects_outcome_cumulative_target_date)


target_pct_target_date = (total_new_res_target_date + existing_res_target_date)/TER_target_date
target_pct = target_pct_target_date
target_status_words_target_date = ifelse(target_pct >= 0.8, 'on track', ifelse(target_pct >= 0.75, 'slightly behind', 'significantly behind'))
target_status_color_target_date = ifelse(target_pct >= 0.8, 'green', ifelse(target_pct >= 0.75, 'orange', 'red'))

## end date ----
total_new_res_end_date = (forward_projects_outcome_cumulative_end_date + current_projects_outcome_cumulative_end_date)

target_pct_end_date = (total_new_res_end_date + existing_res_end_date)/TER_end_date
target_pct = target_pct_end_date

target_status_words_end_date = ifelse(target_pct >= 0.8, 'on track', ifelse(target_pct >= 0.75, 'slightly behind', 'significantly behind'))
target_status_color_end_date = ifelse(target_pct >= 0.8, 'green', ifelse(target_pct >= 0.75, 'orange', 'red'))


