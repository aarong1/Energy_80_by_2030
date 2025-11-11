
pipeline |> 
  group_by(
    broad_status,
    na_end_planning,
    na_end_connection,
    na_end_construction) |> 
  summarise(
    n = n(),
    MW = sum(`Installed Capacity (MWelec)`,na.rm = T),
    missing_MW = sum(is.na(`Installed Capacity (MWelec)`))
  ) #|> write.csv('temp1.csv')


(   pipeline %>%
    summarise(
      planning_wk = mean(Planning, na.rm = TRUE),
      connection_wk = mean(Connection, na.rm = TRUE),
      construction_wk = mean(Construction, na.rm = TRUE),
      
      planning_sd = sd(Planning, na.rm = TRUE),
      connection_sd = sd(Connection, na.rm = TRUE),
      construction_sd = sd(Construction, na.rm = TRUE),
      
      na_planning = sum(is.na(Planning)),
      na_connection = sum(is.na(Connection)),
      na_construction = sum(is.na(Construction)),
      
      n = n()
    ) |>
    mutate(planning_wk = as.numeric(planning_wk, units = "weeks"),
           connection_wk = as.numeric(connection_wk, units = "weeks"),
           construction_wk = as.numeric(construction_wk, units = "weeks"),
           
           planning_sd = as.numeric(planning_sd, units = "weeks")/(60 * 60 *24 * 7), # convert to weeks
           connection_sd = as.numeric(connection_sd, units = "weeks")/7,
           construction_sd = as.numeric(construction_sd, units = "weeks")/(60 * 60 *24 * 7)
    )
)

pipeline |> 
  # mutate(broad_status = ifelse((broad_status == 'Planning' &
  #                                 na_end_planning == FALSE), 
  #                              'Failed',
  #                              broad_status)) |> 
  #filter(!broad_status %in% c('Failed','Completed')) |> 
  mutate(broad_status = factor(broad_status,
                               levels = c('Planning','Connection','Construction','Failed','Completed'))) |> 
  
  group_by(
    tech,
    broad_status
  ) |> 
  summarise(
    n = n(),
    MW = sum(`Installed Capacity (MWelec)`,na.rm = T),
    missing_MW = sum(is.na(`Installed Capacity (MWelec)`)),
    Avg_MW = mean(`Installed Capacity (MWelec)`,na.rm = T)
  ) |> 
  mutate(capacity_w_missing_est = Avg_MW * missing_MW + MW) # |> 
  # write.csv('temp1.csv')

