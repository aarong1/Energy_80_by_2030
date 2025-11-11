library(readxl)
library(tidyverse)


# ni_repd <- read_excel("./data/renewable-energy-planning-applications-list-april-2002-to-march-2025 (1).xlsx", 
#                            sheet = "List")

uk_repd <- read_excel("./data/repd-q1-apr-2025 (1).xlsx",
                      sheet = "REPD")
(
  pipeline <-  uk_repd |> 
    filter(uk_repd$Country == 'Northern Ireland') |> 
    
    
    transmute(tech = `Technology Type`,
              `Ref ID`,
              `Site Name`,
              `Development Status (short)`,
              `Planning Application Reference`,
              `Planning Permission Expired`,
              
              `Installed Capacity (MWelec)`,
              `CfD Capacity (MW)`,
              
              `Planning Application Submitted`,
              `Planning Permission Granted`,
              `Under Construction`,
              Operational,
              
              Planning =  `Planning Permission Granted`- `Planning Application Submitted` ,
              Connection = `Under Construction` - `Planning Permission Granted` ,
              Construction = Operational - `Under Construction`,
              
              Planning_from_start =  as.numeric(`Planning Permission Granted`-  `Planning Application Submitted`, units = "weeks")/(1),
              Connection_from_start = as.numeric(`Under Construction` - `Planning Application Submitted`, units = "weeks")/(1 ) ,
              Construction_from_start = as.numeric(Operational - `Planning Application Submitted`, units = "weeks")/(1 ),
              
              na_end_planning = is.na(`Planning Permission Granted`),
              na_end_connection = is.na(`Under Construction`),
              na_end_construction = is.na(Operational)
    )  |>
    
    filter(tech %in% c('Wind Onshore', 'Solar Photovoltaics')) 
)

# cbind( count(pipeline,`Development Status (short)`),
#        data.frame(status = c(
#          'Failed',
#          'Planning',
#          'Failed',
#          'Failed',
#          'Failed',
#          'Planning',
#          'Failed',
#          'Connection',
#          'Complete',
#          'Failed',
#          'Failed',
#          'Construction')))

pipeline <- pipeline |> 
  mutate(
    na_planning = is.na(Planning),
    na_connection = is.na(Connection),
    na_construction = is.na(Construction),
    
    broad_status = case_when(
      
      `Development Status (short)` %in%  c('Abandoned',
                                           'Appeal Refused',
                                           'Appeal Withdrawn',
                                           'Application Refused',
                                           'Application Withdrawn',
                                           'Planning Permission Expired',
                                           "Revised") ~ 'Failed',
      
      `Development Status (short)` %in%
        c("Application Submitted" ,
          "Appeal Lodged") ~ 'Planning',
      
      `Development Status (short)` %in% 
        c("Planning Permission granted",
          "Appeal granted",
          "Secretary of state granted",
          'Awaiting Construction',
          'No Application Required' #no NI relevant returns
          
        ) ~ 'Connection',
      
      `Development Status (short)` %in% 
        c('Construction',
          'Under Construction'
        ) ~ 'Construction',
      
      `Development Status (short)` %in%   c('Decommissioned', "Operational") ~ 'Completed')
  )



# NEW Ruleset -----
pipeline |> 
  pull(`Installed Capacity (MWelec)`) |>
  sum(na.rm = T)

pipeline |> 
  pull(`Installed Capacity (MWelec)`) |>
  is.na() |> 
  sum(na.rm = T)

# NEW RULESET ---
# >Sys.Date()
#"2025-08-31"

# >as.Date("2025-08-31") -  6*365 
# "2020-09-01"

# >as.Date("2025-08-31") -  2*365 
# "2023-09-01"

#Planning_expired < as.Date("2025-08-31") - 2*365
#Planning < as.Date("2025-08-31") -  5*365 

pipeline <- pipeline |> 
  mutate(broad_status = ifelse(
    (
      ((as.Date(`Planning Permission Granted`) + 6*365) < (as.Date("2025-08-31") - 2*365) ) & 
        (`Development Status (short)` == 'Awaiting Construction')),
    'Failed', 
    broad_status),
    
    ruleset = ifelse(
      (
        ((as.Date(`Planning Permission Granted`) + 6*365) < (as.Date("2025-08-31") - 2*365) ) & 
          (`Development Status (short)` == 'Awaiting Construction')),
      'Permission Expired', 
      'Mapping'),
    
    ) |> 
  
  mutate(broad_status = ifelse(
         (
           (`Planning Application Submitted` < (as.Date("2025-08-31") -  5*365)) & 
             (`Development Status (short)` == 'Application Submitted')),
         'Failed', 
         broad_status),
         
         ruleset = ifelse(
           (
             (`Planning Application Submitted` < (as.Date("2025-08-31") -  5*365)) & 
               (`Development Status (short)` == 'Application Submitted')),
           'Application Lapsed', 
           ruleset)
  )

#count(pipeline,ruleset, wt=`Installed Capacity (MWelec)`)

pipeline |> 
  pull(`Installed Capacity (MWelec)`) |>
  sum(na.rm = T)

pipeline |> 
  pull(`Installed Capacity (MWelec)`) |>
  is.na() |> 
  sum(na.rm = T)


plot_pipeline <- pipeline |> 
  mutate(broad_status = ifelse((broad_status == 'Planning' &
                                  na_end_planning == FALSE), 
                               'Failed',
                               broad_status)) |> 
  filter(!broad_status %in% c('Failed','Completed')) |> 
  mutate(broad_status = factor(broad_status,
                               levels = c('Planning','Connection','Construction','Completed','Failed'))) |> 
  
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
  mutate(capacity_w_missing_est = Avg_MW * missing_MW + MW) 



plot_pipeline |> 
  
  ggplot()+
  geom_col(aes(broad_status, n, fill = tech))+
  theme_minimal(base_family = "Avenir") +
  labs(title = "Number of Projects at each stage of development process",
       subtitle = 'Includes all projects with status implicating stage. See notes on data quality and treatment of revised applications', 
       x = "Stage",
       y = "Number")

plot_pipeline |> 
  
  ggplot()+
  geom_col(aes(broad_status, MW, fill = tech), position = 'dodge') +
  theme_minimal(base_family = "Avenir") +
  labs(title = "Capacity at each stage of development process",
       subtitle = 'Includes all projects with status implicating stage. See notes on data quality and treatment of revised applications', 
       x = "Stage",
       y = "Capacity (MW)") +
  geom_label(aes(x = broad_status, y = MW, label = paste(round(MW,1), 'MW'), color = tech),
             size = 3.5, vjust = 0,hjust = 0.5, position = position_dodge(width=1))#+
# scale_fill_brewer(palette = "Pastel1") +
# scale_color_brewer(palette = "Pastel1") 



pipeline |> 
  pivot_longer(cols = c(Planning, Connection, Construction), 
               names_to = "Stage", 
               values_to = "Duration") |>
  mutate(Duration = Duration/(60 * 60 *24 * 7)) |> # convert to weeks
  filter(tech %in% c('Wind Onshore', 'Solar Photovoltaics')) |>
  group_by(tech) |> # summarise( n() )
  
  ggplot() +
  geom_density(aes(Duration, group = Stage )) +
  facet_wrap(~Stage, scales = "free") +
  theme_minimal(base_family = "Avenir")  

pipeline |>
  pivot_longer(cols = ends_with('from_start'),
               names_to = "Stage", 
               values_to = "Duration") |>
  #mutate(Duration = Duration/(60 * 60 *24 * 7)) |> # convert to weeks
  
  filter(tech %in% c('Wind Onshore', 'Solar Photovoltaics')) |>
  
  ggplot() +
  geom_density(aes(Duration, fill =  Stage), color = 'white', alpha = 0.6) +
  facet_wrap(~tech, scales = "free") +
  theme_minimal(base_family = "Avenir") +
  labs(title = "Time from Submission to Planning Granted, Connection finished and Operation (Construction Finished)",
       subtitle = 'Includes all projects that have completed a stage \nregardless where they subsequently dropped out, or are still in the pipeline', 
       x = "Duration (weeks)",
       y = "Density")


# Transform distribution to normal ----

# plot(density(na.omit(log(as.numeric(pipeline$Planning)))))
# plot(density(na.omit(log(as.numeric(pipeline$Connection)))))
# plot(density(na.omit(sqrt(as.numeric(pipeline$Construction)))))

plot_pipeline <- pipeline |>
  pivot_longer(cols = c(Planning, Connection, Construction),
               names_to = "Stage", 
               values_to = "Duration") |>
  mutate(Duration = Duration/(60 * 60 *24 * 7)) |> # convert to weeks
  
  filter(tech %in% c('Wind Onshore', 'Solar Photovoltaics')) 

plot_pipeline |>
  
  ggplot() +
  geom_density(aes(Duration, fill =  Stage), color = 'white', alpha = 0.6) +
  facet_wrap(~tech+Stage, scales = "fixed") +
  xlim(c(0,400)) +
  geom_vline(data = group_by(plot_pipeline,Stage,tech) |> 
               summarise(avg = mean(Duration,na.rm=T)),
             aes(xintercept = avg,
                 color =  Stage), linetype = "dashed") +
  # geom_label(data = group_by(plot_pipeline,Stage,tech) |> 
  #              summarise(avg = mean(Duration,na.rm=T)),
  #            aes(x = 0, y = 0, label = paste(round(avg/52,1), 'yrs'), color = Stage),
  #            size = 3.5, vjust = 0,hjust =0) +
  
  geom_label(data = group_by(plot_pipeline,Stage,tech) |> 
               summarise(avg = mean(Duration,na.rm=T)),
             aes(x = 0, y = 0, label = paste(round(avg/52,1), 'yrs'), color = Stage),
             size = 3.5, vjust = 0,hjust = 0) +
  
  theme_minimal(base_family = "Avenir") +
  labs(title = "Time from Submission to Planning Granted, Connection finished and Operation (Construction Finished)",
       subtitle = 'Includes all projects that have completed a stage regardless where they subsequently dropped out, or are still in the pipeline', 
       x = "Duration (weeks)",
       y = "Density")



plot_pipeline |>
  
  ggplot() +
  geom_density(aes(Duration, fill =  Stage), color = 'white', alpha = 0.6) +
  facet_wrap(~tech+Stage, scales = "free") +
  xlim(c(0,400)) +
  geom_vline(data = group_by(plot_pipeline,Stage,tech) |> 
               summarise(avg = mean(Duration,na.rm=T)),
             aes(xintercept = avg,
                 color =  Stage), linetype = "dashed") +
  # geom_label(data = group_by(plot_pipeline,Stage,tech) |> 
  #              summarise(avg = mean(Duration,na.rm=T)),
  #            aes(x = 0, y = 0, label = paste(round(avg/52,1), 'yrs'), color = Stage),
  #            size = 3.5, vjust = 0,hjust =0) +
  
  geom_label(data = group_by(plot_pipeline,Stage,tech) |> 
               summarise(avg = mean(Duration,na.rm=T)),
             aes(x = 0, y = 0, label = paste(round(avg/52,1), 'yrs'), color = Stage),
             size = 3.5, vjust = 0,hjust = 0) +
  
  theme_minimal(base_family = "Avenir")  +
  labs(title = "Time from Submission to Planning Granted, Connection finished and Operation (Construction Finished)",
       subtitle = 'Includes all projects that have completed a stage regardless where they subsequently dropped out, or are still in the pipeline', 
       x = "Duration (weeks)",
       y = "Density")

#dotplot  -----

plot_pipeline %>%
  ggplot() +
  geom_dotplot(
    mapping = aes(Duration, fill =  Stage), 
    color = 'white', 
    alpha = 0.6)+
  facet_wrap(~tech+Stage, scales = "free") +
  theme_minimal () +
  labs(title = "Time from Submission to Planning Granted, Connection finished and Operation (Construction Finished)",
       subtitle = 'Includes all projects that have completed a stage regardless where they subsequently dropped out, or are still in the pipeline', 
       x = "Duration (weeks)",
       y = "Density")



