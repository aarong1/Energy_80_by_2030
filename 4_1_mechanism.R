
source('4_0_mechanism_utils.R')

# get pre - planning projects anticipated start date up until 2030

# input forecast of pre - planning

### optimistic 
### conservative
### sampled from month-wise historic 2023-2024 submissions
### other

## ruleset ----

# if planning still waiting for a decision after 5 years we fail s
# if planning expired date before a year we fail it 

#this decreases the project pipeline and consequently the progression probability


library("dtplyr")
#rowwise is not defined

### Pre-planning ----

preplanning_configuration = 'conservative' # optimistic # survey #

end_date = '2032-12-01'
nrun = 30;

offshore_wind = TRUE
offshore_wind_date = '2032-01-01'
offshore_wind_capacity = 0.6

transition_configuration <- 'empirical' # custom

transition_probs <- function() {
  transition_probs <- tribble(
    ~from, ~to, ~prob,
    'Planning', 'Connection', 77.54/100,
    'Connection', 'Construction', 78.02/100,
    'Construction', 'Completed', 100/100
  )
}

duration_configuration <- 'empirical' # custom


stage_duration <- function() {
  
  stage_duration <- tribble(
    ~from, ~to,~tech, ~wks,
    'Planning', 'Connection',' Solar Photovoltaic', 33.70714,
    'Connection', 'Construction',' Solar Photovoltaic', 70.88571,
    'Construction', 'Completed',' Solar Photovoltaic', 17.59184,
    
    'Planning', 'Connection','Wind Onshore', 116.08599 ,
    'Connection', 'Construction','Wind Onshore', 182.20476,
    'Construction', 'Completed','Wind Onshore', 51.72167,
  )
  
}

projections_projects <-  switch(EXPR = 'conservative',
  'conservative' = projections_projects_conservative,
  'optimistic' = projections_projects_optimistic,
  'survey' = projections_projects_survey
)

# projections_projects <-  case_when(
#   preplanning_configuration == 'conservative' ~ list(projections_projects_conservative),
#   preplanning_configuration == 'optimistic' ~ list(projections_projects_optimistic),
#   preplanning_configuration == 'survey' ~ list(projections_projects_survey)
# )


shiny_forecast <- function(){
  projections_projects |> 
  filter(type == 'forecast') |> 
  filter(Date > '2024-12-01') |> 
    filter(Date< end_date) |> 
  select(tech, ,Date, date, n ) |> 
  mutate(lower = floor(n),
         upper = ceiling(n),
         remainder = n-lower) |> 
  rowwise() |> 
  mutate(sample_deterministic = 
           sample(c(lower, upper), 
                  size = 1, 
                  prob = c(1-remainder, remainder)))
}

forward_projects <- data.frame(); 
for(j in 1:nrun){
  print(j)
  
 for( i in 1:nrow(shiny_forecast())){
   print(i)
   n <- t(shiny_forecast())[,i]
   #print(n)
   nn <- lift_historic_projects(pipeline,
                                n['tech'],
                                n[['Date']],
                                n=as.numeric(n['sample_deterministic']))
   #print(nn)
   
    nn$run = j
    
   forward_projects <- rbind(forward_projects, nn)
   
 }
 }


forward_projects <- forward_projects |> 
  mutate(broad_status = 'Planning') |> 
  mutate(broad_status_start = 'Planning') |> 
  mutate(Date = as.Date(Date))
  # lazy_dt()

# passed_connection_time = (get_empirical_time(pipeline,tech,broad_status))
# passed_connection_time = (get_empirical_time(pipeline,tech,broad_status))
# passed_connection_time = (get_empirical_time(pipeline,tech,broad_status))

# get_empirical_time(pipeline,'Solar Photovoltaics','Planning')
# get_empirical_time(pipeline,'Solar Photovoltaics','Connection')
# get_empirical_time(pipeline,'Solar Photovoltaics','Construction')

# get_empirical_time(pipeline,'Wind Onshore','Planning')
# get_empirical_time(pipeline,'Wind Onshore','Connection')
# get_empirical_time(pipeline,'Wind Onshore','Construction')


forward_projects <- forward_projects |> 
  rowwise() |> 
  mutate(passed_planning = sample(c(T,F),
                                  replace = T,
                                  prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                           1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                  size = 1)) |>
  
  mutate(
      passed_planning_time = (get_empirical_time(pipeline,tech,broad_status)),
      passed_planning_time_wk = as.numeric(passed_planning_time, units = 'weeks' ) ,#(60*60*24*7),
      passed_planning_date = Date + passed_planning_time) |>
  mutate(broad_status = 'Connection') |> 
  
  mutate(passed_connection = sample(c(T,F),
                                  replace = T,
                                  prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                           1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                  size = 1)) |> 
  
  mutate(
    passed_connection_time = (get_empirical_time(pipeline,tech,broad_status)),
    passed_connection_time_wk = as.numeric(passed_connection_time, units = 'weeks' ) ,#(60*60*24*7),
    passed_connection_date = passed_planning_date + passed_connection_time
    ) |> 
  
  mutate(broad_status = 'Construction')|> 
  mutate(passed_construction = sample(c(T,F),
                                    replace = T,
                                    prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                             1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                    size = 1)) |> 
  mutate(
      passed_construction_time = (get_empirical_time(pipeline,tech,broad_status)),
      passed_construction_time_wk = as.numeric(passed_construction_time, units = 'weeks' ) ,#(60*60*24*7),
      passed_construction_date = passed_connection_date + passed_construction_time) |> 
  
  mutate(broad_status = 'Completed')


# sample from survey
# sample from empirical distribution
# date + runif(1, 1, 50), 


forward_projects_outcome_first <- forward_projects |> 
  filter(passed_planning &
           passed_connection &
           passed_construction) |> 
  mutate(finished = #format(passed_construction_date, format = '%Y-%m')) |> 
           floor_date( passed_construction_date,'month')) |> 
  group_by(finished ,
           run) |> 
  summarise(MW = sum(`Installed Capacity (MWelec)`,na.rm = T),
            no_proj = n()) |> 
  ungroup()


# yrs = unique(floor_date(seq( from = as.Date('2025-01-01'), 
#                              to = as.Date('2031-01-01'),
#                              by =1 ),'year'))
# qtr = unique(floor_date(seq( from = as.Date('2025-01-01'), 
#                              to = as.Date('2031-01-01'),
#                              by =1 ),'quarter'))


mons = unique(floor_date(seq( from = as.Date('2025-01-01'), 
                to = as.Date(end_date),
                by = 1 ), 'month')
              )

# wks = unique(floor_date(seq( from = as.Date('2025-01-01'), 
#                        to = as.Date('2031-01-01'),
#                        by =1 ),'week'))



forward_projects_outcome_first <- left_join(
expand.grid(
            finished = mons,
            run = 1:nrun),
forward_projects_outcome_first
          ) |> 
  replace_na(replace = list(MW = 0, no_proj = 0)) |> 
  arrange(finished)

conf_level <- 0.95
alpha      <- 1 - conf_level

forward_projects_outcome <- forward_projects_outcome_first |> 
  group_by(finished) |> 
  summarise(
    mean   = mean(MW, na.rm = TRUE),
    median = median(MW, na.rm = TRUE),
    sd     = sd(MW, na.rm = TRUE),
    
    # avg_projects = mean(),
    n      = dplyr::n(), #nrun,#
    se     = sd / sqrt(n),
    # CLT / t-interval
    
    u = mean+2*se,
    l = mean-2*se,
    
    lwr_norm = max(0,mean + qt(alpha/2, df = n - 1) * se),
    upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
    # Bootstrap percentile CI
    
    lwr_pct  = quantile(MW, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
    upr_pct  = quantile(MW, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE),
  
    .groups  = "drop"
  ) #|> 

ggplot(forward_projects_outcome)+
  geom_line(aes(finished,mean))+
  #geom_line(aes(finished,median),col = 'black')+
  
  # geom_ribbon(aes(finished,mean,ymin = lwr_pct, ymax = upr_pct),
  #             alpha = 0.5, fill = "skyblue") +
  
  geom_ribbon(aes(finished,mean,ymin = lwr_norm, ymax = upr_norm),
              alpha = 0.4, fill = "red") +
  
  # geom_smooth(aes(finished,y = mean),
  #             alpha = 0.4, col = "black",se=FALSE) +
  # 
  # geom_smooth(aes(finished,y = lwr_norm),
  #             alpha = 0.4, col = "black",se=F) +
  # geom_smooth(aes(finished,y = upr_norm),
  #             alpha = 0.4, col = "black",se=F) +
  xlim(c(as.Date('2025-01-01'),NA))+
    theme_minimal(base_family = "Avenir") +
  labs(title = 'Projected Monthly new renewable capacity from projects in pre-planning',
       subtitle = '',
       y = 'MW',
       x = 'Completion',
       caption = paste('timestamp:',Sys.time(),'bootstrap:',nrun))






#Cumulative
forward_projects_outcome_cumulative <- forward_projects_outcome_first |> 
  group_by(run) |>
  arrange(finished) |> 
  mutate(cs = cumsum(MW)) |> 
  group_by(finished) |> 
  summarise(mean = mean(cs),
            median = median(cs, na.rm = TRUE),
            sd     = sd(cs, na.rm = TRUE),
            
            # avg_projects = mean(),
            n      = nrun, #dplyr::n(), #dplyr::n(), #,#
            se     = sd / sqrt(n),
            
            # CLT / t-interval
            u = mean + 2 * se,
            l = mean - 2 * se,
            lwr_norm = mean - qt(alpha/2, df = n - 1) * se,
            upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
            
            # Bootstrap percentile CI
            lwr_pct  = quantile(cs, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE)  ,
            upr_pct  = quantile(cs, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE)  ,
 
            .groups  = "drop") #|>
  # mutate( bci = boot.ci(b, type = "bca"))

#Extremely Skewed
# forward_projects |>
#   filter(year(passed_construction_date) == "2030") |>
#   ggplot(
#     aes(`Installed Capacity (MWelec)`, 
#              group= run, fill = run)
#     ) + 
#   geom_histogram(position = 'dodge')


# forward_projects_outcome_first |>
#   group_by(run) |>
#   arrange(finished) |>
#   mutate(cs = cumsum(MW)) |>
#   ungroup() |>
#   ggplot() +
#   geom_line(aes(finished, cs, group = run, col = (run)))

# forward_projects_outcome_first |> 
#   group_by(run) |>
#   arrange(finished) |> 
#   mutate(cs = cumsum(MW)) |> 
#   group_by(finished) |> 
#   
#   filter(finished == as.Date('2029-01-01')) |> 
#   
#   summarise( mean(cs),
#              quantile(cs, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
#              max(cs),
#              min(cs)
#              )
# 
# forward_projects_outcome_cumulative |> 
#   filter(finished == as.Date('2029-01-01')
#          ) 

  ggplot(ungroup(forward_projects_outcome_cumulative)) +
  
   #geom_line(aes(finished, median))+
    geom_line(aes(finished, mean))+
 
    # geom_line(data = forward_projects_outcome_first |>
    #             group_by(run) |>
    #             arrange(finished) |>
    #             mutate(cs = cumsum(MW)) |>
    #             ungroup() ,mapping=aes(finished, cs, group = run, col = (run)))+
  
  geom_ribbon(aes(finished,mean,ymin = lwr_pct, ymax = upr_pct),
              alpha = 0.5, fill = "red") +
    
      theme_minimal(base_family = "Avenir") +
    labs(title = 'Projected Cumulative new renewable capacity from projects in pre-planning',
         subtitle = '.',
         y = 'MW',
         x = 'Completion',
         caption = paste('timestamp:',Sys.time(),'bootstrap:',nrun))
  

  

    
    # geom_ribbon(aes(finished,mean,ymin = lwr_norm, ymax = upr_norm),
    #             alpha = 0.4, fill = "red") +
    # 
    # # normal/t CI ribbon
    # geom_ribbon(aes(finished,mean,ymin = l, ymax = u),
    #             alpha = 0.4, fill = "orange")
  
  

####### Current Projects ##############

  current_projects <- pipeline  |> 
  filter(broad_status %in% c('Planning','Connection','Construction')) |> 
  # filter(`Development Status (short)` != 'Revised')|> 
  select(
    `Development Status (short)`,
  `Installed Capacity (MWelec)`,
  'Ref ID',
  Planning,
  Connection,
  Construction,
  'date' = `Planning Application Submitted`,
  passed_planning_date = `Planning Permission Granted`,
  `Planning Permission Granted`,
  passed_connection_date = `Under Construction`,
  passed_construction_date = `Operational`,
  
  'Date' = `Planning Application Submitted`,
  'tech',
  broad_status)


splitting_criteria <- current_projects$broad_status

list_statuses_original <- current_projects %>%
  mutate(broad_status_start = broad_status) %>%
  mutate(      passed_planning = T,
               passed_planning_time = Planning,       
               passed_planning_time_wk = as.numeric(Planning,unit = 'weeks'),#(60*60*24*7),    
               #passed_planning_date = NA,    
               
               passed_connection = T,          
               passed_connection_time = Connection,     
               passed_connection_time_wk = as.numeric(Connection,unit = 'weeks'),
               #passed_connection_date = NA,
               
               passed_construction = T,
               passed_construction_time = Construction,
               passed_construction_time_wk = as.numeric(Construction,unit = 'weeks'),
              # passed_construction_date = NA
               
               ) |>
         
  split(f = splitting_criteria) 

#| The is a plain text comment
#| 
  #left_join(transition_probs(), by = c('broad_status' = 'from')) |>


current_projects_projected_forward <- data.frame()

for (j in 1:nrun){
  print(j)
list_statuses <- list_statuses_original

while( sum(
  sapply(
    list_statuses, 
    function(pipeline){
      sum(pipeline$broad_status!='Completed')})) != 0) {
  
  # print(
  #   sum(
  #   sapply(
  #     list_statuses, 
  #     function(pipeline){
  #       sum(pipeline$broad_status!='Completed')}))
  #   )

for (i in 1:length(list_statuses)){
  
  print(i)
  #print(list_statuses[[i]])
  switch(
    unique(list_statuses[[i]]$broad_status),
    Planning = {list_statuses[[i]] <- 
      list_statuses[[i]] |> 
      rowwise() |>
      mutate(passed_planning = sample(c(T,F),
                                      replace = T,
                                      prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                               1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                      size = 1)) |>
      mutate(
        passed_planning_time = (get_empirical_time(pipeline,tech,broad_status)),
        passed_planning_time_wk = as.numeric(passed_planning_time, units = 'weeks' ) ,#(60*60*24*7),
        passed_planning_date = Date + passed_planning_time) |> 
      
      mutate(broad_status = 'Connection') |> ungroup()
},
  
    Connection = {  list_statuses[[i]] <- list_statuses[[i]] |> 
      
    # mutate(
    # passed_planning = NA,            
    # passed_planning_time = NA,       
    # passed_planning_time_wk = NA   ) |> 

      rowwise() |>
      # mutate(passed_connection = sample(c(T,F),
      #                                   replace = T,
      #                                   prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
      #                                            1-transition_probs()$prob [transition_probs()$from == broad_status]),
      #                                   size = 1)
      mutate(passed_connection = ifelse(runif(n = 1) < transition_probs()$prob [transition_probs()$from == broad_status],
                                        T,
                                        F)
      
             ) |> 
      
      mutate(
        passed_connection_time = (get_empirical_time(pipeline,tech,broad_status)),
        passed_connection_time_wk = as.numeric(passed_connection_time, units = 'weeks' ) ,#(60*60*24*7),
        passed_connection_date = passed_planning_date + passed_connection_time) |> 
      
      mutate(broad_status = 'Construction')|> ungroup()
    },
  
  
  
  Construction = {
    list_statuses[[i]] <- list_statuses[[i]] |> 
      
      # mutate(
      #   passed_planning = NA,
      #   passed_planning_time = NA,       
      #   passed_planning_time_wk = NA,    
      #   passed_connection = NA,          
      #   passed_connection_time = NA,     
      #   passed_connection_time_wk = NA  ) |> 
      rowwise() |> 
      mutate(passed_construction = sample(c(T,F),
                                          replace = T,
                                          prob = c(transition_probs()$prob [transition_probs()$from == broad_status],
                                                   1-transition_probs()$prob [transition_probs()$from == broad_status]),
                                          size = 1)) |> 
      
      mutate(
        passed_construction_time = (get_empirical_time(pipeline,tech,broad_status)),
        passed_construction_time_wk = as.numeric(passed_construction_time, units = 'weeks' ) ,#(60*60*24*7),
        passed_construction_date = passed_connection_date + passed_construction_time) |> 
      
      mutate(broad_status = 'Completed') |> ungroup()
  }
  )
}

}

current_projects_projected_forward <- rbind(current_projects_projected_forward,
                                            reduce(list_statuses,.f = rbind) |> mutate(run = j)
)
}
  

  
 current_projects_outcome_first <-   current_projects_projected_forward |> 
   filter(passed_planning &
            passed_connection &
            passed_construction) |> 
   mutate(MW = `Installed Capacity (MWelec)`,
          yr = year(passed_construction_date),
          mn = month(passed_construction_date)
          )
 
 
# Monthly SUM
 
 current_projects_outcome_first <- current_projects_outcome_first |> 
    mutate(finished = #format(passed_construction_date, format = '%Y-%m')) |> 
             floor_date( passed_construction_date,'month')) |> 
    group_by(finished,
             run) |> 
    summarise(MW = sum(`MW`) ) 
 
 current_projects_outcome_first <- 
   expand.grid(yr = 2012:year(end_date),
               mn = 1:12,
               run = 1:nrun#,
               #tech = c('Wind Onshore','Solar Photovoltaics') 
   )|> 
   mutate(finished = as.Date(paste0(yr, ifelse(mn < 10, paste0('-0', mn,'-01'), paste0('-',mn,'-01'))))) |> 
   left_join(current_projects_outcome_first) |> 
   replace_na(list(MW=0))
 
 
 conf_level <- 0.95
 alpha      <- 1 - conf_level
 
 current_projects_outcome <- current_projects_outcome_first |> 
    group_by(finished) |> 
    summarise(
      mean   = mean(MW, na.rm = TRUE,trim=0.05),
      median = median(MW, na.rm = TRUE),
      sd     = sd(MW, na.rm = TRUE),
      n      = nrun,#dplyr::n(),
      se     = sd / sqrt(n),
      # CLT / t-interval
      
      u=max(0,mean+2 * se),
      l=max(0,mean-2 * se),
      lwr_norm = mean + qt(alpha/2, df = n - 1) * se,
      upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
      # Bootstrap percentile CI
      lwr_pct  = quantile(MW, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
      upr_pct  = quantile(MW, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE),
      .groups  = "drop"
      
    ) 
 
 # current_projects_outcome_first |> 
 #   filter(finished=='2018-01-01')

 # View(filter(pipeline,`Installed Capacity (MWelec)` == 27))

 # current_projects_outcome_first |>
 #   ggplot() +
 #   geom_boxplot(aes(finished , MW, group = finished)) 

 ggplot(current_projects_outcome, aes(x = finished, y = mean)) +
   geom_line(color = "black", linewidth = 1) +
   # bootstrap percentile ribbon
   
   geom_ribbon(aes(ymin = l, ymax = u),
               alpha = 0.8, fill = "skyblue") +
   
   # geom_ribbon(aes(ymin = lwr_pct, ymax = upr_pct),
   #             alpha = 0.6, fill = "red") +
   # normal/t CI ribbon
   # geom_ribbon(aes(ymin = lwr_norm, ymax = upr_norm),
   #             alpha = 0.4, fill = "orange")
     theme_minimal(base_family = "Avenir") + 
   labs(y = "MW",
        x = "Completed",
        title = "Projection of monthly new renewable capacity from current pipeline",
        subtitle = "",
        caption = paste('timestamp:',Sys.time(),'bootstrap:',nrun)) 
 
 ### Cumulative SUM----
 
 current_projects_outcome_cumulative_first <- current_projects_outcome_first 
 
 
 
 current_projects_outcome_cumulative_first <- current_projects_outcome_cumulative_first |> 
   group_by(finished,
            run) |> 
   summarise(MW = cumsum(`MW`) ) |>
   group_by(run) |> 
   arrange((finished)) |> 
   # count(passed_construction_date,tech,wt = MW) |> 
   mutate(cs = cumsum(MW)) 
 
 current_projects_outcome_cumulative <- current_projects_outcome_cumulative_first |> 
 group_by(finished) |> 
   summarise(
     mean   = mean(cs, na.rm = TRUE,trim=0.05),
     median = median(cs, na.rm = TRUE),
     sd     = sd(cs, na.rm = TRUE),
     n      = nrun,#dplyr::n(),
     se     = sd / sqrt(n),
     # CLT / t-interval
     
     u=mean+2*se,
     l=mean-2*se,
     lwr_norm = mean + qt(alpha/2, df = n - 1) * se,
     upr_norm = mean + qt(1 - alpha/2, df = n - 1) * se,
     # Bootstrap percentile CI
     lwr_pct  = quantile(cs, probs = alpha/2, names = FALSE, type = 7, na.rm = TRUE),
     upr_pct  = quantile(cs, probs = 1 - alpha/2, names = FALSE, type = 7, na.rm = TRUE),
     .groups  = "drop"
     
   ) 
 
 ggplot(current_projects_outcome_cumulative)+
   geom_line(aes(finished,mean))+
   geom_ribbon(aes(finished,mean,ymin = lwr_pct, ymax = upr_pct),
               alpha = 0.5, fill = "skyblue")+
     theme_minimal(base_family = "Avenir") + 
   labs(y = "MW",
        x = "Completed",
        title = "Projection of Cumulative new renewable capacity from Current pipeline",
        subtitle = "",
        caption = paste('timestamp:',Sys.time(),'bootstrap:',nrun))
 
 
 # select(yr,mn,date,MW,broad_status_start) |> 
 #   group_by(yr,mn) |> 
 #   summarise(MW = sum(MW, na.rm = T)) |> 
 #   ungroup() |> 
 #   mutate(date = as.Date(paste0(yr, '-', mn,'-01'))) |>
 #   arrange(date)
 
 

# expand.grid(yr = 2023:year(end_date),
#             mn = 1:12,
#             tech = c('Wind Onshore','Solar Photovoltaics') )|> 
#   mutate(passed_construction_date = paste0(yr, ifelse(mn < 10, paste0('-0', mn), paste0('-',mn)))) |> 
#   left_join(current_eval, by = c('tech','passed_construction_date')) |> 
#   count(passed_construction_date,tech,wt=MW) |> 
#   group_by(tech) |> 
#   mutate(cumsum = cumsum(n)) |>
#   
#   ggplot()+
#   geom_col(aes(passed_construction_date,cumsum,fill = tech))
# 
#   select(yr,mn,date,MW,broad_status_start) |> 
#   group_by(yr,mn) |> 
#   summarise(MW = sum(MW, na.rm = T)) |> 
#   ungroup() |> 
#   mutate(date = as.Date(paste0(yr, '-', mn,'-01'))) |>
#   arrange(date)
  
  
  

# get transition probs (from shiny app)

# input +/- 10%

  
# get offshore wind estimate and date
# 
# 500MW  2029-01
# 
# set baseline 
# 
# 2025 renewable 3161.9 GWhr
# 
# \textbf{GW (nameplate)}=\frac{\text{TWh/yr}}{8.76\times \text{CF}}
# 
# 
# GW = TWh.yr / (8.76 *CF)
# 
# TWhr.yr = GW * 8.76 * CF
# 
# # 3.1619/(8.76*0.22) = 1.64 GW = 1640 MW
# 8/(8.76*0.22) = 4.1511 GW = 4151 MW
# 
# then set duration
# sample progression/ no progression immediately
# 
# 
# sample projects progression through a stage as a uniform distribution of stage duration
# 
# duration 50 weeks
# runif(1,1,50)
# 
# then apply probability of progressing

 save(
  
 forward_projects_outcome,
 current_projects_outcome,
 forward_projects_outcome_cumulative,
 current_projects_outcome_cumulative,
 forward_projects_outcome_first,
 current_projects_outcome_first,
file = 'init_vars.RData')


