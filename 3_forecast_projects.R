#forecast_projects.R

end_date = '2039-12-01'


# Forecast Projects forward in time
# for Solar PV and and Onshore Wind

(pipeline_forecast_prep <- pipeline |> 
  group_by(
  date = format.Date (pipeline$`Planning Application Submitted`, format = '%Y-%m'),
  month =( month (pipeline$`Planning Application Submitted`) ),
  tech,
  year = (year (pipeline$`Planning Application Submitted`))
  )  |> 
  
  summarise( n=n()) |> 
  #filter(month> '2017-01')

  filter(year >2022) 
  
)


#y <- cbind(y,pred = predictions)

projections_projects <- expand.grid(year = min(pipeline_forecast_prep$year): (max(pipeline_forecast_prep$year)-1),
            month = 01:12,
            tech = unique(pipeline_forecast_prep$tech)) |>
  mutate(Date = as.Date(format = '%Y-%m-%d',paste0(year,
                              ifelse(month <10, paste0('-0', month,'-'), paste0('-',month,'-')),
                              "01",
                       sep = ''))) |>
  mutate(date = paste0(year, 
                       ifelse(month <10, paste0('-0', month), paste0('-',month))
                      )) |>
  left_join(pipeline_forecast_prep, by = c('date', 'tech','year','month')) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  arrange(date) |>
  mutate(type = 'actual')
  

projections_projects |>
  ggplot() +
  geom_col(aes(date, n, fill= tech)) +
  theme(axis.text.x = element_text(angle = 90,   # rotation angle
                                   vjust = 1,    # vertical justification
                                   hjust = 1))


# Conservative ----


pd <- glm(data = projections_projects,
          formula = n ~ month*tech+year+offset(log(n+1)), family = 'quasipoisson')


pred = predict(pd, newdata = projections_projects, type = 'response')
addnew <- projections_projects |> mutate(n = as.numeric(pred), type = 'predicted')
projections_projects_conservative <- rbind(projections_projects,addnew)



newdata <- expand.grid(year = (max(pipeline_forecast_prep$year)): year(end_date),
                 month = 01:12,
                 tech = unique(pipeline_forecast_prep$tech)) |>
  mutate(Date = as.Date(format = '%Y-%m-%d',paste0(year,
                                                   ifelse(month <10, paste0('-0', month,'-'), paste0('-',month,'-')),
                                                   "01",
                                                   sep = ''))) |>
  mutate(date = paste0(year, 
                       ifelse(month <10, paste0('-0', month), paste0('-',month))
  )) |>
  left_join(pipeline_forecast_prep, by = c('date', 'tech','year','month')) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  arrange(date) |>
  mutate(type = 'forecast')

pred2 = predict(pd, newdata = newdata, type = 'response')

newdata <- newdata|> mutate(n = as.numeric(pred2))
projections_projects_conservative <- rbind(projections_projects_conservative,newdata)


projections_projects_conservative |>
  ggplot() +
  geom_line(aes(Date, n, group = type, color= type)) +
  geom_point(aes(Date, n, group = type, color= type)) +
  
  # geom_point(aes(date, n, group = tech, color= type)) +
  
  # geom_point(aes(date, pred,color=tech)) +
    theme_minimal(base_family = "Avenir") +
  facet_grid(vars(tech) ) + #ylim(0,10) +
  theme(axis.text.x = element_text(angle = 90,   # rotation angle
                                   vjust = 1,    # vertical justification
                                   hjust = 1)) +
  labs(title = 'Projection of Project Planning Appliation Submissions not in the the Pppeline',
      subtitle = 'Conservative stimate pre-planning pipeline. We estimated all of 2025 regardless of \nsome applications already being submitted',
      y='Projects')

# Optimistic ----

pd_optimistic <- glm(data = projections_projects,
                     formula = n ~ month*tech + year, family = 'quasipoisson')



pred_optimistic = predict(pd_optimistic, newdata = projections_projects, type = 'response')
addnew <- projections_projects |> mutate(n = as.numeric(pred_optimistic), type = 'predicted')
projections_projects_optimistic <- rbind(projections_projects,addnew)



newdata <- expand.grid(year = (max(pipeline_forecast_prep$year)-1): year(end_date),#(max(pipeline_forecast_prep$year)+3),
                       month = 01:12,
                       tech = unique(pipeline_forecast_prep$tech)) |>
  mutate(Date = as.Date(format = '%Y-%m-%d',paste0(year,
                                                   ifelse(month <10, paste0('-0', month,'-'), paste0('-',month,'-')),
                                                   "01",
                                                   sep = ''))) |>
  mutate(date = paste0(year, 
                       ifelse(month <10, paste0('-0', month), paste0('-',month))
  )) |>
  left_join(pipeline_forecast_prep, by = c('date', 'tech','year','month')) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  arrange(date) |>
  mutate(type = 'forecast')

pred_optimistic = predict(pd_optimistic, newdata = newdata, type = 'response')

newdata <- newdata |> mutate(n = as.numeric(pred_optimistic))
projections_projects_optimistic <- rbind(projections_projects_optimistic,newdata)


projections_projects_optimistic|>
  mutate(type = factor(type, levels = c('actual','forecast','predicted')) )|>
  
  ggplot() +
  geom_line(aes(Date, n, group = type, color= type)) +
  geom_point(aes(Date, n, group = type, color= type)) +
  
  # geom_point(aes(date, n, group = tech, color= type)) +
  
  # geom_point(aes(date, pred,color=tech)) +
    theme_minimal(base_family = "Avenir") +
  facet_grid(vars(tech) ) + #ylim(0,10) +
  theme(axis.text.x = element_text(angle = 90,   # rotation angle
                                   vjust = 1,    # vertical justification
                                   hjust = 1)) +
  labs(title = 'Projection of Project Planning Appliation Submissions not in the the Pppeline',
       subtitle = 'Optimistic estimate pre-planning pipeline. We estimated all of 2025 regardless of \nsome applications already being submitted',
       y='Projects')


# Sampled ----

sampled_survey <- pipeline_forecast_prep |> 
  mutate(month = as.numeric(str_split_1(date ,'-')[2])) |> 
  group_by(tech, month) |>
  summarise(n=mean(as.numeric(n)))

sampled_survey <- expand.grid(month = 01:12,
            tech = unique(pipeline_forecast_prep$tech)) |>
  left_join(sampled_survey) |> 
  replace_na(list(n=0)) 

new_data = expand.grid(year = (max(pipeline_forecast_prep$year)-1): year(end_date),#(max(pipeline_forecast_prep$year)+3),
            month = 01:12,
            tech = unique(pipeline_forecast_prep$tech)) |>
  mutate(mon = ifelse(month <10, paste0('0', month),month)) |> 

  mutate(Date = as.Date(format = '%Y-%m-%d',paste0(year,
                                                   ifelse(month <10, paste0('-0', month,'-'), paste0('-',month,'-')),
                                                   "01",
                                                   sep = ''))) |>
  mutate(date = paste0(year, 
                       ifelse(month <10, paste0('-0', month), paste0('-',month))
  )) |>
  left_join(pipeline_forecast_prep, by = c('date', 'tech','year','month')) |>
  mutate(n = ifelse(is.na(n), 0, n)) |>
  arrange(date) |>
  mutate(type = 'forecast')

projections_projects_survey <- new_data |> 
  
  select(-n) |> 
  left_join(sampled_survey,by=c('tech','month')) |> 
  replace_na(list(n=0))

actual_data <- #new_data |> 
  #filter(Date<'2025-01-01') |> 
  #projections_projects_survey |>
  
  pipeline_forecast_prep |> 
  mutate(Date = as.Date(format = '%Y-%m-%d',paste0(year,
                                                 ifelse(month <10, paste0('-0', month,'-'), paste0('-',month,'-')),
                                                 "01",
                                                 sep = ''))) |> 
  mutate(mon = as.numeric(str_split_1(date ,'-')[2])) |> 
  mutate(type='actual') 

predicted_data <- new_data |> 
  filter(Date<'2025-01-01') |> 
  mutate(type='predicted') 

projections_projects_survey <- rbind(projections_projects_survey,actual_data)

projections_projects_survey <- rbind(projections_projects_survey,predicted_data)


projections_projects_survey |>
  mutate(type = factor(type, levels = c('actual','forecast','predicted')) )|>
  ggplot() +
  geom_line(aes(Date, n, group = type, color= type)) +
  geom_point(aes(Date, n, group = type, color= type)) +
  
  # geom_point(aes(date, n, group = tech, color= type)) +
  
  # geom_point(aes(date, pred,color=tech)) +
    theme_minimal(base_family = "Avenir") + 
  facet_grid(vars(tech) ) + #ylim(0,10) +
  theme(axis.text.x = element_text(angle = 90,   # rotation angle
                                   vjust = 1,    # vertical justification
                                   hjust = 1)) +
  labs(title = 'Projection of Project Planning Appliation Submissions not in the the Pppeline',
       subtitle = 'Surveyed estimate pre-planning pipeline.\n
       We estimated all of 2025 regardless of some applications already being submitted',
       y = 'Projects')



#Offshore Wind ----

ggplot() +
  annotate('line',color='green',x = c(2025:2035,2029), y = c(rep(0,5),rep(0.6,6),0.6)) +
  geom_hline(yintercept=1) +
  geom_ribbon(fill='green',alpha=0.4,aes(x = c(2035,2029), y = c(0,0.6), xmin = c(2029),ymin = c(0),ymax=0.6)) +
  ylim(c(0,1.4)) +
  theme_minimal(base_family = "Avenir") +
  labs(title = 'Projected capacity estimates of offshore wind',
       subtitle = 'Offshore only. Absolute point estimate',
       y = 'MW',
       x = 'Completion')+
  scale_x_continuous(breaks = seq(2025, 2035, 1))  # <--- force integer labels




