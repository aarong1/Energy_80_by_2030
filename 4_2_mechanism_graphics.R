
# Pre-planning  ----
## Cumulative ----

preplanning_cumulative_echart <- function(forward_projects_outcome_cumulative) {
  
  x <- ungroup(forward_projects_outcome_cumulative) %>%
    mutate(Lower = lwr_pct,
           Upper = upr_pct) |> 
    
    e_charts(finished, backgroundColor='white', borderRadius =50) %>%
    
    e_line(name = 'Upper',
           symbol= 'none',
           serie = Upper,
           #stack = 'confidence-band',
           legend = F,
           
           color = "#ff0000",
           areaStyle =
             list(color = "#ff0000", opacity = 0.4
             )
    ) |> 
    # 95% interval ribbon (between lwr_pct and upr_pct)
    e_line(name = 'Lower',
           symbol= 'none',
           serie = Lower,
           #stack = 'confidence-band',
           #max = Upper,
           legend = F,
           color = "#ff0000",
           itemStyle = list(
             list(color = "white", opacity = 1)
           ),
           areaStyle =
             list(color = "white", opacity = 1)
    ) %>%
    e_line(mean, name = "Pre-planning",
           symbolSize = 0,
           color = "black"
    ) |> 
    # e_title(
    #   text = "Projected capacity estimates per year from Anticipated Projects",
    #   subtext = paste0(
    #     "Projections to end 2031. Pre-planning.", #\n
    #     " Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"), 
    #     ",   Bootstrap: ", nrun)
    # ) %>%
    e_grid(top = "25%", bottom = "10%", left = "10%", right = "15%") |>
    e_legend(top = "15%")  |> # right = "5%", orient = "horizontal"
    e_tooltip(trigger = "axis") %>%
    e_x_axis(name = "Completion") %>%
    e_y_axis(name = "MW") |> 
    e_x_axis(
      axisLine   = list(lineStyle = list(color = "#eee")),  # light axis line
      axisLabel  = list(color = "#ccc"),                    # light text
      splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2")) # light grid lines
    ) |>
    e_y_axis(
      axisLine   = list(lineStyle = list(color = "#eee")),
      axisLabel  = list(color = "#ccc"),
      splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2"))
    )    ;x

}

#preplanning_cumulative_echart(forward_projects_outcome_cumulative)

# Pre-planning ----
## Yearly ----

preplanning_yearly_echart <- function(forward_projects_outcome) {

e <- ungroup(forward_projects_outcome) %>%
  mutate(Lower = lwr_pct,
         Upper = upr_pct) |> 
  
  e_charts(finished) %>%
  
  e_line(name = 'Upper',
         symbol= 'none',
         serie = upr_norm,
         
         legend = F,
         color = "rgba(255, 0, 0, 0.4)",      
         
         #itemStyle = list(color = "white"),      
         areaStyle = 
           list(color = "#ff0000", opacity = 0.4
           )
  ) |> 
  # 95% interval ribbon (between lwr_pct and upr_pct)
  e_line(name = 'Lower',
         symbol= 'none',
         serie = lwr_norm,
         #max = Upper,
         legend = F,
         color = "rgba(255, 0, 0, 0.4)",      
         # itemStyle = list(
         #   list(color = "white", opacity = 1)
         # ),
         areaStyle = 
           list(color = "white", opacity =1)
  ) %>%
  e_line(mean, name = "Pre-planning",
         symbolSize = 0,
         color = "darkred"
  ) |> 
  # e_title(
  #   text = "Projected capacity estimates per year from Anticipated Projects",
  #   subtext = paste0(
  #     "Projections for Pre-planning.", #\n
  #     " Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"), 
  #     ",   Bootstrap: ", nrun)
  # ) %>%
  e_grid(top = "25%", bottom = "10%", left = "10%", right = "15%") |>
  e_legend(top = "15%")  |> # right = "5%", orient = "horizontal"
  e_tooltip(trigger = "axis") %>%
  e_x_axis(name = "Completion") %>%
  e_y_axis(name = "MW")    |> 
  e_x_axis(
    axisLine   = list(lineStyle = list(color = "#eee")),  # light axis line
    axisLabel  = list(color = "#ccc"),                    # light text
    splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2")) # light grid lines
  ) |>
  e_y_axis(
    axisLine   = list(lineStyle = list(color = "#eee")),
    axisLabel  = list(color = "#ccc"),
    splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2"))
  ) ;e

}

#preplanning_yearly_echart(forward_projects_outcome)
  
 ###  --- ----

# Current Projects ----

## cumulative ----

current_cumulative_echart <- function(current_projects_outcome_cumulative){


ungroup(current_projects_outcome_cumulative) %>%
  mutate(Lower = lwr_pct,
         Upper = upr_pct) |> 
  
  e_charts(finished) %>%
  
  e_line(name = 'Upper',
         symbol= 'none',
         serie = Upper,
         
         legend = F,
         
         color = "rgba(255, 0, 0, 0.4)",      
         areaStyle = 
           list(color = "skyblue", opacity = 0.4
           )
  ) |> 
  # 95% interval ribbon (between lwr_pct and upr_pct)
  e_line(name = 'Lower',
         symbol= 'none',
         serie = Lower,
         #max = Upper,
         legend = F,
         color = "rgba(255, 0, 0, 0.4)",
         itemStyle = list(
           list(color = "white", opacity = 1)
         ),
         areaStyle = 
           list(color = "white", opacity =1)
  ) %>%
  e_line(mean, name = "Current",
         symbolSize = 0,
         color = "black"
  ) |> 
  # e_title(
  #   text = "Projected capacity estimates per year from Anticipated Projects",
  #   subtext = paste0(
  #     "Projections to end 2031. Current", #\n
  #     " Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"), 
  #     ",   Bootstrap: ", nrun)
  # ) %>%
  e_grid(top = "25%", bottom = "10%", left = "10%", right = "15%") |>
  e_legend(top = "15%")  |> # right = "5%", orient = "horizontal"
  e_tooltip(trigger = "axis") %>%
  e_x_axis(name = "Completion") %>%
  e_y_axis(name = "MW")    |> 
  e_x_axis(
    axisLine   = list(lineStyle = list(color = "#eee")),  # light axis line
    axisLabel  = list(color = "#ccc"),                    # light text
    splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2")) # light grid lines
  ) |>
  e_y_axis(
    axisLine   = list(lineStyle = list(color = "#eee")),
    axisLabel  = list(color = "#ccc"),
    splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2"))
  ) 

}

#current_cumulative_echart(current_projects_outcome_cumulative)
  

## Yearly ----


current_yearly_echart <- function(current_projects_outcome){


ungroup(current_projects_outcome) %>%
    mutate(rn =row_number()) |> 
    #filter(mean >0.5) |>
    #sample_frac(0.9) %>%
    arrange(rn) |> 
  rowwise() %>%
  mutate(Lower = max(0,l, na.rm=TRUE),
         Upper = u) |> 
  ungroup() |>
  
  e_charts(finished) %>%
  
  e_line(name = 'Upper',
         symbol= 'none',
         smooth = T,
         serie = Upper,
         
         legend = F,
         color = "rgba(255, 0, 0, 0.4)",
         
         #itemStyle = list(color = "white"),
         areaStyle =
           list(color = "skyblue", opacity = 0.4
           )
  ) |>
  # 95% interval ribbon (between lwr_pct and upr_pct)
  e_line(name = 'Lower',
         symbol= 'none',
         serie = Lower,
         smooth = T,
         #max = Upper,
         legend = F,
         color = "rgba(255, 0, 0, 0.4)",
         # itemStyle = list(
         #   list(color = "white", opacity = 1)
         # ),
         areaStyle =
           list(color = "white", opacity =1)
  ) %>%
  e_line(mean, name = "Current",
         symbolSize = 0,
         color = "black"
  ) |> 
  # e_title(
  #   text = "Projected capacity estimates per year from Anticipated Projects",
  #   subtext = paste0(
  #     "Projections for Current Pipeline of projects.", #\n
  #     " Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"), 
  #     ",   Bootstrap: ", nrun)
  # ) %>%
  e_grid(top = "25%", bottom = "10%", left = "10%", right = "15%") |>
  e_legend(top = "15%")  |> # right = "5%", orient = "horizontal"
  e_tooltip(trigger = "axis") %>%
  e_x_axis(name = "Completion") %>%
  e_y_axis(name = "MW")     |> 
  e_x_axis(
    axisLine   = list(lineStyle = list(color = "#eee")),  # light axis line
    axisLabel  = list(color = "#ccc"),                    # light text
    splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2")) # light grid lines
  ) |>
  e_y_axis(
    axisLine   = list(lineStyle = list(color = "#eee")),
    axisLabel  = list(color = "#ccc"),
    splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2"))
  ) 

}

#current_yearly_echart(current_projects_outcome)
  

offshore_wind_plot <- function( offshore_wind_date, offshore_wind_capacity, offshore_included) {
  
  # Offshore Wind ----
  
  year = 2025: (year(end_date)+1)
  
  df <- tibble(
    year = year
  )
  df2 <- tibble(
    
    year = year(offshore_wind_date) ,
    mw = offshore_wind_capacity*offshore_included
  )
  
  df <- left_join(df, df2) |> 
    fill(mw) |> 
    replace_na(list(mw=0)) 
  
  df |>
    mutate(year = as.character(year)) |> 
    mutate(mw=mw*1000) |> 
    e_charts(year) |>
    e_line(mw, step = "end", name = "Offshore", symbol = 'none',
           itemStyle = list(color='limegreen',opacity = 0.6),
    
           areaStyle = list(color='limegreen',opacity = 0.6)) |>
    e_legend() |> 
    # e_title(
    #   "Projected capacity estimates of offshore wind",
    #   "Offshore only. Point Estimate of Capacity and operational Date"
    # ) |>  
    e_axis_labels(x = "Completion", y = "MW") |>
    e_tooltip(trigger = "axis") |>
    e_y_axis(min = 0) |>
    e_legend(show = FALSE) |>
    e_grid(top = "25%", bottom = "10%", left = "10%", right = "15%") |>
    e_x_axis(max = as.character(year(end_date)),
      axisLine   = list(lineStyle = list(color = "#eee")),  # light axis line
      axisLabel  = list(color = "#ccc"),                    # light text
      splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2")) # light grid lines
    ) |>
    e_y_axis(max =1000,
      axisLine   = list(lineStyle = list(color = "#eee")),
      axisLabel  = list(color = "#ccc"),
      splitLine  = list(show = TRUE, lineStyle = list(color = "#f2f2f2"))
    ) 
    # Add a caption using the graphic component
    # e_graphic(list(
    #   type = "text",
    #   left = "center",
    #   bottom = 2,
    #   style = list(
    #     text = paste("timestamp:", Sys.time(), "bootstrap:", nrun),
    #     fontSize = 12,
    #     fill = "#666"
    #   )
    # )
  # )

}

#offshore_wind_plot( offshore_wind_date, offshore_wind_capacity)

# # Echarts w option ----
# 
#   title = list(
#     text = "Confidence Band",
# option <- list(
#     subtext = "Example in MetricsGraphics.js",
#     left = "center"
#   ),
#   tooltip = list(
#     trigger = "axis",
#     axisPointer = list(
#       type = "cross",
#       animation = FALSE,
#       label = list(
#         backgroundColor = "#ccc",
#         borderColor = "#aaa",
#         borderWidth = 1,
#         shadowBlur = 0,
#         shadowOffsetX = 0,
#         shadowOffsetY = 0,
#         color = "#222"
#       )
#     ),
#     formatter = htmlwidgets::JS(
#       "function (params) {
#         return (
#           params[2].name +
#           '<br />' +
#           ((params[2].value - base) * 100).toFixed(1) + '%'
#         );
#       }"
#     )
#   ),
#   grid = list(
#     left = "3%",
#     right = "4%",
#     bottom = "3%",
#     containLabel = TRUE
#   ),
#   xAxis = list(
#     type = "category",
#     data = 2010:2020,
#     axisLabel = list(
#       formatter = htmlwidgets::JS(
#         "function (value, idx) {
#           var date = new Date(value);
#           return idx === 0 ? value : [date.getMonth() + 1, date.getDate()].join('-');
#         }"
#       )
#     ),
#     boundaryGap = FALSE
#   ),
#   yAxis = list(
#     axisLabel = list(
#       formatter = htmlwidgets::JS(
#         "function (val) { return (val - base) * 100 + '%'; }"
#       )
#     ),
#     axisPointer = list(
#       label = list(
#         formatter = htmlwidgets::JS(
#           "function (params) {
#             return ((params.value - base) * 100).toFixed(1) + '%';
#           }"
#         )
#       )
#     ),
#     splitNumber = 3
#   ),
#   series = list(
#     list(
#       name = "L",
#       type = "line",
#       data = 1:10,
#       lineStyle = list(opacity = 0),
#       stack = "confidence-band",
#       symbol = "none"
#     ),
#     list(
#       name = "U",
#       type = "line",
#       data = 1:10,
#       lineStyle = list(opacity = 0),
#       areaStyle = list(color = "#ccc"),
#       stack = "confidence-band",
#       symbol = "none"
#     ),
#     list(
#       type = "line",
#       data = 10:20,
#       itemStyle = list(color = "#333"),
#       showSymbol = FALSE
#     )
#   )
# )
# 
# 
# e_chart() |> 
#   e_list(list = option) 
# 
# #|> 


