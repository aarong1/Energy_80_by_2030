
transition_probs_empirical <- transition_probs <- function() {
  transition_probs <- tribble(
    ~from, ~to, ~prob,
    'Planning', 'Connection', 77.54/100,
    'Connection', 'Construction', 78.02/100,
    'Construction', 'Completed', 100/100
  )
  transition_probs
}


stage_duration_empirical <- stage_duration <- function() {
  
  stage_duration <- tribble(
    ~from, ~to,~tech, ~wks,
    'Planning', 'Connection','Solar Photovoltaic', 33.70714,
    'Connection', 'Construction','Solar Photovoltaic', 70.88571,
    'Construction', 'Completed','Solar Photovoltaic', 17.59184,
    
    'Planning', 'Connection','Wind Onshore', 116.08599 ,
    'Connection', 'Construction','Wind Onshore', 182.20476,
    'Construction', 'Completed','Wind Onshore', 51.72167,
  )
  stage_duration
  
}



lift_historic_projects <- function(pipeline, tech, date, n) {
  historic_projects <- pipeline |> 
    filter(tech == {{tech}}) |> 
    slice_sample(n = {{n}}) |> 
    mutate(Date = {{date}}) |> 
    select(`Installed Capacity (MWelec)`,tech,`Ref ID`,Date)
  
  return(historic_projects)
  
}

get_empirical_time <- function(x, tech, stage) {
  sample(size = 1, na.omit(pipeline[pipeline$tech == {{tech}},][[{{stage}}]]))
}

get_empirical_time(pipeline,'Solar Photovoltaics','Planning')
get_empirical_time(pipeline,'Solar Photovoltaics','Connection')
get_empirical_time(pipeline,'Solar Photovoltaics','Construction')



