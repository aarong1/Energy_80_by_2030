library(tidyverse)
library(shiny)
library(bslib)
library(htmltools)
'red' #ff4741


circular_value <- function(value=50,id =runif(1) ){
  
  labels = c('below cost effective', 
             'lower threshold cost effective',
             'upper threshold cost effective',
             'cost effective'
  )
  
  below_values =c(0,30, 35, 40, 45, 55, 60, 65, 70, 80)
  
  colours = rev(c('mediumseagreen','lightgreen','lightgoldenrodyellow','yellow','moccasin','orange','lightsalmon','lightcoral', '#ff4741','#000'))
  
  colours_index <- max(which(!value<=below_values))
  print(value)
  print(colours_index)
  print(colours)
  print(which(value<below_values))
  print(colours[colours_index])
  div(
    # border:solid ',colours[colours_index],' 15px ; /* lightgreen */
    #   border-radius:50%;
    # transform: scale(1);
    # padding-top:1%;
    # width:100%;
    # height:100%;
    # transition: transform 1s ease-in-out;
   
    HTML(paste0('<head>
    
    <style>
   .inner{  
     border-radius:50%;

  padding-top:1%;
  width:100%;
  height:100%;
  transform: scale(1);
  transition: transform 1s ease-in-out;
   }
  
  .inner:hover {
    transform: scale(1.2);
          transition: transform 0.5s ease-in-out;
          
  .content:hover {
    transform: none !important;
    
  </style>
       </head>')),
    div(
      div(class='outer',style ='margin:10px;width:100%;height:100%;border:solid 15px cyan;border-radius:50%;aspect-ratio: 1 / 1;', ##13b5cb
          
          div(id =as.character(id),class ='inner',
              
              style = paste0('
  border:solid ',colours[colours_index],' 15px ; /* lightgreen */ 

    '),
              div( class='content',
                   style ='padding-top:1%;

border-radius:50%;

font-weight:bold;

    display:flex;

    align-items:center;

    align-content:center;
    
    justify-content: space-around;

    flex-direction:column;

    gap:-1rem;',
                   
                   
                   #     div(style='text-align:center;',
                   #         p('ICER'),
                   # p(style = 'font-size:10px;','Incremental Cost Effectiveness Ratio')) ,
                   
                   div(style='text-align:center;margin-top:-5px;',
                       p(style = 'display:inline-block;font-size:13px;',
                         #div(class='myTargetElement', style = 'display:inline-block;width:95%;font-size:1.5rem',value,'%'), 
                         # p(style = ';',''),
                         p(style = 'display:inline-block;',class = 'badge pill-rounded bg-purple', 'RES/Demand'),
                         br(),
                         p(class='badge bg-info',value,'%')))),
              
              # div(style='text-align:center;',
              # )
          ))
    )
  )
}
# 
# browsable(page_fluid(
#   circular_value(85)
# ))




big_circular_value <- function(value=50,id =runif(1) ){
  
  labels = c('below cost effective', 
             'lower threshold cost effective',
             'upper threshold cost effective',
             'cost effective'
  )
  
  below_values =c(0,30, 35, 40, 45, 55, 60, 65, 70, 80)
  
  colours = rev(c('mediumseagreen','lightgreen','lightgoldenrodyellow','yellow','moccasin','orange','lightsalmon','lightcoral', '#ff4741','#000'))
  
  colours_index <- max(which(!value<=below_values))
  print(value)
  print(colours_index)
  print(colours)
  print(which(value<below_values))
  print(colours[colours_index])
  div(
    # border:solid ',colours[colours_index],' 15px ; /* lightgreen */
    #   border-radius:50%;
    # transform: scale(1);
    # padding-top:1%;
    # width:100%;
    # height:100%;
    # transition: transform 1s ease-in-out;
    
    HTML(paste0('<head>
    
    <style>
   .inner{  
     border-radius:50%;

  padding-top:1%;
  width:100%;
  height:100%;
  transform: scale(1);
  transition: transform 1s ease-in-out;
   }
  
  .inner:hover {
    transform: scale(1.2);
          transition: transform 0.5s ease-in-out;
          
  .content:hover {
    transform: none !important;
    
  </style>
       </head>')),
    div(
      div(class='outer',style ='margin:10px;width:100%;height:100%;border:solid 15px white;border-radius:50%;aspect-ratio: 1 / 1;', ##13b5cb
          
          div(id =as.character(id),class ='inner',
              
              style = paste0('
  border:solid ',colours[colours_index],' 35px ; /* lightgreen */ 

    '),
              div( class='content',
                   style ='padding-top:1%;
                   
                   height:100%;


border-radius:50%;

font-weight:bold;

    display:flex;

    align-items:center;

    align-content:center;
    
    justify-content: space-around;

    flex-direction:column;

    gap:-1rem;',
                   
                   
                   #     div(style='text-align:center;',
                   #         p('ICER'),
                   # p(style = 'font-size:10px;','Incremental Cost Effectiveness Ratio')) ,
                   
                   div(style='text-align:center;margin-top:-5px;',
                       p(style = 'display:inline-block;font-size:13px;',
                         #div(class='myTargetElement', style = 'display:inline-block;width:95%;font-size:1.5rem',value,'%'), 
                         # p(style = ';',''),
                         p(style = 'display:inline-block;',class = 'badge pill-rounded bg-info-subtle text-dark', 'RES/Demand'),
                         br(),
                         p(class=' fs-5 badge bg-info',value,'%')))),
              
              # div(style='text-align:center;',
              # )
          ))
    )
  )
}
# 
# browsable(page_fluid(
#   circular_value(85)
# ))

