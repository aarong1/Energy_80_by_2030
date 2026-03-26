sticky_side_bar <- function(ns) {
  div(style = 'position:sticky; top:8%; height:90vh;',
      div(class = 'py-2 px-5 me-5 rounded-5 position-relative',
          
          span(class="badge bg-info rounded-2 rounded-pill",
               style="top:-15px; left:10px; position:absolute;",
               h6(style="text-align:center;color:white;", '80% by 2030')
          ),
          
          div(class = '',
                # 'alert alert-success',
              style = "background-color:#2196F3; justify-content:center;border-radius:25px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
              tags$small(class='mb-1 text-white text-center','Proportion of Demand Renewable Sources'),
              h3(textOutput(ns("last_year_target_pct"), inline = TRUE),tags$span("%"), style = "color:white;"),
              tags$small(class = 'text-centre text-center','Total RES/ Demand'),
              tags$h4(class = 'mt-3 text-white text-centre text-center','by 2030')
          ),

          div(
            style = "background:white;border:5px solid cornflowerblue;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
            tags$small(class='text-muted','SNSP'),
            h5(textOutput(ns('last_year_snsp')), '%'),
            # tags$small(class='text-muted text-black text-center','Pre-planning Pipeline'),
            tags$div(class='h4 text-black text-centre text-center','up to end 2030')
          ),
          
          div(
            style = "background:white;border:5px solid #FFDE21;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:10px auto;",
            tags$small(class='text-muted', 'Net Energy Flow'),
            h3(textOutput(ns('last_year_net_energy_flow')), 'MW'),
            tags$small(class='text-muted text-centre text-center','current pipeline to 2030')
          ),

        
          
          div(class = 'shadow-sm',
              style = "background:white;border:5px solid salmon;justify-content:center;border-radius:35px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
              tags$small(class='text-muted', 'Change in Dispatch Down'),
              h4(span(textOutput(ns('last_year_change_in_dispatch_down')), "MW/yr")),
              tags$small(class='text-muted text-centre text-center',' by 2030')
          )
      )
  )
}