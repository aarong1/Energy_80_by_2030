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
              tags$small(class='mb-1 text-muted text-center','Proportion of Demand Renewable Sources'),
              h3(textOutput(ns("last_year_target_pct"), inline = TRUE),tags$span("%"), style = "color:white;"),
              tags$small(class = 'text-muted text-centre text-center','Total RES/ Demand'),
              tags$h4(class = 'mt-3 text-white text-centre text-center','by 2030')
          ),
          
          # div(
          #   style = "background:white;justify-content:center;border-radius:55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
          #   tags$small(class='text-muted', 'Cumulative Total New Capacity'),
          #   h4(span(textOutput(ns('total_new_res_target_date1')), "MW/yr")),
          #   tags$small(class='text-muted text-centre text-center','all new RES by 2030')
          # ),
          # 
          # div(
          #   style = "background:white;border:5px solid salmon;justify-content:center;border-radius:55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:10px auto;",
          #   tags$small(class='text-muted', 'Annual New Capacity'),
          #   h3(textOutput(ns('new_res_current_cumulative_target_date1')), 'MW'),
          #   tags$small(class='text-muted text-centre text-center','current pipeline to 2030')
          # ),
          # 
          # div(
          #   style = "background:white;border:5px solid salmon;justify-content:center;border-radius:55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin:20px auto;",
          #   tags$small(class='text-muted','Annual New Capacity'),
          #   h5(textOutput(ns('new_res_preplanning_cumulative_target_date1')), 'MW'),
          #   tags$small(class='text-muted text-centre text-center','Pre-planning Pipeline'),
          #   tags$div(class='h4 text-black text-centre text-center','up to end 2030')
          # )
      )
  )
}