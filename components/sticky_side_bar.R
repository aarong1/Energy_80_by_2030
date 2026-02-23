sticky_side_bar <- function(){

div(style = 'position:sticky; top:8%; height:90vh;',
    
    # Loading overlay - centered and doesn't affect layout
    # div(id= 'loading', 
    #     class = 'alert alert-danger rounded-3 p-3',
    #     style = 'position:fixed;top:5%;left:90%;transform:translate(-50%,-50%);z-index:9999;opacity:1;display:none;box-shadow:0 4px 6px rgba(0,0,0,0.3);',
    #     div(class='d-flex gap-3 align-items-center',
    #         span(class="loader"),
    #         
    #         h4(class = 'mb-0 lead','Loading '),
    #         span(class = 'badge pill-rounded text-bg-light',
    #              style = '',
    #              tags$small(class=' mb-0','Inputs locked')
    #         )
    #     )
    #     
    # ),
    #bg-danger-subtle
    div(class = 'py-2 px-5 me-5  rounded-5 position-relative',#offset = 1, mx-5
        
        # div(class = 'alert alert-primary rounded-2 top-0 start-100',
        #     h3(style = 'text-align:center',':80% Target:')),
        
        span( class="badge bg-info rounded-2 rounded-pill",
              style="
            top: -15px;
            left: 10px;
           position: absolute;",
              
              h6(style="text-align:center;color:white;",'80% by 2030')
        ),
        
        div(class = 'alert alert-success',
            style =
              "justify-content: center;border-radius: 25px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
            tags$small(class = 'mb-1 text-muted text-center', 'Proportion of Demand Renewable Sources'),
            h3(textOutput(inline = T, 'target_pct_target_date'),"%"),
            #h5( "2 MW/yr"),
            tags$small(class = 'text-muted text-centre text-center','Total RES/ TER'),
            tags$h4(class = 'mt-3text-white text-centre text-center','by 2030')
            
        ),
        div(
          style =
            "background:white;justify-content: center;border-radius: 55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
          tags$small(class = 'text-muted', 'Cumulative Total New Capacity'),
          h4(span(textOutput(inline = T,'total_new_res_target_date'),"MW/yr")),
          #h4( "2 MW/yr"),
          tags$small(class = 'text-muted text-centre text-center','all new RES by 2030'),
        ),
        div(
          style =
            "background:white;border:5px solid salmon;justify-content: center;border-radius: 55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 10px auto;",
          tags$small(class = 'text-muted', 'Annual New Capacity'),
          # h3( "2 MW/yr"),
          h3(textOutput(inline = T,'new_res_current_cumulative_target_date'),'MW'),
          tags$small(class = 'text-muted text-centre text-center','current pipeline to 2030'),
        ), 
        div(
          style =
            "background:white;border:5px solid salmon;justify-content: center;border-radius: 55px;display:flex;flex-direction:column;align-items:center;width:150px;height:150px;margin: 20px auto;",
          tags$small(class = 'text-muted', 'Annual New Capacity'),
          # h5( "2 MW/yr"),
          h5(textOutput(inline = T,'new_res_preplanning_cumulative_target_date'),'MW'),
          tags$small(class = 'text-muted text-centre text-center','Pre-planning Pipeline'),
          
          tags$div(class = 'h4 text-black text-centre text-center','up to end 2030')
        )
    )
)
  
}
