
startup_overlay_div <- function(overlay_out_time_ms = 8000, main_in_time_ms=7000){
      tags$div(id = "startup-overlay", 
         
              tags$head( tags$script(HTML("
               
               setTimeout(function() {
  $('#main-content').css({
    opacity: 1,
    pointerEvents: 'auto',
    visibility: 'visible'
  });
}, 9000);

  setTimeout(function() {
    const container = $('#datatables_wrapper');
    const originalWidth = container.width();

    container.css('width', originalWidth + 1);
    setTimeout(() => {
      container.css('width', originalWidth);
    }, 50);
  }, 7000); // run after your overlay fades
")),
               
         tags$script(HTML(paste0("
    setTimeout(function() {

        $('#startup-overlay').fadeOut(300);

    }, ",overlay_out_time_ms,");  // 10 seconds = 10000 ms
    //5000
    setTimeout(function() {
      

        $('body').fadeIn('slow');
        $('#datatables_wrapper').fadeIn('slow');
        
                const container = $('#datatables_wrapper');
        const originalWidth = container.width();
        container.css('width', originalWidth + 1); 
        

    }, ",main_in_time_ms,");  // 10 seconds = 10000 ms
    //7000
  ")))),
#page_fluid(
  tags$style('
/* Base skeleton box style for charts */

.shimmer-chart, .strobe-chart {
  background: #000000; /* Black background */
  position: relative;
  overflow: hidden;
  border-radius: 10px;
  z-index: 4;
  /* border: 1px solid white; */
  box-shadow: 5px 5px white;
}

.shimmer-marquee, strobe-marquee {
  background: #000000; /* Black background */
  position: relative;
  overflow: hidden;
  border-radius: 0px;
  z-index: 4;
  border-top: 3px solid white;
  border-bottom: 3px solid white;
}

/* Chart shapes */
.skeleton-bar-horizontal,
.skeleton-bar-vertical,
.skeleton-line-chart,
.skeleton-pie-chart {
 /*  background: white; White base shape to simulate chart */
  position: relative;
}

/* Chart dimensions */
.skeleton-bar-horizontal {
  width: 100%;
  height: 5px;
   border:solid 1px white;
     border-radius: 5px;
  box-shadow: 3px 3px white;
}

.skeleton-bar-vertical {
  /*width: 100px;
  height:100%;
  */
  border:solid 3px white;
  border-radius: 12px;
  box-shadow: 4px 4px white;
}


.skeleton-bar-vertical-white {
  /*width: 100px;
  
  height:100%;
  */
  
  border: solid 5px black;
  border-radius: 10px;
  box-shadow: 3px 3px black;
}

.skeleton-bar-grid-vertical {
  width: 10px;
  height: 100px;
  box-shadow: none;
  background:white;

}

.skeleton-bar-grid-horizontal {
  width: 100%;
  height: 10px;
  box-shadow: none;
  background:white;
}

.skeleton-line-chart {
  width: 100%;
  height: 150px;
}



.grow {
  height: 0;
  width: 40px;
  background-color: white;
  border-radius: 10px;
  animation: growHeight 4s ease-out forwards 2s;
  overflow: hidden;
}

@keyframes growHeight {

  to {
    height: 80%;
  }
}


/* Strobe effect */
.strobe-chart/*::before*/ ,.strobe-marquee::before {
  content: "";
  /*position: absolute;
  top: 0; */
  width: 100%;
  height: 100%;
  background: black; 
   /* background: rgb(0,114,206);*/
  animation: strobe 4s ease-in-out 2s 1 forwards;
  z-index: 5;
  pointer-events: none;
}

@keyframes strobe {
  0% {
    background: black;
  }
  90% {
    background:white;
  }
    100% {
    background:white;
  }
  
}


/* Shimmer effect */
.shimmer-chart:hover::before,.shimmer-marquee:hover::before {
  content: "";
  position: absolute;
  top: 0;
  left: -150%;
  width: 200%;
  height: 100%;
  background: linear-gradient(
    90deg,
    rgba(255, 255, 255, 0) 0%,
    rgba(211, 211, 211, 0.2) 50%,
    rgba(255, 255, 255, 0) 100%
  );
  animation: shimmer 4s ease-in-out 0s 1 alternate ;
  z-index: 5;
  pointer-events: none;
}


/* Shimmer effect .shimmer-chart::before,*/
.shimmer-marquee::before {
  content: "";
  position: absolute;
  top: 0;
  left: -150%;
  width: 200%;
  height: 100%;
  background: linear-gradient(
    90deg,
    rgba(255, 255, 255, 0) 0%,
    rgba(211, 211, 211, 0.2) 50%,
    rgba(255, 255, 255, 0) 100%
  );
  animation: shimmer 3s ease-in-out 0s 2 forwards ;
  z-index: 5;
  pointer-events: none;
}



@keyframes shimmer {
  0% {
    left: -150%;
  }
  100% {
    left: 150%;
  }
}

@keyframes strobe-border {
  30% {
    box-shadow: 10px 5px white;
    border: none;
  }
  100% {
    box-shadow: 0px 0px 3px white;
  }
}

.strobe-border {
  animation: strobe-border 3s ease 2s 1 forwards;
}

.parent {
padding-top:45px;
display: grid;
grid-template-columns: repeat(7, 1fr);
grid-template-rows: 0.2fr repeat(6, 1fr);
grid-column-gap: 35px;
grid-row-gap: 35px;
/* background:black; */
/* background: rgb(0,114,206); */
background: rgba(10,10,10,0.98);

    position: fixed;
    top: 0%; /*110px*/
    left: 0;
    width: 100%;
    height: 100%;
}

.div1 { grid-area: 2 / 1 / 3 / 8; }
.div2 { grid-area: 3 / 5 / 5 / 7; }
.div3 { grid-area: 3 / 2 / 6 / 4; }
.div4 { grid-area: 5 / 5 / 6 / 7; }
.div5 { grid-area: 1 / 6 / 2 / 8; }
.div6 { grid-area: 1 / 2 / 2 / 4; }
'),
  
div(style = "position:fixed;z-index:1000000;",
  
  div(class="parent",
  
div(class="div1",
    div(class="shimmer-marquee",style='height:80%;'#,
        #p(class = 'text-white fs-1 fa text-justify pt-2',"Population Health Model")#,
        #icon(name='mouse',style = 'visibility:hidden;')
        
    )
    ),

div(class  = "div6",
    p(class = 'text-white fs-1  text-justify pt-2',"Energy Simulation Platform")#,
    
),

div(class="div5",  
    
    # icon(  class = ' fs-5 px-3 pt-2 text-light', 'house-laptop') ,
    # icon(  class = ' fs-5 px-3 pt-2 text-light', 'sitemap') ,
    # icon(  class = ' fs-5 px-3 pt-2 text-light', 'comments')
    ),



div(class="div2",

div( style = "border: solid 1px black; padding:15px; border-radius:10px; height:22vh;padding:5%;width: 100%",
  # <!-- Vertical Bar Chart Placeholder -->
  div(style="display: flex; align-items: baseline; justify-content:center; gap: 15px ;margin-inline:40px;height:100%",
      #div(class="shimmer-chart skeleton-bar-grid-vertical m-1"),
  ###########
######## !!!!!!! percentage-based height collapses to zero unless the parent has an explicit height.!!!!!!! ######
  ###########
    div(class="skeleton-bar-vertical shimmer-chart", style = "width:15%;height:70%"),
    div(class="skeleton-bar-vertical", style = "width:15%;height:80%"),
    div(class="skeleton-bar-vertical", style = "width:15%;height:80%"),
    div(class="skeleton-bar-vertical", style = "width:15%;height:60%"),
    div(class="skeleton-bar-vertical", style = "width:15%;height:50%"),
    div(class=" grow", style = "width:15%;height:0px")
),
#div(class=" shimmer-chart skeleton-bar-grid-horizontal m-2")
  )
),

div(class="div3 ",  # <!-- Line Chart Placeholder -->
  div(style = 'height:60vh;', style="border: solid 1px black;border-radius:25px;box-shadow: 2px 5px white;",
      div(style = 'height:40vh;', class="shimmer-chart h-25 m-4"),
      div(style = 'height:40vh;', class="shimmer-chart h-25 m-4"),
      div(style = 'height:40vh;', class="shimmer-chart h-25 m-4"),
      #div(style = 'height:70vh;', class="shimmer-chart h-25 m-4"),

      )
  ),
  

    div(class = "div4 d-flex gap-5",
# <!-- Pie Chart Placeholder -->
#div(class="strobe-chart h-75 mt-5"),
  div(style="display: flex; align-items: baseline; gap: 12px ;height:70%;width:100%;margin-top:100px;",
div(class="skeleton-bar-vertical-white border-0 strobe-chart", style = "width:15%;height:5%"),
div(class="skeleton-bar-vertical-white border-0 strobe-chart", style = "width:15%;height:50%"),
div(class="skeleton-bar-vertical-white border-0 strobe-border", style = "width:15%;height:80%"),
div(class="skeleton-bar-vertical-white border-0 strobe-chart", style = "width:15%;height:40%"),
div(class="skeleton-bar-vertical-white border-0 strobe-chart", style = "width:15%;height:60%")

),

div(style="display: flex; align-items: baseline; gap: 12px ;height:70%;width:100%;margin-top:100px;",
    div(class="skeleton-bar-vertical-white bg-white border-0", style = "width:10%;height:05%"),
    div(class="skeleton-bar-vertical-white bg-white border-0", style = "width:10%;height:15%"),
    div(class="skeleton-bar-vertical-white bg-white border-0", style = "width:10%;height:25%"),
    div(class="skeleton-bar-vertical-white bg-white border-0", style = "width:10%;height:40%"),
    div(class="skeleton-bar-vertical-white bg-white border-0", style = "width:10%;height:55%"),
    div(class="skeleton-bar-vertical-white bg-white border-0", style = "width:10%;height:75%")
)
)

# div( style = " padding:15px; height:13vh; padding:10%;width: 30vw;",
#      # <!-- Vertical Bar Chart Placeholder -->
#      div(style="display: flex; align-items: baseline; flex-direction:column; gap: 12px ;height:12vh;",
#          div(class=" skeleton-bar-grid-vertical m-1"),
#          ###########
#          ######## !!!!!!! percentage-based height collapses to zero unless the parent has an explicit height.!!!!!!! ######
#          ###########
#          div(class="skeleton-bar-horizontal strobe-chart", style = "border:none;box-shadow:none;height:15%;width:70%"),
#          div(class="skeleton-bar-horizontal strobe-chart", style = "border:none;box-shadow:none;height:15%;width:80%"),
#          div(class="skeleton-bar-horizontal strobe-chart", style = "border:none;box-shadow:none;height:15%;width:80%"),
#          div(class="skeleton-bar-horizontal strobe-chart", style = "border:none;box-shadow:none;height:15%;width:60%"),
#          div(class="skeleton-bar-horizontal strobe-chart", style = "border:none;box-shadow:none;height:15%;width:50%")#,
#          #div(class="strobe-chart grow", style = "width:15%;height:0px")
#          
#      ),
#      div(class="skeleton-bar-grid-horizontal m-2")
# )






  # <!-- Horizontal Bar Chart Placeholder -->
  #div(class="skeleton-chart skeleton-bar-horizontal")
  


)
)
  
)
}

# page_fluid(startup_overlay_div()) |> htmltools::browsable()

