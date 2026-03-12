colour_scale_bar <- function(){
  HTML('<head>
  <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>Rounded Colour Band Mockup</title>
      <style>
      :root {
        --band-height: 18px;
        --band-radius: 8px;
        --track-radius: 10px;
        --track-bg: #f3f4f6;
          --track-border: #d1d5db;
      }
    
    * { box-sizing: border-box; }
    
    # body {
    #   margin: 0;
    #   min-height: 100vh;
    #   display: grid;
    #   place-items: center;
    #   background: #ffffff;
    #     font-family: Arial, Helvetica, sans-serif;
    #   padding: 32px;
    # }
    
    .wrap1 {
      width: 100%;
      padding: 10px;
    }
    
    .label1 {
      margin: 0 0 10px;
      font-size: 14px;
      color: #374151;
    }
    
    /*
    .track1 {
      width: 100%;
      padding: 4px;
      border: 1px solid var(--track-border);
      border-radius: var(--track-radius);
      background: var(--track-bg);
    }
    */
    .line1 {
      display: grid;
      grid-template-columns: repeat(10, 1fr);
      gap: 4px;
      align-items: center;
    }
    
    .band1 {
      height: var(--band-height);
      border-radius: var(--band-radius);
      box-shadow: inset 0 0 0 1px rgba(0,0,0,0.08);
    }
    
    .c1 { background: mediumseagreen; }
    .c2 { background: lightgreen; }
    .c3 { background: lightgoldenrodyellow; }
    .c4 { background: yellow; }
    .c5 { background: moccasin; }
    .c6 { background: orange; }
    .c7 { background: lightsalmon; }
    .c8 { background: lightcoral; }
    .c9 { background: #ff4741; }
        .c10 { background: #000000; }
            </style>
            </head>
            <body>
            <div class="wrap1">
                <div class="track1">
                  <div class="line1">
                    <div class="band1 c1"></div>
                      <div class="band1 c2"></div>
                        <div class="band1 c3"></div>
                          <div class="band1 c4"></div>
                            <div class="band1 c5"></div>
                              <div class="band1 c6"></div>
                                <div class="band1 c7"></div>
                                  <div class="band1 c8"></div>
                                    <div class="band1 c9"></div>
                                      <div class="band1 c10"></div>
                                        </div>
                                        </div>
                                        </div>
                                        </body>')
}


htmltools::browsable(colour_scale_bar())
