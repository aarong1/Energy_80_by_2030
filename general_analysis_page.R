# general_analysis_page

library(leaflet)
library(sf)

# names(uk_repd)
# [29] "X-coordinate"                           
# [30] "Y-coordinate"     

ni_xy <- uk_repd %>%
  filter(Country == "Northern Ireland") %>%
  mutate(x=`X-coordinate`, y=`Y-coordinate`) |> 
  filter(!is.na(x) & !is.na(y)) |> 
  filter(row_number()!=471)# |> 
# rbind(data.frame(x=0,y=0))

  crs_in <- 29902   # change to 2157 if your X/Y look like ITM (E ~ 500–700k, N ~ 600–950k)
  #crs_in <- 2157
  crs_in <- 27700  #uk grid
  ni_sf_wgs <- st_as_sf(ni_xy, coords = c("x","y"), crs = crs_in) |>
  #  st_transform(27700)
   st_transform(4326)
  
  leaflet(ni_xy) |> 
    addTiles() |> 
      addCircles(data = ni_sf_wgs, lng = ~st_coordinates(ni_sf_wgs)[,1], lat = ~st_coordinates(ni_sf_wgs)[,2],
                 radius = 5, color = "blue", fillOpacity = 0.5)
