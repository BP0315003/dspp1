#-------------------------------------------------------------------
# Load libraries
#-------------------------------------------------------------------
library(leaflet)
library(viridisLite)
library(htmlwidgets)

#-------------------------------------------------------------------
#Select data for map
#-------------------------------------------------------------------

mapdata <- activity_joined %>% 
          select (lsoa21cd = lsoa_code, 
                  active_75_prop = `active_aged 75+`) %>%
  mutate(active_75_pct = round(active_75_prop * 100,1))   #Convert to a percentage and round

activity_map_data_shp <- lsoa_shape_england %>%
  left_join(activity_joined, by = c("LSOA21CD" = "lsoa_code")) %>% 
  filter(str_detect(LSOA21NM, "^Bristol") |
           str_detect(LSOA21NM, "^North Somerset") |
           str_detect(LSOA21NM, "^South Gloucestershire"))%>%
  select(LSOA21CD, LSOA21NM, LAT, LONG, geometry, GlobalID, BNG_E, BNG_N, `active_aged 75+`) %>%
  mutate(active_75_pct = round(`active_aged 75+` * 100,1))   #Convert to a percentage and round
  
#-------------------------------------------------------------------
# Ready the shape file to use, filter to local area
#-------------------------------------------------------------------

#Replace NA with 0
mapdata[, 2:3] <- lapply(mapdata[, 2:3], function(x) ifelse(is.na(x), 0, x)) 


# transforms shape file for leaflet to use
lsoa_shp_bnssg <-lsoa_shape_england %>%
st_transform('+proj=longlat +datum=WGS84') %>% 
#filter to bnssg
filter(rowSums(sapply(c("South Gloucestershire", "Bristol", "North Somerset"), function(pattern) grepl(pattern, LSOA21NM))) > 0)

#convert to lower case
names(lsoa_shp_bnssg) <- tolower(names(lsoa_shp_bnssg))
names(mapdata) <- tolower(names(mapdata))

#trim white space (the join failed otherwise)
lsoa_shp_bnssg$lsoa21cd <- trimws(lsoa_shp_bnssg$lsoa21cd)
mapdata$lsoa21cd <- trimws(mapdata$lsoa21cd)

# Join shapes and data
lsoa_shp_data <- lsoa_shp_bnssg %>% left_join(mapdata, by = "lsoa21cd")
lsoa_shp_data <- lsoa_shp_data %>%
  mutate(across(8:8, ~ifelse(is.na(.), 0, .))) 


# Calculate the bounding box of your shapefile
bbox <- st_bbox(lsoa_shp_bnssg)
lat_mid <- as.numeric((bbox[2]+bbox[4])/2)
long_mid <- as.numeric((bbox[1]+bbox[3])/2)

#centre map on middle of BNSSG area
center_lat <- 51.477581
center_lng <- -2.585760

#Colours to match ggplot colours of  other maps, direction -1 means highest values are darkest
viridis_colors <- viridisLite::viridis(256, option = "A", direction = -1)  

cols_val1 <- colorNumeric(
  palette = viridis_colors,      
  domain = lsoa_shp_data$active_75_pct

 
)

# --- 3. Build the Leaflet map ---
map <- leaflet((lsoa_shp_data)) %>%
  
  # can use default leaflet tiles or specify provider 
  addProviderTiles(provider = providers$CartoDB.Voyager) %>%
  
  # add LSOA shapes with colour coding
  addPolygons(data = lsoa_shp_data,
              fillColor = ~cols_val1(active_75_pct), 
              color = "black",
              stroke = T,
              weight = 1,
              fillOpacity = 0.8,
              
              #option to highlight area when hovering over it
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = T),
              
              #pop-up text
              popup = ~paste0(
                "LSOA: ", lsoa21cd,
                "<br> LSOA name: ", lsoa21nm,
                "<br> Age 75 plus, % Active: ", active_75_pct, "%" 
              )
  ) %>% 
  
  
  # add legend for heat map
  addLegend("topleft", 
            pal = cols_val1,
            values = lsoa_shp_data$active_75_pct,
            title = "% Active",
            opacity = 0.8) %>%
  # Initial view zoom
  setView(lng = center_lng, lat = center_lat, zoom = 10.0)
  
# Display map title

map <- onRender(map, "
  function(el, x) {
    // Create and style the title
    var title = document.createElement('div');
    title.innerHTML = 'Percentage active, ages 75+, BNSSG';
    Object.assign(title.style, {
      position: 'absolute',
      top: '10px',
      left: '50%',
      transform: 'translateX(-50%)',
      fontFamily: 'Arial, sans-serif',
      fontSize: '18px',
      fontWeight: 'bold',
      color: '#003087',
      textAlign: 'center',
      zIndex: 9999,
      background: 'rgba(255,255,255,0.7)',
      padding: '2px 6px',
      borderRadius: '4px'
    });
    el.appendChild(title);

    // Create and style the subtitle
    var subtitle = document.createElement('div');
    subtitle.innerHTML = 'Sport England - Active lives, 2021/22';
    Object.assign(subtitle.style, {
      position: 'absolute',
      top: '35px',
      left: '50%',
      transform: 'translateX(-50%)',
      fontFamily: 'Arial, sans-serif',
      fontSize: '12px',
      color: '#003087',
      textAlign: 'center',
      zIndex: 9999,
      background: 'rgba(255,255,255,0.7)',
      padding: '2px 6px',
      borderRadius: '4px'
    });
    el.appendChild(subtitle);
  }
")

# --- 4. Display map ---
map

#save(lsoa_shp_data, file=paste0('02 Data_Images/lsoa_shp_data','.RData'))
#save(mapdata, file=paste0('02 Data_Images/mapdata','.RData'))