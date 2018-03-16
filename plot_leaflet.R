#Sample data (n=12; data collected in 4 different days of December, 2016)

#Projected coordinates. CRS = EPSG3857 (http://spatialreference.org/ref/sr-org/7483/)
#Starting x and y coordinates (where wind data was observed).

latMid = wind$Lat*111000
m_per_deg_lat = 111132.954 - 559.822 * cos( 2 * latMid ) + 1.175 * cos( 4 * latMid);

lonMid = wind$Lon
m_per_deg_lon = 111132.954 * cos ( latMid );

measure <- function(lon1,lat1,lon2,lat2) {
  R <- 6378.137                                # radius of earth in Km
  dLat <- (lat2-lat1)*pi/180
  dLon <- (lon2-lon1)*pi/180
  a <- sin((dLat/2))^2 + cos(lat1*pi/180)*cos(lat2*pi/180)*(sin(dLon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c
  return (d * 1000)                            # distance in meters
}

hmeasure(0,0,wind$Lon,wind$Lat)


start.x <- wind$Lon*111000
start.y <- wind$Lat*111000

#Wind variables (speed, direction and date)
w.speed <- wind$mean_wind
w.direction <- wind$wind_dir
w.date <- do.call("as.Date",
                  list(x = c("1-Dec-2016", "1-Dec-2016", "1-Dec-2016", "5-Dec-2016", "5-Dec-2016", "5-Dec-2016", "9-Dec-2016", "9-Dec-2016", "9-Dec-2016", "12-Dec-2016", "12-Dec-2016", "12-Dec-2016"),
                       format = "%d-%b-%Y")) #date of data collection (yyyy-mm-dd)
id <- c(1:length(start.x)) #id of sample data

#Dataframe with georeferenced wind data
df <- data.frame(id=id,start.x=start.x,start.y=start.y,w.speed=w.speed,w.direction=w.direction)
df <- data.frame(df[1:120,],w.date=c(rep(w.date[1],40),rep(w.date[4],40),rep(w.date[7],40)))
date

# ,
#                  w.date=w.date)
head(df,5)

#------------------------------
#Step 2 - Complement `df` with auxiliary coordinates for representing wind as arrowhead lines.

#Line parameters
line.length <- 150000 #length of polylines representing wind in the map (meters)
arrow.length <- 50500 #lenght of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)

for (i in c(1:nrow(df))){
  
  #coordinates of end points for wind lines (the initial points are the ones where data was observed)
  if (df$w.direction[i] <= 90) {
    end.x <- df$start.x[i] + (cos((90 - df$w.direction[i]) * 0.0174532925) * line.length)
  } else if (df$w.direction[i] > 90 & df$w.direction[i] <= 180) {
    end.x <- df$start.x[i] + (cos((df$w.direction[i] - 90) * 0.0174532925) * line.length)
  } else if (df$w.direction[i] > 180 & df$w.direction[i] <= 270) {
    end.x <- df$start.x[i] - (cos((270 - df$w.direction[i]) * 0.0174532925) * line.length)
  } else {end.x <- df$start.x[i] - (cos((df$w.direction[i] - 270) * 0.0174532925) * line.length)}
  
  if (df$w.direction[i] <= 90) {
    end.y <- df$start.y[i] + (sin((90 - df$w.direction[i]) * 0.0174532925) * line.length)
  } else if (df$w.direction[i] > 90 & df$w.direction[i] <= 180) {
    end.y <- df$start.y[i] - (sin((df$w.direction[i] - 90) * 0.0174532925) * line.length)
  } else if (df$w.direction[i] > 180 & df$w.direction[i] <= 270) {
    end.y <- df$start.y[i] - (sin((270 - df$w.direction[i]) * 0.0174532925) * line.length)
  } else {end.y <- df$start.y[i] + (sin((df$w.direction[i] - 270) * 0.0174532925) * line.length)}
  
  #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
  end.arrow.x <- end.x + (cos((df$w.direction[i] + arrow.angle) * 0.0174532925) * arrow.length)
  end.arrow.y <- end.y - (sin((df$w.direction[i] + arrow.angle) * 0.0174532925) * arrow.length)
  
  end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y)) 
}

end.xy <- end.xy.df[-1,]
df <- data.frame(df,end.xy) #df with observed and auxiliary variables
head(df,3)

#------------------------------
#Step 3 - Create an object of class `SpatialLinesDataFrame` to use within `leaflet`.

lines <- data.frame(cbind(lng=c(df$start.x,df$end.x,df$end.arrow.x),
                          lat=c(df$start.y,df$end.y,df$end.arrow.y),
                          id=c(rep(df$id,3))))

lines.list <- list()

library(sp)

for (i in c(1:max(lines$id))){
  line <- subset(lines,lines$id==i)
  line <- as.matrix(line[,c(1:2)])
  line <- Line(line) #object of class 'Line'
  lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
}

sp.lines <- SpatialLines(lines.list) #object of class 'SpatialLines'
proj4string(sp.lines) <- CRS("+init=epsg:3857") #define CRS

#Convert CRS to geographic coordinates (http://spatialreference.org/ref/epsg/4326/)
#for overlaying on OpenStreetMaps tiles in Leaflet
sp.lines <- spTransform(sp.lines, CRS("+init=epsg:4326"))

rownames(df) = df$id
#Join wind variables (id, speed, direction and date) to object of class 'SpatialLines'
sp.lines.df <- SpatialLinesDataFrame(sp.lines, df[,c(1,4:6)]) #object of class 'SpatialLinesDataFrame'
str(sp.lines.df) #inspect object structure

#------------------------------
# Code for next steps mostly adapted from https://rstudio.github.io/leaflet/
#------------------------------

#------------------------------
#Step 4 - Generate interactive and **static** map of wind speed and direction.

library(leaflet)

#popup settings
labels <- paste0("ID: ",sp.lines.df@data$id,
                 "<br>Wind speed: ",sp.lines.df@data$w.speed," Km/h<br>",
                 "Wind direction: ",sp.lines.df@data$w.direction," degrees azimuth<br>",
                 "Date: ", "Saiki")

#pallete settings
pal <- colorNumeric(palette = colorRampPalette(c("Orange", "Darkred"))(5),
                    domain = 0:max(sp.lines.df@data$w.speed))

#Create object fo class 'leaflet' 'htmlwidget'
m <- leaflet(sp.lines.df) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%# add default OpenStreetMap map tiles
  addPolylines(color = ~pal(w.speed), opacity=1, weigh = 3, popup = labels) %>%
  addLegend("bottomright", pal = pal, values = ~w.speed,
            title = "Wind speed <br> (km/h)",
            opacity = 1) %>%
  fitBounds(sp.lines.df@bbox[1,1], sp.lines.df@bbox[2,1], sp.lines.df@bbox[1,2], sp.lines.df@bbox[2,2])

#Plot map
m

