library(grid)
library(rgdal)

setwd("Z://Data peljas/Gridded_Yosik/")

system("unzip A_IXCN01RJTD160100_C_RJTD_20180316020547_98.kmz")
# og = readOGR("A_IXCN01RJTD160100_C_RJTD_20180316020547_98.kml","Himawari")


# attr(og, "driver")
# attr(og, "nlayers")
foo = as.character(unlist(read.delim("A_IXCN01RJTD160100_C_RJTD_20180316020547_98.kml")))
i_data = grep(foo, pattern = "Placemark")
foo = foo[i_data]
joz = c()
for(i in 1:length(i_data)){
  joz[i] = strsplit(foo[i],split = "value")[[1]][[4]]
  joz[i] = gsub(pattern = "</",joz[i],replacement = "")
  joz[i] = as.numeric(gsub(pattern = ">",joz[i],replacement = ""))
}
arah = as.numeric(joz)
joz = c()
for(i in 1:length(i_data)){
  joz[i] = strsplit(foo[i],split = "value")[[1]][[6]]
  joz[i] = gsub(pattern = "</",joz[i],replacement = "")
  joz[i] = as.numeric(gsub(pattern = ">",joz[i],replacement = ""))
}
V = as.numeric(joz)

joz = c()
for(i in 1:length(i_data)){
  joz[i] = strsplit(foo[i],split = "value")[[1]][[2]]
  joz[i] = gsub(pattern = "</",joz[i],replacement = "")
  joz[i] = as.numeric(gsub(pattern = ">",joz[i],replacement = ""))
}
P = as.numeric(joz)

joz = c()
lon = c()
lat = c()
for(i in 1:length(i_data)){
  joz[i] = strsplit(foo[i],split = "Point")[[1]][[2]]
  lon[i] = strsplit(joz[i],split = ",")[[1]][[1]]
  lat[i] = strsplit(joz[i],split = ",")[[1]][[2]]
  lon[i] = gsub(pattern = "><coordinates>",lon[i],replacement = "")
  lat[i] = gsub(pattern = "</coordinates></",lat[i],replacement = "")
}
lon  = as.numeric(lon)
lat  = as.numeric(lat)
ilon = which(lon > 90 & lon< 140)

out = data.frame(lon = lon,lat = lat,V = V,D=arah,P=P)
wind = out[ilon,1:4]
class(wind) = c("tbl_df","tbl","data.frame")
colnames(wind) = colnames(wind.dt)
colnames(wind.dt)
dim(wind.dt)
geom_raster(data = wind)
(wind)

library(ggplot2)

which(is.na(wind))
x11()
ggplot(wind, 
       aes(x = Lon , 
           y = Lat, 
           fill = mean_wind, 
           angle = wind_dir, 
           radius = scales::rescale(mean_wind, c(2, .9)))) +
  # geom_raster() +
  geom_spoke(arrow = arrow(length = unit(.07, 'inches'))) + 
  scale_fill_distiller(palette = "RdYlGn") + 
  coord_equal(expand = 2) + 
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal')






