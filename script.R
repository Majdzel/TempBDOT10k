#-----TEMPERATURA-----
library(raster)
library(terra)
library(LST)
library(sf)
library(tmap)
library(osmdata)
library(ggplot2)
B4= raster("2/B4.tif")
B5= raster("2/B5.tif")
B10= raster("2/B10.tif")
zasieg = shapefile("shp/lbn_92.shp")

B4 = crop(B4,zasieg)
B5 = crop(B5,zasieg)
B10 = crop(B10,zasieg)

bands = stack(`B4`,
              `B5`,
              `B10`)

RAD = calc(bands[[3]], function(x) x * 0.00033420 + 0.1-0.29)

bands = stack(bands,
              RAD)

BT = calc(bands[[4]],function(x) { 1321.0789 / (log(774.8853 / x + 1)) - 273.15 })
bands = stack(bands,
              BT)

NDVI = {bands[[2]]-bands[[1]]}/{bands[[2]]+bands[[1]]}
bands = stack(bands,
              NDVI)

NDVI = NDVI(Red = bands[[1]], NIR = bands[[2]])
PV = Pv(NDVI = NDVI,minNDVI = 0.2, maxNDVI = 0.5)
E = 0.004*PV+0.986
LST = BT/(1+(10.8*BT/14388)*log10(E))

f = file.path("D:/studia_mgr/Analizy/TempBDOT10k/wyniki/temp.tif")
writeRaster(LST,f)

#-----ANALIZA------
library(exactextractr)

#---Rastry---
temp = raster("C:/Users/lenovo/Documents/GitHub/TempBDOT10k//wyniki/temp.tif")

#---Poligony---
cment = read_sf("C:/Users/lenovo/Documents/GitHub/TempBDOT10k/shp/bdot10k/cmentarze.shp")
parki = read_sf("C:/Users/lenovo/Documents/GitHub/TempBDOT10k/shp/bdot10k/parki.shp")
lasy  = read_sf("C:/Users/lenovo/Documents/GitHub/TempBDOT10k/shp/bdot10k/lasy.shp")
place = read_sf("C:/Users/lenovo/Documents/GitHub/TempBDOT10k/shp/bdot10k/place.shp")

#---ExtractValue-----
cment = subset(cment,cment$X_KOD =="KUSC01")
cment$Srednia = exact_extract(temp,cment,'mean')

parki = subset(parki,parki$X_KOD =="KUSK04")
parki$Srednia = exact_extract(temp,parki,'mean')

lasy = subset(lasy,lasy$X_KOD =="PTLZ01")
lasy$Srednia = exact_extract(temp,lasy,'mean')

place$Srednia = exact_extract(temp,place,'mean')


write_sf(cment,"D:/studia_mgr/Analizy/TempBDOT10k/wyniki/cmentarze.shp")


#---Wizualizacja--------
tm_shape(zasieg) + 
  tm_polygons() +
  tm_shape(cment) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0) +
  tm_layout(main.title = "Średnia temperatura: Cmentarze") +
  tm_compass(type = "arrow", position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))
  
tm_shape(zasieg) + 
  tm_polygons() +
  tm_shape(place) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0) +
  tm_layout(main.title = "Średnia temperatura: Place") +
  tm_compass(type = "arrow", position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

tm_shape(zasieg) + 
  tm_polygons() +
  tm_shape(lasy) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0) +
  tm_layout(main.title = "Średnia temperatura: Lasy") +
  tm_compass(type = "arrow", position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

tm_shape(zasieg) + 
  tm_polygons() +
  tm_shape(parki) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0) +
  tm_layout(main.title = "Średnia temperatura: Parki") +
  tm_compass(type = "arrow", position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))


tm_shape(zasieg) + 
  tm_polygons() +
  tm_shape(cment) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0, legend.show = FALSE) +
  tm_layout(main.title = "Średnia temperatura wszystkich obiektów") +
  tm_shape(place) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0) +
  tm_shape(lasy) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0, legend.show = FALSE) +
  tm_shape(parki) + 
  tm_polygons("Srednia", palette = "Reds", lwd = 0, legend.show = FALSE) +
  tm_compass(type = "arrow", position = c("right", "top")) + 
  tm_scale_bar(position = c("right", "bottom"))

# Narysowanie histogramu
hist(place$Srednia,xlab = "Średnia temperatura", ylab = "Liczba obiektów", main = "Wykres średnich temperatur: place", col="Red")
vals <- hist(place$Srednia, plot = FALSE)$counts
text(x = seq_along(vals), y = vals, labels = vals, pos = 3)

