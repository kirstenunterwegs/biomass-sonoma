# This script contains the data processing and analysis Code for the term_project_work "Mapping 
# forest aboveground biomass from Landsat data with regression modelling" in the WS 2019/2020,
# in the module "Quantitative Methods for Geographers". 

# install.packages("sf")
# install.packages("rgdal")
install.packages("raster")
#load libaries
library("sf")
library("rgdal")
library(raster)
library(ggplot2)

# load biomass points with st_read
point_HARV <- st_read("Biomass/biomass_sonoma_subset.shp")


# loading data with gdal

biomass <- rgdal::readOGR("~/Documents/global_change_geography/Quantitative_Mehods/biomass_project/data/Biomass/biomass_sonoma_subset.shp")
landsat <- raster("~/Documents/global_change_geography/Quantitative_Mehods/biomass_project/data/Landsat/20130615_landsatBAP_subset.bsq")

# plotting the data> ISSUES plotting the data!

ggplot(data = biomass$biomass)
point_HARV$ID <- rep("", nrow(point_HARV))
point_HARV$ID <- (1:2927)
plot(point_HARV$ID, point_HARV$biomass)

# creating histogram of the data
hist(point_HARV$biomass, main = "Histogram of biomass points", xlab = "Biomass", ylab = "Frequency", cex.lab = 1.5)

# extract band information
band4_landsat <- raster("data/Landsat/20130615_landsatBAP_subset.bsq", 
         band = 4)
band5_landsat <- raster("data/Landsat/20130615_landsatBAP_subset.bsq", 
                        band = 5)

# calculate the NDVI (https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/vegetation-indices-NDVI-in-R/)
landsat_ndvi <- ((band4_landsat - band5_landsat) / (band4_landsat + band5_landsat))
landsat_ndvi

plot(landsat_ndvi,
     main = "NDVI of Sonoma_subimage",
     axes = FALSE, box = FALSE)

# view distribution of NDVI values
hist(landsat_ndvi,
     main = "NDVI: Distribution",
     col = "springgreen",
     xlab = "NDVI Index Value")

# assign NDVI values to biomass-points
biomass$NDVI1 <- extract(landsat_ndvi, biomass, method='simple', buffer=NULL, small=FALSE, 
                         cellnumbers=FALSE,fun=NULL, na.rm=TRUE, layer, nl, df=FALSE, factors=FALSE)
plot(biomass$NDVI1, biomass$biomass)

# build model biomass_points ~ NDVI
model1 <- glm(biomass ~ NDVI1, data = biomass, family = gaussian(link ="log"))
model1

biomass <-data.frame(biomass)

biomass$fitted1 <- predict(model1, type = "response")
ggplot(biomass, aes(x = NDVI1, y = biomass))+
  geom_point(data=biomass, aes(x=NDVI1, y=biomass),pch=20)+
  geom_line(data = biomass, aes(x = NDVI1, y = fitted1), col="red") 
