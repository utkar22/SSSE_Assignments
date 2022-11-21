#Comment 1 - November 9

#Download
#R-4.2.2 for Windows - https://cran.r-project.org/bin/windows/base/
#RStudio Desktop - https://posit.co/download/rstudio-desktop/
####
  
install.packages("gstat") # installs package "gstat"
library(gstat) # loads package gstat
library(sp) # loads package sp

#open the help manual for state package using the ? operator
?gstat
?sp

#Press ctrl+L to clear the console

# Next we will use dataset 'meuse' available in the sp package

data(meuse) # For more details refer Burrough and McDonnell 1998

#Remove meuse

remove(meuse)

# Relaod meuse

data(meuse)

#Explore data structure
class(meuse) #result is data-frame- basically means tabular data in row-column format

#See in Environment that meuse has 155 observations (i.e., rows) across 14 variables (i.e., columns)

#Now let's View meuse - careful! capital V; or you can simply click on meuse
View(meuse)

#Dataset contains x,y location information; 4 heavy metals along floodplains of a river; and some covariates. More details in Burrough and McDonnell 1998


#Examine the first few rows of the data - i.e., Preview
head(meuse)

#Examine the column names of of the data
colnames(meuse)

#What is the dimension of these data? That is, how many rows and columns in these data?
dim(meuse)
nrow(meuse)
ncol(meuse)
#So a data frame is like a matrix

#Examine individual values in the data frame - meuse 
meuse[1,1]
meuse[1,5] # Note : [] operator

#Examine values from a specific column
meuse[1,"zinc"]

#Examine a specific column
meuse$zinc
head(meuse$zinc) #Note the $ operator


# Examine the class of a specific column (Note : $ operator)
# Note that all items within a column have to be the same type
class(meuse$zinc)
class(meuse$landuse)

#Export data to .csv file
write.csv(meuse,"F:\\OUTPUTS\\Courses\\Monsoon2022\\SpatialStatisticsAndSpatialEconometrics\\R sessions\\meuse.csv")
write.csv(meuse$zinc,"F:\\OUTPUTS\\Courses\\Monsoon2022\\SpatialStatisticsAndSpatialEconometrics\\R sessions\\meuse_zinc.csv")


#Compute summary statistics for zinc concentrations (ppm)
summary(meuse$zinc)

#Compute the variance and standard deviation of zinc concentrations
var(meuse$zinc)
sd(meuse$zinc)

#Plot a histogram of zinc concentrations
hist(meuse$zinc)


#Summary of workflow
# Installed gstat and sp packages
# Explored data frame meuse provided by the sp package
# Expored R data frame to CSV format in a prescribed location in the drive
# R functions: head, colnames, nrow, ncol, dim, class, write.csv, summary, var, sd, hist
# R operations: ? , [] , $


#Commnet 2 - November 16

library(gstat)
library(sp)

data(meuse)
class(meuse)

#make a copy of meuse
meuse.sp <- meuse # Note: <- operator is an 'equals' operator

# We now want to convert the tabular data frame to a spatial data frame
# The way to do this is to use a command called "coordinates" - let's use Help to learn more

?coordinates

coordinates(meuse.sp) <- (~x+y) # objects that start with a '~' operator in R is called a 'formula'. i.e., ~ is R formula operator

# Examine the type of meuse.sp variable
class(meuse.sp)

# Compare the class of meuse.sp with meuse
class(meuse)


#Examine the first few rows of the meuse.sp data - i.e., Preview
head(meuse.sp)

#Examine the column names of of the data
colnames(meuse.sp@data) # Note: if you run colnames(meuse.sp) the result will be NULL. Specifically, you are examining the data within the spatial data frame

#What is the dimension of these data? That is, how many rows and columns in these data?
dim(meuse.sp) # Note: same result for dim(meuse.sp@data)
nrow(meuse.sp) # Note: same result for dim(meuse.sp@data)
ncol(meuse.sp) # Note: same result for dim(meuse.sp@data)

#Examine individual values in the spatial data frame - meuse.sp 
meuse.sp[1,1]
meuse.sp[1,5] 
meuse.sp[2,5] # Note : [] operator

#Examine values from a specific column
meuse.sp[1,"zinc"]

#Examine a specific column
meuse.sp$zinc
head(meuse.sp$zinc) #Note the $ operator


# Examine the class of a specific column (Note : $ operator)
# Note that all items within a column have to be the same type
class(meuse.sp$zinc)
class(meuse.sp$landuse)

# Compute summary statistics for zinc concentration (ppm)
summary(meuse.sp$zinc)

# Compute variance and standard deviation for zinc concentration (ppm)
var(meuse.sp$zinc)
sd(meuse.sp$zinc)

# Plot a histogram for zince concentration (ppm)
hist(meuse.sp$zinc)

# Make a spatial plot of zinc concentration (ppm). Note this operation is not possible for ordianry data frames

bubble(meuse.sp, "zinc", 
       col = c("#00ff0070","#00ff0099"),
       main = "Zinc concentration in parts per million")

# Question: which spatial data model best characterizes meuse.sp spatial data frame?


########################################################

# Comment 3 - November 16

########################################################



#Next module - Experimental Variogram



########################################################

# A function to make spatial dataset
make_spatial_data_meuse <- function()
{
    #load the "meuse" dataset (available in sp library)
  data(meuse)
  
    #make a copy of meuse
  meuse.sp <- meuse
  
    #convert meuse to spatial data format
  coordinates(meuse.sp) <- (~x+y)
  
  return(meuse.sp)
}

#Spatial data input to variagram estimation
sp.data.in <- make_spatial_data_meuse()

head(sp.data.in)

#Plot spatial data

bubble(sp.data.in, "zinc", 
       col = c("#00ff0070","#00ff0099"),
       main = "Zinc concentration in parts per million")

# Now think about spatial stationary - notice spatial trend NW --> SE

# Seeking help from R to estimate a variogram
?variogram

#experimental variogram cloud - without removing trend

lzn.vgm.cloud = variogram(log(zinc)~1, sp.data.in, cloud=TRUE) #Note that we are studying spatial variation in logarithm of zinc concentrations

#Plot and identify point pairs in the variogram cloud

plot(plot(lzn.vgm.cloud, identify=TRUE), sp.data.in)

#experimental variogram without removing trend

lzn.vgm = variogram(log(zinc)~1, sp.data.in)

plot(lzn.vgm,
     cex=1.5, # siz of marker
     pch=19, # type of marker (filled circle)
     xlab="Spatial Lag (h)",
     ylab="Gamma(h), Semivariance")



#Remove the trend-1 (modeling trend along the x coordinates)

lzn.vgm.detrend.x = variogram(log(zinc)~x, sp.data.in)

plot(lzn.vgm.detrend.x,
     cex=1.5, # siz of marker
     pch=19, # type of marker (filled circle)
     xlab="Spatial Lag (h)",
     ylab="Gamma(h), Semivariance")


#Remove the trend-2 (modeling trend along the y coordinates)

lzn.vgm.detrend.y = variogram(log(zinc)~y, sp.data.in)

plot(lzn.vgm.detrend.y,
     cex=1.5, # siz of marker
     pch=19, # type of marker (filled circle)
     xlab="Spatial Lag (h)",
     ylab="Gamma(h), Semivariance")

#Remove the trend-3 (modeling trend along the x and y coordinates)

lzn.vgm.detrend.xy = variogram(log(zinc)~x+y, sp.data.in)

plot(lzn.vgm.detrend.xy,
     cex=1.5, # siz of marker
     pch=19, # type of marker (filled circle)
     xlab="Spatial Lag (h)",
     ylab="Gamma(h), Semivariance")


#Remove the trend-4 (modeling trend along the -x and y coordinates)

lzn.vgm.detrend.xy_ = variogram(log(zinc)~(-x)+y, sp.data.in)

plot(lzn.vgm.detrend.xy_,
     cex=1.5, # siz of marker
     pch=19, # type of marker (filled circle)
     xlab="Spatial Lag (h)",
     ylab="Gamma(h), Semivariance")

#Plot all variograms on a single graph

library(plotly)

# Comparison - 1

plotly::plot_ly(,type="scatter", mode="markers") %>%
  add_trace(data=lzn.vgm,
            x=~dist,
            y=~gamma,
            name="variogram") %>%
  add_trace(data=lzn.vgm.detrend.xy,
            x=~dist,
            y=~gamma,
            name="variogram_detrended_xy") %>%
  add_trace(data=lzn.vgm.detrend.xy_,
            x=~dist,
            y=~gamma,
            name="variogram_detrended_xy_") %>%
  add_trace(data=lzn.vgm.detrend.x,
            x=~dist,
            y=~gamma,
            name="variogram_detrended_x") %>%
  add_trace(data=lzn.vgm.detrend.y,
            x=~dist,
            y=~gamma,
            name="variogram_detrended_y") %>%
  layout(xaxis=list(title="Spatial Lag (h)"),
         yaxis=list(title="Semivariance, gamma(h)"))

# Comparison - 2

plotly::plot_ly(,type="scatter", mode="markers") %>%
  add_trace(data=lzn.vgm,
            x=~dist,
            y=~gamma,
            name="variogram") %>%
  add_trace(data=lzn.vgm.detrend.xy,
            x=~dist,
            y=~gamma,
            name="variogram_detrended_xy") %>%
  add_trace(data=lzn.vgm.detrend.xy_,
            x=~dist,
            y=~gamma,
            name="variogram_detrended_xy_") %>%
  layout(xaxis=list(title="Spatial Lag (h)"),
         yaxis=list(title="Semivariance, gamma(h)"))

# Note we can add other regressors in the detrending exercise


#Comment 4 - November 16

########################################################










#Next module - pertinence of direction in the experimental variogram










########################################################







#experimental anisotropic (directional) variogram - without removing trend




lzn.vgm.dir = variogram(log(zinc)~1, sp.data.in, alpha = c(0,45,90,135)) #Note that we are studying spatial variation in logarithm of zinc concentrations




#Plot the directional variogram




plot(lzn.vgm.dir,

     cex=1.5, # siz of marker

     pch=19, # type of marker (filled circle)

     xlab="Spatial Lag (h)",

     ylab="Gamma(h), Semivariance")




#additional parameters within the variogram function




lzn.vgm.dir1 = variogram(log(zinc)~1, sp.data.in, alpha = c(0,45,90,135),

                        cutoff = 1000, width = 100) #Note that we are studying spatial variation in logarithm of zinc concentrations







#Plot the directional variogram




plot(lzn.vgm.dir1,

     cex=1.5, # siz of marker

     pch=19, # type of marker (filled circle)

     xlab="Spatial Lag (h)",

     ylab="Gamma(h), Semivariance")




#Try width = 50




lzn.vgm.dir2 = variogram(log(zinc)~1, sp.data.in, alpha = c(0,45,90,135),

                         cutoff = 1000, width = 50) #Note that we are studying spatial variation in logarithm of zinc concentrations







#Plot the directional variogram




plot(lzn.vgm.dir2,

     cex=1.5, # siz of marker

     pch=19, # type of marker (filled circle)

     xlab="Spatial Lag (h)",

     ylab="Gamma(h), Semivariance")
     
     
     
#Comment 5 - November 21

########################################################



#Next module - Fitting Modeled Variogram 



########################################################

install.packages("gstat")
install.packages("plotly")
library(gstat)
library(sp)

# load groundwater dataset - CSV file
# Data source -  India WRIS dataset
up.gwl <- read.csv("C://Users//hp//Desktop//SSSE_monsson2022//R project//GroundwaterData//up_state_gwl_v6.csv")
# Note R operation - read.csv
View(up.gwl)
head(up.gwl)
range(up.gwl$Year) # Note R operation - range

# select data for the year 2015
up.gwl.2015 <- subset(up.gwl, Year == 2015)
# Note R operation -  subset

# select data for western UP
westup.gwl.2015 <- subset(up.gwl.2015, District %in% c("MUZAFFARNAGAR", "GHAZIABAD",
                                                       "BAGHPAT", "MEERUT", "HAPUR"))

#remove missing values from these data
westup.gwl.2015.1 <- na.omit(westup.gwl.2015)

#convert to spatial points dataframe
head(westup.gwl.2015.1)

coordinates(westup.gwl.2015.1) <- (~Lon+Lat)

#read shapefile
install.packages("rgdal")
library(rgdal)

IND.shp <- readOGR("C://Users//hp//Desktop//SSSE_monsson2022//R project//GroundwaterData//IND_adm2.shp")

#Subset shapfile
west.up.shp <- IND.shp[IND.shp$NAME_2 %in%
                         c("Meerut", "Ghaziabad", "Baghpat", "Hapur"),
]

plot(IND.shp)
plot(west.up.shp)

# Plot spatial data
# make a spatial plot of post-monsoon groundwater levels
install.packages("tmap")
install.packages("raster")
install.packages("stars")
library(stars)
library(raster)
library(tmap)
westup.map <- tm_shape(west.up.shp) +
  tm_fill(col = "#F3E2B1", alpha = 0.75) +
  tm_borders(col = "white", lwd = 2.0) +
  tm_text("NAME_2", size = 1, ymod = -0.3, xmod = 0.2,
          remove.overlap = FALSE) +
  tm_shape(westup.gwl.2015.1) +
  tm_dots(col = "blue", size = "PostMonsoon", scale = 1.2,
          shape = 21,
          popup.vars=c("Well Code"="WlCode"),
          palette = "-RdYlBu") +
  tm_compass(north = 0, type = "rose", position=c("right", "top"), size = 2) +
  tm_grid(labels.inside.frame = FALSE,
          n.x = 4,n.y = 4,
          lwd = 0, alpha = 0.1,
          projection = "+proj=longlat",
          labels.format = list(fun=function(x) {paste0(x,intToUtf8(176))} ) ) +
  tm_layout(legend.outside = TRUE,
            legend.title.size = 0.7,
            title = "Post Monsoon GWL in meters")

# Now plot
westup.map

#########################################################

# Estimate a variogram for post monsoon, constant mean
# experimental variogram - without removing trend
postM.vgm = variogram(PostMonsoon~1, westup.gwl.2015.1)

# plot the experimental variogram
plot(postM.vgm,
     cex = 1.5, # size of marker
     pch = 19,  # type of marker (filled circle)
     xlab = "Spatial Lag (h)",
     ylab = "Gamma(h) (Semi-variance)")

# fit a model variogram
# model/theoretical variogram
# Free to choose! - This is art!
# The default fitting method is weighted least squares.
# Please read the gstat manual for employing OLS or maximum likelihood
postM.fit.sp = fit.variogram(postM.vgm, model = vgm(40, "Sph", 0.4, 5))
postM.fit.ex = fit.variogram(postM.vgm, model = vgm(40, "Exp", 0.4, 5))

# View Results
postM.fit.sp
postM.fit.ex

# plot both the fitted and experimental variograms together
plot(postM.vgm, postM.fit.sp,
     cex = 1.5, # size of marker
     pch = 19,  # type of marker (filled circle)
     lwd = 2,
     xlab = "Spatial Lag (h)",
     ylab = "Gamma(h) (Semi-variance)")

# Goodness of fit, sum of squared errors
attr(postM.fit.sp, "SSErr")
attr(postM.fit.ex, "SSErr")



########################################################################

########################## K R I G I N G ###############################

########################################################################


# load the gstat and sp libraries
library(gstat)
library(sp)
library(rgdal)
library(tmap)
library(plotly)
library(raster)

map_predicted_gwl <- function(df)
{
  r   <- raster(df, layer="var1.pred")
  r.m <- mask(r, west.up.shp)
  
  tm_shape(r.m) +
    tm_raster(n=10, palette="-RdBu", auto.palette.mapping=FALSE,
              title="Prediction map \n(in meters)") +
    tm_shape(west.up.shp) + tm_polygons(alpha = 0, lwd = 2) +
    tm_text("NAME_2", size = 0.75, ymod = -0.3, xmod = 0.2,  
            remove.overlap = FALSE) +
    tm_legend(legend.outside=TRUE)
}


map_variance_gwl <- function(df)
{
  r   <- raster(df, layer="var1.var")
  r.m <- mask(r, west.up.shp)
  
  tm_shape(r.m) +
    tm_raster(n=7, palette ="Reds",
              title="Variance map \n(in squared meters)") +
    tm_shape(west.up.shp) + tm_polygons(alpha = 0, lwd = 2) +
    tm_text("NAME_2", size = 0.75, ymod = -0.3, xmod = 0.2,  
            remove.overlap = FALSE) +
    tm_legend(legend.outside=TRUE)
}

# kriging
# we need a grid
# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(westup.gwl.2015.1, "regular", n=200000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object



# kriging
proj4string(grd) <- proj4string(westup.gwl.2015.1)
krg <- krige(PostMonsoon~1, westup.gwl.2015.1, grd, postM.fit.sp)

#Recall functions we defined above
map_predicted_gwl(krg)
map_variance_gwl(krg)

# cross validation
postM.cv <- krige.cv(PostMonsoon~1,westup.gwl.2015.1,postM.fit.sp,nmax = 10)


# regress predicted values on observed values
plot(postM.cv@data$observed, postM.cv@data$var1.pred)
abline(lm(postM.cv@data$var1.pred~postM.cv@data$observed), col = "blue")
summary(lm(postM.cv@data$var1.pred~postM.cv@data$observed))

# Correlation Coefficient between predicted and observed values
cor.test(postM.cv@data$var1.pred, postM.cv@data$observed, na.rm=TRUE, method = "pearson")
