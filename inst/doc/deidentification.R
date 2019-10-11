## ----libraries, echo=T, eval= T, message=FALSE, warning= FALSE-----------
# R libraries
library(tangles);library(digest);library(raster);library(sp)

# DATA SETS
# POINT PATTERN
data("HV_subsoilpH")

# RASTER OBJECT
data("hunterCovariates_sub")

## ----vshifts, echo=T, eval= F, message=FALSE, warning= FALSE-------------
#  ## Horizontal shift
#    leap_X<- function(xyData=NULL){
#      r.num<- sample(-999999:999999, 1)
#      xyData[,1]<- xyData[,1] + r.num
#      return(list(xyData, r.num))
#    }
#  
#  
#  ## Vertical shift
#    leap_Y<- function(xyData=NULL){
#      r.num<- sample(-999999:999999, 1)
#      xyData[,2]<- xyData[,2] + r.num
#      return(list(xyData, r.num))
#    }

## ----rshifts, echo=T, eval= F, message=FALSE, warning= FALSE-------------
#  ## Step 3 (data rotation)
#    rotate_XY<- function(xyData=NULL){
#  
#      # pick a point at random from the dataset
#      row.sample<- sample(1:nrow(xyData),1)
#      origin.point<- xyData[row.sample,]
#  
#      ## Prep data for rotation
#      x<- t(xyData[,1])
#      y<- t(xyData[,2])
#      v = rbind(x,y)
#  
#      x_center = origin.point[1]
#      y_center = origin.point[2]
#  
#      #create a matrix which will be used later in caclculations
#      center <-  v
#      center[1,]<- as.matrix(x_center)
#      center[2,]<- as.matrix(y_center)
#  
#      if (rasterdata == TRUE){
#        deg<- sample(c(90,180,270),1, replace = F)} else { # choose a random orientation
#          deg<- sample(1:359,1, replace = F)} # choose a random orientation
#  
#      theta = (deg * pi)/180      # express in radians
#  
#      # rotation matrix
#      R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
#  
#      # do the rotation...
#      s = v - center    # shift points in the plane so that the center of rotation is at the origin
#      so = R%*%s           # apply the rotation about the origin
#      vo = so + center   # shift again so the origin goes back to the desired center of rotation
#  
#      # pick out the vectors of rotated x- and y-data
#      xyData<- cbind(vo[1,], vo[2,])
#      return(list(xyData,origin.point,deg))
#    }
#  

## ----tangles, echo=T, eval= T, message=FALSE, warning= FALSE-------------
xyData<- as.matrix(HV_subsoilpH[,1:2])
tangles.out<- tangles(data = xyData, depth = 3, rasterdata = FALSE, raster_object = FALSE, saveTangles = TRUE, path = tempdir())
str(tangles.out)  

## ----tangler, echo=T, eval= T, message=FALSE, warning= FALSE-------------
# First entangle the point pattern
xyData<- as.matrix(HV_subsoilpH[,1:2])
tangles.out<- tangles(data = xyData, depth = 5, rasterdata = TRUE, raster_object = FALSE, saveTangles = TRUE, path = tempdir())

# Now entangle the raster object
tangler.out<- tangler(data = hunterCovariates_sub, tanglerInfo = tangles.out[[2]], raster_object = TRUE, stub = "myname", saveTangles = TRUE, path = tempdir())

## ----t_plot, echo=T, eval= T, message=FALSE, warning= FALSE,fig.width=7, fig.height=5----
# Plotting
# original data
pHV_subsoilpH<- HV_subsoilpH
coordinates(pHV_subsoilpH)<- ~ X + Y
plot(hunterCovariates_sub[[1]], main="orginal data"); plot(pHV_subsoilpH, add=T)

# tangled data
tPP<- as.data.frame(tangles.out[[1]])
coordinates(tPP)<- ~ X + Y
plot(tangler.out[[1]], main="tangled data");plot(tPP, add=T)

## ----detangles, echo=T, eval= F, message=FALSE, warning= FALSE-----------
#  # points
#  xyData<- as.matrix(tangles.out[[1]])
#  point_detang<- detangles(data=xyData, tanglerInfo=tangles.out[[2]], raster_object = FALSE, stub = "hv_fix", hash_key = "UNIQUE_HASH_KEY_HERE", saveTangles = TRUE, path = tempdir())
#  
#  #rasters
#  raster_detang<- detangles(data=tangled.origi, tanglerInfo=tangles.out[[2]], raster_object = TRUE, stub = "hv_fix", hash_key = "UNIQUE_HASH_KEY_HERE", saveTangles = TRUE, path = tempdir())

