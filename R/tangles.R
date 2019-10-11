## Function for tangling original XY coordinates
## There are 3 types of transformation:
# Shifting up or down (step1)
# Shifting left ot right (step2)
# Rotating coordinates about a randomly selected origin (step3)
# Given a selected number of depths, a step sequence is randomised
# Spatial coordinates are transformed at each step and are then used as input for the following step
# until we have reached the maximum depth. 

## Inputs
# data: Can be either a 2 column MATRIX of spatial coordinates or a raster object
# depth: the number of transformations to perfrom. Default is three
# rasterdata: A logical. If set to TRUE the rotations will be made at right angles only. This is to preserve the nature of the raster data
# raster_object: A logical. This is to stipulate that a raster object is to be transformed and not just a set of points
# saveTangles: save outputs of function to file

## Outputs
# A list object that contains
# 1. The transformed coordinates or transformed raster object (dpendent on the inputs)
# 2. A seperate list object to be used for untangling the transformed coordinates
# The outputs are optinally written to file to the working directory with file stub nanes of tangledXY and detangler respectively.
# These files have a commmon hash key as part of their filename.
# The hash key is generated from the detangler object using the sha256 hash algorithm

tangles<- function(data=NULL, depth=3, rasterdata = FALSE, raster_object = FALSE, saveTangles = FALSE, path = NULL){
  
  # tabularise the raster data
  if (raster_object == TRUE){
    tempD <- data.frame(cellNos = seq(1:ncell(data)))
    vals <- as.data.frame(getValues(data))
    tempD<- cbind(tempD, vals)
    tempD <- tempD[complete.cases(tempD), ]
    cellNos <- c(tempD$cellNos)
    gXY <- data.frame(xyFromCell(data, cellNos, spatial = FALSE))
    xyData<- as.matrix(gXY)} else {xyData <- data}
  
  ###### Internalised Step Functions
  ## Step 1 (shifting X)
  leap_X<- function(xyData=NULL){
    r.num<- sample(-999999:999999, 1)
    xyData[,1]<- xyData[,1] + r.num
    return(list(xyData, r.num))
  }
  ## End Step 1 (shifting X)
  
  ## Step 2 (shifting Y)
  leap_Y<- function(xyData=NULL){
    r.num<- sample(-999999:999999, 1)
    xyData[,2]<- xyData[,2] + r.num
    return(list(xyData, r.num))
  }
  ## End Step 2 (shifting Y)
  
  ## Step 3 (data rotation)
  rotate_XY<- function(xyData=NULL){
    # pick a point at random from the dataset
    row.sample<- sample(1:nrow(xyData),1)
    origin.point<- xyData[row.sample,]
    
    ## Prep data for rotation
    x<- t(xyData[,1])
    y<- t(xyData[,2])
    v = rbind(x,y)
    
    x_center = origin.point[1]
    y_center = origin.point[2]
    
    #create a matrix which will be used later in calculations
    center <-  v
    center[1,]<- as.matrix(x_center)
    center[2,]<- as.matrix(y_center)
    
    if (rasterdata == TRUE){
      deg<- sample(c(90,180,270),1, replace = F)} else { # choose a random orientation
        deg<- sample(1:359,1, replace = F)} # choose a random orientation
    
    theta = (deg * pi)/180      # express in radians
    
    R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
    
    # do the rotation...
    s = v - center    # shift points in the plane so that the center of rotation is at the origin
    so = R%*%s           # apply the rotation about the origin
    vo = so + center   # shift again so the origin goes back to the desired center of rotation
    
    # pick out the vectors of rotated x- and y-data
    xyData<- cbind(vo[1,], vo[2,])
    return(list(xyData,origin.point,deg))
  }
  ## END Step 3 (data rotation)
  
  
  ########################################## END internalised step functions
  
  ## randomise the sequence of steps with defined depth
  step.random<- sample(1:3, depth, replace = T)
  
  ## cycle through the step sequence
  s3.cnt<- 1
  seq.mat<- matrix(NA, nrow = depth, ncol = 6)
  for (i in 1:depth){
    seq.step<- step.random[i]
    
    # if step 1
    if(seq.step == 1){
      step1.out<- leap_X(xyData = xyData)
      # save outputs
      xyData<- step1.out[[1]]
      seq.mat[i,1]<- seq.step
      seq.mat[i,2]<- step1.out[[2]]}
    
    # if step 2
    if(seq.step == 2){
      step2.out<- leap_Y(xyData = xyData)
      # save outputs
      xyData<- step2.out[[1]]
      seq.mat[i,1]<- seq.step
      seq.mat[i,2]<- step2.out[[2]]}
    
    # if step 3
    if(seq.step == 3){
      step3.out<- rotate_XY(xyData = xyData)
      # save outputs
      xyData<- step3.out[[1]]
      seq.mat[i,1]<- seq.step
      seq.mat[i,3:4]<- step3.out[[2]]
      seq.mat[i,5]<- step3.out[[3]]
      seq.mat[i,6]<- s3.cnt
      s3.cnt<- s3.cnt + 1}
  }
  
  seq.dat<- as.data.frame(seq.mat)
  names(seq.dat)<- c("step", "leap_dist", "origin_X", "origin_Y", "degree", "s3_count")
  
  xyData<- as.data.frame(xyData)
  names(xyData)<- c("X", "Y")
  
  # generate a hash
  hash.out<- digest(seq.dat ,"sha256") # first try
  deTangler<- list(hash = hash.out, step_sequence= step.random, unpicker= seq.dat)
  
  # write de-tangler to file
  nm1<- paste0(path, "/detangler_", hash.out, ".rds")
  if (saveTangles == TRUE){
  saveRDS(object = deTangler, file = nm1)}
  
  # rasterise tabular data
  if (raster_object == TRUE){
    tDat<- cbind(xyData, tempD)
    if (ncol(tDat) > 4){
      rasterOuts<- stack()
      for (z in 4:ncol(tDat)){
        rasterOuts<- stack(rasterOuts, rasterFromXYZ(tDat[,c(1,2,z)]))}
      } else {
      rasterOuts<- rasterFromXYZ(tDat[,c(1,2,4)])}
    # write revised coordinates to file
    nm2<- paste0(path, "/tangledXY_raster", hash.out, ".rds")
    if (saveTangles == TRUE){
    saveRDS(object = rasterOuts, file = nm2)}
    return(list(rasterOuts, deTangler))
    } else {
      # write revised coordinates to file
      nm2<- paste0(path, "/tangledXY_", hash.out, ".rds")
      if (saveTangles == TRUE){
      saveRDS(object = xyData, file = nm2)}
      return(list(xyData, deTangler))}}

    

  
#### END



