## Function for untangling transformed coordinates back to original XY coordinates or rasters
## Reverses the steps of the tangles function



## Inputs
# data: 2 column MATRIX of transformed spatial coordinates or transformed raster object
# tanglerInfo: object the was saved from the tangle function that has the steps and parameters need to do the untangle.
# saveTangles: save outputs of function to file

## Outputs
# The original spatial point pattern or rasters


detangles<- function(data=NULL, tanglerInfo=NULL, raster_object = FALSE, stub = NULL, hash_key=NULL, saveTangles = FALSE, path = NULL){
  
  #check for hash key match
  if(as.character(tanglerInfo[1]) != hash_key){
    stop("ERROR: detangler object does not match these de-identified data")
  }
  
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
  leap_Xba<- function(xyData=NULL, r.num=NULL){
    xyData[,1]<- xyData[,1] - r.num
    return(xyData)}
  ## End Step 1 (shifting X)
  
  ## Step 2 (shifting Y)
  leap_Yba<- function(xyData=NULL, r.num=NULL){
    xyData[,2]<- xyData[,2] - r.num
    return(xyData)}
  ## End Step 2 (shifting Y)
  
  ## Step 3 (data rotation)
  rotate_XYba<- function(xyData=NULL, deg=NULL, origin.point=NULL){
    
    ## Prep data for rotation
    x<- t(xyData[,1])
    y<- t(xyData[,2])
    v = rbind(x,y)
    
    #origin point
    x_center = origin.point[1]
    y_center = origin.point[2]
    
    #create a matrix which will be used later in calculations
    center <-  v
    center[1,]<- as.matrix(x_center)
    center[2,]<- as.matrix(y_center)
    
    bdeg<- 360 - deg  # choose a random orientation
    theta = (bdeg * pi)/180      # express in radians
    
    R = matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), nrow=2)
    
    # do the rotation...
    s = v - center    # shift points in the plane so that the center of rotation is at the origin
    so = R%*%s           # apply the rotation about the origin
    vo = so + center   # shift again so the origin goes back to the desired center of rotation
    
    # pick out the vectors of rotated x- and y-data
    xyData<- cbind(vo[1,], vo[2,])
    return(xyData)}
  ## END Step 3 (data rotation)
  
  
  ########################################## END internalised step functions
  
  
  
  ## cycle through the step sequence
  for (i in 1:nrow(tanglerInfo$unpicker)){
    jp<- nrow(tanglerInfo$unpicker) - (i-1)
    seq.step<- tanglerInfo$unpicker$step[jp]
    
    # if step 1
    if(seq.step == 1){
      step1.out<- leap_Xba(xyData = xyData , r.num = tanglerInfo$unpicker$leap_dist[jp])
      # save outputs
      xyData<- step1.out}
    
    # if step 2
    if(seq.step == 2){
      step2.out<- leap_Yba(xyData = xyData, r.num = tanglerInfo$unpicker$leap_dist[jp])
      # save outputs
      xyData<- step2.out}
    
    # if step 3
    if(seq.step == 3){
      step3.out<- rotate_XYba(xyData = xyData, deg = tanglerInfo$unpicker$degree[jp], origin.point = c(tanglerInfo$unpicker$origin_X[jp], tanglerInfo$unpicker$origin_Y[jp]))
      # save outputs
      xyData<- step3.out}}
  
  xyData<- as.data.frame(xyData)
  names(xyData)<- c("X", "Y")
  
  hash.out<- tanglerInfo$hash
  ## Need capture output to save hash key to a readme file
  
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
    nm2<- paste0(path, "/detangledXY_raster",stub, "_", hash.out, ".rds")
    if (saveTangles == TRUE){
    saveRDS(object = rasterOuts, file = nm2)}
    return(rasterOuts)} else {
      # write revised coordinates to file
      nm2<- paste0(path, "/detangledXY_",stub, "_", hash.out, ".rds")
      if (saveTangles == TRUE){
      saveRDS(object = xyData, file = nm2)}
      return(xyData)}}

  
#### END


