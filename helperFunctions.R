
### Converts results from a tapply to a data.frame -----------------------------


tapply2DF <- function(xData,          # Vector being tapply'd 
                      byField,        # Field to split vector by
                      xFunc,          # Function to apply
                      newName='Var',  # Name of new variable 
                      idName='ID',
                      na.rm=FALSE)    # Name of identification field 
{
  
 ## Execute tapply()
  
  xTable <- as.data.frame(tapply(xData, byField, xFunc, na.rm=na.rm))
  
 ## Add names and new fields
  
  # Give calculated field a name
  names(xTable) <- newName
  
  # Add id field and give it a name
  xTable[ ,2] <- rownames(xTable)
  names(xTable)[2] <- idName
  
  # Reorder columns
  xTable <- xTable[ ,c(2, 1)]
  
  # Remove existing field names
  rownames(xTable) <- 1:nrow(xTable)
  
 ## Return values
  
  return(xTable)
}

### Point to surface converter, with optional clip ------------------------------

point2Surface <- function(points, 
                          surf.data, 
                          res, 
                          idp.val=2,
                          clip=NULL,
                          verbose=0){
  
  # ARGUMENTS
  #
  # points: SpatialPointsDataFrame of observations
  # surf.data: vector of values at the points used to create surface
  # res: resolultion or size of the grid cells
  # ipd.val: inverse distance paramter (1 to 4??)
  # clip: SpatialPolygons to clip the results to  
  
  require(sp)
  require(gstat)
  
  ## Check points
  
  if(class(points) != "SpatialPointsDataFrame" & 
     class(points) != "SpatialPoints"){ 
    return('Points data must be SpatialPointsDataFrame or SpatialPoints object')
  }
  
  
  ## Check surface data  
  
  if(length(surf.data) != nrow(points@data)){ 
    return('Data to interpolate not same length as point data')
  }
  
  ## Extract bounding coordinates
  
  xy.box <- bbox(points)
  
  # expand out resolution
  xy.box[,1] <- xy.box[,1] - res 
  xy.box[,2] <- xy.box[,2] + res 
  
  ## Build grid of points
  
  xs <- seq(xy.box[1, 1], xy.box[1,2], res)
  ys <- seq(xy.box[2, 1], xy.box[2,2], res)
  xy.grid <- as.data.frame(expand.grid(xs, ys))
  colnames(xy.grid) <- c("x", "y")
  
  ## Create a SPDF and Pixel DF  
  
  xy.grid <- SpatialPointsDataFrame(cbind(xy.grid$x, xy.grid$y), data=xy.grid,
                                    proj4string=CRS(proj4string(points)))
  xy.pix <- as(xy.grid, "SpatialPixelsDataFrame")
  
  
  ## Create idw image  
  
  image.out <- idw(surf.data ~ 1, points, xy.pix, idp=idp.val, debug.level=verbose)
  
  ## If trim
  
  if(!is.null(clip)){
    
    # Check format
    if(class(clip) != "SpatialPolygonsDataFrame" & 
       class(clip) != "SpatialPolygons"){ 
      return(paste0('Points data must be SpatialPolygonsDataFrame',
                    'or SpatialPolygons object'))
    }
    
    # Make clip
    in.xy <- gContains(clip, xy.grid, byid=T) 
    image.out <- image.out[which(in.xy), ] 
  }
  
  ## Return 
  
  return(image.out)
  
}







################################################################################  
# Great a grid covering a set of points ----------------------------------------

createGridPoints <- function(X, gscale, mask=F){
  
  require(sp)
  
  # Set bounding range  
  BBox <- X@bbox
  xmin <- round(BBox[1,1] - gscale, -round(log(gscale,10),0))
  xmax <- round(BBox[1,2] - gscale, -round(log(gscale,10),0))
  ymin <- round(BBox[2,1] - gscale, -round(log(gscale,10),0))
  ymax <- round(BBox[2,2] - gscale, -round(log(gscale,10),0))
  xrange <- xmax - xmin
  yrange <- ymax - ymin
  
  # set up capture object
  grid.coords <- matrix(nrow=(yrange/gscale) * (xrange/gscale), ncol = 2)
  x.nums <- (xmin/gscale):(xmax/gscale)
  y.nums <- (ymin/gscale):(ymax/gscale)
  
  # Assign Points 
  for(x in 1:(xrange/gscale)){
    for(y in 1:(yrange/gscale)){
      grid.coords[((x-1)*(yrange/gscale))+y,] <- c(x.nums[x]*gscale,
                                                   y.nums[y]*gscale)
    }
  }
  
  # Create Full Grid
  grid.sp <- SpatialPointsDataFrame(grid.coords, 
                                    data=as.data.frame(1:dim(grid.coords)[1]))
  
  # Mask
  if(mask){  
    gi <- gIntersects(X, grid.sp, byid=T)
    grid.sp <- grid.sp[which(gi),]
  }
  
  # Return grip
  
  return(grid.sp)
  
}

################################################################################
# Plot Quartile Moving Average Map ---------------------------------------------

plotQMA <- function(X.Grid, qma.data, ma.var, near=100, perc=FALSE, 
                    rb.col="RdBu",pcex=.4,legx=1226000,legy=270000, 
                    np.round=2, breaks="none"){
  
  require(sp)
  
  mv <- which(colnames(qma.data) == ma.var)
  
  # Estimate Moving Average
  X.Grid@data$Mov.Ave <- 0
  X.Grid@data$Power <- 0
  
  for(i in 1:dim(X.Grid@data)[1]){
    dists <- sqrt(((X.Grid@coords[i,1]-qma.data$X)^2) + 
                    ((X.Grid@coords[i,2]-qma.data$Y)^2))
    near.dists <- order(dists)[1:near]
    x.dists <- dists[near.dists]
    x.dists <- ifelse(x.dists<10,10,x.dists)
    x.dists <- x.dists/max(x.dists)
    x.dists <- (1-x.dists^3)^3
    x.sum <- sum(x.dists)
    x.val <- sum((qma.data[near.dists,mv] * x.dists)/ x.sum)
    X.Grid@data$Mov.Ave[i] <- x.val
    X.Grid@data$Power[i] <- x.sum+.01
  }
  
  # Set up Colors
  ma.col <- brewer.pal(4,rb.col)[4:1]
  ma.col[2] <- "#C4C4C4"
  X.Grid@data$Col <- ma.col[1]
  
  # Set Quartiles and assign Colors
  if(breaks[1] == "none"){
    q.25 <- quantile(X.Grid@data$Mov.Ave,.25)
    q.50 <- quantile(X.Grid@data$Mov.Ave,.5)
    q.75 <- quantile(X.Grid@data$Mov.Ave,.75)
  }
  
  if(breaks[1] != "none"){
    q.25 <- breaks[1]
    q.50 <- breaks[2]
    q.75 <- breaks[3]
    
  }
  X.Grid@data$Col[X.Grid@data$Mov.Ave > q.25 & 
                    X.Grid@data$Mov.Ave <= q.50 ] <- ma.col[2]
  X.Grid@data$Col[X.Grid@data$Mov.Ave > q.50 & 
                    X.Grid@data$Mov.Ave <= q.75 ] <- ma.col[3]
  X.Grid@data$Col[X.Grid@data$Mov.Ave > q.75 ] <- ma.col[4]
  
  
  # Assign Alphas
  x.alpha <- X.Grid@data$Power/max(X.Grid@data$Power)
  #x.alpha <- ifelse(x.alpha>mean(x.alpha),1,x.alpha/(median(x.alpha)))
  x.alpha[x.alpha<.01] <- .01
  x.cols <- paste0(X.Grid@data$Col,substr(as.character(x.alpha),3,4))
  
  
  
  # Plot
  #   qmaSP <- SpatialPointsDataFrame(cbind(qma.data$X,qma.data$Y), data = qma.data)
  #   plot(qmaSP,cex=.1)
  plot(Seattle.Clip,bor="gray50",lwd=2)
  plot(X.Grid,col=x.cols,pch=15,cex=pcex+.2,add=T)
  
  
  # Add Legend
  
  if(perc){
    legend(legx, legy, c(paste0(" < ",round(q.25,2)*100,"%"),
                         paste0(round(q.25,2)*100,"% - ",round(q.50,2)*100,"%"),
                         paste0(round(q.50,2)*100,"% - ",round(q.75,2)*100,"%"),
                         paste0(" > ", round(q.75,2)*100,"%")),
           col=ma.col, cex=.7, pch=15)
  }
  if(!perc){
    legend(legx, legy, c(paste0(" < ", round(q.25, np.round)),
                         paste0(round(q.25, np.round), " - ", 
                                round(q.50, np.round)),
                         paste0(round(q.50, np.round), " - ", 
                                round(q.75, np.round)),
                         paste0(" > " , round(q.75, np.round))),
           col=ma.col, cex=.7, pch=15)
  }
}


### Function for calculating the Pseudo R2 of a spatial error model ----------------------

calcPseudoR2 <- function(semModel){
  pR2 <- (1 - (as.numeric(semModel$SSE) / 
                 sum((semModel$y - mean(semModel$y)) ^ 2)))
  return(pR2)
}

### Function for building a spatial weights matrix (NOT COMPLETE)

createSWM <- function(data # SpatialPointsDataFrame
                      ,knn  # Number of neighbors desired
                      ,distWeighted = TRUE # Distance Weighted ?
                      ,nugget = 1 # How much to add to identical points
){
  
  ## Create distance weighting function
  
  dwf <- function(x) {1 / ((x + nugget) ^ 2)}
  
  ## Create Neighbor List
  nbList <- knn2nb(knearneigh(data, knn))
  
  ## Create Distances
  if(distWeighted){
    nbDists <- nbdists(nbList, data)    
  }
  
  ## Building Weights Matrix
  swm <- listw2U(nb2listw(nbList, glist = lapply(nbDists, dwf)
                          , style="W",zero.policy=T))    
}

### Chow test of spatial models ----------------------------------------------------------

spatChowTest <- function(mod.rest,    # Restricted model
                         mod.unrest   # Unrestricted model
)
{
  
  ## Extract likelihoods from models
  
  lrest <- mod.rest$LL
  lunrest <- mod.unrest$LL
  
  ## Extract number of parameter
  
  k <- mod.rest$parameters - 2
  
  ## Calculate and return chow values 
  
  spchow <- - 2.0 * (lrest - lunrest)
  pchow <- pchisq(spchow, k, lower.tail=FALSE)
  return(list(spchow, pchow, k))
} 


# Place multiple ggplots into a configuration ----------------------------------

ggMultiPlots <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### Tool for setting bbox for use in get_map -------------------------------------

ggmapBBox <- function(shp.file, 
                      zoom=1
)
{
  
  # ARGUMENTS
  #
  # shp.file:  shapefile of area to extract
  # zoom:      zoom factor where 1 is perfect clip to shp.file
  
  ## Set Libraries
  
  require(ggmap)
  
  ## Extract Bounding Box  
  
  bbox <- bbox(shp.file) 
  
  ## Apply Zoom Features  
  
  #NW Hemisphere
  if(bbox[1,1]<0 & bbox[2,1] > 0){
    return(c(bbox[1,1]*zoom, bbox[2,1]*(1/zoom),
             bbox[1,2]*(1/zoom), bbox[2,2]*zoom))
  }
  
  #NE Hemisphere
  if(bbox[1,1] > 0 & bbox[2,1] > 0){
    return(c(bbox[1,1]*(1/zoom), bbox[2,1]*zoom,
             bbox[1,2]*(1/zoom), bbox[2,2]*zoom))
  }
  
  #SE Hemisphere
  if(bbox[1,1]>0 & bbox[2,1] < 0){
    return(c(bbox[1,1]*(1/zoom), bbox[2,1]*zoom,
             bbox[1,2]*zoom, bbox[2,2]*(1/zoom)))
  }
  
  #SW Hemisphere
  if(bbox[1,1] > 0 & bbox[2,1] > 0){
    return(c(bbox[1,1]*zoom, bbox[2,1]*(1/zoom),
             bbox[1,2]*zoom, bbox[2,2]*(1/zoom)))
  }
  
}


