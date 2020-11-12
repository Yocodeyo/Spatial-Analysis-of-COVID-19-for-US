hexbin_map <- function(spdf, ...) {
  #hexagonal binning
  hbins <- fMultivar::hexBinning(coordinates(spdf),...)
  # Set up the hexagons to plot, as polygons
  u <- c(1, 0, -1, -1, 0, 1)
  u <- u * min(diff(unique(sort(hbins$x))))
  v <- c(1,2,1,-1,-2,-1)
  v <- v * min(diff(unique(sort(hbins$y))))/3
  # Construct each polygon in the sp model
  hexes_list <- vector(length(hbins$x),mode='list')
  for (i in 1:length(hbins$x)) {
    pol <- Polygon(cbind(u + hbins$x[i], v + hbins$y[i]),hole=FALSE)
    hexes_list[[i]] <- Polygons(list(pol),i) }
  # Build the spatial polygons data frame
  hex_cover_sp <- SpatialPolygons(hexes_list,proj4string=CRS(proj4string(spdf)))
  hex_cover <- SpatialPolygonsDataFrame(hex_cover_sp,data.frame(z=hbins$z),match.ID=FALSE)
  # Return the result
  return(hex_cover)
}

choose_bw <- function(spdf) { 
  #choose bandwidth for KDE analysis
  X <- coordinates(spdf)
  sigma <- c(sd(X[ ,1]), sd(X[ ,2])) * (2 / (3 * nrow(X))) ^ (1/6)
  return(sigma / 1000)
}