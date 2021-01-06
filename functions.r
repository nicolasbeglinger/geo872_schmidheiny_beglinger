############################################
### Spatial Analysis - Project Functions ###
############################################


# Initialize

pkgTest <- function(x) 
{
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("sf")
pkgTest("tidyverse")
pkgTest("tmap")

#for palette explorer
pkgTest("tmaptools")
pkgTest("shiny")
pkgTest("shinyjs")


crs_lv95  <- "+init=epsg:2056"

tmap_mode("view")


# MyHexBin

MyHexBin <- function(x, extent = NA, cellsize = 100, mrg = 100)
{
  
  if (is.na(extent)) {
    require(sf)
    x1 <- st_coordinates(x)[,1]
    y2 <- st_coordinates(x)[,2]
    minx1 <- min(x1) - mrg
    maxx1 <- max(x1) + mrg
    miny2 <- min(y2) - mrg
    maxy2 <- max(y2) + mrg
  } else {
    minx1 <- extent[1] - mrg
    maxx1 <- extent[3] + mrg
    miny2 <- extent[2] - mrg
    maxy2 <- extent[4] + mrg
  }
  
  # coords defines the bounding rectangle of the hexagonal tessellation,
  # including the margin defined by parameter mrg.
  # Alternatively, coords might be defined by the coordinates of a particular 
  # window that one might be interested in. Would require changing this code.
  coords <- list(rbind(c(minx1,miny2), c(minx1,maxy2),
                       c(maxx1,maxy2), c(maxx1,miny2), c(minx1,miny2)))
  # Turn bounding coords into an sfc_POLYGON object and inherit CRS from x
  pg = st_sfc(st_polygon(coords), crs = st_crs(x))
  
  # Create a hexagonal grid --> sfc_POLYGON object
  hexPol <- st_make_grid(pg, cellsize = cellsize, square = FALSE)
  
  hexPol_npts <- aggregate(x[1], hexPol, length) %>%
    replace(is.na(.), 0) #%>% rename(npts_hex = id)
  
  return(hexPol_npts)
}


# CountMap

countMap = function(hexBinFunction, data, cellsize = 100, title, datapoints, pointopacity = 1, mode = "view") {
  
  hexData = MyHexBin(data, st_bbox(accidents), cellsize = cellsize, mrg = 100) %>%
    filter(!ïAccUID == 0)
  
  quantDiff = quantile(hexData$ïAccUID, probs = seq(0,1,0.0025))
  
  if (mode == "view") {
    tm_shape(datapoints) +
      tm_dots(
        size = 0.01,
        alpha = pointopacity
      ) +
      tm_shape(hexData) +
      tm_polygons(
        col = "ïAccUID",
        id = "ïAccUID",
        palette = c("#F7F7F7", "#F6B596", "#CB4A42", "#67001F"),
        alpha = 0.7,
        style = "fixed",
        breaks = c(min(hexData$ïAccUID),
                   quantDiff[320], 
                   quantDiff[360],
                   quantDiff[376],
                   quantDiff[393],
                   quantDiff[397],
                   quantDiff[400],
                   max(hexData$ïAccUID)),
        lwd = 0.1
      ) +
      tm_layout(title = title)
    
  } else if (mode == "return") {
    return(hexData)
    
  } else {
    warning("Mode has to be \"view\" or \"return\"!")
  }
}


# ChiMap

chiMap = function(hexBinFunction, reference, focus, cellsize, title, focuspoints = TRUE, accidentspoints = NULL, pointopacity = 1, mode = "view") {
  hexRef = hexBinFunction(reference, st_bbox(reference), cellsize = cellsize, mrg = 100)
  
  hexFoc = MyHexBin(focus, st_bbox(reference), cellsize = cellsize, mrg = 100) %>%
    mutate(expect = (nrow(focus) / nrow(reference)) * hexRef$ïAccUID) %>%
    mutate(chi = (ïAccUID-expect)/sqrt(expect),
           id = row_number()) %>%
    filter(!is.na(chi))
  
  quantDiff = quantile(hexFoc$chi, probs = seq(0,1,0.0025))
  
  if (mode == "view") {
    tm1 = tm_shape(hexFoc) +
      tm_polygons(
        col = "chi",
        id = "chi",
        palette = c("#053061", "#3885BC", "#A4CEE3", "#F7F7F7", "#F6B596", "#CB4A42", "#67001F"),
        alpha = 0.7,
        style = "fixed",
        breaks = c(min(hexFoc$chi), 
                   quantDiff[2], 
                   quantDiff[9], 
                   quantDiff[40], 
                   quantDiff[360], 
                   quantDiff[393], 
                   quantDiff[400], 
                   max(hexFoc$chi)),
        lwd = 0.1
      ) +
      tm_layout(title = title)
    if (!is.null(accidentspoints)) {
      tm1 = tm1 + tm_shape(accidentspoints) +
        tm_dots(
          size = 0.01,
          alpha = pointopacity,
          col = 'black'
        )
    }
    tm1 = tm1 + 
      tm_shape(reference) +
      tm_dots(
        size = 0.01,
        alpha = pointopacity,
        col = "red"
      )
    if (focuspoints) {
      tm1 = tm1 + tm_shape(focus) +
        tm_dots(
          size = 0.01,
          alpha = pointopacity,
          col = 'yellow'
        )
    }
    tm1
  } else if (mode == "return") {
    return(hexFoc)
    
  } else {
    warning("Mode has to be \"view\" or \"return\"!")
  }
}


# MyShiftBins

MyShiftBins <- function(x, extent = NA, cellsize = 100, mrg = 100) {
  
  notHalf = ((cellsize/2) / sqrt(3)) * 2
  half = cellsize * 0.5
  notQuarter = notHalf * 0.5
  matrix = rbind(c(0,0,0,0), 
                 c(0,0,-notHalf,-notHalf), 
                 c(-half,-half,-notQuarter,-notQuarter))
  
  for (i in c(1,2,3)) {
    
    print(i)
    
    if (is.na(extent)) {
      require(sf)
      x1 <- st_coordinates(x)[,1]
      y2 <- st_coordinates(x)[,2]
      minx1 <- min(x1) - mrg
      maxx1 <- max(x1) + mrg
      miny2 <- min(y2) - mrg
      maxy2 <- max(y2) + mrg
    } else {
      minx1 <- extent[1] - mrg
      maxx1 <- extent[3] + mrg
      miny2 <- extent[2] - mrg
      maxy2 <- extent[4] + mrg
    }
    
    minx1 = minx1 + matrix[i,1]
    maxx1 = maxx1 + matrix[i,2]
    miny2 = miny2 + matrix[i,3]
    maxy2 = maxy2 + matrix[i,4]
    
    # coords defines the bounding rectangle of the hexagonal tessellation,
    # including the margin defined by parameter mrg.
    # Alternatively, coords might be defined by the coordinates of a particular 
    # window that one might be interested in. Would require changing this code.
    coords <- list(rbind(c(minx1,miny2), c(minx1,maxy2),
                         c(maxx1,maxy2), c(maxx1,miny2), c(minx1,miny2)))
    # Turn bounding coords into an sfc_POLYGON object and inherit CRS from x
    pg = st_sfc(st_polygon(coords), crs = st_crs(x))
    
    # Create a hexagonal grid --> sfc_POLYGON object
    hexPol <- st_make_grid(pg, cellsize = cellsize, square = FALSE)
    
    hexPol_npts <- aggregate(x[1], hexPol, length) %>%
      replace(is.na(.), 0) #%>% rename(npts_hex = id)
    
    if (i == 1) {
      xy = hexPol_npts
    } else {
      xy = rbind(xy, hexPol_npts)
    }
  }
  
  return(xy)
}




