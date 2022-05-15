#' Plot contour of AERMOD.PLT output file in mapview
#'
#' Create a mapview contourline object from AERMOD.PLT
#'
#' @param plt string, path to the AERMOD.PLT file
#' @param epsg number, epsg to set crs in the raster object, default epsg 32632
#' @param levels vector of the levels for the contourplot
#' @param export logical, export mapview object as hmtl and png files? The default value is equal to FALSE
#' @param name_of_map_layer string, name of the mapview layer
#' @param string_filename string,  name of the output files
#' @param trans_factor number, transformation factor to be applied to calculated values, leave it as default = 1
#' @return mapview object exported as html and png files
#' @export

mapview_aermod_plt_contour <- function(plt,
                             epsg = 32632,
                             levels = NULL,
                             export = FALSE,
                             name_of_map_layer = 'mapview_layer_name',
                             string_filename = 'export_filename',
                             trans_factor = 1){

  net_id <- x <- y <- average_conc <- NULL

  # call read_aremod_plt
  tbl <- read_aermod_plt(plt)

  # select data, just grid receptors, remove rows corresponding to discrete receptors, where "net_id" is NA
  tbl <- tbl %>%
    tidyr::drop_na(net_id) %>% # this is the line for skipping discrete receptors
    dplyr::select(x, y, average_conc) %>%
    dplyr::mutate(average_conc = average_conc * trans_factor) #eventually apply transf_factor


  # transform to raster
  r<-raster::rasterFromXYZ(tbl, crs = epsg)

  # contourlines form raster: pay attention here because it gets a SpatialLinedDataFrame
  #grd<-raster::rasterToContour(grd, levels=c(1,3,5))
  #grd<-raster::rasterToContour(grd, levels=levels)

  # checking user input value
  if(is.null(levels)) {

    # use default of contour line
    r<-raster::rasterToContour(r)

    # need to sort the factor levels of the spatialLinesDataFRame
    r@data$level<-factor(r@data$level,
                           as.character(sort(as.numeric(r@data$level))))
  } else {

    # here it is not necessary to sort the factor levels, not completely understand that...
    r<-raster::rasterToContour(r, levels=levels)

  }

  # mapview
  map <- mapview::mapview(r,
                          #define the palette
                          #color = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(8, 'RdYlBu'))),
                          #alpha.region= 0.5,
                          #legend.opacity=0.5,
                          layer.name = name_of_map_layer)


  # eventually export the map to hml and png
  if(export) rfunctions::export_mapview(map, string_filename)

  # and finally return the map
  map

}
