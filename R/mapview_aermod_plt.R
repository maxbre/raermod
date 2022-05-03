#' Plot AERMOD.PLT output file in mapview
#'
#' Create mapview object from AERMOD.PLT file and export as html and png files
#'
#' @param plt string ,path to the AERMOD plt file
#' @param epsg number, epsg to set crs in the raster object, default epsg 32632
#' @param name_of_map_layer string, name of the mapview layer
#' @param string_filename string,  name of the output files
#' @param trans_factor number, transformation factor to be applied to calculated values, leave it as default = 1
#' @return mapview object exported as html and png files
#' @export

mapview_aermod_plt<-function(plt, name_of_map_layer = 'mapview_layer_name', string_filename = 'export_filename', epsg = 32632,  trans_factor = 1){

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

  # eventually to hardcode the binning in mapview
  #my_bins<-c(0, 1, 2, 3, 4, 5, 10, 100, round(max(raster::values(r)),0))
  #my_bins<-pretty(range(raster::values(r)))

  # mapview
  map <- mapview::mapview(r,
                          # define the palette
                          col.regions = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(8, 'RdYlBu'))),
                          # here eventually define the binning
                          #at = my_bins,
                          na.color ='transparent',
                          alpha.region= 0.5,
                          legend.opacity=0.5,
                          layer.name = name_of_map_layer)

  # export html, png, call function
  rfunctions::export_mapview(map, string_filename)

}
