#' Calculate separation distances
#'
#' Get a a simple feature data frame of separation distances from the source
#' (origin) point to some predefined concentration contour lines
#'
#' @param plt string, path to the AERMOD.PLT file
#' @param epsg number epsg to set crs in the raster object
#' @param x_source x coordinates of the source (origin) point
#' @param y_source y coordinates of the source (origin) point
#' @param levels vector of defined levels for the contour lines
#' @param degree_step angle degree to scan directions where calculate distances from source (origin) point
#' @param trans_factor number, transformation factor to be applied to calculated values, leave it as default = 1
#' @return a simple feature data frame
#' @export

get_sep_dist_aermod_plt <- function(plt,
                                    epsg = 32632,
                                    x_source,
                                    y_source,
                                    levels = NULL,
                                    degree_step = 5,
                                    trans_factor = 1){


  net_id <- x <- y <- average_conc <- NULL

  # this is for getting rid of mapview warning, not to worry about
  # https://github.com/r-spatial/mapview/issues/422
  options(rgdal_show_exportToProj4_warnings = "none")

  # call read_aremod_plt
  tbl <- read_aermod_plt(plt)

  # select data, just grid receptors, remove rows corresponding to discrete receptors, where "net_id" is NA
  tbl <- tbl %>%
    tidyr::drop_na(net_id) %>% # this is the line for skipping discrete receptors
    dplyr::select(x, y, average_conc) %>%
    dplyr::mutate(average_conc = average_conc * trans_factor) #eventually apply transf_factor


  # transform to raster
  r<-raster::rasterFromXYZ(tbl, crs = epsg)

  # re-project raster to new crs
  r<- raster::projectRaster(r, crs=sp::CRS(paste0('+init=epsg:', 4326)))

  # checking user input value
  if(is.null(levels)) {

    # use default of contour line
    # spatial lines data frame
    cl<-raster::rasterToContour(r)

  } else {

    # spatial lines data frame
    cl<-raster::rasterToContour(r, levels=levels)

  }

  # transform contour lines to sf feature
  cl_sf <- sf::st_as_sf(cl)

  # order levels
  cl_sf$level<-factor(cl_sf$level, levels=sort(as.numeric(cl_sf$level)))

  # define coordinates of the origin point
  # create simple feature with appropriate crs
  pt_source <- sf::st_sfc(sf::st_point(c(x_source, y_source)), crs = epsg)

  # # transform sf crs to geo, epsg 4326
  pt_source <- sf::st_transform(pt_source, crs =  4326)

  # get lng and lat coordinates of the source point
  lng_source <- sf::st_coordinates(pt_source)[1]
  lat_source <- sf::st_coordinates(pt_source)[2]

  # get the max value of the minimum distance (shortest path) from origin to contourlines
  dist_orig_contour <- max(sf::st_distance(cl_sf, pt_source))

  # create vector of bearing angles of predefined amplitude as degree_step
  #degree_step <- 5
  bearing_angles <- seq(0, 360-degree_step, degree_step)

  # get destination point at a given bearing angle
  # returning coordinates of destination point
  # keep it a safe factor 100 for distance
  pts_dest <- geosphere::destPoint(p = c(lng_source, lat_source),
                                   b = bearing_angles,
                                   d = dist_orig_contour*100)

  # https://gis.stackexchange.com/questions/312289/r-create-multiple-linestrings-from-multiple-coordinates

  # create dataframe: lon, lat from origin to destination points
  orig_dest_ls_sf<-data.frame(lng_source, lat_source, pts_dest)

  # create an helper function for dealing with calculation by rows fo df
  st_segment <- function(r){sf::st_linestring(t(matrix(unlist(r), 2, 2)))}

  # add geometry to df
  orig_dest_ls_sf$geom <- sf::st_sfc(sapply(1:nrow(orig_dest_ls_sf), function(i){st_segment(orig_dest_ls_sf[i,])}, simplify=FALSE))

  # tranSform to sf linestring: from origin to destination points
  orig_dest_ls_sf <-sf::st_sf(orig_dest_ls_sf, crs = 4326)

  # add bearing angles to sf
  orig_dest_ls_sf$bearing_angle <- bearing_angles

  # get sf of the intersection points between the two features
  intersect_sf <- sf::st_intersection(cl_sf, orig_dest_ls_sf)

  # get the distance matrix
  dist_m <- sf::st_distance(pt_source, intersect_sf)

  # add distance to sf
  intersect_sf$distance <- as.vector(units::drop_units(t(dist_m)))

  intersect_sf

}
