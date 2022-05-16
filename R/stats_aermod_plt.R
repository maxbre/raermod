#' Statistics about AERMOD.PLT output file
#'
#' Get relevant statistics for the AERMOD.PLT output file from a given reference point (a source emission)
#'
#' @param plt string, path to the AERMOD.PLT file
#' @param x_source numeric, x coordinate of a given reference point (a source emission point)
#' @param y_source numeric, y coordinate of a given reference point (a source emission point)
#' @return tibble with: x and y coordinates, concentration values, distances from the reference point (a source emission), compass degrees from the North, the date expressed in the form 'yymmddhh', and the 'net id' corresponding to min and max statistics
#' @export


stats_aermod_plt<- function(plt, x_source, y_source){

  # read tbl data (call user function)
  tbl<-read_aermod_plt(plt)

  # index for min concentration value
  i_min<-which.min(tbl$average_conc)
  # value of min concentration
  c_min <- min(tbl$average_conc)
  #c_min <- tbl$average_conc[i_min]

  # x, y coordinates and date corresponding at min value
  x_min <- tbl$x[i_min]
  y_min <- tbl$y[i_min]
  date_min <- tbl$date_conc_[i_min]
  net_min <- tbl$net_id[i_min]

  # index for max concentration value
  i_max<-which.max(tbl$average_conc)
  # value of max concentration
  c_max <- max(tbl$average_conc)
  #c_max <- tbl$average_conc[i_max]

  # x, y coordinates date corresponding at max value
  x_max <- tbl$x[i_max]
  y_max <- tbl$y[i_max]
  date_max <- tbl$date_conc_[i_max]
  net_max <- tbl$net_id[i_max]

  # linear distance of min concentration from the source
  d_min <-sqrt((x_source - x_min)^2+(y_source - y_min)^2)

  # linear distance of max concentration from the source
  d_max <- sqrt((x_source - x_max)^2+(y_source - y_max)^2)

  # calculate bearing angles
  # between the source point
  # and other relevant points (min, max, p98)

  # by recalling that
  # rad : pi = deg : 180
  # two functions to transform
  # from radians to degrees
  # from degrees to radians
  #rad2deg <- function(rad) {rad / pi * 180}
  #deg2rad <- function(deg) {deg / 180 * pi}

  # bearing angle for point at min distance
  #ba_min <- rad2deg(atan2((y_source - y_min), (x_source - x_min)))

  #bearing angle for point at max distance
  #ba_max <- rad2deg(atan2((y_source - y_max), (x_source - x_max)))

  # now is needed the transformation
  # from polar to compass degrees (from the North)

  # all that is dealt much more easily
  # with the use of complex number approach
  # SEE OTHER FUNCTIONS IN MY PKG!!!


  #(x_max - x_source)
  #(y_max - y_source)
  #deg<-car2deg((x_max - x_source), (y_max - y_source))

  deg_N_max<-rfunctions::car2deg_N((x_max - x_source), (y_max - y_source))
  deg_N_min<-rfunctions::car2deg_N((x_min - x_source), (y_min - y_source))

  # calculate relevant statistics
  tib_min<-tibble::tibble(stat = "min", x_coord = x_min, y_coord = y_min,
                          conc_value = c_min, dist = d_min,
                          deg_N = deg_N_min, date = date_max,
                          net_id = net_min)

  tib_max<-tibble::tibble(stat = "max", x_coord = x_max, y_coord = y_max,
                          conc_value = c_max, dist = d_max,
                          deg_N = deg_N_max, date = date_min,
                          net_id = net_max)

  dplyr::bind_rows(tib_min, tib_max)

  }
