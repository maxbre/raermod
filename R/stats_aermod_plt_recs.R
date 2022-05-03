#' Statistics about discrete receptors in AERMOD.PLT output file
#'
#' Get relevant statistics for discrete receptors in AERMOD.PLT output file
#'
#' @param plt string, path to the AERMOD.PLT file
#' @return tibble with: net_id, x and y coordinates, average concentration value, date expressed in the form 'yyyyjjjh'
#' @export
#'

stat_aermod_plt_recs <- function(plt){

  net_id <- x <- y <- average_conc <- date_conc_ <- NULL

  rec <- read_aermod_plt(plt) %>%
    dplyr::select(net_id = net_id,
                  x_coord = x,
                  y_coord = y,
                  conc_value = average_conc,
                  date = date_conc_)%>%
    dplyr::filter(is.na(net_id))

  rec

}
