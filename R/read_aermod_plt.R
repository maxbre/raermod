#' Read AERMOD.PLT output files as a tabular data
#'
#' Returns a tibble of the output file AERMOD.PLT
#'
#' @param plt a string path to the AERMOD.PLT file
#' @param skip number of rows to skip, leave as default
#' @return an object of type tibble
#' @export
#' @examples \dontrun{
#' read_aermod_plt('./AERMOD.PLT')
#' }

read_aermod_plt <-function(plt, skip = 6){

  # get columns by positions of empty rows
  col_positions <- readr::fwf_empty(plt, skip = skip)

  # get and clean column names
  col_names<-readr::read_fwf(plt, col_positions = col_positions, skip = skip, n_max = 1) %>%
    dplyr::select(-1) %>%                            # delete useless (empty) column
    dplyr::slice(1) %>%                              # get first row
    purrr::flatten_chr() %>%                         # transform to character vector
    stringr::str_replace_all("[^[:alnum:]]", "_") %>%  # some data cleaning
    stringr::str_to_lower()

  # get tabular dataset and do some tidying-up
  tbl<-vroom::vroom_fwf(plt,
                        id = "path_filename",
                        skip = skip+2, # offset of 2 rows
                        col_positions = col_positions)%>%
    dplyr::select(-2)%>%  # delete useless (empty) column, the very same as above
    dplyr::rename_with(~col_names, !"path_filename")

  tbl

}
