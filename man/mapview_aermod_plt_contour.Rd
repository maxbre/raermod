% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapview_aermod_plt_contour.R
\name{mapview_aermod_plt_contour}
\alias{mapview_aermod_plt_contour}
\title{Plot contour of AERMOD.PLT output file in mapview}
\usage{
mapview_aermod_plt_contour(
  plt,
  epsg = 32632,
  levels = NULL,
  export = FALSE,
  name_of_map_layer = "mapview_layer_name",
  string_filename = "export_filename",
  trans_factor = 1
)
}
\arguments{
\item{plt}{string, path to the AERMOD.PLT file}

\item{epsg}{number, epsg to set crs in the raster object, default epsg 32632}

\item{levels}{vector of the levels for the contourplot}

\item{export}{logical, export mapview object as hmtl and png files? The default value is equal to FALSE}

\item{name_of_map_layer}{string, name of the mapview layer}

\item{string_filename}{string,  name of the output files}

\item{trans_factor}{number, transformation factor to be applied to calculated values, leave it as default = 1}
}
\value{
mapview object exported as html and png files
}
\description{
Create a mapview contourline object from AERMOD.PLT
}
