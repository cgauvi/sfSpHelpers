% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linestring_split.R
\name{remove_line_endpoints_from_split}
\alias{remove_line_endpoints_from_split}
\title{Splits line into segments using SfSpHelpers::split_lines and then removes the first and last segments (if possible)
and then groups the line string by id}
\usage{
remove_line_endpoints_from_split(
  shp_line,
  id_col,
  min_distance_m_to_remove = 10
)
}
\arguments{
\item{min_distance_m_to_remove}{}
}
\description{
Not exported, only kept as reference as it peforms the same function as remove_line_endpoints_lwgeom which
is preferable
}
