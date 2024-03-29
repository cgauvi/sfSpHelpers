% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scalePolygons.R
\name{scale_geom_df}
\alias{scale_geom_df}
\title{Scale an sf object with *POLYGON geometry}
\usage{
scale_geom_df(df, scale_factor = 0.75, proj_new = NULL)
}
\arguments{
\item{scale_factor}{}
}
\description{
Convert to projected coordinates before performing the affine transformation
}
\details{
Inspired by Robin Lovelace's https://geocompr.robinlovelace.net/geometric-operations.html
}
