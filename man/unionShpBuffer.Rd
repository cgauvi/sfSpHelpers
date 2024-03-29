% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buffer.R
\name{unionShpBuffer}
\alias{unionShpBuffer}
\title{Create a buffer around an sf object + merge the resulting polygons using st_union}
\usage{
unionShpBuffer(shp, dist)
}
\arguments{
\item{shp, }{sf object}

\item{dist, }{distance (supposed to be in some valid unit - e.g. degreees or meters)}
}
\value{
shpUnionedBuff, sf object with potentially less features than shp
}
\description{
Create a buffer around an sf object + merge the resulting polygons using st_union
}
