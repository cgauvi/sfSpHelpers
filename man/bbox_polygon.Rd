% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bboxPolygon.R
\name{bbox_polygon}
\alias{bbox_polygon}
\title{Create a st_polygon from a bounding box}
\usage{
bbox_polygon(shp, buffer = 0)
}
\arguments{
\item{shp}{sf or sfc object that we want to bound within some box}

\item{buffer}{dist to add around the bounding box to avoid the weird boundary effects}
}
\value{
st_polygon forming a box
}
\description{
Really useful function to define bounding box enveloppe
}
\details{
See https://stackoverflow.com/questions/45719790/create-voronoi-polygon-with-simple-feature-in-r
}
\examples{
\dontrun{
shp_trees <-  st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
shp_trees \%>\% bbox_polygon()
}

}
