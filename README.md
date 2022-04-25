
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SfSpHelpers

<!-- badges: start -->

[![R-CMD-check](https://github.com/cgauvi/sfSpHelpers/workflows/R-CMD-check/badge.svg)](https://github.com/cgauvi/sfSpHelpers/actions)
<!-- badges: end -->

Basic `R` package to perform routine spatial operations

## Installation

You can install the development version of SfSpHelpers from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cgauvi/sfSpHelpers")
```

## Example

For instance, create a 2D kernel density heatmap from a sf object with
POINT geometry to show the density of points. Can then easily be mapped
using mapview or ggplot2

``` r
library(SfSpHelpers)
library(sf)
library(ggplot2)

#Trees
shp_trees <-  st_read('https://www.donneesquebec.ca/recherche/dataset/bc5afddf-9439-4e96-84fb-f91847b722be/resource/bbdca0dd-82df-42f9-845b-32348debf8ab/download/vdq-arbrepotentielremarquable.geojson')
 
#Neighborhoods - read with the get_zipped_remote_shapefile util from the open data portal 
shp_neigh <-  get_zipped_remote_shapefile("https://www.donneesquebec.ca/recherche/dataset/5b1ae6f2-6719-46df-bd2f-e57a7034c917/resource/508594dc-b090-407c-9489-73a1b46a8477/download/vdq-quartier.zip")


shp_tree_bbox_poly <- bbox_polygon(shp_trees)
shp_neigh <- st_intersection(shp_neigh, shp_tree_bbox_poly)

#get_polygon_heatmap Works with polygons also (takes centroid implicitely)
shp_polyons <- get_polygon_heatmap(shp_trees , bw=.001, gsize=500 )
 
#Can use the colors produced automatically, but this is a red to yellow gradient 
ggplot(shp_polyons)+
    geom_sf(aes( fill = colors),lwd=0) +
    scale_fill_viridis_d() +
    geom_sf(data=shp_neigh, aes(col=NOM),alpha=0) + 
    ggplot2::theme_minimal(base_family="Roboto Condensed", base_size=11.5) +
    theme(legend.position = 'none') + 
    ggtitle('Exceptional tree density') + 
    coord_sf(datum=NA) + 
    labs(subtitle = 'Quebec City neighborhoods',
         caption ='Source: https://www.donneesquebec.ca - vdq-arbrepotentielremarquable.geojson')
```

<img src="https://github.com/cgauvi/sfSpHelpers/blob/master/man/figures/README-example-1.png" width="100%" />

See the [vignettes](https://cgauvi.github.io/sfSpHelpers/) for more
details.
