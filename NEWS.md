

sfSpHelpers v0.6 17519c486aeda413707f8d59101df8b3d6f5b1d5  December 22 2022
==============

Changes:

* Debugged download zip shp : should work when run multiple times with different files
* More examples and documentation


sfSpHelpers v0.5 786f583d5a137f8c17a4beee263f938619fa5666 September 12 2022
==============

Changes:

* Removed download zip shp file test (too long) + removed all osmisochrone code and moved to separate repo



sfSpHelpers v0.4.2 b76ef3ae55790a05649d761036b99f618eb8fe0b June 20 2022
==============

Changes:

* Exposed st_sf_linestring_from_points which will be much more useful on its own (as opposed to called by st_linestring_to_sf_linestring_endpoints)


sfSpHelpers v0.4.1 e73e0a03e173c95935f4bbfde86b56bb33227257 Apr 24 2022
==============

Changes:

* Fixed broken dependency with ggvoronoi



sfSpHelpers v0.4 326e4426b252b867cf4133a061af4ce01c914d2c Apr 24 2022
==============

Changes:

* Implemented osm bbox + bbox from vector
* Removed useless makeValid functions
* Debugged vignettes + added voronoi vignette


sfSpHelpers v0.3 c5e9ee7c2c86c9cd2672e463fa581371e22ae34c Apr 23 2022
==============

Changes:

* Implemented linestring split functions
* Removed useless dependencies - e.g. GeneralHelpers
* Removed useless functions that could be replaced by existing e.g st_read_remote is the same as st_read
* Debugged functions: get_zipped_remote_shapefile 


sfSpHelpers v0.2 - ce153047ea9b36273f95d89987d3b8b82f239410  Mon Mar 28 2022
==============

Changes:

* Implemented osmIsochrone functions



sfSpHelpers v0.1 Sun Jan 17  b297a4c2e839f62a724e44747d0b1d721f457140
==============

Changes:

* New polygon scaling function


sfSpHelpers v0.0 Tue Aug 25 2020  372f03bfab48f818db4aeb8d2be61ee05a8dad6c
==============

Changes:

* New k-means function + voronoi interpolation


