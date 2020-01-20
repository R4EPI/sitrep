#' Fake spatial data as polygons
#' This function returns a polygon which is split in to regions based on a
#' supplied vector of names
#' @param regions A string of names for each region to label the polygon with
#' @importFrom sf st_polygon st_make_grid st_intersection st_sf st_set_crs
#' @importFrom utils read.csv
#' @references The coordinates used for the polygon are of Vienna, Austria.
#' based off government data (see [metadata](https://www.data.gv.at/katalog/dataset/stadt-wien_bezirksgrenzenwien))
#' @export
gen_polygon <- function(regions) {

  # get file path
  path <- system.file("extdata", "coords.csv", package = "sitrep")

  # read in coordinates as matrix
  coords <- as.matrix(read.csv(path))
  # change to list
  coords <- list(coords)

  # create a polygon from coordinates
  original_poly <- sf::st_polygon(coords)

  # define how many regions we want in our polygon
  # high <- ceiling(length(regions) / 2)
  high <- ceiling(sqrt(length(regions)))
  # change polygon to grid (subdivisions as squares)
  gridding <- sf::st_make_grid(original_poly, n = c(high, high),
                               square = TRUE , what = "polygons")

  # only keep grid parts inside original boundary
  geometry <- sf::st_intersection(gridding, original_poly)

  # check if regions is less that grid produces (for odd nums regions)
  squares <- length(geometry) - length(regions)

  # create labels for regions
  labeling <- regions

  # fix names for those with umatched regions
  if (squares > 0) {
    labeling <- c(regions, rep.int(NA, squares))
  }

  # polygon in to a list column which can be used as simple features for plot
  output_poly <- sf::st_sf(tibble::tibble(name = labeling, geometry = geometry))

  # Sets coordinate reference systwem to WGS84
  output_poly <- sf::st_set_crs(output_poly, value = 4326)

  output_poly
}
