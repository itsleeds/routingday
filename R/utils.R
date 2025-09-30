#' @export
path_to_sf <- function(dp, verts, simplify = FALSE) {
  # Check for emplyr paths
  if (length(dp[[1]]) > 0) {
    path <- verts[match(dp[[1]], verts$id), ]
    path <- matrix(c(path$x, path$y), ncol = 2)
    path <- sf::st_linestring(path)

    if (simplify) {
      path <- sf::st_as_sfc(list(path), crs = 4326)
      path <- sf::st_transform(path, 27700)
      path <- sf::st_simplify(path, 5, preserveTopology = TRUE)
      path <- sf::st_transform(path, 4326)
      path <- path[[1]]
    }
    return(path)
  } else {
    return(NA)
  }
}

#' @export
paths_to_sf <- function(paths, verts) {
  dp.list <- purrr::map(paths, path_to_sf, verts = verts, .progress = TRUE)
  dp.list <- unname(dp.list)
  dp.list
}