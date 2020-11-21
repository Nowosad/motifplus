#' Extracts a local landscape from categorical raster data based on its id and provided window.
#'
#' @param id - an id of a local landscpe
#' @param windows_sf - the output of [motif::lsp_add_sf]
#' @param x - the `stars` object to extract from
#'
#' @export
lsp_extract2 = function(id, windows_sf, x){
  windows_sf = windows_sf[windows_sf$id == id, ]
  x = stars::st_as_stars(x[sf::st_bbox(windows_sf)])
  names(x) = "data"
  x$data = as.factor(x$data)
  x
}
