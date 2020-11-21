#' Converts a list-column signature into a number of columns
#'
#' @param x - an lsp object
#'
#' @export
lsp_restructure = function(x){
  x_attr = attributes(x)

  unnested_signature = do.call(rbind, lapply(x$signature, tibble::as_tibble))
  x["signature"] = NULL

  x = tibble::as_tibble(cbind(x, unnested_signature))
  x_attr$names = names(x)
  attributes(x) = x_attr

  x
}
