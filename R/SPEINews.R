#' @title SPEINews
#'
#' @description Show the NEWS file of the \pkg{SPEI} package.
#'
#' @details (See description)
#'
#' @export
#'
SPEINews <- function() {
  file <- file.path(system.file(package = "SPEI"), "NEWS.md")
  file.show(file)
}
