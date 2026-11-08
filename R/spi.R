


#' @rdname Drought-indices
#' @export
#'
spi <- function(data, scale, kernel = list(type = "rectangular", shift = 0),
                distribution = "Gamma", fit = "ub-pwm", na.rm = FALSE,
                ref.start = NULL, ref.end = NULL, keep.x = FALSE, params = NULL, verbose = TRUE, ...) {
  # input checks

  # spei() reports the options for the SPEI; reword them for the SPI
  to_spi <- function(x) {
    sub(
      "Standardized Precipitation Evapotranspiration Index (SPEI)",
      "Standardized Precipitation Index (SPI)", x,
      fixed = TRUE
    )
  }
  sol <- withCallingHandlers(
    spei(
      data, scale, kernel, distribution, fit, na.rm,
      ref.start, ref.end, keep.x, params, verbose
    ),
    message = function(m) {
      message(to_spi(conditionMessage(m)), appendLF = FALSE)
      invokeRestart("muffleMessage")
    }
  )
  sol$info <- to_spi(sol$info)
  sol$index <- "SPI"
  sol$call <- match.call(expand.dots = FALSE)

  return(sol)
}
