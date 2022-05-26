spi <- function(x, y,...) UseMethod('spi')



#' @rdname Drought-indices
#' @export
#' 
spi <- function(data, scale, kernel=list(type='rectangular',shift=0),
	distribution='Gamma', fit='ub-pwm', na.rm=FALSE,
	ref.start=NULL, ref.end=NULL, x=FALSE, params=NULL, verbose=TRUE, ...){
	sol <- spei(data, scale, kernel, distribution, fit, na.rm,
		ref.start, ref.end, x, params, verbose)
	sol$call <- match.call(expand.dots=FALSE)

	return(sol)
	
}
