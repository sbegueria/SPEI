# Produces plots of all the kernel functions available.
kern.plot  <- function(scale=12, shift=0) {
	par(mfrow=c(2,2))
	plot(c(kern(scale,'rectangular',shift),0),type='s',
		xlab='',ylab='', main='rectangular')
	plot(c(kern(scale,'triangular',shift),0),type='s',
		xlab='',ylab='', main='triangular')
	plot(c(kern(scale,'circular',shift),0),type='s',
		xlab='',ylab='', main='circular')
	plot(c(kern(scale,'gaussian',shift),0),type='s',
		xlab='',ylab='', main='gaussian')
}