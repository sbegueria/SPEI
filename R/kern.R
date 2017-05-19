# A function for generating kernels needed for computing the SPI and SPEI at different time scales.
#
kern <- function(scale, type='rectangular', shift=0) {
	if(type!='rectangular' & type!='triangular' & type!='circular' & type!='gaussian') {
		stop('type must be one of: rectangular, triangular, circular, gaussian')
	}
	#
	s <- scale
	h <- shift
	if(h>=s) {
		stop('Parameter "shift" must be lower than "scale"')
	}
	if(h<0) {
		stop('Parameter "shift" cannot have a negative value')
	}
	if(s<=0) {
		stop('Parameter "scale" must be higher than zero')
	}
	#
	if(type=='rectangular' | s==1) k <- rep(1,s)
	if(type=='triangular') k <- s:1
	if(type=='circular') k <- (s^2+(1-(1:s)^2))
	if(type=='gaussian') k <- (1/0.4)*1/sqrt(2*pi*1^2)*
		exp(-(seq(0,-3,-3/(s-1))-0)^2/2*1^2)
	if(h) k <- c(k[(h+1):2],k[1:(s-h)])
	#return(k/sum(k))
	return(k/mean(k))
}
