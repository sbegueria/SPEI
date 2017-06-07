# Maximum-likelihood fitting to a Generalized Logistic distribution function.
parglo.maxlik <- function(x,ini) {
	# generalized logistic log-likelihood function
	glo.loglik <- function(theta,x){
		if (!are.parglo.valid(list(type='glo',para=theta),nowarn=TRUE) | theta[[3]]==0) 
			return(1000000)
		gamma <- theta[1]
		alpha <- theta[2]
		kappa <- theta[3]
		y <- 1-kappa*((x[!is.na(x)]-gamma)/alpha)
		if (min(y)<=0) return(1000000)
		y <- -(1/kappa)*log(y)
		n <- length(x)
		logl <- -n*log(alpha) - (1-kappa)*sum(y) - 2*sum(log(1+exp(-y)))
		return(-logl) # optim() does minimization by default
	}
	# optimize
	o <- optim(par=ini, fn=glo.loglik, x=x)
	#o <- optim(par=ini, fn=glo.loglik, x=x,
	#	lower=c(-Inf,0.00001,-0.5), upper=c(Inf,Inf,0.5))
	return(list(type='glo',
		para=o$par,
		source='parglo.loglik',value=o$value,count=o$count,
		conv=o$convergence,msg=o$message))
}
