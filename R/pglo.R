#' @title Probability of generalized logistic distribution
#' 
#' 
#' @description See parglo.maxlik
#' 
#' 
#' @details See parglo.maxlik
#' 
#' 
#' @rdname Generalized-Logistic
#' 
#'
#' @importFrom lmomco are.parglo.valid
#'
#'
#' @export
#'
pglo <- function (x, para) {
	if (!are.parglo.valid(para)) 
		return()
	SMALL <- 1e-15
	XI <- para$para[1]
	A <- para$para[2]
	K <- para$para[3]
	f <- vector(mode = "numeric")
	for (i in seq(1, length(x))) {
		if (is.na(x[i])) {
			f[i] <- NA
			next
		}
		Y <- (x[i] - XI)/A
		if (K == 0) {
			f[i] <- 1/(1 + exp(-Y))
			next
		}
		ARG <- 1 - K * Y
		if (ARG > SMALL) {
			Y <- -log(ARG)/K
			f[i] <- 1/(1 + exp(-Y))
			next
		}
		if (K < 0) {
			f[i] <- 0
			next
		}
		if (K > 0) {
			f[i] <- 1
			next
		}
		warning("Should not be here in execution")
	} # next i
	return(f)
}