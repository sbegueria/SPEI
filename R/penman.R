# FAO-56 Penman-Monteith reference evapotranspiration (ET_0)
#
#TO DO:
# 1: poner una serie de warnings para informar al usuario en pantalla de las opciones que se
#    están utilizando en el cálculo.
# 2: poner un parámetro 'verbose=TRUE' que controle si se desea que la función	
#    devuelva estos warnings o no.
# 3: versión para datos diarios (existe una versión ya, colgada en mi blog).
penman <-
function(Tmin, Tmax, U2, Ra=NA, lat=NA, Rs=NA, tsun=NA, CC=NA, ed=NA, Tdew=NA, RH=NA, P=NA, P0=NA, z=NA, crop='short', na.rm=FALSE) {

	if (sum(is.na(Tmin),is.na(Tmax),is.na(U2))>0 & na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	if (((length(Ra)>1 & sum(is.na(Ra))>0) |
		(length(Rs)>1 & sum(is.na(Rs))>0) |
		(length(tsun)>1 & sum(is.na(tsun))>0) |
		(length(CC)>1 & sum(is.na(CC))>0) |
		(length(ed)>1 & sum(is.na(ed))>0) |
		(length(Tdew)>1 & sum(is.na(Tdew))>0) |
		(length(RH)>1 & sum(is.na(RH))>0) |
		(length(P)>1 & sum(is.na(P))>0) |
		(length(P0)>1 & sum(is.na(P0))>0)) &
		na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	#if (length(Ra)==1 & length(lat)!=ncol(as.matrix(Ra))) {
	if (is.na(Ra[1]) & is.na(lat[1])) {
		stop('Error: One of Ra or lat must be provided')
	}
if (!is.na(Ra[1])) {
	warning('Using user-provided (Ra)')
}

	if (length(Rs)!=length(Tmin) & length(tsun)!=length(Tmin) & length(CC)!=length(Tmin)) {
		stop('Error: One of Rs, tsun or CC must be provided')
	}	
	if (length(Tmin)!=length(Tmax) | length(Tmin)!=length(U2)) {
		stop('Error: Data must be of the same lenght')
	}
	if (length(P)!=length(Tmax) & is.na(z)) {
		stop('Error: Elevation above sea level (z) must be specified if P is not provided.')
	}
if (is.na(z)) {
	warning('Specifying the elevation above sea level (z) is highly recommended in order to compute the clear-sky solar radiation.')
}
	
	ET0 <- Tmin*NA

	if (!is.ts(Tmin)) {
		Tmin <- ts(as.matrix(Tmin),frequency=12)
	} else {
		Tmin <- ts(as.matrix(Tmin),frequency=frequency(Tmin),start=start(Tmin))
	}
	n <- nrow(Tmin)
	m <- ncol(Tmin)
	c <- cycle(Tmin)

	# Mean temperature
	T <- (Tmin+Tmax)/2

	# 1. Latent heat of vaporization, lambda (eq. 1.1)
	lambda <- 2.501 - 2.361e-3*T
	
	# 4. P: atmospheric pressure, kPa
	if (nrow(as.matrix(P))!=n) {
		if (length(P0)==n) {
			# estimate from sea level pressure (eq. 1.6)
			P <- P0 %*% as.matrix(((293-0.0065*z)/293)^5.26)
		} else {
			# assume a constant pressure
			P0  <- matrix(101.3,n,m)
			P <- P0 %*% as.matrix(((293-0.0065*z)/293)^5.26)
		}
	}

	# 3. Psychrometric constant, gamma (eq. 1.4)
	gamma <- 1.63e-3*P/lambda

	# 6. Saturation vapour pressure, ea
	# saturation vapour pressure at tmx (eq. 1.10, p. 66)
	etmx <- 0.611*exp((17.27*Tmax)/(Tmax+237.3))
	# saturation vapour pressure at tmn (eq. 1.10, p. 66)
	etmn <- 0.611*exp((17.27*Tmin)/(Tmin+237.3))
	# mean saturation vapour pressure (eq. 1.11, p. 67)
	ea <- (etmx+etmn)/2

	# 2. Slope of the saturation vapour pressure function, Delta (eq. 1.3)
	Delta <- 4099*ea/(T+237.3)^2
	#Delta <- 2504*exp((12.27*T)/(T+237.3))/(T+237.3)^2

	# 7. Actual vapour pressure, ed
	if(length(ed)!=n) {
		if (length(Tdew)==n) {
			# (eq. 1.12, p. 67)
			ed <- 0.611*exp((17.27*Tdew)/(Tdew+237.3))
		} else if(length(RH)==n) {
			# (eq. 1.16, p. 68)
			ed <- RH / ((50/etmn)+(50/etmx))
		} else {
			# (eq. 1.19, p. 69)
			ed <- etmn
		}
	}

	# Sunset hour angle (needed if no radiation data is available)
	if (nrow(as.matrix(Ra))!=n | {nrow(as.matrix(Rs))!=n & nrow(as.matrix(tsun))==n}) {
		# Note: For the winter months and latitudes higher than 55º the following
		# equations have limited validity (Allen et al., 1994).
		# J: number of day in the year (eq. 1.27)
		#J <- as.integer(30.5*c-14.6)
		# more accurate option:
		mlen <- c(31,28,31,30,31,30,31,31,30,31,30,31)
		msum <- c(0,31,59,90,120,151,181,212,243,273,302,334)+15
		J <- msum[c]
		# delta: solar declination, rad (1 rad = 57.2957795 deg) (eq. 1.25)
		delta <- 0.409*sin(0.0172*J-1.39)
		# dr: relative distance Earth-Sun, [] (eq. 1.24)
		dr <- 1 + 0.033*cos(0.0172*J)
		# omegas: sunset hour angle, rad (eq. 1.23)
		latr <- lat/57.2957795
		sset <- t(-as.matrix(tan(latr))%*%tan(delta))
		omegas <- sset*0
		omegas[sset>={-1} & sset<=1] <- acos(sset[sset>={-1} & sset<=1])
		# correction for high latitudes
		omegas[sset<{-1}] <- max(omegas)
	}
	
	# 9. Extraterrestrial radiation, Ra (MJ m-2 d-1)
	if (nrow(as.matrix(Ra))!=n) {
		# Estimate Ra (eq. 1.22)
		Ra <- 37.6*dr*(omegas*sin(latr)*sin(delta)+cos(latr)*cos(delta)*sin(omegas))
		Ra <- ifelse(Ra<0,0,Ra)
	}

	# 11. Net radiation, Rn (MJ m-2 d-1)
	# Net radiation is the sum of net short wave radiation Rns and net long wave
	# (incoming) radiation (Rnl).
	# Rs: daily incoming solar radiation (MJ m-2 d-1)
	if (nrow(as.matrix(Rs))!=n) {
		# nN: relative sunshine fraction []
		if (nrow(as.matrix(tsun))==n) {
			# Based on sunshine hours 
			# 10. Potential daylight hours (day length, h), N (eq. 1.34)
			N <- 7.64*omegas
			nN <- tsun/N
		} else if (nrow(as.matrix(CC))==n) {
			# Based on cloud cover
			nN <- (100-CC)/100
		}
		# (eq. 1.37)
		as <- 0.25; bs <- 0.5
		Rs <- (as+bs*(nN))*Ra
	}
	# Rso: clear-sky solar radiation (eq. 1.40)
	# Note: mostly valid for z<6000 m and low air turbidity
	#if (ncol(as.matrix(z))==ncol(as.matrix(Tmin))) {
	if (!is.na(z)) {
		Rso <- matrix(0.75+2e-5*z,n,m,byrow=TRUE) * Ra
	} else {
		Rso <- (0.75+2e-5*840) * Ra
	}
	# Empirical constants
	ac <- 1.35; bc <- -0.35; a1 <- 0.34; b1 <- -0.14
	# Reference crop albedo
	alb <- 0.23
	# Rn, MJ m-2 d-1 (eq. 1.53)
	Rn <- (1-alb)*Rs - (ac*Rs/Rso+bc) * (a1+b1*sqrt(ed)) * 4.9e-9 *
		((273.15+Tmax)^4+(273.15+Tmin)^4)/2	
	Rn[Rs==0] <- 0

	# Soil heat flux density, G
	# Using forward / backward differences for the first and last observations,
	# and central differences for the remaining ones.
	G <- rep(NA,length(T))
	G[1] <- 0.14*(T[2]-T[1])
	G[2:{length(T)-1}] <- 0.07*(T[3:length(T)]-T[1:{length(T)-2}])
	G[length(T)] <- 0.14*(T[length(T)]-T[length(T)-1])

	# Wind speed at 2m, U2 (eq. 1.62)
	#U2 <- U2 * 4.85203/log((zz-0.08)/0.015)

	# Daily ET0 (eq. 2.18)
	if (crop=='short') {
		c1 <- 900; c2 <- 0.34 # short reference crop (e.g. clipped grass, 0.12 m)
	} else {
		c1 <- 1600; c2 <- 0.38 # tall reference crop (e.g. alfalfa, 0.5 m)
	}
	ET0 <- (0.408*Delta*(Rn-G) + gamma*(c1/(T+273))*U2*(ea-ed)) /
		(Delta + gamma*(1+c2*U2))

	# Transform ET0 to mm month-1
	ET0 <- ifelse(ET0<0,0,ET0)*mlen[c]
	colnames(ET0) <- rep('ET0_pen',m)
	
	return(ET0)
}