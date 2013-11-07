# Hargraves reference evapotranspiration (ET_0)
#
hargreaves <-
function(Tmin, Tmax, Ra=NA, lat=NA, Pre=NA, na.rm=FALSE) {

	if (length(Ra)==1 & length(lat)!=ncol(as.matrix(Tmin))){
		stop('Error: lat should be specified for estimating external radiation if Ra is not provided, and should have the same number of elements than Tmin.')
	}
	if (sum(is.na(Tmin),is.na(Tmax))!=0 & na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	if (length(Ra)>1 & sum(is.na(Ra))!=0 & na.rm==FALSE) {
		stop('Error: Data must not contain NAs')
	}
	if (length(Tmin)!=length(Tmax)) {
		stop('Error: Tmin and Tmax must be of the same lenght')
	}
	if (length(Ra)>1 & length(Ra)!=length(Tmin)) {
		stop('Error: Ra must be of the same lenght than Tmin and Tmax')
	}
	if (is.ts(Tmin) & frequency(Tmin)!=12) {
		stop('Error: Data should be a monthly time series (frequency = 12)')
	}
	
	if (!is.ts(Tmin)) {
		Tmin <- ts(as.matrix(Tmin),frequency=12)
	} else {
		Tmin <- ts(as.matrix(Tmin),frequency=frequency(Tmin),start=start(Tmin))
	}
	n <- nrow(Tmin)
	m <- ncol(Tmin)
	c <- cycle(Tmin)
	ET0 <- Tmin*NA

	# mean temperature, ºC
	T <- (Tmin+Tmax)/2
	
	# temperature range, ºC
	Tr <- Tmax-Tmin
	Tr <- ifelse(Tr<0,0,Tr)
	
	# external radiation, MJ m-2 d-1
	if (length(Ra)==1) {
		# estimate Ra, following Allen et al. (1994)
		# number of day in the year
		J <- as.integer(30.5*c-14.6)
		# solar declination, rad (1 rad = 57.2957795 deg)
		delta <- 0.409*sin(0.0172*J-1.39)
		# relative distance Earth-Sun, []
		dr <- 1 + 0.033*cos(0.0172*J)
		# sunset hour angle, rad
		latr <- lat/57.2957795
		sset <- -tan(latr)*tan(delta)
		omegas <- sset*0
		omegas[sset>={-1} & sset<=1] <- acos(sset[sset>={-1} & sset<=1])
		# correction for high latitudes
		omegas[sset<{-1}] <- max(omegas)
		# Ra, MJ m-2 d-1
		Ra <- 37.6*dr*(omegas*sin(latr)*sin(delta)+cos(latr)*cos(delta)*sin(omegas))
		Ra <- ifelse(Ra<0,0,Ra)
	}
	
	# Daily ET0, mm day-1
	if (length(Pre)!=n) {
		# Use original Hargreaves
		ET0 <- 0.0023 * 0.408*Ra * (T+17.8) * Tr^0.5
	} else {
		# Use modified method
		ab <- Tr-0.0123*Pre
		ET0 <- 0.0013 * 0.408*Ra * (T+17.0) * ab^0.76
		ET0[is.nan(ab^0.76)] <- 0
	}
	ET0 <- ifelse(ET0<0,0,ET0)
	
	# Transform ET0 to mm month-1
	mlen <- c(31,28.25,31,30,31,30,31,31,30,31,30,31)
	ET0 <- mlen[c]*ET0
	colnames(ET0) <- rep('ET0_har',m)

	return(ET0)
}