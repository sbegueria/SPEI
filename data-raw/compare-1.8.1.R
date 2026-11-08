# Compare results of SPEI 1.9.0 with SPEI 1.8.1
#
# Used for the section "Changes in results with respect to 1.8.1" in NEWS.md.
# Run once with each version and compare the two output files, e.g.:
#   git worktree add ../SPEI-1.8.1 v1.8.1
#   Rscript data-raw/compare-1.8.1.R ../SPEI-1.8.1 old.rds
#   Rscript data-raw/compare-1.8.1.R . new.rds
# Each element of the saved list is a numeric vector of results (or an error
# message), so old and new can be compared element by element.

# Usage: Rscript scenarios.R <pkg_dir> <out.rds>
a <- commandArgs(TRUE)
suppressMessages(devtools::load_all(a[1], quiet = TRUE, export_all = FALSE))
data(wichita); data(balance); data(cabinda)
q <- function(e) tryCatch(suppressMessages(suppressWarnings(e)), error = function(err) paste("ERROR:", conditionMessage(err)))
fit <- function(x) if (is.list(x) && !is.null(x$fitted)) as.numeric(x$fitted) else x
out <- list()
P <- ts(wichita$PRCP, start = c(1980, 1), frequency = 12)
PET <- thornthwaite(wichita$TMED, 37.6475, verbose = FALSE)
B <- ts(wichita$PRCP - PET, start = c(1980, 1), frequency = 12)
# synthetic dry-season precipitation: 40 years, zero months frequent in summer
set.seed(42)
pz <- c(.05, .05, .1, .15, .25, .4, .6, .6, .35, .15, .05, .05)
mm <- rep(1:12, 40)
D <- ts(ifelse(runif(480) < pz[mm], 0, rgamma(480, 1.2, scale = 45)), start = c(1981, 1), frequency = 12)
for (sc in c(1, 3, 12)) {
  out[[paste0("spi_gamma_wichita_", sc)]] <- q(fit(spi(P, sc, verbose = FALSE)))
  out[[paste0("spi_gamma_dry_", sc)]] <- q(fit(spi(D, sc, verbose = FALSE)))
  out[[paste0("spi_pe3_wichita_", sc)]] <- q(fit(spi(P, sc, distribution = "PearsonIII", verbose = FALSE)))
  out[[paste0("spi_pe3_dry_", sc)]] <- q(fit(spi(D, sc, distribution = "PearsonIII", verbose = FALSE)))
  out[[paste0("spi_loglogistic_wichita_", sc)]] <- q(fit(spi(P, sc, distribution = "log-Logistic", verbose = FALSE)))
  out[[paste0("spei_default_wichita_", sc)]] <- q(fit(spei(B, sc, verbose = FALSE)))
  out[[paste0("spei_default_balance_", sc)]] <- q(fit(spei(as.matrix(balance), sc, verbose = FALSE)))
  out[[paste0("spei_ppwm_wichita_", sc)]] <- q(fit(spei(B, sc, fit = "pp-pwm", verbose = FALSE)))
  out[[paste0("spei_maxlik_wichita_", sc)]] <- q(fit(spei(B, sc, fit = "max-lik", verbose = FALSE)))
}
out$spei_gaussian_kernel_wichita_12 <- q(fit(spei(B, 12, kernel = list(type = "gaussian", shift = 0), verbose = FALSE)))
out$spei_ref_wichita_12 <- q(fit(spei(B, 12, ref.start = c(1980, 1), ref.end = c(1999, 12), verbose = FALSE)))
# user parameters calibrated on a reference period, applied to the full series
cal <- q(spi(window(D, end = c(2000, 12)), 1, verbose = FALSE))
out$spi_gamma_params_dry_1 <- q(fit(spi(D, 1, params = cal$coefficients, verbose = FALSE)))
out$spi_gamma_samefit_dry_1 <- q(fit(spi(D, 1, ref.end = c(2000, 12), verbose = FALSE)))
# PET
out$tho_vector <- q(as.numeric(thornthwaite(wichita$TMED, 37.6475, verbose = FALSE)))
out$tho_ts <- q(as.numeric(thornthwaite(ts(wichita$TMED, start = c(1980, 1), frequency = 12), 37.6475, verbose = FALSE)))
out$tho_matrix <- q(as.numeric(thornthwaite(cbind(wichita$TMED, wichita$TMED), c(37.6475, 37.6475), verbose = FALSE)))
out$har_vector <- q(as.numeric(hargreaves(wichita$TMIN, wichita$TMAX, lat = 37.6475, verbose = FALSE)))
out$har_matrix <- q(as.numeric(hargreaves(cbind(wichita$TMIN, wichita$TMIN), cbind(wichita$TMAX, wichita$TMAX), lat = c(37.6475, 37.6475), verbose = FALSE)))
out$har_pre_vector <- q(as.numeric(hargreaves(wichita$TMIN, wichita$TMAX, lat = 37.6475, Pre = wichita$PRCP, verbose = FALSE)))
out$har_lat70 <- q(as.numeric(hargreaves(wichita$TMIN[1:120], wichita$TMAX[1:120], lat = 70, verbose = FALSE)))
attach(wichita)
out$pen_tsun_ICID <- q(as.numeric(penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE, verbose = FALSE)))
out$pen_tsun_FAO <- q(as.numeric(penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE, method = "FAO", verbose = FALSE)))
out$pen_tsun_ASCE <- q(as.numeric(penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, na.rm = TRUE, method = "ASCE", verbose = FALSE)))
out$pen_CC <- q(as.numeric(penman(TMIN, TMAX, AWND, CC = ACSH, lat = 37.6475, z = 402.6, na.rm = TRUE, verbose = FALSE)))
out$pen_tsun_P <- q(as.numeric(penman(TMIN, TMAX, AWND, tsun = TSUN, lat = 37.6475, z = 402.6, P = rep(96.9, length(TMIN)), na.rm = TRUE, verbose = FALSE)))
out$pen_tsun_lat70 <- q(as.numeric(penman(TMIN, TMAX, AWND, tsun = pmin(TSUN, 12), lat = 70, z = 402.6, na.rm = TRUE, verbose = FALSE)))
detach(wichita)
out$pen_cabinda_Rs_ICID <- q(as.numeric(penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2, Rs = cabinda$Rs, lat = -5.533, z = 20, RH = cabinda$RH, verbose = FALSE)))
out$pen_cabinda_Rs_FAO <- q(as.numeric(penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2, Rs = cabinda$Rs, lat = -5.533, z = 20, RH = cabinda$RH, method = "FAO", verbose = FALSE)))
out$pen_cabinda_Rs_ASCE <- q(as.numeric(penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2, Rs = cabinda$Rs, lat = -5.533, z = 20, RH = cabinda$RH, method = "ASCE", verbose = FALSE)))
out$pen_cabinda_tsun_ASCE <- q(as.numeric(penman(cabinda$Tmin, cabinda$Tmax, cabinda$U2, tsun = cabinda$tsun, lat = -5.533, z = 20, RH = cabinda$RH, method = "ASCE", verbose = FALSE)))
saveRDS(out, a[2])
