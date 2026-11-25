## Submission

<!-- DRAFT for 1.10.0, prepared 2026-11-25. Update the check results and the
reverse dependency results before submitting. -->

This is an update of SPEI from 1.9.0 to 1.10.0. It adds new functions
(`scale_scan()`, `scale_extract()`, `index_events()`), daily and
temperature-only reference evapotranspiration and a new method in
`penman()`, time information for gridded (3-d array) input, a new example
dataset (`tarazona`, 25 KB), and five vignettes. The results of existing
calls are unchanged, except for wind speeds below 0.5 m/s in `penman()`, as
recommended by the revised FAO-56; NEWS.md describes it.

## R CMD check results

0 errors | 0 warnings | 1 note

* The only NOTE is "unable to verify current time", from the local network.

* Local: macOS (aarch64), R 4.4, `R CMD check --as-cran`.
* GitHub Actions: macOS, Windows and Ubuntu (R release), Ubuntu (R devel and
  oldrel-1).

## URLs

* https://spei.csic.es (the SPEI dataset maintained by the authors) uses a
  certificate from the GEANT/HARICA chain. It verifies correctly with
  OpenSSL and in browsers, but some libcurl CA bundles do not yet include the
  HARICA TLS RSA Root CA 2021 and report an SSL certificate problem.

## Reverse dependencies

We checked the 6 reverse dependencies (ClimInd, CSIndicators, dendroTools,
lmomPi, NPRED, WASP) with R CMD check against SPEI 1.10.0. <!-- results -->
