## Submission

This is an update of SPEI from 1.8.1 to 1.9.0. It fixes bugs found in a full
audit of the package. Some of these fixes deliberately change results for
specific options, as 1.8.1 gave wrong or empty results for them; NEWS.md has a
section listing each change in results and its size.

## R CMD check results

0 errors | 0 warnings | 0 notes

* Local: macOS (aarch64), R 4.4, `R CMD check --as-cran`.
* GitHub Actions: macOS, Windows and Ubuntu (R release), Ubuntu (R devel and
  oldrel-1).

## Reverse dependencies

We checked the 5 reverse dependencies (ClimInd, CSIndicators, lmomPi, NPRED,
WASP) with R CMD check against SPEI 1.9.0. All gave Status: OK.
