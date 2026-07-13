## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Notes

* checking CRAN incoming feasibility ... NOTE

      Maintainer: 'Ivan Iakimov <ivan.iakimov@ih21.org>'

      New submission

      Possibly misspelled words in DESCRIPTION:
        Borda (11:64)
        Janecek (10:6)

  These are not misspellings. "Borda" is the surname of Jean-Charles de
  Borda, after whom the Borda count is named. "Janecek" is the surname of
  Karel Janecek, the author of the D21 method referenced in the Description.

## Test environments

* Local: Windows 11, R 4.6.1 (2026-06-24 ucrt), R CMD check --as-cran
  — 0 errors | 0 warnings | 0 notes
* win-builder (R-devel, 2026-07-09 r90225 ucrt)
  — 0 errors | 0 warnings | 1 note (New submission; proper names as above).
* R-hub v2: linux, macos, macos-arm64, windows (all R-devel), plus
  clang-ASAN/UBSAN, gcc-ASAN and valgrind — OK.

  The R-hub 'rchk' job fails, but does not apply here: electsys21 is a
  pure R package with no compiled code.
