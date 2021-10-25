######## Run Genepop without Outlier Loci ########
#
# Re-run Fst calculations in genepop without outlier loci
# Note that Fst calculations with all loci were completed 
#     using the genepop executable, not in R.
#
# MF 10/22/2021
#
##################################################


# Load Packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(genepop)
devtools::install_github('ericarcher/strataG', build_vignettes = TRUE)
library(strataG)



# Run Genepop -------------------------------------------------------------

Fst(inputFile=here::here('results','outliers','korea-pcod-final-filtered-outliers-removed-genepop.txt'),
    pairs=TRUE,
    outputFile=here::here('results','outliers','genepop-fst-outliers-removed'))


Fst(inputFile=here::here('results','outliers','korea-pcod-final-filtered-migrants-removed-outliers-removed-genepop.txt'),
    pairs=TRUE,
    outputFile=here::here('results','outliers','genepop-fst-migrants-removed-outliers-removed'))


# Test for Differentiation ------------------------------------------------

test_diff(
  inputFile=here::here('results','outliers','korea-pcod-final-filtered-outliers-removed-genepop.txt'),
  genic = FALSE,
  pairs = TRUE,
  outputFile = "",
  settingsFile = "",
  dememorization = 10000,
  batches = 100,
  iterations = 5000,
  verbose = interactive()
)




