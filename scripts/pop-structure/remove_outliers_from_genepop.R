######## Remove Outlier Loci from Genepop ########
#
# Remove outlier loci from genepop file in order to
#    calculate Fst without outliers
#
# MF 10/3/2021
#
##################################################


# Load Packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(adegenet)
# remotes::install_github("romunov/zvau")
library(zvau)



# Read in Data ------------------------------------------------------------
# genepop file
mygen <-read.genepop(here::here('data','genepop',"korea-pcod-final-filtered.gen"))
pop(mygen) # populations in genepop should reflect collections

# genepop file without migrants
mygen2 <-read.genepop(here::here('data','genepop',"korea-pcod-final-filtered-migrants-removed.gen"))
pop(mygen2) # populations in genepop should reflect collections


# list of outlier loci (across all programs)
outliers <- read.csv(here::here('results','outliers','all_outliers_list.csv'))



# Remove Outliers & Save --------------------------------------------------

mygen_filtered=mygen[loc=-outliers$locus_id]
# check to make sure loci were removed
length(mygen@all.names)
length(mygen_filtered@all.names)


mygen2_filtered=mygen2[loc=-outliers$locus_id]
# check to make sure loci were removed
length(mygen2@all.names)
length(mygen2_filtered@all.names)

# save 
writeGenPop(gi=mygen_filtered, file.name=here::here('results','outliers','korea-pcod-final-filtered-outliers-removed.gen'), 
            comment='Korean Pacific cod filtered final genepop, stacks batch 8; without outlier loci; MF 10/3/2021')
## !! this function writes out the loci on one line, separated by ",". I manually edited the output in notepad++ and replaced ", " with "\r\n"

writeGenPop(gi=mygen2_filtered, file.name=here::here('results','outliers','korea-pcod-final-filtered-migrants-removed-outliers-removed.gen'), 
            comment='Korean Pacific cod filtered final genepop, stacks batch 8; without migrant individuals; without outlier loci; MF 10/3/2021')
## !! this function writes out the loci on one line, separated by ",". I manually edited the output in notepad++ and replaced ", " with "\r\n"


