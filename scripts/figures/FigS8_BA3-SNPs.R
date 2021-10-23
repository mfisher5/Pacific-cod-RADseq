########### Fisher et al. Fig S8 ##############
#
# Graph of BayesAss3 results on migration
#
# MF 10/22/2020
#
#############################################


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)


# Read in data ------------------------------------------------------------
dat <- read.delim(here::here('results','bayesass_pairwise_migration_rates.txt'))

## format
dat <- dat %>%
  mutate(populations=paste(from,"-",to)) %>%
  mutate(populations=recode(populations, `south - west`="west - south",
                            `east - south`="south - east",
                            `east - west`="west - east"))


# Plot --------------------------------------------------------------------
png(here::here('results','FigureS8.png'), res=100, height=500,width=600)
ggplot(dat, aes(x=populations,y=mean*100, col=direction, pch=direction)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=(X95..lower)*100, ymax=(X95..upper)*100), size=1, width=0.10) +
  xlab("Population Comparison") + ylab("Migration Rate (%)") +
  scale_color_manual(values=c("black","gray47")) +
  scale_shape_manual(values=c(17,16)) +
  ylim(c(0,5)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13),
        axis.line.y.left=element_blank())
dev.off()



