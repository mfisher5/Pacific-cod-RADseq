#################### P.Cod Korea Catch ####################
#
# M. Fisher - Data from Dr. Sukyung Kang
#
############################################################

library(tidyverse)
library(here)


dat <- read.csv(here::here('data','pcod_catch.csv'))
head(dat)

png("FigureS3.png", height=400, width=700)
ggplot(dat, aes(x=year,y=catch.MT/1000)) +
  geom_point(size=1) +
  geom_line() +
  scale_x_continuous(breaks=seq(1925,2015,by=10),
                     expand = c(0, 3)) +
  scale_y_continuous(expand = c(0, 0.5)) +
  xlab("Year") +
  ylab("Catch (1,000 MT)") +
  theme_bw() + theme(panel.grid.major.x=element_blank(),
                     panel.grid.minor.x=element_blank(),
                     axis.text=element_text(size=14),
                     axis.title=element_text(size=16))
dev.off()
