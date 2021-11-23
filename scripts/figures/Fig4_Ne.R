########### Fisher et al. Fig4 ##############
#
# Compare Pacific cod Ne estimates between Korean and N. American populations
#
# MF 5/9/2018
#
#############################################


# Load Packages -----------------------------------------------------------
library(readr)
library(tidyverse)
library(scales)
library(here)



# Read in Data ------------------------------------------------------------
mydata <- read_delim(here::here('results','pop-size',"Ne_KoreaMAF05_Alaska_corrected_bysite.txt"), "\t")



# Prepare Data for Graph --------------------------------------------------


# filter out individual sites when pooled across coast
plotdat <- mydata %>%
  filter(!(Site %in% c("Pohang", "Namhae"))) %>%
  filter(!(Site_ID %in% c("Geoje 2014","Jinhaeman Dec","Jinhaeman Feb")))


# assign coast designations
plotdat <- mutate(plotdat, coast= ifelse(Population=="North America", "(North America)",
                                         ifelse(Site %in% c('Block 161','Boryeong'), 'West',
                                                ifelse(Site == 'Jukbyeon','East','South'))))


# set infinite upper confidence level to max on graph (just for graphing)
plotdat$Ne1_lower[9] <- 43000

# order / arrange
plotdat$coast <- factor(plotdat$coast, levels=c("West","South","East","(North America)"))
site_order <- c("Boryeong", "Block 161", 
                "Jinhaeman Bay", "Geoje 2013", "South 2014",
                "Jukbyeon",
                "Salish Sea",
                "Wash. Coast", "Hecate Strait",
                "P.W. Sound", "Kodiak",
                "Unimak Pass","Adak")




# Graph Ne ----------------------------------------------------------------

#################### plotting function to squish axis ####################
squish_trans <- function(from, to, factor) {
  
  trans <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squished", trans, inv))
}
###########################################################


# save as png
png(here::here('results','Figure4.png'), res=100, height=550,width=600)
ggplot(plotdat, aes(x=factor(Site_ID, levels=site_order), y=Ne1)) +
  geom_point(aes(pch = Population, col=coast), size = 3)+
  geom_errorbar(aes(ymin=Ne1_lower, ymax = Ne1_upper), size = 0.5, width=0.5)+
  geom_segment(aes(x=9,xend=9,y=10007,yend=39188)) +
  geom_segment(aes(x=9,xend=9,y=39188,yend=41000)) +
  ylab(expression("1000 x N"["e"])) +
  xlab("") +
  scale_y_continuous(limits=c(0,41000), breaks = seq(0,40000, 10000), labels = c("0", "10", "20", "30", "40")) +
  scale_color_manual(values=c("firebrick4","deepskyblue","forestgreen",'black'), name="Coast - Korea") +
  scale_shape_manual(name="Region", values=c(16,17)) +
  geom_vline(xintercept=6.5) +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.5, size = 13),
        axis.title.y=element_text(size=13),
        axis.text.y=element_text(size = 12),
        legend.key = element_blank(),
        legend.text = element_text(size=12, family="sans"),
        legend.title= element_text(size=12, family="sans"),
        panel.background = element_rect(fill="white",
                                        linetype=1,
                                        color="black"),
        panel.grid.major.y = element_line(colour="grey47", linetype = 2),
        panel.grid.minor.y = element_line(colour="grey87", linetype = 2),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
dev.off()



# save as high-res PDF
fig4 <- ggplot(plotdat, aes(x=factor(Site_ID, levels=site_order), y=Ne1)) +
  geom_point(aes(pch = Population, col=coast), size = 2)+
  geom_errorbar(aes(ymin=Ne1_lower, ymax = Ne1_upper), size = 0.3, width=0.5)+
  geom_segment(aes(x=9,xend=9,y=10007,yend=39188), size=0.3) +
  geom_segment(aes(x=9,xend=9,y=39188,yend=41000), size=0.3) +
  ylab(expression("1000 x N"["e"])) +
  xlab("") +
  scale_y_continuous(limits=c(0,41000), breaks = seq(0,40000, 10000), labels = c("0", "10", "20", "30", "40")) +
  scale_color_manual(values=c("firebrick4","deepskyblue","forestgreen",'black'), name="Coast - Korea") +
  scale_shape_manual(name="Region", values=c(16,17)) +
  geom_vline(xintercept=6.5) +
  theme(axis.text.x=element_text(angle=90, hjust = 1, vjust=0.5, size = 10, color="black"),
        axis.title.y=element_text(size=10),
        axis.text.y=element_text(size = 8),
        legend.key = element_blank(),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size=9, family="sans"),
        legend.title= element_text(size=9, family="sans"),
        panel.background = element_rect(fill="white",
                                        linetype=1,
                                        color="black"),
        panel.grid.major.y = element_line(colour="grey47", linetype = 2, size = 0.25),
        panel.grid.minor.y = element_line(colour="grey87", linetype = 2, size = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())
ggsave(here::here('results','Figure4.pdf'), plot=fig4, device="pdf", dpi=600, height=2000,width=2400, units="px")


