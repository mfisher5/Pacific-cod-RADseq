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

sample_data <- read_delim(here::here('data',"sampling_data_newJB.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)

sample_list <- read_delim(here::here('data',"sample_list_wjb.txt"), "\t", escape_double = FALSE, trim_ws = TRUE)


mydata <- right_join(sample_data, sample_list, by="Sample.ID")
dim(mydata)
dim(sample_list)



# 50% Length-at-Maturity --------------------------------------------------

Site <- c("Pohang", "Jukbyeon", "Jukbyeon","Namhae", "Geoje", "Block 161", "Boryeong", "Jinhaeman Bay")
LAM_Female = c(44.02, 58.27, 44.02, 44.02,44.02,44.02,44.02,44.02)
LAM_Male <- c(32.66,58.82, 32.66,32.66,32.66,32.66,32.66,32.66)
linetype <- c(1,1,2,1,1,1,1,1)   # note Jukbyeon has two LAMs; one for sampling site, one for migrant source

mat_data <- cbind(Site, LAM_Female, LAM_Male, linetype)
mat_data <- as.data.frame(mat_data)
mat_data$LAM_Female <- as.numeric(LAM_Female)
mat_data$LAM_Male <- as.numeric(LAM_Male)
head(mat_data)



# Prepare Data for Graph --------------------------------------------------

## filter for NAs, make numeric
data = subset(mydata, !is.na(BW.g))
data = subset(data, !is.na(TL.cm))
data$TL.cm <- as.numeric(data$TL.cm)
data$TL.cm <- as.numeric(data$TL.cm)
head(data)


# order sites
neworder <- c("Block 161", "Boryeong", "Namhae", "Geoje", "Jinhaeman Bay", "Pohang","Jukbyeon")
plotdata <- arrange(transform(data,
                              Site=factor(Site,levels=neworder)),Site)
plotdata <- plotdata %>% dplyr::select(-GW.g, -GSI)








# Figure 2. Total Length v. Body Weight -----------------------------------

#################### plotting function to squish axis ####################
squish_trans <- function(from, to, factor) {
  
  trans <- function(x) {
    
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    if (any(is.na(x))) return(x)
    
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


## max for x axis
xmax <-  max(as.numeric(data$TL.cm), na.rm = TRUE) + 1


## create colors for male, female
## --- color
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
ggcols <- gg_color_hue(2)
## --- grayscale
ggcols <- c("grey0","grey50")
mycols <- c("#5ab4ac","#f1a340")

## plot
fig2 <- ggplot(plotdata, aes(x=TL.cm, y=BW.g)) +
  geom_point(aes(col = Sex, pch = Disperser), size = 3) +
  geom_point(data=filter(plotdata, Disperser == "Yes"), aes(x=TL.cm, y=BW.g), col = "grey40", pch = 2, size = 3) +
  facet_wrap(~Site, nrow = 4, ncol = 2) +
  xlab("Total Length (cm)") +
  ylab("Body Weight (g)") +
  scale_color_manual(values=mycols, na.value="grey50") +
  scale_shape_manual(values=c(1,17)) +
  scale_x_continuous(trans = squish_trans(from=80,to=110,factor=4)) +
  theme(axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"),
        strip.text = element_text(size = 12)) +
  geom_vline(data=mat_data, aes(xintercept = LAM_Female, lty = linetype), col = mycols[1]) +
  geom_vline(data=mat_data, aes(xintercept = LAM_Male, lty = linetype), col = mycols[2]) +
  guides(linetype=FALSE) + theme_bw() + theme(strip.background = element_rect(fill="white"),
                                              axis.text = element_text(size=11),
                                              legend.text = element_text(size=11),
                                              legend.title=element_text(size=12),
                                              axis.title=element_text(size=12),
                                              strip.text=element_text(size=12))

## plot
png(here::here('results','Figure2.png'), res=150, height=900,width=950)
fig2
dev.off()



ggsave(here::here('results','Figure2.pdf'), plot=fig2, device="pdf", dpi=600, height=3400,width=4500, units="px")







# Figure S4. GSI from Gonad Weight ----------------------------------------

gsi_data <- data %>%
  filter(!is.na(GSI)) %>%
  mutate(maturity = ifelse(Sex == "F", ifelse(TL.cm > 44.02, "Mature", "Immature"), ifelse(TL.cm > 32.66, "Mature", "Immature"))) %>%
  mutate(Site=ifelse(Site=="Geoje", "Geoje 2014", Site))


gsi_migrant_data <- filter(gsi_data, Migrant == "Yes")


## create colors for male, female
## --- color
mycols <- c("#5ab4ac","#f1a340")
## --- grayscale
# mycols <- c("grey0","grey50")


png(here::here('results',"FigureS4.png"))
ggplot(gsi_data, aes(x = Site, y = GSI)) +
  geom_boxplot() +
  geom_point(data = gsi_migrant_data, aes(x = Site, y = GSI, col=Sex), pch = 17, size = 5) +
  facet_wrap(~maturity) +
  xlab("Collection") +
  ylab("Gonadosomatic Index (%)") +
  scale_y_continuous(breaks = seq(0,40,5), labels = c(0,5,10,15,20,25,30,35,40))+
  scale_color_manual(values=mycols, na.value="grey50") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), 
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())
dev.off()












