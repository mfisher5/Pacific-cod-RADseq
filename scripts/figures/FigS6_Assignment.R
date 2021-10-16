########### Fisher et al. FigS6 ##############
#
# Assignment success by sampling site
#
# MF 6/20/2020
#
#############################################


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)




# Read Data & Prepare for Graphing ----------------------------------------
regdata <- read.table(here::here('results',"assigner_ranked.no.imputation.results.summary.stats.tsv"), header=TRUE, sep="\t",stringsAsFactors=F)
regdata$MARKER_NUMBER <- as.character(regdata$MARKER_NUMBER)

# rename populations for plotting
regdata$CURRENT <- gsub("YellowSea", "Block 161", regdata$CURRENT)
regdata$CURRENT <- gsub("Geoje_2014-15", "Geoje 2014", regdata$CURRENT)
regdata$CURRENT <- gsub("Geoje_2013-14", "Geoje 2013", regdata$CURRENT)
regdata$CURRENT <- gsub("JinhaeBay_Dec", "Jinhaeman Dec.", regdata$CURRENT)
regdata$CURRENT <- gsub("JinhaeBay_Feb", "Jinhaeman Feb.", regdata$CURRENT)
regdata$CURRENT <- gsub("OVERALL", "Overall", regdata$CURRENT)
View(regdata)



# rearrange order 
neworder <- c("Block 161", "Boryeong", "Jukbyeon","Overall","Geoje 2013", "Geoje 2014", "Namhae", "Jinhaeman Dec.", "Jinhaeman Feb.", "Pohang")
regdata <- arrange(transform(regdata,
                             CURRENT=factor(CURRENT,levels=neworder)),CURRENT)
# assign coasts
regdata <- regdata %>%
  mutate(Region=ifelse(CURRENT %in% c("Block 161", "Boryeong"),"West",
                       ifelse(CURRENT == "Jukbyeon", "East","South")))

# remove overall assignment success
regdata_pops <- regdata %>% filter(CURRENT != "Overall")



# Graph Data --------------------------------------------------------------

png(here::here('results','FigureS6.png'), res=200, height=1400, width=1200)
ggplot(regdata_pops, aes(x=MARKER_NUMBER,y=MEAN/100))+
  geom_errorbar(aes(ymin=(MEAN-SE)/100, ymax = (MEAN+SE)/100))+
  geom_point(aes(col=Region))+
  geom_hline(aes(yintercept=0.50), col="grey60",lty=2) +
  geom_hline(aes(yintercept=1.0), col="grey60",lty=2) +
  geom_hline(aes(yintercept=0.75), col="grey60",lty=2) +
  geom_hline(aes(yintercept=0.25), col="grey60",lty=2) +
  geom_hline(aes(yintercept=0), col="black") +
  facet_wrap(~CURRENT, nrow=3,ncol=3)+
  scale_x_discrete(limits=c("10","50","100","200","500","1000","2000","5000","5804")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,1.1)) +
  scale_color_manual(values=c("firebrick4","deepskyblue","forestgreen")) +
  labs(x="Number of Loci")+
  labs(y="Assignment Success (Proportion)")+
  guides(fill= FALSE, size= FALSE, col=FALSE) +
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.x=element_text(size=13,face="bold"),
        axis.text.x=element_text(size=12,face="bold", angle=90, hjust=1, vjust=0.5),
        axis.title.y=element_text(size=13,face="bold", angle=90),
        axis.text.y=element_text(size=12,face="bold"),
        axis.line.y.left=element_blank(),
        strip.text=element_text(size=12,face="bold"),
        strip.background=element_rect(fill="grey78", color="grey78"))
dev.off()


