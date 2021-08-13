getwd()

regdata <- read.table("data/assignment.ranked.no.imputation.results.summary.stats.tsv", header=TRUE, sep="\t",stringsAsFactors=F)
regdata$MARKER_NUMBER <- as.character(regdata$MARKER_NUMBER)
View(regdata)
# rename populations for plotting
regdata$CURRENT <- gsub("YellowSea", "Block 161", regdata$CURRENT)
regdata$CURRENT <- gsub("Geoje_2014-15", "Geoje 2014-15", regdata$CURRENT)
regdata$CURRENT <- gsub("Geoje_2013-14", "Geoje 2013-14", regdata$CURRENT)
regdata$CURRENT <- gsub("JinhaeBay_Dec", "Jinhae Bay Dec.", regdata$CURRENT)
regdata$CURRENT <- gsub("JinhaeBay_Feb", "Jinhae Bay Feb.", regdata$CURRENT)
regdata$CURRENT <- gsub("OVERALL", "Overall", regdata$CURRENT)
View(regdata)

# rearrange order 
neworder <- c("Block 161", "Boryeong", "Jukbyeon","Overall","Geoje 2014-15", "Geoje 2013-14", "Namhae", "Jinhae Bay Dec.", "Jinhae Bay Feb.", "Pohang")
regdata <- arrange(transform(regdata,
                             CURRENT=factor(CURRENT,levels=neworder)),CURRENT)
View(regdata)
regdata <- regdata %>%
  mutate(Region=ifelse(CURRENT %in% c("Block 161", "Boryeong"),"West",
                       ifelse(CURRENT == "Jukbyeon", "East","South")))

# plot populations
regdata_pops <- filter(regdata, CURRENT != "Overall")
png("Assignment_bysite_sites_thl0-5.png", res=100, height=700, width=600)
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
        axis.title.x=element_text(size=14, family="Helvetica",face="bold"),
        axis.text.x=element_text(size=13,family="Helvetica",face="bold", angle=90, hjust=1, vjust=0.5),
        axis.title.y=element_text(size=14, family="Helvetica",face="bold", angle=90),
        axis.text.y=element_text(size=13,family="Helvetica",face="bold"),
        axis.line.y.left=element_blank(),
        strip.text=element_text(size=13, family="Helvetica", face="bold"),
        strip.background=element_rect(fill="grey78", color="grey78"))
dev.off()

plot <- ggplot(regdata_pops, aes(x=MARKER_NUMBER,y=MEAN))+
  geom_point()+
  geom_errorbar(aes(ymin=MEAN-SE, ymax = MEAN+SE))+
  scale_x_discrete(limits=c("10","50","100","200","500","1000","2000","5000","5804"))
x_title="Number of loci"
y_title="Assignment success (%)"
png("Assignment_bysite_sites_thl0-5_adj_names.png", width=720, height = 960)
plot + facet_grid(~CURRENT)+
  facet_wrap(~CURRENT, nrow=3,ncol=3)+
  labs(x=x_title)+
  labs(y=y_title)+
  guides(fill= FALSE, size= FALSE)+
  coord_cartesian(ylim=c(0,100))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="black", linetype="dashed"),
        axis.title.x=element_text(size=18, family="Helvetica",face="bold"),
        axis.text.x=element_text(size=16,family="Helvetica",face="bold", angle=90, hjust=0, vjust=0.5),
        axis.title.y=element_text(size=18, family="Helvetica",face="bold"),
        axis.text.y=element_text(size=16,family="Helvetica",face="bold"),
        strip.text=element_text(size=14))
#ggsave("Assignment_bysite_sites_THL0-5.pdf",width=20,height=30,dpi=300,units="cm",useDingbats=F)
dev.off()