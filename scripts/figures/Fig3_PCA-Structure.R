########### Fisher et al. Fig3 ##############
#
# PCA, DAPC, and Structure viz of population structure
#
# MF 5/9/2020
#
#############################################



# Load Packages -----------------------------------------------------------
library(adegenet)
library(hierfstat)
library(tidyverse)
library(here)

# this script also uses functions from Thibaut Jombart's script `compoplot_source_code.R`
source(here::here('scripts','figures','compoplot_source_code.R'))



# Read in data ------------------------------------------------------------
## genepop
my_data_all <- read.genepop(here::here('data','genepop','korea-pcod-final-filtered.gen'))
# To replace missing data information with the mean
X <- scaleGen(my_data_all, NA.method="mean")

## structure classifications
sdata <- read.delim(here::here('results','structure_k3_assignment-figure.txt'))



# Run PCA -----------------------------------------------------------------
# To conduct the PCA. IF YOU DO NOT KNOW HOW MANY AXES TO RETAIN
# pca_all <- dudi.pca(X,cent=FALSE,scale=FALSE)
# barplot(pca_all$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))
# summary(pca_all)

# To conduct the PCA. IF YOU KNOW HOW MANY AXES TO RETAIN
pca_all <- dudi.pca(X,cent=FALSE,scale=FALSE,scannf=FALSE,nf=2) #nf = number to retain
barplot(pca_all$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))
summary(pca_all)



# Run DAPC ----------------------------------------------------------------
# Subset for only southern populations
my_data_south <- my_data_all[which(my_data_all$pop %in% c("PO010715_07", "GE011215_11", "NA021015_30","JB121807_29","JB021108_23","GEO020414_30_2")),]
popNames(my_data_south)
popNames(my_data_all)


## find optimal number of principal components
# mydata_dapc <- dapc(my_data_south,my_data_south$pop,n.pca=234,n.da=6) ##Retain all pca / da 
# test_a_score <- optim.a.score(mydata_dapc) ##Use graph to identify optimal number of PCs

## run dapc only on optimal number of principal components
mydata_dapc <- dapc(my_data_south,my_data_south$pop,n.pca=42,n.da=6) ##42 PCs is the optimal number here



# Plot PCA/DAPC/STRUCTURE ----------------------------------------------

## PCA colors & Labels
#Color of points (order in genepop)
col.pca <- c("darkgray","mediumorchid1","aquamarine2","firebrick4","forestgreen","deepskyblue", "deepskyblue4", "coral1","mediumorchid4")
#Color of legend (order easier to read)
col_leg.pca <- c("firebrick4","coral1","aquamarine2","mediumorchid4", "mediumorchid1","deepskyblue", "deepskyblue4", "gray","forestgreen")
#Point shapes for legend (here is by region)
points_leg.pca <- c(17,17,16,16,16,16,16, 16,15)

## DAPC colors & Labels
## create the population labels for the legend and set the colors for each population##
pop_labels <- c("Pohang", "Geoje 2013", "Namhae", "Jinhaeman Dec.", "Jinhaeman Feb.", "Geoje 2014")
pop_cols <- c("gray45","mediumorchid4","seagreen1","deepskyblue", "deepskyblue4", "mediumorchid2")

##### PLOT #####
png(here::here('results','Figure3.png'), res=200, height=1300, width=1600)
par(mfrow=c(1,1))
layout(matrix(c(3, 3, 2,
                3, 3, 1,
                3, 3, 1), nrow=3, byrow=TRUE))
# add dapc of southern groups only
scatter.dapc(mydata_dapc,scree.da=FALSE, lty=1, lwd=2,posi.da="bottomright", scree.pca = FALSE,cellipse=0,leg=FALSE,label=NULL, pch = 19,csub=2,col=pop_cols,cex=1.5,clabel=1,solid=0.7, inset.solid=0.7)
mtext(text="b)", at=c(4,6), cex=1.5)

# add legend in its own area
par(xpd=TRUE)
plot.new()
legend (x=-0.2,y=1.8,legend = c("Block 161", "Boryeong", "Namhae", 
                             "Geoje 2014", "Geoje 2013", "Jinhaeman Dec.", "Jinhaeman Feb.", "Pohang", "Jukbyeon"), 
        col = col_leg.pca, border = FALSE, bty = "n", cex = 1.8, 
        pt.cex=2, y.intersp = 1, title = NULL,pch=points_leg.pca)
par(xpd=FALSE)


#to graph PCA with lines between samples
pca.plot.all <- s.class(pca_all$li, fac=pop(my_data_all), 
                        col=alpha(col.pca,0.7), #color of points. will retain lines between points
                        clabel=0, #remove population labels
                        cellipse=0, #remove ellipses; to add back in, make >=1
                        cpoint=4,
                        grid=FALSE, #otherwise will have light gray grid markers
                        pch=c(16,16,16,17,15,16,16, 17,16)[as.numeric(pop(my_data_all))], #change point shapes
                        axesell=FALSE)
#add.scatter.eig(pca_all$eig[1:50],posi="bottom", 3,2,1,ratio=.2)
mtext(text="a)", at=c(46,6), cex=1.5)


# add composition plot with structure output
par(fig=c(0.02,0.55,0.9,0.99),oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new=TRUE,xpd=TRUE)
compoplot.matrix.edit(as.matrix(sdata_raw[,2:4]),col.pal=c("darkred","darkslategray2","forestgreen"),
                      legend=FALSE,border=NA,space = c(0,0), ylim=c(0,1),yaxt='n',show.lab=FALSE)
axis(2, at = seq(0, 1, 1), las = 1, line=-1)


dev.off()




