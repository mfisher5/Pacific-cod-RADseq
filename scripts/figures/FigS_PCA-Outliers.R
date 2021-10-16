######## PCA with Outlier Loci Only #########
#
# Determine population segregation based only on outlier
#   loci 
#
# M. Fisher 6/28/2020
#
########################################################

# Load Packages -----------------------------------------------------------
library(adegenet)
library(hierfstat)
library(tidyverse)
library(here)



# Read in data ------------------------------------------------------------
## genepop
my_data_all <- read.genepop(here::here('data','genepop','korea-pcod-final-filtered.gen'))

## outlier loci lists
outliers <- read.csv(here::here('results','outliers','all_outliers_list.csv'),colClasses=c("character"))
bayenv   <- read.csv(here::here('results', 'outliers','bayenv_logBF_2plus_temp.csv'))


## select only outlier loci
dat_outliers <- my_data_all[,loc=outliers$locus_id]
length(my_data_all@all.names)
length(dat_outliers@all.names)

## select only bayenv temp-associated loci
dat_bayenv <- my_data_all[,loc=as.character(bayenv$LocusName)]
length(dat_bayenv@all.names)

# To replace missing data information with the mean
X_outliers <- scaleGen(dat_outliers, NA.method="mean")
X_bayenv   <- scaleGen(dat_bayenv, NA.method="mean")

# Run PCA -----------------------------------------------------------------
# To conduct the PCA. IF YOU DO NOT KNOW HOW MANY AXES TO RETAIN
# pca_outliers <- dudi.pca(X,cent=FALSE,scale=FALSE)
# barplot(pca_outliers$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))
pca_bayenv <- dudi.pca(X_bayenv,cent=FALSE,scale=FALSE)
barplot(pca_bayenv$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))


# To conduct the PCA. IF YOU KNOW HOW MANY AXES TO RETAIN
pca_outliers <- dudi.pca(X_outliers,cent=FALSE,scale=FALSE,scannf=FALSE,nf=2) #nf = number to retain
pca_bayenv   <- dudi.pca(X_bayenv,cent=FALSE,scale=FALSE,scannf=FALSE,nf=2) #nf = number to retain


# Run DAPC ----------------------------------------------------------------
# Subset for only southern populations
dat_south <- dat_outliers[which(dat_outliers$pop %in% c("PO010715_07", "GE011215_11", "NA021015_30","JB121807_29","JB021108_23","GEO020414_30_2")),]
popNames(dat_south)
popNames(dat_outliers)

dat_south_bayenv <- dat_bayenv[which(dat_bayenv$pop %in% c("PO010715_07", "GE011215_11", "NA021015_30","JB121807_29","JB021108_23","GEO020414_30_2")),]
popNames(dat_south_bayenv)


## find optimal number of principal components
outliers_dapc <- dapc(dat_south,dat_south$pop,n.pca=234,n.da=6) ##Retain all pca / da 
test_a_score <- optim.a.score(outliers_dapc) ##Use graph to identify optimal number of PCs

bayenv_dapc <- dapc(dat_south_bayenv,dat_south_bayenv$pop,n.pca=234,n.da=6) ##Retain all pca / da 
test_a_score <- optim.a.score(bayenv_dapc) ##Use graph to identify optimal number of PCs


## run dapc only on optimal number of principal components
outliers_dapc <- dapc(dat_south,dat_south$pop,n.pca=24,n.da=6) ##42 PCs is the optimal number here
bayenv_dapc <- dapc(dat_south_bayenv,dat_south_bayenv$pop,n.pca=40,n.da=6) ##Retain all pca / da 



# Plot PCA / DAPC ---------------------------------------------------------

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

png(here::here('results','FigureS.png'), res=100,height=600,width=900)
par(mfrow=c(1,1))
layout(matrix(c(1,1, 5, 2,2,
                3,3, 5, 4,4), nrow=2, byrow=TRUE))
s.class(pca_outliers$li, fac=pop(my_data_outliers), 
        col=alpha(col.pca,0.7), #color of points. will retain lines between points
        clabel=0, #remove population labels
        cellipse=0, #remove ellipses; to add back in, make >=1
        cpoint=2,
        grid=FALSE, #otherwise will have light gray grid markers
        pch=c(16,16,16,17,15,16,16, 17,16)[as.numeric(pop(my_data_outliers))], #change point shapes
        axesell=FALSE)
mtext(text="a)", at=c(-9), cex=1.5)

scatter.dapc(outliers_dapc,scree.da=FALSE, lty=1, lwd=2,posi.da="bottomright", scree.pca = FALSE,cellipse=0,leg=FALSE,label=NULL, pch = 19,csub=2,col=pop_cols,cex=1.5,clabel=1,solid=0.7, inset.solid=0.7)
mtext(text="b)", at=c(-3.25,6), cex=1.5)

s.class(pca_bayenv$li, fac=pop(dat_bayenv), 
        col=alpha(col.pca,0.7), #color of points. will retain lines between points
        clabel=0, #remove population labels
        cellipse=0, #remove ellipses; to add back in, make >=1
        cpoint=2,
        grid=FALSE, #otherwise will have light gray grid markers
        pch=c(16,16,16,17,15,16,16, 17,16)[as.numeric(pop(dat_bayenv))], #change point shapes
        axesell=FALSE)
mtext(text="c)", at=c(-12.3), cex=1.5)

scatter.dapc(bayenv_dapc,scree.da=FALSE, lty=1, lwd=2,posi.da="bottomright", scree.pca = FALSE,cellipse=0,leg=FALSE,label=NULL, pch = 19,csub=2,col=pop_cols,cex=1.5,clabel=1,solid=0.7, inset.solid=0.7)
mtext(text="d)", at=c(-4.35,6), cex=1.5)

# add legend in its own area
par(xpd=TRUE)
plot.new()
legend (x=-0.55,y=1,legend = c("Block 161", "Boryeong", "Namhae", 
                                "Geoje 2014", "Geoje 2013", "Jinhaeman Dec.", "Jinhaeman Feb.", "Pohang", "Jukbyeon"), 
        col = col_leg.pca, border = FALSE, bty = "n", cex = 1.8, 
        pt.cex=2, y.intersp = 1, title = NULL,pch=points_leg.pca)
par(xpd=FALSE)
dev.off()


