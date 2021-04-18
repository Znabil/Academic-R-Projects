library(tidyverse) #data manipilation
library(GGally) # nice scatterplot matrix
library(FactoMineR) # PCA computation
library(factoextra) # nice plotting for PCA objects
library(gridExtra) # to build grid of plots

#check you have the correct data set
cereals <- read.table("cerealdata.txt", header=TRUE, as.is=TRUE, na.strings="-1")
cereals1=na.omit(cereals)
rownames(cereals1)=abbreviate(cereals1$Name)
cereal.pc=select(cereals1,calories,protein,fat,sodium,fiber,carbo,sugars,potass,shelf)
cereal.pc=na.omit(cereal.pc)
head(cereal.pc)
cereal.pc$shelf=as.factor(cereal.pc$shelf)

#summary of the variables, shelf is a factor
summary(cereal.pc)
str(cereal.pc)

#covariance matrix, very different variances
cov(cereal.pc[,-9])

# Before starting with PCA, a nice scatterplot matrix
ggpairs(cereal.pc, lower = list(continuous="points",combo="facetdensity",mapping=aes(color=shelf)))

#load FactoMineR and perform a PCA analysis on matrix R, not S. Store the results in object cereal_pca_r. We are using shelf as
# a supplementary qualitative variable. By default 5 components are calculated, use ncp= to change it.

cereal_pca_r=PCA(cereal.pc,quali.sup=9,ncp=8,scale.unit=TRUE, graph=FALSE)

# by default dimensions 1 and 2 are plotted. We are using that option. To change them use axes=c(1,3).
# type cereal_pca_r to see the extensive list of results provided in the output of PCA()
cereal_pca_r

#summary of the numerical output
summary(cereal_pca_r)

#Working on the map of points, representation of individuals
plot(cereal_pca_r, cex=0.7)

# coloring points by variable shelf
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9)
# change individual color by group
fviz_pca_ind(cereal_pca_r,  habillage="shelf")
fviz_pca_ind(cereal_pca_r,  label="none", habillage="shelf")

#labels for those points with cosin squared greater than 0.7, for example.
# It selects observations with cos2[,1]+cos2[,2]>0.7
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7")
# plotting only previously selected observations
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect=1)
#selecting a color for unselected points
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 0.7", unselect="grey70")
# To select the five observations that contribute the most to the two first components, the more extreme individuals in both components
plot(cereal_pca_r, cex=0.7, shadow=TRUE, habillage=9, invisible="quali", select="cos2 5", unselect=1)

#selecting particular individuals by their names
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=c("Trix", "H_N_", "Whts"))
#selecting particular individuals by their row in the dataset
plot(cereal_pca_r, cex=0.8, shadow=TRUE, habillage=9, invisible="quali", select=1:10)
# Controlling automatically the color of individuals using the cos2 values (the quality of the individuals on the factor map)
fviz_pca_ind(cereal_pca_r, col.ind="cos2", repel=TRUE) +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.50)

# What if I want the previous plot but showing also the shelf each cereal is on? 
# Solution
newdf=data.frame(cos2=cereal_pca_r$ind$cos2[,1]+cereal_pca_r$ind$cos2[,2],shelf=cereal.pc$shelf, 
                 PC1=cereal_pca_r$ind$coord[,1],PC2=cereal_pca_r$ind$coord[,2])

ggplot(data=newdf, aes(x=PC1, y=PC2, colour=cos2, shape=shelf))+
  geom_text(aes(label=rownames(newdf)), hjust=1.5)+geom_jitter()

## Working on variables, circle of correlations
plot(cereal_pca_r, choix="var")
## or with package factoextra
fviz_pca_var(cereal_pca_r)

# Ploting the 3 variables that contribute the most to the representation 
plot(cereal_pca_r, shadow=TRUE,choix="var", select="contrib 3" )
# selecting variables by their contributions, quality of representation greater than 0.7
plot(cereal_pca_r, shadow=TRUE,choix="var", select="cos2 0.7" )
# control variable colors using their contribution
fviz_pca_var(cereal_pca_r, col.var="contrib")

# Change the gradient color
fviz_pca_var(cereal_pca_r, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=55)+theme_bw()

# Quality of each variable representation on these two dimensions
cereal_pca_r$var$cos2[,1:2][,1]+ cereal_pca_r$var$cos2[,1:2][,2]

# First a table, then a barplot of variables contribution to each dimension with ggplot
rbind(cereal_pca_r$var$contrib, TOTAL=colSums(cereal_pca_r$var$contrib))
var.contrib=cereal_pca_r$var$contrib
my.grid=expand.grid(x=rownames(var.contrib), y=colnames(var.contrib))
my.grid$values= as.vector(var.contrib)
ggplot(my.grid, aes(x=y, y=values))+geom_bar(stat="identity", aes(fill=x), position=position_dodge())+
  scale_fill_brewer(palette="Dark2")
ggplot(my.grid, aes(x=x, y=values))+geom_bar(stat="identity", aes(fill=y), position=position_dodge())

### Objects (observations) contribution
objcontrib=data.frame(C1=cereal_pca_r$ind$contrib[,1],C2=cereal_pca_r$ind$contrib[,2],n=rownames(cereal.pc))

# Barplots of object contributions to PC 1 with ggplot and package gridExtra
G1=ggplot(objcontrib[1:36,],aes(x=n, y=C1))+geom_bar(position=position_dodge(), stat="identity", fill="steelblue")+geom_text(aes(label=n), vjust=1.6, color="white", size=2.5, position=position_dodge(0.9))+geom_hline(yintercept=100/74)
G2=ggplot(objcontrib[31:74,],aes(x=n, y=C1))+geom_bar(position=position_dodge(), stat="identity", fill="steelblue")+geom_text(aes(label=n), vjust=1.6, color="white", size=2.5, position=position_dodge(0.9))+geom_hline(yintercept=100/74)
grid.arrange(G1,G2,nrow=2)
# compare them with the position of each point in the observations plot

#If you want to order observations by the first component value (score), make a data frame with the two first scores, for instance 
obsor=data.frame(C1=cereal_pca_r$ind$coord[,1],C2=cereal_pca_r$ind$coord[,2])
head(obsor[order(obsor[,1], decreasing=TRUE),])

# Checking some equalities
sum(cereal_pca_r$var$cos2[1,1:8])
sum(cereal_pca_r$ind$cos2[1,])
###
sum(cereal_pca_r$var$cos2[1:8,1])
mean(cereal_pca_r$ind$coord[,1]^2)
###
sum(cereal_pca_r$ind$contrib[,1])


#scree plot
par(mfrow=c(1,1))
plot(cereal_pca_r$eig[,1], type="l")
points(cereal_pca_r$eig[,1])

# scree plot (barplot type)
barplot(cereal_pca_r$eig[,1], names.arg=rownames(cereal_pca_r$eig))

#biplot
fviz_pca_biplot(cereal_pca_r)

# Eigenvectors
cereal_pca_r$svd$V

#correlations between variables and dimensions and significance tests
# For the first (second) component, are there any differences in the categorical variable?
dimdesc(cereal_pca_r,axes=c(1,2)) 

concat1 = cbind.data.frame(cereal.pc[,9],cereal_pca_r$ind$coord[,1:2])
boxplot(concat1[,2]~concat1[,1])

##Ellipses
#This function draws confidence ellipses around the categories of a supplementary categorical variable. The objective is to see 
#whether the categories of a #categorical variable are significantly different from each other.
#It uses a data set with the categorical variable and the coordinates of the individuals on the principal components.
fviz_pca_ind(cereal_pca_r, label="none", habillage=cereal.pc$shelf,
             addEllipses=TRUE, ellipse.level=0.95)

## reconstruction of the original data matrix usig 2 PC's
reconstruction1 = reconst(cereal_pca_r,ncp=2)
coeffRV(reconstruction1, cereal.pc[,1:8])
## coeffRV: it measures the closeness of two sets of points. It's a multivariate generalization
## of the squared Pearson correlation coefficient (0-1). It uses the concept of permutation distribution.

#### What follows below is very optional, not very important ############
############################################################################
### BIPLOTGUI, ONLY in WINDOWS, transform shelf again to numerical values
cereal.pc.b=cereal.pc
cereal.pc.b$shelf=as.numeric(cereal.pc.b$shelf)
Biplots(Data=cereal.pc.b[,-9])
#############################################################################

######################### OPTIONAL ##########################################
# This is a function that provides the p-value of the test for the null
# hypothesis that the first q PC's are adequate in representing a certain
# proportion of the total variation in the data. It can be obtained using the material in
# Anderson (1984)'s book titled "An Introduction to Multivariate Statistical
# Analysis".  Ranjan Maitra (03/30/2012)

#This is an inference result and therefore multivariate normality is required. 
#The data we are using can't be assumed to have this distribution.

PCs.proportion.variation <- function(lambda, q = 1, propn, nobs)
{
  den <- sum(lambda) # sum of all the eigenvalues
  num <- sum(lambda[1:q]) # sum of the first q eigenvalues
  if (num/den >= propn) return(1)
  else {
    se <- sqrt(2 * sum(lambda[-(1:q)])^2 * sum(lambda[1:q]^2) +
                 2 * sum(lambda[1:q])^2 * sum(lambda[-(1:q)]^2)) /
      (nobs * den^2)
    #asymptotic sd of the test statistic
    test.stat <- (num/den - propn)/se
    return(pnorm(test.stat))
  }
}

lambda3=cereal_pca_r$eig[,1]
PCs.proportion.variation(lambda=lambda3, q=3,propn=0.78, nobs=65)

PCs.proportion.variation(lambda=lambda3, q=3,propn=0.77, nobs=65)
###############################################################################


