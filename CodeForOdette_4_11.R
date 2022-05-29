##Color for plot and legend
library(RColorBrewer)
library(tidyverse)
#library(shiny)
# Import data
Species.raw <- read.csv(choose.files(), header=TRUE, row.names=1, sep=",", check.names=FALSE)


# Tidy/subset imported data



# PCA using base function - prcomp()
speciesData <- prcomp(Species.raw, scale=TRUE)
# Summary
s <- summary(speciesData)

speciesDataFrame = data.frame(speciesData$x)


### Screeplot
# compute total variance
varianceDF <- data.frame(PC= paste0("PC",1:16),
                         variance = speciesData$sdev^2 / sum(speciesData$sdev^2))
PC= paste0("PC",1:16)
variance = speciesData$sdev^2 / sum(speciesData$sdev^2)

head(varianceDF)
Scree = function(){
  print("These bars represent the variance of principal components for the PCA")
  
screeplotPCA = qplot(PC, variance) +
  geom_col()+
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.4)

print(screeplotPCA)
}


PCA = function(){
# Create groups
pch.group <- c(rep(21, times=1), rep(22, times=2), rep(24, times=3), rep(25, time = 3), rep(23, times=2),rep(22, times=3),rep(25, times=2))
col.group <- c(rep("skyblue2", times=1), rep("gold", times=2), rep("green2", times=3), rep("red", times=3),rep("purple", times=2),rep("deeppink2", times=3), rep("gray50", times=2))

###Graphic
#Data grouping for Plot
MML = speciesData$x[,1]
MMH = speciesData$x[,2]
MSL = speciesData$x[,3]
MSW = speciesData$x[,4]
CondyleLenAP = speciesData$x[,5]
CondyleWidML = speciesData$x[,6]
CondyleNeckLenAp = speciesData$x[,7]
CondyleNeckWidML = speciesData$x[,8]
HCA = speciesData$x[,9]
CAA = speciesData$x[,10]
GlenoidFossa = speciesData$x[,11]
ArticularEminDepth = speciesData$x[,12]
ArticularEminThick = speciesData$x[,13]
ArticularEminSteep = speciesData$x[,14]
MaxHeadH = speciesData$x[,15]
MaxHeadL = speciesData$x[,16]
#MaxHeadW = speciesData$x[,17]


  print("What is the first principal Component you'd like to select?")
 
 principalComponent1 = switch(menu(c("Max Mandible Length","Max Mandible Height","Max Skull Height","Max Skull Width", "Condyle Length", "Condyle Width","Condyle Neck Length", "Condyle Neck Width", "Horizontal Condylar Angle", "Condyle Axis Angle", "Glenoid Fossa", "Depth of Articular Emminence", "Thickness of Articular Emminence", "Steepness of Articular Emminence", "Max Head Height","Max Head Length"  )) + 1,
         cat("Nothing done\n"),MML,MMH,MSL,MSW,CondyleLenAP,CondyleWidML,CondyleNeckLenAp,CondyleNeckWidML,HCA,CAA,GlenoidFossa,ArticularEminDepth,ArticularEminThick,ArticularEminSteep,MaxHeadH,MaxHeadL )
  
 print("What is the second principal Component you'd like to select?")
 principalComponent2 = switch(menu(c("Max Mandible Length","Max Mandible Height","Max Skull Height","Max Skull Width", "Condyle Length", "Condyle Width","Condyle Neck Length", "Condyle Neck Width", "Horizontal Condylar Angle", "Condyle Axis Angle", "Glenoid Fossa", "Depth of Articular Emminence", "Thickness of Articular Emminence", "Steepness of Articular Emminence", "Max Head Height","Max Head Length"  )) + 1,
                              cat("Nothing done\n"),MML,MMH,MSL,MSW,CondyleLenAP,CondyleWidML,CondyleNeckLenAp,CondyleNeckWidML,HCA,CAA,GlenoidFossa,ArticularEminDepth,ArticularEminThick,ArticularEminSteep,MaxHeadH,MaxHeadL )
# Plot individuals
plot(principalComponent1, principalComponent2, xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""), pch=pch.group, col="black", bg=col.group, cex=2, las=1, asp=1, xlim=c(-6,6), ylim=c(-5,5))
# Add grid lines
abline(v=0, lty=2, col="grey50")
abline(h=0, lty=2, col="grey50")
# Add labels
text(speciesData$x[,1], speciesData$x[,2], labels=row.names(speciesData$x), pos=c(1,3,4,2), font=2)

# Get co-ordinates of variables (loadings), and multiply by 10
l.x <- speciesData$rotation[,1]*10
l.y <- speciesData$rotation[,2]*10

# Draw arrows
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=1.5)


# Label position
l.pos <- l.y # Create a vector of y axis coordinates
lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
hi <- which(l.y > 0) # Get variables on the top half
# Replace values in the vector
l.pos <- replace(l.pos, lo, "1")
l.pos <- replace(l.pos, hi, "3")

# Variable labels
text(l.x, l.y, labels=row.names(speciesData$rotation), col="red", pos=l.pos)

# Get individuals (observations) as a matrix
tab <- matrix(c(speciesData$x[,1], speciesData$x[,2]), ncol=2)
# Calculate correlations
#numbers dependent upon individuals
#c1 <- cor(tab[1,])
c2 <- cor(tab[2:3,])
c3 <- cor(tab[4:6,])
c4 <- cor(tab[7:9,])
c5 <- cor(tab[10:11,])
c6 <- cor(tab[12:14,])
c7 <- cor(tab[15:16,])



# Load package
library(ellipse)
# Plot ellipse
#polygon(ellipse(c1*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[1,]), level=0.95), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
polygon(ellipse(c2*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[2:3,]), level=0.95), col=adjustcolor("gold", alpha.f=0.25), border="gold")
polygon(ellipse(c3*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[4:6,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="green2")
polygon(ellipse(c4*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[7:9,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="red")
polygon(ellipse(c5*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[10:11,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="purple")
polygon(ellipse(c6*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[12:14,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="deeppink2")
polygon(ellipse(c7*(max(abs(speciesData$rotation))*1), centre=colMeans(tab[15:16,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="gray50")

# Add legend
#legend("topleft", legend=c("Pigs", "Rats", "Humans"), col="black", pt.bg=c("skyblue2", "gold", "green2"), pch=c(21, 22, 24), pt.cex=1.5)
}
Scree()
PCA()


