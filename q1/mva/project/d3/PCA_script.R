library("readxl")
library('ggplot2')
library(ggbiplot)

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
df<-read.csv('datasets/listings_D3_v7.csv')
df <- df[complete.cases(df),]



numeriques<-which(sapply(df,is.numeric))
numeriques

df_pca<-df[,numeriques]

#removing variables detected as numeric that are actually categorical
df_pca$phone_verification <- NULL
df_pca$email_verification <- NULL
df_pca$work_email_verification <- NULL
df_pca$latitude <- NULL
df_pca$longitude <- NULL

cor(df_pca)

#removing variables with high correlation and redundancies
# df_pca$host_total_listings_count <- NULL

df_pca$calculated_host_listings_count <- NULL
df_pca$review_scores_value <- NULL




# PRINCIPAL COMPONENT ANALYSIS 

pc1 <- prcomp(df_pca, scale=TRUE)

df_pca.pca <- prcomp(df_pca, scale=TRUE)


summary(pc1)
print(pc1)
str(pc1)


#Amb 6 variables tenim explicat un 60% de la inèrcia. A partir de la setena variable els augments són del 3% o menors.
#Això es pot veure representat en el següent plot:

# % inèrcia que explica cada variable:
pinerEix<- 100*(pc1$sdev^2)/sum(pc1$sdev^2)
barplot(pinerEix, main = 'Individual Inertia')

#Inèrcia acumulada

xx <-barplot(100*cumsum(pc1$sdev[1:dim(df_pca)[2]]^2)/dim(df_pca)[2], main = 'Cummulated Inertia')
percInerAccum<-100*cumsum(pc1$sdev[1:dim(df_pca)[2]]^2)/dim(df_pca)[2]
percInerAccum

# SELECTION OF THE SINGIFICANT DIMENSIONS (keep 60% of total inertia)
nd = 6

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS

Psi = pc1$x[,1:nd] #em guardo només les rotacions que m'interessen (rotacions de les PCi seleccionades)

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES

iden = row.names(df_pca) #vector amb números de l'1 al 12254
etiq = names(df_pca) #noms de les variables numèriques
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS (vector de zeros)

# PLOT OF INDIVIDUALS

#ggbiplot(pc1)
#ggbiplot(pc1, labels=rownames(df_pca))
#df_pca.email_verification <- df$email_verification


#select your axis
#eix1<-2
eix1<-1
#eix2<-3
eix2<-2

plot(Psi[,eix1],Psi[,eix2])
text(Psi[,eix1],Psi[,eix2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")


plot(Psi[,eix1],Psi[,eix2], type="p")
text(Psi[,eix1],Psi[,eix2],labels=iden, cex=0.5)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

install.packages('rgl')
library(rgl)
plot3d(Psi[,1],Psi[,2],Psi[,3])

plot3d(Psi[,2],Psi[,3],Psi[,4])

plot3d(Psi[,3],Psi[,4],Psi[,5])

#Projection of variables

Phi = cor(df_pca,Psi)

#select your axis

X<-Phi[,eix1]
Y<-Phi[,eix2]

plot(Psi[,eix1],Psi[,eix2],type="p")
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)


#zooms
plot(Psi[,eix1],Psi[,eix2],type="p",xlim=c(min(X,0),max(X,0)), ylim=c(-1,1))
axis(side=1, pos= 0, labels = F)
axis(side=3, pos= 0, labels = F)
axis(side=2, pos= 0, labels = F)
axis(side=4, pos= 0, labels = F)
arrows(ze, ze, X, Y, length = 0.07,col="blue")
text(X,Y,labels=etiq,col="darkblue", cex=0.7)



# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCItion OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)


varcat=as.factor(df$host_since)
is.factor(varcat)

varcat
length(varcat)

plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(factor(varcat)),pch=1,col=c(1,2), cex=0.6)

#select your qualitative variable


varcat<-factor(df$host_since)
fdic1 = tapply(Psi[,eix1],varcat,mean)
fdic2 = tapply(Psi[,eix2],varcat,mean) 
#points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="yellow", cex=0.7)


#Now we project both cdgs of levels of a selected qualitative variable without
#representing the individual anymore

plot(Psi[,eix1],Psi[,eix2],type="p")
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#select your qualitative variable
k<-1 #host_since

varcat<-df[,k]
fdic1 = tapply(Psi[,eix1],varcat,mean)
fdic2 = tapply(Psi[,eix2],varcat,mean) 

points(fdic1,fdic2,pch=16,col="blue", labels=levels(varcat))
text(fdic1,fdic2,labels=levels(varcat),col="blue", cex=0.7)

#colors <- rainbow(length(levels(factor(df$host_since))))

library(ggplot2)


#
ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.8, aes(color = log(df$host_total_listings_count)))

#all qualitative together
plot(Psi[,eix1],Psi[,eix2], col=df$host_since)
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
legend(x="bottomleft", legend = levels(factor(df$neighbourhood_group_cleansed)), col = levels(factor(df$host_since)))

#nominal qualitative variables

dcat <- df[,c(1, 2, 3, 6, 8, 9, 10)]

#divide categoricals in several graphs if joint representation saturates

#build a palette with as much colors as qualitative variables 

#colors<-c("blue","red","green","orange","darkgreen")
#alternative
colors<-rainbow(length(dcat))

c<-1
for(k in dcat){
  seguentColor<-colors[c]
  fdic1 = tapply(Psi[,eix1],k,mean)
  fdic2 = tapply(Psi[,eix2],k,mean) 
  text(fdic1,fdic2,labels=levels(factor(k)),col=seguentColor, cex=0.6)
  c<-c+1
}


legend("bottomleft",names(dcat),pch=1,col=colors, cex=0.6)

#determine zoom level
#use the scale factor or not depending on the position of centroids
# ES UN FACTOR D'ESCALA PER DIBUIXAR LES FLETXES MES VISIBLES EN EL GRAFIC
#fm = round(max(abs(Psi[,1]))) 
fm=20

#scale the projected variables
#X<-fm*U[,eix1]
#Y<-fm*U[,eix2]

#represent numerical variables in background
plot(Psi[,eix1],Psi[,eix2],type="n",xlim=c(-1,1), ylim=c(-3,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="black", cex=0.7)

  #adf centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eix1],df[,k],mean)
  fdic2 = tapply(Psi[,eix2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  text(fdic1,fdic2,labels=levels(factor(df[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[dcat],pch=1,col=colors, cex=0.6)


#adf ordinal qualitative variables. Ensure ordering is the correct

dordi<-c(8)


levels(factor(df[,dordi[1]]))
#reorder modalities: when required
df[,dordi[1]] <- factor(df[,dordi[1]], ordered=TRUE,  levels= c("WorkingTypeUnknown","altres sit","temporal","fixe","autonom"))
levels(df[,dordi[1]])

c<-1
col<-1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eix1],df[,k],mean)
  fdic2 = tapply(Psi[,eix2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(df[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(df)[dordi],pch=1,col=colors[1:length(dordi)], cex=0.6)

#using our own colors palette
# search palettes in internet. One might be https://r-charts.com/es/colores/

colors<-c("red", "blue", "darkgreen", "orange", "violet", "magenta", "pink")

#represent numerical variables in background
plot(Psi[,eix1],Psi[,eix2],type="n",xlim=c(-1,1), ylim=c(-3,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.7)

#adf centroids
c<-1
for(k in dcat){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eix1],df[,k],mean)
  fdic2 = tapply(Psi[,eix2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  text(fdic1,fdic2,labels=levels(factor(df[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[dcat],pch=19,col=colors, cex=0.6)


#adf ordinal qualitative variables. Ensure ordering is the correct

dordi<-c(8)


levels(factor(df[,dordi[1]]))
#reorder modalities: when required
df[,dordi[1]] <- factor(df[,dordi[1]], ordered=TRUE,  levels= c("WorkingTypeUnknown","altres sit","temporal","fixe","autonom"))
levels(df[,dordi[1]])

c<-1
col<-length(dcat)+1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eix1],df[,k],mean)
  fdic2 = tapply(Psi[,eix2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(df[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(df)[dordi],pch=19,col=colors[col:col+length(dordi)-1], cex=0.6)


#Make two complementary factorial maps

colors<-c("red", "blue", "darkgreen", "orange", "violet", "magenta", "pink")

#represent numerical variables in background
#plot(Psi[,eix1],Psi[,eix2],type="p",xlim=c(-1,1), ylim=c(-3,1), col="lightgray")
plot(Psi[,eix1],Psi[,eix2],type="n",xlim=c(-1,1), ylim=c(-3,1))
#plot(X,Y,type="none",xlim=c(min(X,0),max(X,0)))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")

#adf projections of numerical variables in background
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.7)

#numerical variables of financial situation

seleccio<-c(4:7,10)
df_pcaMapa1<-df_pca[,seleccio]

#referencia general comu a tots els mapes
arrows(ze, ze, X, Y, length = 0.07,col="lightgray")
text(X,Y,labels=etiq,col="gray", cex=0.7)

#represent in the map1
XMapa1<-Phi[seleccio,eix1]
YMapa1<-Phi[seleccio,eix2]

arrows(ze, ze, XMapa1, YMapa1, length = 0.07,col="green")
text(XMapa1,YMapa1,labels=names(df_pcaMapa1),col="green", cex=0.7)


#adf centroids
dcatMapa1<-c(7)

c<-1
for(k in dcatMapa1){
  seguentColor<-colors[c]
  
  fdic1 = tapply(Psi[,eix1],df[,k],mean)
  fdic2 = tapply(Psi[,eix2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  text(fdic1,fdic2,labels=levels(factor(df[,k])),col=seguentColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[dcatMapa1],pch=19,col=colors, cex=0.6)


#adf ordinal qualitative variables. Ensure ordering is the correct

dordi<-c(8)


levels(factor(df[,dordi[1]]))
#reorder modalities: when required
df[,dordi[1]] <- factor(df[,dordi[1]], ordered=TRUE,  levels= c("WorkingTypeUnknown","altres sit","temporal","fixe","autonom"))
levels(df[,dordi[1]])

c<-1
col<-length(dcat)+1
for(k in dordi){
  seguentColor<-colors[col]
  fdic1 = tapply(Psi[,eix1],df[,k],mean)
  fdic2 = tapply(Psi[,eix2],df[,k],mean) 
  
  #points(fdic1,fdic2,pch=16,col=seguentColor, labels=levels(df[,k]))
  #connect modalities of qualitative variables
  lines(fdic1,fdic2,pch=16,col=seguentColor)
  text(fdic1,fdic2,labels=levels(df[,k]),col=seguentColor, cex=0.6)
  c<-c+1
  col<-col+1
}
legend("topleft",names(df)[dordi],pch=19,col=colors[col:col+length(dordi)-1], cex=0.6)





# PROJECTION OF ILLUSTRATIVE qualitative variables on individuals' map
# PROJECCI? OF INDIVIDUALS DIFFERENTIATING THE Dictamen
# (we need a numeric Dictamen to color)

varcat=factor(df[,1])
plot(Psi[,1],Psi[,2],col=varcat)
axis(side=1, pos= 0, labels = F, col="darkgray")
axis(side=3, pos= 0, labels = F, col="darkgray")
axis(side=2, pos= 0, labels = F, col="darkgray")
axis(side=4, pos= 0, labels = F, col="darkgray")
legend("bottomleft",levels(varcat),pch=1,col=c(1,2), cex=0.6)


# Overproject THE CDG OF  LEVELS OF varcat
fdic1 = tapply(Psi[,1],varcat,mean)
fdic2 = tapply(Psi[,2],varcat,mean) 

text(fdic1,fdic2,labels=levels(factor(varcat)),col="cyan", cex=0.75)


################################################################################
#install.packages("plot3D")
#install.packages('plot3Drgl')
library("plot3D")
library("plot3Drgl")

scatter3D(as.data.frame(Psi)$PC1, as.data.frame(Psi)$PC2, as.data.frame(Psi)$PC3, pch = 19, colvar = as.numeric(as.factor(df$neighbourhood_group_cleansed)), xlab = "PC1 (16,73%)",
          ylab ="PC2 (13,72%)", zlab = "PC3 (11,24%)", ticktype = "detailed", clab = c("Bbq"))

scatter3D(as.data.frame(Psi)$PC1, as.data.frame(Psi)$PC2, as.data.frame(Psi)$PC3, pch = 19, colvar = as.numeric(as.factor(df$property_type)), xlab = "PC1 (16,73%)",
          ylab ="PC2 (13,72%)", zlab = "PC3 (11,24%)", ticktype = "detailed", clab = c("Property Type"))


plotrgl()

scatter3D(as.data.frame(Psi)$PC1, as.data.frame(Psi)$PC2, as.data.frame(Psi)$PC3, pch = 19, colvar = df$email_verification, col = c("#1B9E77", "#D95F02"), add = FALSE, xlab = "PC1 (16,73%)",
          ylab ="PC2 (13,72%)", zlab = "PC3 (11,24%)", ticktype = "detailed", clab = c("Email Verification"))

plotrgl()

scatter3D(as.data.frame(Psi)$PC1, as.data.frame(Psi)$PC2, as.data.frame(Psi)$PC3, pch = 19, colvar = df$calculated_host_listings_count, add = FALSE, xlab = "PC1 (16,73%)",
          ylab ="PC2 (13,72%)", zlab = "PC3 (11,24%)", ticktype = "detailed", clab = c("Calculated listing count"))

plotrgl()

scatter3D(as.data.frame(Psi)$PC1, as.data.frame(Psi)$PC2, as.data.frame(Psi)$PC3, pch = 19, colvar = df$price, xlab = "PC1 (16,73%)",
          ylab ="PC2 (13,72%)", zlab = "PC3 (11,24%)", ticktype = "detailed", clab = c("Price"))

plotrgl()


