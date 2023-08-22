#install.packages('ggplot2')
library('ggplot2')
#install.packages('BBmisc')
library("BBmisc")
#install.packages("devtools")
#devtools::install_github("kassambara/factoextra")
library("factoextra")
#install.packages("mdatools")
library("mdatools")

df<-read.csv('../datasets/listings_D3_v8.csv')

############################ METHOD FOR SAVING PLOTS ###########################  
save <- function(pl, fileName) {
  png(filename=paste("plots2/", fileName,".png", sep = ''))
  plot(pl)
  dev.off()
}


############################# NORMALISATION METHOD #############################  
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}



######################### REMOVAL OF EXTREME OUTLIERS ##########################
df <- df[df$price < 5000, ]
df <- df[df$reviews_per_month < 60, ]


######################### PREPARING DATAFRAME FOR PCA ##########################
numeriques<-which(sapply(df,is.numeric))
df_pca<-df[,numeriques]
df_pca$price <- NULL

#removing variables detected as numeric that are actually categorical
df_pca$phone_verification <- NULL
df_pca$email_verification <- NULL
df_pca$work_email_verification <- NULL
df_pca$latitude <- NULL
df_pca$longitude <- NULL

#cor(df_pca)

# Removing strongly correlated and redundant variables
df_pca$calculated_host_listings_count_entire_homes <- NULL
df_pca$calculated_host_listings_count_private_rooms <- NULL
df_pca$calculated_host_listings_count_shared_rooms <- NULL
df_pca$review_scores_rating <- NULL
df_pca$review_scores_checkin <-NULL
df_pca$review_scores_accuracy <- NULL
df_pca$review_scores_cleanliness <-NULL
df_pca$review_scores_communication <- NULL
df_pca$review_scores_location <- NULL
df_pca$number_of_reviews_l30d <- NULL
df_pca$number_of_reviews_ltm <- NULL

# Normalising PCA dataset
df_pca <- min_max_norm(df_pca)



######################### PRINCIPAL COMPONENT ANALYSIS ######################### 

pc1 <- prcomp(df_pca, scale=TRUE)
df_pca.pca <- prcomp(df_pca, scale=TRUE)


summary(pc1)
print(pc1)
str(pc1)

# This shows us the eigen values. According to this, we should select 6 dimensions
# because the eigen value is greater than 1 for the first 6 dimensions. However,
# we will use 5 dimensions because they explain 60% of the variance.
cumVar <- get_eigenvalue(df_pca.pca)

varPerc <- get_eigenvalue(df_pca.pca)$variance.percent

# Percentage of variance explained by each variable
plot <- fviz_eig(df_pca.pca, addlabels = TRUE, ylim = c(0, 25))
save(plot, "varExplainedByDimensions")

plot <- ggplot(data = cumVar, aes(x=reorder(row.names(cumVar), cumulative.variance.percent),y=cumulative.variance.percent)) +
  geom_bar(stat="identity", fill = "steelblue") +
  ggtitle("Cumulated Variance") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Dimensions") + 
  ylab("Cumulated Variance (%)")
save(plot, "cumulativeVariancePlot")

######################### CONTRIBUTIONS TO DIMENSIONS #########################
###############################################################################
# With 5 variables we can explain 60% of the variance

df$num

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(1), top=25)
save(plot, "contributionsToPC1")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(2), top=25)
save(plot, "contributionsToPC2")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(3), top=25)
save(plot, "contributionsToPC3")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(4), top=25)
save(plot, "contributionsToPC4")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(5), top=25)
save(plot, "contributionsToPC5")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(6), top=25)
save(plot, "contributionsToPC6")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(1,2), top=25)
save(plot, "contributionsToPC1PC2")

plot <- fviz_contrib(df_pca.pca, choice="var", axes=c(1,2,3,4), top=25)
save(plot, "contributionsToPC1PC2PC3PC4")

# SELECTION OF THE SINGIFICANT DIMENSIONS (keep 60% of total variance)
nd = 5

# STORAGE OF THE EIGENVALUES, EIGENVECTORS AND PROJECTIONS IN THE nd DIMENSIONS
Psi = pc1$x[,1:nd] #em guardo només les rotacions que m'interessen (rotacions de les PCi seleccionades)

# STORAGE OF LABELS FOR INDIVIDUALS AND VARIABLES
iden = row.names(df_pca) #vector amb números de l'1 al 12254
etiq = names(df_pca) #noms de les variables numèriques
ze = rep(0,length(etiq)) # WE WILL NEED THIS VECTOR AFTERWARDS FOR THE GRAPHICS (vector de zeros)


# Projection of variables
Phi = cor(df_pca,Psi)

# Represent numerical variables in background
eix1 <- 1
eix2 <- 2

# Select your axis

X<-Phi[,eix1]
Y<-Phi[,eix2]


################################## PC1 & PC2 ###################################
#install.packages('factoextra')
# Visualize variables
library('factoextra')
plot <- fviz_pca_var(df_pca.pca)
save(plot, "arrowPlotPC1PC2")


# PCA PLOTS WITHOUT COLOURS
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.6) +
          ggtitle("PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red")
  
save(plot, "plotPC1PC2")

# PCA PLOTS WITH COLOURS

# Property type
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.6, aes(color = df$property_type)) +
          ggtitle("Property type according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Property Type")
save(plot, "propertyTypePlotPC1PC2")

#Minimum nights
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.25, aes(color = df$minimum_nights >= 30)) +
          ggtitle("Potential only long term rentals according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="30 or more minimum nights")
plot(plot)
save(plot, "minNights30PlotPC1PC2")
     
#Minimum nights
my_colors <- c("white", "black")
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
 geom_point(alpha = 0.05, aes(color = df$minimum_nights > 32)) +
 ggtitle("Potential only long term rentals according to PC1 and PC2") +
 xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
 ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
 geom_hline(yintercept=0, color = "red") +
 geom_vline(xintercept=0, color = "red") +
 scale_color_manual(values = my_colors) + 
 labs(color="Over 31 minimum nights")
save(plot, "minNights32PlotPC1PC2")

# Eixample or Sant Marti
eixampleSantMarti <- (df$neighbourhood_group_cleansed == "Eixample" | df$neighbourhood_group_cleansed == "Sant Marti")
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.25, aes(color = eixampleSantMarti)) +
          ggtitle("Listings in Eixample or Sant Marti according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="In Eixample or Sant Marti")
save(plot, "eixampleSantMartiPlotPC1PC2")

# Eixample or not Eixample
eixample <- (df$neighbourhood_group_cleansed == "Eixample")
my_colors <- c("white", "black")
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.05, aes(color = eixample)) +
  ggtitle("Listings in Eixample according to PC1 and PC2") +
  xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
  ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
  geom_hline(yintercept=0, color = "red") +
  geom_vline(xintercept=0, color = "red") +
  scale_color_manual(values = my_colors) +
  labs(color="In Eixample or Sant Marti")
save(plot, "eixampleSantMartiPlotPC1PC2")



# Host greets
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$hostGreets)) +
          ggtitle("Listings where host greets guests according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Host Greets Guests")
save(plot, "hostGreetsPlotPC1PC2")


#Price
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = log(df$price))) +
          ggtitle("Price according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Log of Price")
save(plot, "pricePlotPC1PC2")

# Host total listings count
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = log(df$host_total_listings_count))) +
          ggtitle("Host total listings according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Log of Host Total Listings")
save(plot, "hostTotalListingsPlotPC1PC2")

# Number of reviews
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = sqrt(df$reviews_per_month))) +
          ggtitle("Number of reviews per month according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Sqrt Number of \nReviews Per Month")
save(plot, "reviewsPerMonthPC1PC2")


# Reviews score value
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$review_scores_value)) +
          ggtitle("Review score values according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Review Score Value")
save(plot, "reviewScoreValuesPlotPC1PC2")


#Accommodates
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$accommodates)) +
          ggtitle("Accommodates according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Accommodates")
save(plot, "accommodatesPlotPC1PC2")


# Host location
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$host_location)) +
          ggtitle("Host location according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Host Location")
save(plot, "hostLocationPlotPC1PC2")

# License
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$license)) +
          ggtitle("License type according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="License Type")
save(plot, "licenseTypePlotPC1PC2")


# Neighbourhood
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$neighbourhood_group_cleansed)) +
          ggtitle("Listing's neighbourhood according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Neighbourhood")
save(plot, "neighbourhoodPlotPC1PC2")

# Outdoor space
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$outdoorSpace)) +
          ggtitle("Listing has outdoor space according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Outdoor Space")
save(plot, "outdoorSpacePlotPC1PC2")

# Aircon
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$aircon)) +
          ggtitle("Lisiting has air conditioning according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Air conditioning")
save(plot, "airconPlotPC1PC2")

# Superhost
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
          geom_point(alpha = 0.8, aes(color = df$host_is_superhost)) +
          ggtitle("Host is superhost according to PC1 and PC2") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Superhost")
save(plot, "superhostPlotPC1PC2")

Neighbourhoods <- levels(factor(df$neighbourhood_group_cleansed))

Acommodates <- c(1:length(Neighbourhoods))
Price       <- c(1:length(Neighbourhoods))


for (i in 1:length(n)) {
  Acommodates[i]  <- median(df$accommodates[df$neighbourhood_group_cleansed == n[i]])
  Price[i] <- median(df$price[df$neighbourhood_group_cleansed == n[i]])
}
tempDf <- data.frame(Neighbourhoods, Acommodates, Price)

plot <- ggplot(data = tempDf, aes(x=reorder(Neighbourhoods, -Acommodates), y=Acommodates)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Neighbourhood") +
  coord_flip()
save(plot, "acommodatesPerNeighbourhood")

plot <- ggplot(data = tempDf, aes(x=reorder(Neighbourhoods, -Acommodates), y=Price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Neighbourhood") +
  coord_flip()
save(plot, "pricePerNeighbourhood")


################################################################################
################################## PC1 & PC3 ###################################
# Represent numerical variables in background
plot <- fviz_pca_var(df_pca.pca, axes = c(1,3))
save(plot, "arrowPlotPC1PC3")

# PCA PLOTS WITHOUT COLOURS
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC3)) +
  geom_point(alpha = 0.6) +
  ggtitle("PC1 and PC3") +
  xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
  ylab(paste("PC2 (", round(varPerc[3], digits = 2), "%)", sep = '')) +
  geom_hline(yintercept=0, color = "red") +
  geom_vline(xintercept=0, color = "red")

save(plot, "plotPC1PC3")

#PCA PLOTS WITH COLOURS

# Price
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC3)) +
          geom_point(alpha = 0.8, aes(color = log(df$price))) +
          ggtitle("Price according to PC1 and PC3") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC3 (", round(varPerc[3], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Log of Price")
save(plot, "pricePlotPC1PC3")


# Bathroom type
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC3)) +
          geom_point(alpha = 0.8, aes(color = df$type_bath)) +
          ggtitle("Bathroom type according to PC1 and PC3") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC3 (", round(varPerc[3], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Bahroom Type")
save(plot, "bathroomTypePlotPC1PC3")


################################################################################
################################## PC1 & PC4 ###################################
# Represent numerical variables in background
plot <- fviz_pca_var(df_pca.pca, axes = c(1,4))
save(plot, "arrowPlotPC1PC4")

# PCA PLOTS WITHOUT COLOURS
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC4)) +
  geom_point(alpha = 0.6) +
  ggtitle("PC1 and PC4") +
  xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
  ylab(paste("PC4 (", round(varPerc[4], digits = 2), "%)", sep = '')) +
  geom_hline(yintercept=0, color = "red") +
  geom_vline(xintercept=0, color = "red")

save(plot, "plotPC1PC4")

# Host since
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC4)) +
          geom_point(alpha = 0.8, aes(color = df$host_since)) +
          ggtitle("Host since according to PC1 and PC4") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC4 (", round(varPerc[4], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Host Since")
save(plot, "hostSincePlotPC1PC4")

# Price
plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC4)) +
          geom_point(alpha = 0.8, aes(color = log(df$price))) +
          ggtitle("Price according to PC1 and PC4") +
          xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
          ylab(paste("PC4 (", round(varPerc[4], digits = 2), "%)", sep = '')) +
          geom_hline(yintercept=0, color = "red") +
          geom_vline(xintercept=0, color = "red") +
          labs(color="Log of Price")
save(plot, "pricePlotPC1PC4")

############################# ADDITIONAL PLOTS #################################

for (name in colnames(df)) {
  plot <- ggplot(data = as.data.frame(Psi), mapping = aes(x = PC1, y = PC2)) +
    geom_point(alpha = 0.6, aes(color = df[[name]])) +
    ggtitle(paste(name, "according to PC1 and PC2")) +
    xlab(paste("PC1 (", round(varPerc[1], digits = 2), "%)", sep = '')) +
    ylab(paste("PC2 (", round(varPerc[2], digits = 2), "%)", sep = '')) +
    geom_hline(yintercept=0, color = "red") +
    geom_vline(xintercept=0, color = "red")
  save(plot, paste(name, "propertyTypePlotPC1PC2", sep = ''))
}

