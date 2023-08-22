#install.packages("ape")
#install.packages('dendextend')
#install.packages('ggdendro')
library(StatMatch)
library(cluster)
library(dendextend)
library(ape)
library(dendextend)
library(plotly)
library(ggplot2)
library(ggdendro)
library(gridExtra)

#Importing dataset
df <- read.csv('../datasets/listings_D3_v8.csv')
dfOg <- read.csv('../datasets/listings_D3_v8.csv')
names(df) #58 variables
summary(df)

#We divide the data into categorical and numerical variables
attach(df)
df_num <- data.frame(host_response_rate, host_acceptance_rate, host_total_listings_count, 
                     latitude, longitude, accommodates, bedrooms, beds, price, minimum_nights,
                     maximum_nights, availability_30, availability_365, number_of_reviews, number_of_reviews_ltm,
                     number_of_reviews_l30d, review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
                     review_scores_checkin, review_scores_communication, review_scores_location,
                     review_scores_value, calculated_host_listings_count, calculated_host_listings_count_entire_homes,
                     calculated_host_listings_count_private_rooms, calculated_host_listings_count_shared_rooms,
                     reviews_per_month, num_baths)

df_cat <- data.frame(host_since, host_location, host_response_time, host_is_superhost, host_has_profile_pic,
                     host_identity_verified, neighbourhood_group_cleansed, property_type, room_type, has_availability,
                     license, instant_bookable, phone_verification, email_verification, work_email_verification,
                     type_bath, wifi, longTermStays, hairDryer, aircon, heating, tv, hostGreets, outdoorSpace, parkingOnPremise,
                     pool, bbq, first_review, last_review)
detach(df)

#First we will normalize the numerical data since the different variables have very different scales
df_num <- as.data.frame(scale(df_num))

df_cat$email_verification <- as.logical(df_cat$email_verification)
df_cat$work_email_verification <- as.logical(df_cat$work_email_verification)
df_cat$phone_verification <- as.logical(df_cat$phone_verification)

#df_cat$email_verification[df_cat$email_verification == 0] <- FALSE
#df_cat$email_verification[df_cat$email_verification == 1] <- TRUE

#df_cat$work_email_verification[df_cat$work_email_verification == 0] <- FALSE
#df_cat$work_email_verification[df_cat$work_email_verification == 1] <- TRUE

#df_cat$phone_verification[df_cat$phone_verification == 0] <- FALSE
#df_cat$phone_verification[df_cat$phone_verification == 1] <- TRUE

#Creating factors for nominal variables
df_cat$host_since <- factor(df_cat$host_since)
df_cat$host_location <- factor(df_cat$host_location)
df_cat$host_response_time <- factor(df_cat$host_response_time)
df_cat$neighbourhood_group_cleansed <- factor(df_cat$neighbourhood_group_cleansed)
df_cat$property_type <- factor(df_cat$property_type)
df_cat$room_type <- factor(df_cat$room_type)
df_cat$license <- factor(df_cat$license)
df_cat$type_bath <- factor(df_cat$type_bath)
df_cat$first_review <- factor(df_cat$first_review)
df_cat$last_review <- factor(df_cat$last_review)

#Now we group both modified dataframes with categorical and numerical variables
df <- data.frame(df_num, df_cat)
str(df)

#Since our dataset has mixed data we apply the gower distance
distance <- daisy(df, metric="gower")

#gower_dist <- gower.dist(df)
h2 <- hclust(distance, method="ward.D2")
plot(h2)
plot(h2, labels=FALSE)
dend_obj2 <- as.dendrogram(h2)
get_branches_heights(dend_obj2, sort = TRUE, decreasing = TRUE)

plot(as.phylo(h2), type = "unrooted", cex = 0.6, no.margin = TRUE)

# Colouring clusters on the dendrogram
col_dend2 <- color_branches(dend_obj2, h = 13)
plot(col_dend2, leaflab = "none")

p2 <- ggplot(segment(data)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))

ggplotly(p2)

#Now lets decide where to cut the tree
#keeping 3 clusters as we have the highest difference in heights
df$c2 <- cutree(h2,3) #Almost the same as c1
df$c2 <- factor(df$c2)
table(df$c2)

#Gravity centers for the clusters above (using 3 clusters --> c1 and c2)
aggregate(as.data.frame(df_num),list(df$c2),mean)

pairs(df_num[,1:4], col=df$c2)## ???

#UNIVARIATE
library(tidyverse)
ggplot(df, aes(x=c2, y=maximum_nights, color=c2)) +
  geom_boxplot() #explains

ggplot(df, aes(x=c2, y=minimum_nights, color=c2)) +
  geom_boxplot()

ggplot(df, aes(x=c2, y=host_total_listings_count, color=c2)) +
  geom_boxplot()

ggplot(df, aes(x=c2, y=calculated_host_listings_count, color=c2)) +
  geom_boxplot() #explains

ggplot(df, aes(x=c2, y=review_scores_value, color=c2)) + #explains
  geom_boxplot()

ggplot(df, aes(x=c2, y=accommodates, color=c2)) + #explains
  geom_boxplot()

ggplot(df, aes(x=c2, y=num_baths, color=c2)) +
  geom_boxplot()




ggplot(df, aes(x=c2, y=beds, color=c2)) + #explains
  geom_boxplot()


library(ggplot2)
ggplot(data=df, aes(x=c2, y=wifi)) +
  geom_bar(stat="identity", fill="steelblue")

ggplot(df, aes(x = c2, fill = wifi)) + 
  geom_bar(position = "dodge")

ggplot(df, aes(x = c2, fill = heating)) + 
  geom_bar(position = "dodge")

ggplot(df, aes(x = c2, fill = phone_verification)) + 
  geom_bar(position = "dodge") #does not say anything

ggplot(df, aes(x = c2, fill = email_verification)) + 
  geom_bar(position = "dodge") #says something

ggplot(df, aes(x = c2, fill = work_email_verification)) + 
  geom_bar(position = "dodge")

ggplot(df, aes(x = c2, fill = aircon)) + #explains
  geom_bar(position = "dodge")

ggplot(df, aes(x = c2, fill = hairDryer)) + #explains
  geom_bar(position = "dodge")


#BIVARIATE
ggplot(df, aes(x=c2, y=accommodates, fill=wifi)) +
  geom_boxplot()

ggplot(df, aes(x=c2, y=accommodates, fill=heating)) +
  geom_boxplot()




#################################### LIAM ####################################
dfOg$c2 <- df$c2


################################## BEDROOMS ##################################
ggplot(dfOg, aes(x=c2, y=bedrooms, color=c2)) + #explains
  geom_boxplot() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())

h1 <- ggplot(dfOg[df$c2==1,], aes(x=bedrooms)) +
  geom_bar(fill="#f8766d") +
  scale_x_continuous(breaks = seq(1, 10, 1), lim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 5000, 1500), lim = c(0, 6000)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

h2 <- ggplot(dfOg[df$c2==2,], aes(x=bedrooms)) +
  geom_bar(fill="#00ba38") +
  scale_x_continuous(breaks = seq(1, 10, 1), lim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 5000, 1500), lim = c(0, 6000)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

h3 <- ggplot(dfOg[df$c2==3,], aes(x=bedrooms)) +
  geom_bar(fill="#619cff") +
  scale_x_continuous(breaks = seq(1, 10, 1), lim = c(0, 10)) +
  scale_y_continuous(breaks = seq(0, 5000, 1500), lim = c(0, 6000)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

grid.arrange(h1, h2, h3)


# P-value is less than 0.05. We reject the null hypothesis
# Bedrooms in C1 > C2 > C3
pairwise.wilcox.test(dfOg$bedrooms,dfOg$c2, exact=F, alternative="less")

############################### REVIEW VALUE ################################
ggplot(dfOg, aes(x=c2, y=review_scores_value, color=c2)) + #explains
  geom_boxplot() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Review Score Value', color='Cluster')
  

h1 <- ggplot(dfOg[df$c2==1,], aes(x=review_scores_value)) +
  geom_histogram(fill="#f8766d") +
  scale_x_continuous(breaks = seq(0, 5, 1), lim = c(0, 5)) +
  scale_y_continuous(breaks = seq(0, 4000, 1000), lim = c(0, 4000)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

h2 <- ggplot(dfOg[df$c2==2,], aes(x=review_scores_value)) +
  geom_histogram(fill="#00ba38") +
  scale_x_continuous(breaks = seq(0, 5, 1), lim = c(NA, 5)) +
  scale_y_continuous(breaks = seq(0, 4000, 1000), lim = c(0, 4000)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

h3 <- ggplot(dfOg[df$c2==3,], aes(x=review_scores_value)) +
  geom_histogram(fill="#619cff") +
  scale_x_continuous(breaks = seq(0, 5, 1), lim = c(0, 5)) +
  scale_y_continuous(breaks = seq(0, 4000, 1000), lim = c(0, 4000)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

grid.arrange(h1, h2, h3)


# P-value is less than 0.05. We reject the null hypothesis
# Means of review value is not equal in the clusters
pairwise.wilcox.test(dfOg$review_scores_value,dfOg$c2, exact=F)




############################ HOST TOTAL LISTINGS #############################
ggplot(dfOg, aes(x=c2, y=log(host_total_listings_count), color=c2)) + #explains
  geom_boxplot() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Log Num Host Listings', color='Cluster')

h1 <- ggplot(dfOg[df$c2==1,], aes(x=log(host_total_listings_count))) +
  geom_histogram(fill="#f8766d") +
  scale_x_continuous(breaks = seq(0, 9, 1), lim = c(NA, 9)) +
  scale_y_continuous(breaks = seq(0, 2500, 1000), lim = c(0, 2500)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

h2 <- ggplot(dfOg[df$c2==2,], aes(x=log(host_total_listings_count))) +
  geom_histogram(fill="#00ba38") +
  scale_x_continuous(breaks = seq(0, 9, 1), lim = c(NA, 9)) +
  scale_y_continuous(breaks = seq(0, 2500, 1000), lim = c(0, 2500)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

h3 <- ggplot(dfOg[df$c2==3,], aes(x=log(host_total_listings_count))) +
  geom_histogram(fill="#619cff") +
  scale_x_continuous(breaks = seq(0, 9, 1), lim = c(NA, 9)) +
  scale_y_continuous(breaks = seq(0, 2500, 1000), lim = c(0, 2500)) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank())

grid.arrange(h1, h2, h3)

# P-value is less than 0.05. We reject the null hypothesis
# Means of host total listings is not equal in the clusters
pairwise.wilcox.test(dfOg$host_total_listings_count,dfOg$c2, exact=F)

############################ HOST RESPONSE RATE ##############################
ggplot(dfOg, aes(x=c2, y=host_response_rate, color=c2)) + #explains
  geom_boxplot() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())

# P-value is less than 0.05. We reject the null hypothesis
# Means of host total listings is not equal in the clusters
pairwise.wilcox.test(dfOg$host_response_rate,dfOg$c2, exact=F)


############################ HOST RESPONSE TIME ##############################
dfOg$host_response_time <- factor(dfOg$host_response_time, levels = c("within an hour", "within a few hours", "within a day", "a few days or more", "Unknown"))
ggplot(dfOg, aes(x = c2, fill = host_response_time)) + #explains
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Count', color='Host Response Time') 

########################### HOST ACCEPTANCE RATE #############################
ggplot(dfOg, aes(x=c2, y=host_acceptance_rate, color=c2)) + #explains
  geom_boxplot() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Host Acceptance Rate', color='Cluster')

# P-value is less than 0.05. We reject the null hypothesis
# Means of acceptance rate is not equal in the clusters
# C1 > C3 > C2
pairwise.wilcox.test(dfOg$host_acceptance_rate,dfOg$c2, exact=F)

############################ NUMBER OF REVIEWS ###############################
ggplot(dfOg, aes(x=c2, y=log(number_of_reviews), color=c2)) + #explains
  geom_boxplot() +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())

################################### LICENSE ##################################
dfOg$license <- factor(df$license, levels = c("Independent", "Exempt", "Hotel", "Other", "Unknown"))
ggplot(dfOg, aes(x = c2, fill = license)) + #explains
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Count', color='License') 


# Independent is more common in C1 than the rest
# Propability of participation depending on foreign status
prop <- table( dfOg$c2, dfOg$license )
# Margin frequencies:
total <- margin.table(table(dfOg$c2, dfOg$license),2)
# Chi squared test over the expected frequencies:
prop.table( prop ,2 )

# p-value is below 0.05 for both cases
# C1 has more independent licensed hosts than C2 and C3
prop.test( t(prop[1:2, 3]), total[3], correct = F)
prop.test( t(prop[c(1,3), 3]), total[3], correct = F)

# p-value is below 0.05 for both cases
# C3 has more exempt licensed hosts than C1 and C2
prop.test( t(prop[2:3, 1]), total[3], correct = F)
prop.test( t(prop[c(1,3), 1]), total[3], correct = F)

################################ PROPERTY TYPE ###############################
dfOg$property_type <- factor(df$property_type, levels = c("Entire rental unit", "Private room in rental unit", "Others"))
ggplot(dfOg, aes(x = c2, fill = property_type)) + #explains
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Count', color='Property Type') 

# Propability of participation depending on foreign status
prop <- table( dfOg$c2, dfOg$property_type )
# Margin frequencies:
total <- margin.table(table(dfOg$c2, dfOg$property_type),2)
# Chi squared test over the expected frequencies:
prop.table( prop ,2 )

# p-value is below 0.05 for both cases
# Entire rental units are more common in C1 than the rest
prop.test( t(prop[1:2, 1]), total[1], correct = F)
prop.test( t(prop[c(1,3), 1]), total[1], correct = F)

# p-value is below 0.05 for both cases
# Private rooms are more common for C3 than C1 and C2 
prop.test( t(prop[2:3, 3]), total[3], correct = F)
prop.test( t(prop[c(1,3), 3]), total[3], correct = F)


################################## BATH TYPE #################################
ggplot(dfOg, aes(x = c2, fill = type_bath)) + #explains
  geom_bar(position = "dodge", color="black") +
  scale_fill_brewer(palette = "YlGnBu") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())


#################################### AIRCON ##################################
ggplot(df, aes(x = c2, fill = aircon)) + #explains
  geom_bar(position = "dodge") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Count', fill='Aircon')

# Propability of participation depending on foreign status
prop <- table( dfOg$c2, dfOg$aircon )
# Margin frequencies:
total <- margin.table(table(dfOg$c2, dfOg$aircon),2)
# Chi squared test over the expected frequencies:
prop.table( prop ,2 )

# p-value is below 0.05 for both cases
# X1 listings are more likely to have aircon than C2 and C3
prop.test( t(prop[1:2, 2]), total[2], correct = F)
prop.test( t(prop[c(1,3), 2]), total[2], correct = F)


###################################### TV ####################################
ggplot(dfOg, aes(x = c2, fill = tv)) + #explains
  geom_bar(position = "dodge") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank())

################################# OUtdoor space ##############################
ggplot(dfOg, aes(x = c2, fill = outdoorSpace)) + #explains
  geom_bar(position = "dodge") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank()) +
  labs(x='Cluster', y='Count', fill='Outdoor Space')


# Propability of participation depending on foreign status
prop <- table( dfOg$c2, dfOg$outdoorSpace )
# Margin frequencies:
total <- margin.table(table(dfOg$c2, dfOg$outdoorSpace),2)
# Chi squared test over the expected frequencies:
prop.table( prop ,2 )

# p-value is below 0.05 for both cases
# X1 listings are more likely to have aircon than C2 and C3
prop.test( t(prop[1:2, 2]), total[2], correct = F)
prop.test( t(prop[c(1,3), 2]), total[2], correct = F)


############################ P-values per cluster ############################
#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum,P){
  #freq dis of fac
  nk <- as.vector(table(P)); 
  n <- sum(nk); 
  #mitjanes x grups
  xk <- tapply(Xnum,P,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk))); 
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}

ValorTestXquali <- function(P,Xquali){
  taula <- table(P,Xquali);
  n <- sum(taula); 
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm; 
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj))); 
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0]; 
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(P)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
}


K<-dim(df)[2]
par(ask=TRUE)


#P must contain the class variable

P<-df$c2
nameP<-"classe"


nc<-length(levels(factor(P)))
nc
pvalk <- matrix(data=0,nrow=nc,ncol=K, dimnames=list(levels(P),names(df)))
nameP<-"Class"
n<-dim(df)[1]


for(k in 1:K){
  if (is.numeric(df[,k])){
    print("Numeric:")
    print(pvalk[,k])
    pvalk[,k] <- ValorTestXnum(df[,k], P)    
  } 
  
  #else{
    #ojo porque si la variable es true o false la identifica amb el tipus Logical i
    #aquest no te levels, por tanto, coertion preventiva
    
   # df[,k]<-as.factor(df[,k])
    
    #calcular els pvalues de les quali
    #print("Qualitative:")
    #print(pvalk[,k])
    #print(ValorTestXquali(P, df[,k]))
  #}
}#endfor




#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:",levels(as.factor(P))[c]));
    print(sort(pvalk[c,]), digits=3) 
  }
}


# Most of the variables are important to define the clusters
# Cluster 1: All
# Cluster 2: All but price
# Cluster 3: All except number_of_reviews, latitude, calculated_host_listings_count_private_rooms
