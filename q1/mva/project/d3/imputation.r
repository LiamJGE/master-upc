library(readr)
#install.packages('mice')
library(mice)

df <- read_csv("datasets/listings_D3_V7_pre.csv")

######################################LIAM######################################
# Imputing Unknown as ES because the great majority of cases are ES
df$host_location[grepl("unknown", df$host_location, ignore.case=TRUE)] <- "ES"
summary(factor(df$host_location))

# Imputing NA's as Unknown because the host chooses to show that information
sum(is.na(df$host_response_time) | df$host_response_time == "N/A")
df$host_response_time[is.na(df$host_response_time) | df$host_response_time == "N/A"] <- "Unknown"
summary(factor(df$host_response_time))


#Change all t and f to TRUE and FALSE
for(att in names(df)) {
  df[[att]][df[[att]] == 'f'] <- FALSE
  df[[att]][df[[att]] == 't'] <- TRUE
}

# Impute NA's to False because we have checked the 5 cases manually
df$host_is_superhost[is.na(df$host_is_superhost) | df$host_is_superhost == "N/A"] <- FALSE
summary(factor(df$host_is_superhost))



df$num_baths[df$num_baths == -1] <- NA

imp <- mice(df[ , c("num_baths", "beds", "bedrooms")]) 

df$num_baths <- complete(imp)$num_baths
df$beds      <- complete(imp)$beds
df$bedrooms  <- complete(imp)$bedrooms

summary(factor(df$num_baths))
summary(factor(df$beds))
summary(factor(df$bedrooms))


summary(factor(df$host_response_rate))




df$host_response_rate[df$host_response_rate == "N/A"] <- NA
df$host_acceptance_rate[df$host_acceptance_rate == "N/A"] <- NA
df$host_response_rate <- as.numeric(df$host_response_rate)
df$host_acceptance_rate <- as.numeric(df$host_acceptance_rate)

normDis <- rnorm(n=sum(is.na(df$host_response_rate)), mean=mean(df$host_response_rate, na.rm = TRUE), sd=sd(df$host_response_rate, na.rm = TRUE))
df$host_response_rate[is.na(df$host_response_rate)] <- round(normDis, digits=2)
df$host_response_rate[df$host_response_rate > 1] <- 1

normDis <- rnorm(n=sum(is.na(df$host_acceptance_rate)), mean=mean(df$host_acceptance_rate, na.rm = TRUE), sd=sd(df$host_acceptance_rate, na.rm = TRUE))
df$host_acceptance_rate[is.na(df$host_acceptance_rate)] <- round(normDis, digits=2)
df$host_acceptance_rate[df$host_acceptance_rate > 1] <- 1

# There is a case that has its price is equal to 0
# This case will be removed because price is our target variable
df <- df[df$price > 0, ]


######################################CLARA######################################
#The rows with NA in first_review are exactly the same as in last_review and in reviews_per_month.
#We will impute them as Unreviewed

df$first_review[is.na(df$first_review)] <- "Unreviewed"
df$last_review[is.na(df$last_review)] <- "Unreviewed"

df$reviews_per_month[is.na(df$reviews_per_month)] <- 0


######################################MARÇAL######################################
zeroRev <- df$number_of_reviews == 0
df$review_scores_accuracy[zeroRev] <- 0
df$review_scores_rating[zeroRev] <- 0
df$review_scores_cleanliness[zeroRev] <- 0
df$review_scores_checkin[zeroRev] <- 0
df$review_scores_communication[zeroRev] <- 0
df$review_scores_location[zeroRev] <- 0
df$review_scores_value[zeroRev] <- 0



#review_scores_rating --> we've seen that the average is not well computed in this column. we'll re-compute it.
df$count_na <- rowSums(is.na(df[30:35])) #NA count of all reviews (except for review_scores_rating) as it'll be the mean of the following reviews
df$row_mean <- round(rowMeans(df[30:35],na.rm=T),digits=2) #Mean of all the reviews
df$row_mean[is.nan(df$row_mean)]<-NA #Changing NaN to NA

#1 rows with 1 NA  --> Inpute it with the mean of the rows
#3 rows with 2 NA  --> Inpute them with the mean of the rows
#6 rows with 3 NA  --> Inpute them with the mean of the rows
#Rest with 6 or more NA (as sometimes review_scores_rating was 0)

#From here we replace all reviews with its mean. Rows that had all NULLS will keep the NA value 
df$review_scores_accuracy[is.na(df$review_scores_accuracy)] <- df$row_mean[is.na(df$review_scores_accuracy)]
df$review_scores_accuracy[is.nan(df$review_scores_accuracy)]<-NA

df$review_scores_cleanliness[is.na(df$review_scores_cleanliness)] <- df$row_mean[is.na(df$review_scores_cleanliness)]
df$review_scores_cleanliness[is.nan(df$review_scores_cleanliness)]<-NA

df$review_scores_checkin[is.na(df$review_scores_checkin)] <- df$row_mean[is.na(df$review_scores_checkin)]
df$review_scores_checkin[is.nan(df$review_scores_checkin)]<-NA

df$review_scores_communication[is.na(df$review_scores_communication)] <- df$row_mean[is.na(df$review_scores_communication)]
df$review_scores_communication[is.nan(df$review_scores_communication)]<-NA

df$review_scores_location[is.na(df$review_scores_location)] <- df$row_mean[is.na(df$review_scores_location)]
df$review_scores_location[is.nan(df$review_scores_location)]<-NA

df$review_scores_value[is.na(df$review_scores_value)] <- df$row_mean[is.na(df$review_scores_value)]
df$review_scores_value[is.nan(df$review_scores_value)]<-NA

#AIRBNB_scores_rating_subset2 <- round(df[(df$count_na > 0 & df$count_na < 6 ),c(30:35,60)],digits=2)
#df$count_na2 <- rowSums(is.na(df[30:35]))

##################################################################################################################

#As we didn't have information about the reviews in those that had all revies NULL, we inpute them by the mean of each column
normDis <- rnorm(n=sum(is.na(df$review_scores_accuracy)), mean=mean(df$review_scores_accuracy, na.rm = TRUE), sd=sd(df$review_scores_accuracy, na.rm = TRUE))
df$review_scores_accuracy[is.na(df$review_scores_accuracy)] <- round(normDis, digits=2)
df$review_scores_accuracy[df$review_scores_accuracy > 5] <- 5
df$review_scores_accuracy[df$review_scores_accuracy < 0] <- 0

normDis <- rnorm(n=sum(is.na(df$review_scores_cleanliness)), mean=mean(df$review_scores_cleanliness, na.rm = TRUE), sd=sd(df$review_scores_cleanliness, na.rm = TRUE))
df$review_scores_cleanliness[is.na(df$review_scores_cleanliness)] <- round(normDis, digits=2)
df$review_scores_cleanliness[df$review_scores_cleanliness > 5] <- 5
df$review_scores_cleanliness[df$review_scores_cleanliness < 0] <- 0

normDis <- rnorm(n=sum(is.na(df$review_scores_checkin)), mean=mean(df$review_scores_checkin, na.rm = TRUE), sd=sd(df$review_scores_checkin, na.rm = TRUE))
df$review_scores_checkin[is.na(df$review_scores_checkin)] <- round(normDis, digits=2)
df$review_scores_checkin[df$review_scores_checkin > 5] <- 5
df$review_scores_checkin[df$review_scores_checkin < 0] <- 0

normDis <- rnorm(n=sum(is.na(df$review_scores_communication)), mean=mean(df$review_scores_communication, na.rm = TRUE), sd=sd(df$review_scores_communication, na.rm = TRUE))
df$review_scores_communication[is.na(df$review_scores_communication)] <- round(normDis, digits=2)
df$review_scores_communication[df$review_scores_communication > 5] <- 5
df$review_scores_communication[df$review_scores_communication < 0] <- 0

normDis <- rnorm(n=sum(is.na(df$review_scores_location)), mean=mean(df$review_scores_location, na.rm = TRUE), sd=sd(df$review_scores_location, na.rm = TRUE))
df$review_scores_location[is.na(df$review_scores_location)] <- round(normDis, digits=2)
df$review_scores_location[df$review_scores_location > 5] <- 5
df$review_scores_location[df$review_scores_location < 0] <- 0

normDis <- rnorm(n=sum(is.na(df$review_scores_value)), mean=mean(df$review_scores_value, na.rm = TRUE), sd=sd(df$review_scores_value, na.rm = TRUE))
df$review_scores_value[is.na(df$review_scores_value)] <- round(normDis, digits=2)
df$review_scores_value[df$review_scores_value > 5] <- 5
df$review_scores_value[df$review_scores_value < 0] <- 0

##################################################################################################################

#Inputation of review_scores_rating taking into account the mean of the inputed (or not) rows.

df$review_scores_rating <- round(rowMeans(df[30:35], na.rm = TRUE), digits = 2)

df$count_na <- NULL
df$row_mean <- NULL


################################################################################
#Imputation of particular instances

df$host_total_listings_count[is.na(df$host_total_listings_count) & df$neighbourhood_group_cleansed == "Sants-Montjuic"] <- 3
df$host_total_listings_count[is.na(df$host_total_listings_count) & df$neighbourhood_group_cleansed == "Eixample"] <- 1
df$host_total_listings_count[is.na(df$host_total_listings_count) & df$neighbourhood_group_cleansed == "Les Corts"] <- 1

df$host_since[is.na(df$host_since) & df$neighbourhood_group_cleansed == "Sants-Montjuic"] <- 2013
df$host_since[is.na(df$host_since) & df$neighbourhood_group_cleansed == "Eixample"] <- 2012
df$host_since[is.na(df$host_since) & df$neighbourhood_group_cleansed == "Les Corts"] <- 2015

df$host_has_profile_pic[is.na(df$host_has_profile_pic) & df$neighbourhood_group_cleansed == "Sants-Montjuic"] <- TRUE
df$host_has_profile_pic[is.na(df$host_has_profile_pic) & df$neighbourhood_group_cleansed == "Eixample"] <- FALSE
df$host_has_profile_pic[is.na(df$host_has_profile_pic) & df$neighbourhood_group_cleansed == "Les Corts"] <- TRUE

df$host_identity_verified[is.na(df$host_identity_verified) & df$neighbourhood_group_cleansed == "Sants-Montjuic"] <- TRUE
df$host_identity_verified[is.na(df$host_identity_verified) & df$neighbourhood_group_cleansed == "Eixample"] <- FALSE
df$host_identity_verified[is.na(df$host_identity_verified) & df$neighbourhood_group_cleansed == "Les Corts"] <- FALSE


################################################################################
numCols <- which(sapply(df, is.numeric))
df1 <- df[numCols] #df with only quantitative variables

cor(df[numCols])

sum(is.na(df$host_total_listings_count))


notEqualListingsCount = df$calculated_host_listings_count != 
  df$calculated_host_listings_count_entire_homes +
  df$calculated_host_listings_count_private_rooms +
  df$calculated_host_listings_count_shared_rooms

df$calculated_host_listings_count[notEqualListingsCount] <- df$calculated_host_listings_count_entire_homes[notEqualListingsCount] +
                                                            df$calculated_host_listings_count_private_rooms[notEqualListingsCount] +
                                                            df$calculated_host_listings_count_shared_rooms[notEqualListingsCount] 

df$host_total_listings_count[df$host_total_listings_count == 0] <- df$calculated_host_listings_count[df$host_total_listings_count == 0]


sum(is.na(df))


write.csv(df, "datasets/listings_D3_v8.csv", row.names = FALSE)
