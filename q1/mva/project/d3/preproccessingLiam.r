#install.packages('sets')
library(sets)
#install.packages("hash")
library(hash)


library(readr)
listings <- read_csv("datasets/listings.csv")
names(listings)



library(readxl)
df <- read_excel("listings_D3_V6.xlsx")

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


for(att in names(df)) {
 print(summary(factor(df[[att]])))
}


df$beds[df$num_baths == -1]
df$bedrooms[df$num_baths == -1]

for (i in 1:5) {
  print(i)
  print(mean(df$num_baths[df$bedrooms == i]))
}
  
install.packages('mice')
library(mice)

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

df$host_response_rate[is.na(df$host_response_rate)] <- mean(df$host_response_rate, na.rm = TRUE)
df$host_acceptance_rate[is.na(df$host_acceptance_rate)] <- mean(df$host_acceptance_rate, na.rm = TRUE)

hist(x=df$host_response_rate)
hist(x=df$host_acceptance_rate)



cols <- c("num_baths", "bedrooms", "beds", "minimum_nights", "number_of_reviews",
          "number_of_reviews_ltm", "number_of_reviews_l30d")

hist(x=df$price[df$price < 1000])
boxplot(x=df$price[df$price < 1000])

for (col in cols) {
  print(col)
  temp <- df[[col]]
  print("Cutoff")
  print(mean(temp) + 3*sd(temp))
  outliers <- sum(temp > (mean(temp) + 3*sd(temp)))
  print("Number of outliers")
  print(outliers)
  print("Ratio of outliers")
  print(outliers/length((temp)))
  cat("\n")
}


##FUMADA MUY GRANDE
nums <- unlist(lapply(df, is.numeric), use.names = FALSE)  

print("Correlation for response rate")
corrResp <- cor(x=df$host_response_rate, y=test, use = "complete.obs", method="spearman")

print("Correlation for response rate")
corrAcc  <- cor(x=df$host_acceptance_rate, y=test, use = "complete.obs", method="spearman")

corrResp[abs(corrResp)>0.3 & !is.na(corrResp)]
corrAcc[abs(corrAcc)>0.5 & !is.na(corrAcc)]

corrAcc

print(sum(is.na(df$host_response_rate)))
print(sum(is.na(df$host_acceptance_rate)))

install.packages("caret")
library(caret)
dummy <- dummyVars(" ~ .", data=df)

test <- data.frame(predict(dummy, newdata = df)) 




# We convert the amenities into a dictionary to be able to identify the most popular
# amenities
amenities <- listings$amenities
numAmen <- hash()

for(instance in amenities) {
  tempAmen <- strsplit(instance, split=",")

  for(amenity in tempAmen[[1]]) {
    amenity <- tolower(gsub("([^A-Za-z0-9 ])+", "", x = trimws(amenity)))
    if(amenity != "" ) {
      if (is.null(numAmen[[amenity]])) {
        numAmen[[amenity]] <- 1
      } else {
        numAmen[[amenity]] <- numAmen[[amenity]] + 1 
      } 
    }
  }
}

# Lising 50 most common amenities
listNumAmen <- unlist(as.list(numAmen))
indexes <- order(listNumAmen, decreasing=TRUE)[1:50]
print(listNumAmen[indexes])
print(length(listNumAmen))


# The following step is to determine the patterns we must use to generalise the
# list of amenities
keysNumAmen <- keys(numAmen)

# Showing amenities with wifi 
keysNumAmen[grepl("wifi", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with hair dryer 
keysNumAmen[grepl("hair dryer", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with air conditioning or manufactorers 
keysNumAmen[grepl("air condition", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("sung conditioner", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("fuji", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("lg conditioner", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("toshiba conditioner", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("mitsubishi conditioner", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with heating or similar 
keysNumAmen[grepl("heating", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("portable heater", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with TV
keysNumAmen[grepl("tv", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with Host greets you or similar
keysNumAmen[grepl("host", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with outdoor areas
keysNumAmen[grepl("patio", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[grepl("yard", keysNumAmen, ignore.case = TRUE)]

# Showing amenities with pool
keysNumAmen[grepl(" pool", keysNumAmen, ignore.case = TRUE)]
keysNumAmen[keysNumAmen=="pool"]

# Showing amenities with barbecue
keysNumAmen[grepl("bbq", keysNumAmen, ignore.case = TRUE)]

# Showing long stays allowed
keysNumAmen[grepl("long", keysNumAmen, ignore.case = TRUE)]

# Showing on premises parking allowed
keysNumAmen[grepl("on premises", keysNumAmen, ignore.case = TRUE)]