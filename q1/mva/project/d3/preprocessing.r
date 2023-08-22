#install.packages('sets')
library(sets)
#install.packages("hash")
library(hash)

#install.packages("writexl")
library("writexl")



library(readr)
library(readxl)
df <- read_excel("listings.xlsx")
amenities <- df$amenities
df2 <- read_excel("AIRBNB_Variables_Used_D2.xlsx")
#names(df)
#names(df2)

df <- df[names(df2)]

#ADDING AMENITIES COLUMN (FROM airbnb_stats.xlsx)
#-------------------------------------------------------------------------------
df$amenities <- amenities
names(df)


#HOST VERIFICATIONS
#-------------------------------------------------------------------------------
# Creation of 3 new binary categorical variables from host_verifications attribute.

#The three new attributes are the following:
#  a. phone_verification (0 = 'no', 1 = 'yes')
#  b. email_verification (0 = 'no', 1 = 'yes')
#  c. work_email_verification (0 = 'no', 1 = 'yes')

df$phone_verification <- 0
df$email_verification <- 0
df$work_email_verification <-0
print(length(df$host_verifications))

for( i in 1:length(df$host_verifications)){
  if (df$host_verifications[i] == 'none'){next}
  if (grepl('phone', df$host_verifications[i]) == TRUE){
    df$phone_verification[i] <- 1
  }
  
  if (grepl('email', df$host_verifications[i]) == TRUE){
    df$email_verification[i] <- 1
  }
  
  if (grepl('work_email', df$host_verifications[i]) == TRUE){
    df$work_email_verification[i] <- 1
  }
}


df$host_verifications <- NULL

#HOST_sINCE 
#-------------------------------------------------------------------------------
# Change host_since variable

df$host_since <- format(df$host_since, format="%Y")

format(df$host_since, format="%Y")
# Change property_type variable

for(i in 1:length(df$property_type)) {         
  if(df$property_type[i]!= "Entire rental unit" & df$property_type[i]!= "Private room in rental unit"){
    df$property_type[i] = 'Others'
  }
}


## Change neighbourhood_group_cleansed variable

for(i in 1:length(df$neighbourhood_group_cleansed)) {         
  if(substr(df$neighbourhood_group_cleansed[i], 1, 2) == "Gr"){
    df$neighbourhood_group_cleansed[i] = "Gracia"
  }
  else if (df$neighbourhood_group_cleansed[i] == "Horta-GuinardÃ³"){
    df$neighbourhood_group_cleansed[i] = "Horta-Guinardo"
  }
  else if (substr(df$neighbourhood_group_cleansed[i], 1, 6) == "Sant M"){
    df$neighbourhood_group_cleansed[i] = "Sant Marti"
  }
  else if (df$neighbourhood_group_cleansed[i] == "Sants-MontjuÃ¯c"){
    df$neighbourhood_group_cleansed[i] = "Sants-Montjuic"
  }
  else if (substr(df$neighbourhood_group_cleansed[i], 1, 5) == "Sarri"){
    df$neighbourhood_group_cleansed[i] = "Sarria-Sant Gervasi"
  }
}


#HOST LOCATION
#--------------------------------------------------------------------------------

df$host_location
df$host_location <- sub('.*,\\s*', '', df$host_location)

df$host_location[grepl("barcelona", df$host_location, ignore.case=TRUE) |
                                         grepl("spain", df$host_location, ignore.case=TRUE) |
                                         grepl("Sepulveda", df$host_location, ignore.case=TRUE) |
                                         grepl("Carrer Sant Vicen√ß", df$host_location, ignore.case=TRUE) |
                                         grepl("espa", df$host_location, ignore.case=TRUE) |
                                         grepl("catal", df$host_location, ignore.case=TRUE) |
                                         grepl("17300", df$host_location, ignore.case=TRUE) |
                                         grepl("madri", df$host_location, ignore.case=TRUE) |
                                         grepl("Sp", df$host_location, ignore.case=TRUE) |
                                         grepl("Premi√† de Mar", df$host_location, ignore.case=TRUE)] <- "ES"

df$host_location[grepl("ital", df$host_location, ignore.case=TRUE)] <- "IT"

df$host_location[grepl("fran", df$host_location, ignore.case=TRUE)] <- "FR"

df$host_location[grepl("kingdom", df$host_location, ignore.case=TRUE)] <- "UK"

df$host_location[grepl("states", df$host_location, ignore.case=TRUE)] <- "US"

df$host_location[is.na(df$host_location)] <- "Unknown"

countries = c("ES","FR","IT","GB","US","Unknown")
`%!in%` <- Negate(`%in%`)

for (val in df$host_location)
{
  if (val %!in% countries)
  {
    df$host_location <- replace(df$host_location,df$host_location==val,"Other")
  }
}

df$host_location <- df$host_location

#FIRST AND LAST REVIEW
#-------------------------------------------------------------------------------

AIRBNB_first_review <- df$first_review
AIRBNB_last_review <- df$last_review
AIRBNB_first_review <- format(AIRBNB_first_review, format="%Y-%m")
AIRBNB_last_review <- format(AIRBNB_last_review, format="%Y-%m")
AIRBNB_first_review
AIRBNB_last_review
df$first_review <- AIRBNB_first_review
df$last_review <- AIRBNB_last_review

head(df)

#PRICE TO NUMBER
#-------------------------------------------------------------------------------

AIRBNB_price <- df$price
head(AIRBNB_price)
AIRBNB_price <- as.numeric(gsub("[\\$,]","",AIRBNB_price))
head(AIRBNB_price,n=100)

df$price <- AIRBNB_price

# LICENSE
#-------------------------------------------------------------------------------
#HUTB -> Independent
#HB -> Hotel
#Exempt -> Exempt  
#Other -> Other
#NA -> Unknown

license <- df$license
#Change all license containing HUT and HTU (includes hutb, hutg and typos) to independent
license[grepl("hu", license, ignore.case=TRUE) |
          grepl("htu", license, ignore.case=TRUE) |
          grepl("h u", license, ignore.case=TRUE)] <- "Independent"

#Change all license containing HB to hotel
license[grepl("hb", license, ignore.case=TRUE)] <- "Hotel"

#Change license long term rentals, exempto and special cases (Ay. Horta-Ginardo) to exempt
license[grepl("exempt", license, ignore.case=TRUE) |
          grepl("horta", license, ignore.case=TRUE) |
          grepl("long", license, ignore.case=TRUE) |
          grepl("período", license, ignore.case=TRUE) |
          grepl("mini", license, ignore.case=TRUE) |
          grepl("31 d", license, ignore.case=TRUE) |
          grepl("32 d", license, ignore.case=TRUE) |
          grepl("32 n", license, ignore.case=TRUE) |
          grepl("media", license, ignore.case=TRUE) |
          grepl("monthly", license, ignore.case=TRUE) |
          grepl("larga", license, ignore.case=TRUE) |
          grepl("media", license, ignore.case=TRUE)] <- "Exempt"

#Change NA to unknown
license[is.na(license)] <- "Unknown"

#Change everything else to Other
license[!grepl("independent", license, ignore.case=TRUE) &
          !grepl("hotel", license, ignore.case=TRUE) &
          !grepl("exempt", license, ignore.case=TRUE) &
          !grepl("unknown", license, ignore.case=TRUE)] <- "Other"

summary(factor(license))
df$license <- license

# BATHROOM_TEXT TO NUM_BATHS
#-------------------------------------------------------------------------------
#Extracts the number of bathrooms per listing. Converts df that contain "half"
#in bathroom_text to 0.5 and NA's are converted to -1 to be dealt with later for imputation.
num_baths <- df$bathrooms_text
num_baths[grepl("half", num_baths, ignore.case = TRUE)] <- 0.5
num_baths[is.na(num_baths)] <- -1
num_baths <- as.numeric(unlist(regmatches(num_baths, gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*",num_baths, perl = TRUE))))

summary(factor(num_baths))
df$num_baths <- num_baths


# BATHROOM_TEXT TO TYPE_BATHS
#-------------------------------------------------------------------------------
#We classify the bathrooms into 3 different types:
# - Private if bathroom_text contains private
# - Shared if bathroom_text contains shared
# - Unknown if bathroom_text doesn't specify if the bathrooms are private or shared. Also includes NA's
type_bath <- df$bathrooms_text
type_bath[grepl("private", type_bath, ignore.case = TRUE)] <- "Private"
type_bath[grepl("shared", type_bath, ignore.case = TRUE)] <- "Shared"
type_bath[!grepl("private", type_bath, ignore.case = TRUE) &
            !grepl("shared", type_bath, ignore.case = TRUE)] <- "Unknown"

summary(factor(type_bath))
df$type_bath <- type_bath

# AMENITIES
#-------------------------------------------------------------------------------
# Now that we have identified the patterns, we will filter the amenities list per
# instance
amenities <- df$amenities
# Remove first and last 2 characters to facilitate parsing
amenities <- substr(amenities, 2, nchar(amenities)-2)

# Split string by commas into a list
amenities <- strsplit(amenities, split=",")

# Only keep alphanumeric characters i.e. remove "\"
prevLength <- length(amenities)
for(i in 1:(length(amenities))) {
  amenities[[i]] <- tolower(gsub("([^A-Za-z0-9 ])+", "", x = trimws(amenities[[i]])))
  
  # Generalising values for our selected amenities
  amenities[[i]][grepl("wifi", amenities[[i]], ignore.case = TRUE)] <- "wifi"
  
  amenities[[i]][grepl("hair dryer", amenities[[i]], ignore.case = TRUE)] <- "hair dryer"
  
  amenities[[i]][grepl("air condition", amenities[[i]], ignore.case=TRUE) |
                   grepl("sung conditioner", amenities[[i]], ignore.case=TRUE) |
                   grepl("fuji", amenities[[i]], ignore.case=TRUE) |
                   grepl("lg conditioner", amenities[[i]], ignore.case=TRUE) |
                   grepl("toshiba conditioner", amenities[[i]], ignore.case=TRUE) |
                   grepl("mitsubishi conditioner", amenities[[i]], ignore.case=TRUE)] <- "air conditioning"
  
  amenities[[i]][grepl("heating", amenities[[i]], ignore.case = TRUE) |
                   grepl("portable heater", amenities[[i]], ignore.case=TRUE)] <- "heating"
  
  amenities[[i]][grepl("patio", amenities[[i]], ignore.case = TRUE) |
                   grepl("yard", amenities[[i]], ignore.case=TRUE)] <- "outdoor space"
  
  amenities[[i]][grepl(" pool", amenities[[i]], ignore.case = TRUE) |
                   "pool" == amenities[[i]]] <- "pool"
  
  amenities[[i]][grepl("bbq", amenities[[i]], ignore.case = TRUE)] <- "bbq"
  
  amenities[[i]][grepl("on premises", amenities[[i]], ignore.case = TRUE)] <- "parking on premises"
  
  amenities[[i]][grepl("tv", amenities[[i]], ignore.case = TRUE)] <- "tv"
  
  
  # Converting to set to remove duplicates and perform an intersection. Then, we
  # convert it to a list because it is easier to deal with. We have a joker to deal
  # with empty sets
  amenities[[i]] <- unlist(as.list(set_union(set_intersection(as.set(amenities[[i]]), 
                                                              set("wifi", "hair dryer", "air conditioning", "heating", 
                                                                  "outdoor space", "pool", "bbq", "long term stays allowed", 
                                                                  "parking on premises", "tv", "host greets you")), set("Joker"))))
  
}

# We will create new boolean features of the most interesting amenities
df$wifi             <- grepl("wifi", amenities)
df$longTermStays    <- grepl("long term stays allowed", amenities)
df$hairDryer        <- grepl("hair dryer", amenities)
df$aircon           <- grepl("air conditioning", amenities)
df$heating          <- grepl("heating", amenities)
df$tv               <- grepl("tv", amenities)
df$hostGreets       <- grepl("host greets you", amenities)
df$outdoorSpace     <- grepl("outdoor space", amenities)
df$parkingOnPremise <- grepl("parking on premises", amenities)
df$pool             <- grepl("pool", amenities)
df$bbq              <- grepl("bbq", amenities)

names(df)

summary(factor(df$bbq))

# We remove the unnecessary features now
df$amenities <- NULL
df$bathrooms_text <- NULL

df

write.csv(df, "listings_D3_v7_pre.csv", row.names = FALSE)
