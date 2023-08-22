#install.packages('httr')
#install.packages("writexl")
library(httr)
library("writexl")

#HOST LOCATION
#--------------------------------------------------------------------------------

AIRBNB_Variables_Used_D3$host_location
AIRBNB_Variables_Used_D3$host_location <- sub('.*,\\s*', '', AIRBNB_Variables_Used_D3$host_location)

AIRBNB_Variables_Used_D3$host_location[grepl("barcelona", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("spain", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("Sepulveda", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("Carrer Sant Vicen√ß", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("espa", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("catal", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("17300", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("madri", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("Sp", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE) |
                                   grepl("Premi√† de Mar", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE)] <- "ES"

AIRBNB_Variables_Used_D3$host_location[grepl("ital", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE)] <- "IT"

AIRBNB_Variables_Used_D3$host_location[grepl("fran", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE)] <- "FR"

AIRBNB_Variables_Used_D3$host_location[grepl("kingdom", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE)] <- "UK"

AIRBNB_Variables_Used_D3$host_location[grepl("states", AIRBNB_Variables_Used_D3$host_location, ignore.case=TRUE)] <- "UK"

AIRBNB_Variables_Used_D3$host_location[is.na(AIRBNB_Variables_Used_D3$host_location)] <- "Unknown"

countries = c("ES","FR","IT","GB","US","Unknown")
`%!in%` <- Negate(`%in%`)

for (val in AIRBNB_Variables_Used_D3$host_location)
{
  if (val %!in% countries)
  {
    AIRBNB_Variables_Used_D3$host_location <- replace(AIRBNB_Variables_Used_D3$host_location,AIRBNB_Variables_Used_D3$host_location==val,"Other")
  }
}

AIRBNB_Variables_Used_D3$host_location <- AIRBNB_Variables_Used_D3$host_location

#-------------------------------------------------------------------------------
#ADDING AMENITIES COLUMN (FROM airbnb_stats.xlsx)
AIRBNB_Variables_Used_D3$amenities <- AIRBNB_Stats$amenities
names(AIRBNB_Variables_Used_D3)

summary(factor(AIRBNB_Variables_Used_D3$host_since))

#-------------------------------------------------------------------------------
#HOST SINCE

AIRBNB_host_since <- AIRBNB_Variables_Used_D3$host_since
AIRBNB_host_since <- format(AIRBNB_host_since, format="%Y")
AIRBNB_host_since
AIRBNB_Variables_Used_D3$host_since <- AIRBNB_host_since

head(AIRBNB_Variables_Used_D3)

#-------------------------------------------------------------------------------
#FIRST AND LAST REVIEW

AIRBNB_first_review <- AIRBNB_Variables_Used_D3$first_review
AIRBNB_last_review <- AIRBNB_Variables_Used_D3$last_review
AIRBNB_first_review <- format(AIRBNB_first_review, format="%Y")
AIRBNB_last_review <- format(AIRBNB_last_review, format="%Y")
AIRBNB_first_review
AIRBNB_last_review
AIRBNB_Variables_Used_D3$first_review <- AIRBNB_first_review
AIRBNB_Variables_Used_D3$last_review <- AIRBNB_last_review

head(AIRBNB_Variables_Used_D3)

#-------------------------------------------------------------------------------
#PRICE TO NUMBER

AIRBNB_price <- AIRBNB_Variables_Used_D3$price
head(AIRBNB_price)
AIRBNB_price <- as.numeric(gsub("[\\$,]","",AIRBNB_price))
head(AIRBNB_price,n=100)

AIRBNB_Variables_Used_D3$price <- AIRBNB_price

#-------------------------------------------------------------------------------
#DELETE 3 LAST ROWS
AIRBNB_T_F <- AIRBNB_Variables_Used_D3
AIRBNB_T_F <- is.na(AIRBNB_T_F)
AIRBNB_T_F
AIRBNB_T_F$count_na <- rowSums(is.na(AIRBNB_T_F))

summary(factor(AIRBNB_T_F$count_na))

#Checking the count_na column we see that there are 3 rows with more than 40 NA values. We delete them

AIRBNB_T_F<-AIRBNB_T_F[!(AIRBNB_T_F$count_na==41 | AIRBNB_T_F$count_na==42),]
AIRBNB_T_F
AIRBNB_Variables_Used_D3 <- AIRBNB_T_F

write_xlsx(AIRBNB_Variables_Used_D3,"AIRBNB_Variables_Used_D3.xlsx")
