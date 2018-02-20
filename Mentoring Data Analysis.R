getwd()

library(dplyr)

# Treat zipcode as string for NJ & CT zip codes

FY16data <- read.csv("ClientDemoGraphicsFY16-Priv.csv", colClasses=c("cl_location_zipcode"="character"))
FY17data <- read.csv("ClientDemoGraphicsFY17-Priv.csv", colClasses=c("cl_location_zipcode"="character"))

dim(FY16data)
summary(FY16data)

# Structure for the entire data frame
str(FY16data, list.len=ncol(FY16data))

dim(FY17data)
summary(FY17data)
str(FY17data, list.len=ncol(FY17data))

levels(FY16data$se_session_type)
levels(FY16data$se_counseling_type)
levels(FY16data$se_chapter_loc_name)
levels(FY16data$cl_business_type_code)
levels(FY16data$cl_business_type_desc)
levels(FY16data$cl_gender)
levels(FY16data$cl_race)
levels(FY16data$cl_ethnicity_value)
levels(FY16data$cl_legalentity_value)
levels(FY16data$cl_legalentity_other_value)

levels(FY17data$se_session_type)
levels(FY17data$se_counseling_type)
levels(FY17data$se_chapter_loc_name)
levels(FY17data$cl_business_type_code)
levels(FY17data$cl_business_type_desc)
levels(FY17data$cl_gender)
levels(FY17data$cl_race)
levels(FY17data$cl_ethnicity_value)
levels(FY17data$cl_legalentity_value)
levels(FY17data$cl_legalentity_other_value)

county <- read.csv("county_table.csv", colClasses=c("zip_prefix"="character"))
str(county)
hood <- read.csv("zip_table.csv", colClasses=c("zip"="character"))
str(hood)

# Function to determine no variance
noVar <- function(x){
  return(length(unique(x))==1)
}

# temp <- apply(FY16data, 2, noVar)
# temp
# is.vector(temp)
# FY16noVar <- FY16data[, !temp]

# Get rid of columns that has no variance

FY16var <- FY16data[, !apply(FY16data, 2, noVar)]
dim(FY16var)

FY17var <- FY17data[, !apply(FY17data, 2, noVar)]
dim(FY17var)

# Add columns for county, neighborhood, borough

FY16var$cl_location_zipcode_prefix <- substr(FY16var$cl_location_zipcode, 1, 3)
ncol(FY16var)
FY16var <- merge(FY16var, county, by.x="cl_location_zipcode_prefix", by.y="zip_prefix", all.x=TRUE)
ncol(FY16var)
FY16var <- merge(FY16var, hood, by.x="cl_location_zipcode", by.y="zip", all.x=TRUE)
ncol(FY16var)

FY17var$cl_location_zipcode_prefix <- substr(FY17var$cl_location_zipcode, 1, 3)
ncol(FY17var)
FY17var <- merge(FY17var, county, by.x="cl_location_zipcode_prefix", by.y="zip_prefix", all.x=TRUE)
ncol(FY17var)
FY17var <- merge(FY17var, hood, by.x="cl_location_zipcode", by.y="zip", all.x=TRUE)
ncol(FY17var)