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

# county <- read.csv("county_table.csv", colClasses=c("zip_prefix"="character"))
# str(county)
# hood <- read.csv("zip_table.csv", colClasses=c("zip"="character"))
# str(hood)

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

# FY16var$cl_location_zip_prefix <- substr(FY16var$cl_location_zipcode, 1, 3)
# dim(FY16var)
# FY16var <- merge(FY16var, county, by.x="cl_location_zip_prefix", by.y="zip_prefix", all.x=TRUE)
# dim(FY16var)
# FY16var <- merge(FY16var, hood, by.x="cl_location_zipcode", by.y="zip", all.x=TRUE)
# dim(FY16var)


# FY17var$cl_location_zipcode_prefix <- substr(FY17var$cl_location_zipcode, 1, 3)
# dim(FY17var)
# FY17var <- merge(FY17var, county, by.x="cl_location_zipcode_prefix", by.y="zip_prefix", all.x=TRUE)
# dim(FY17var)
# FY17var <- merge(FY17var, hood, by.x="cl_location_zipcode", by.y="zip", all.x=TRUE)
# dim(FY17var)

str(FY16var, list.len=ncol(FY16var))
summary(FY16var)

str(FY17var, list.len=ncol(FY17var))
summary(FY17var)

# Remove columns that are not relevant

FY16eval <- FY16var
FY17eval <- FY17var

# ----------------------------------------------------

# Remove rows with zipecode="NULL"
FY16eval$cl_location_zipcode=="NULL"
sum(FY16eval$cl_location_zipcode=="NULL")

FY16eval <- FY16eval[!FY16eval$cl_location_zipcode=="NULL", ]
dim(FY16eval)

# Duplicate column as se_chapter_loc_name
FY16eval$se_node_title <- NULL

# Data entry code (vs. description)
FY16eval$se_event_type_value <- NULL
FY16eval$se_chapter_loc_nid <- NULL
FY16eval$cl_business_type_code <- NULL
FY16eval$cl_hear_about_score_code <- NULL

# Exclude se_session_type "Close Out" or "Non-Session"
sum(FY16eval$se_session_type=="Close Out")
sum(FY16eval$se_session_type=="Non-Session")
FY16eval <- FY16eval[!(FY16eval$se_session_type=="Close Out" | FY16eval$se_session_type=="Non-Session"), ]
dim(FY16eval)

# Exclude se_event_type_desc "None" or "Update"
sum(FY16eval$se_event_type_desc=="None")
sum(FY16eval$se_event_type_desc=="Update")
FY16eval <- FY16eval[!(FY16eval$se_event_type_desc=="None" | FY16eval$se_event_type_desc=="Update"), ]
dim(FY16eval)

# ----------------------------------------------------

