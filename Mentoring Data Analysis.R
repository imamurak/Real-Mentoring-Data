getwd()

library(dplyr)
library(ggplot2)

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

str(FY16eval, list.len=ncol(FY16eval))
summary(FY16eval)

# ----------------------------------------------------

# ----- Chapter Stats ------

# Total number of sessions
length(unique(FY16eval$se_nid))

# Total number of unique clients
length(unique(FY16eval$se_client_nid))

# Total number of mentoring hours for all counselors involved
sum(FY16eval$se_hours_spent_total)

# Total number of mentoring hours for lead counselors
sum(FY16eval$se_hours_spent)

# Number of counselors involved
length(unique(FY16eval$se_volunteer_nid))

# Co-mentoring stats
sum(FY16eval$se_morethanone_flag=="yes")
sum(FY16eval$se_morethanone_flag=="no")

# Mentoring sessions by chapter location
aggregate(se_client_nid ~se_chapter_loc_name, FY16eval, function(x) length(unique(x)))

# Mentoring sessions by session type
aggregate(se_client_nid ~se_session_type, FY16eval, function(x) length(unique(x)))

# Mentoring sessions by counseling type
aggregate(se_client_nid ~se_counseling_type, FY16eval, function(x) length(unique(x)))

# Number of sessions per unique client
aggregate(se_nid ~se_client_nid, FY16eval, function(x) length(unique(x)))

# Number of average mentoring hours per unique client
aggregate(se_client_nid ~se_hours_spent, FY16eval, function(x) length(unique(x)))

# Number of sessions by month
aggregate(se_nid ~month_name, FY16eval, function(x) length(unique(x)))

# Number of sessions by day of week
aggregate(se_nid ~day_name_of_week, FY16eval, function(x) length(unique(x)))

# Number of total mentoring hours by unique client

# Number of sessons by unique client


# ----- Client Demographic Stats -----

# Unique client by state
aggregate(se_client_nid ~cl_location_state, FY16eval, function(x) length(unique(x)))

# Should filter zip code that is only 5 characters long
# Unique client by zip code
aggregate(se_client_nid ~cl_location_zipcode, FY16eval, function(x) length(unique(x)))

# Unique client by gender
aggregate(se_client_nid ~cl_gender, FY16eval, function(x) length(unique(x)))

# Unique client by age
aggregate(se_client_nid ~cl_age, FY16eval, function(x) length(unique(x)))

# Unique client by race
aggregate(se_client_nid ~cl_race, FY16eval, function(x) length(unique(x)))

# Unique client by Latino/Hispanic ethnicity
aggregate(se_client_nid ~cl_ethnicity_value, FY16eval, function(x) length(unique(x)))

# Unique client by business type
aggregate(se_client_nid ~cl_business_type_desc, FY16eval, function(x) length(unique(x)))

# Unique client by home based business
aggregate(se_client_nid ~cl_homebased_value, FY16eval, function(x) length(unique(x)))

# Unique client by legal entity
aggregate(se_client_nid ~cl_legalentity_value, FY16eval, function(x) length(unique(x)))

# Unique client by how they heard about SCORE
temp <- aggregate(se_client_nid ~cl_hear_about_score_desc, FY16eval, function(x) length(unique(x)))
temp
temp <- temp[order(-temp$se_client_nid), ]
temp

# dataset[, order(item)]

# FY16eval %>%
#   group_by(cl_business_type_desc) %>%
#   summarise(n_distinct(se_client_nid))



# ----------------------------------------------------

# install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
# install.packages(c("maps", "mapdata", "ggmap"))

states <- map_data("state")
dim(states)
tristate <- subset(states, region %in% c("new york", "new jersey", "connecticut"))

ggplot(data = tristate) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

eb2 + coord_fixed(xlim = c(-123, -121.0),  ylim = c(36, 38), ratio = 1.3)