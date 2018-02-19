getwd()

FY16data <- read.csv("ClientDemoGraphicsFY16Chp1000.csv")
FY17data <- read.csv("ClientDemoGraphicsFY17Chp1000.csv")

summary(FY16data)
str(FY16data, list.len=ncol(FY16data))

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

