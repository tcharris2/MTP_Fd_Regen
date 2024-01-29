class(regen$survey_date)
unique(regen$survey_date)
regen$survey_date

x <- as.Date.character(regen$survey_date, tryFormats = c("%Y-%b-%d", "%d-%b-%Y"))

class(regen$survey_date)

class(x)
unique(x)


regen$year_planted

y <- as.Date.character(regen$year_planted, tryFormats = c("%Y"))
y

unique(y)
unique(regen$year_planted)




regen_test <- regen

regen_test$survey_year <- NA

year_2021 <- "2021"
year_2020 <- "2020"

regen_test[grep(year_2021, regen_test$survey_date, value = F), "survey_year"] <- 2021
regen_test[grep(year_2020, regen_test$survey_date, value = F), "survey_year"] <- 2020


regen_test$survey_year
unique(regen_test$survey_year) # there is NAs present 
regen_test[is.na(regen_test$survey_year), ] # the NA present is a incorrect entry at Redfish. year = 2022
regen_test[regen_test$location == "Redfish", ] # All of Redfish was surveyed in year 2020 this is a incorrect entry and can be corrected to 2020
regen_test$survey_date[regen_test$survey_date == "08-Jul-2022"] <- "08_Jul-2020" 


unique(regen_test$year_planted)
class(regen_test$year_planted)
class(regen_test$survey_year)
regen_test$age <- regen_test$survey_year - regen_test$year_planted
unique(regen_test$age)
