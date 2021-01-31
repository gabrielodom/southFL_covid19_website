# Query State Linelist
# Gabriel Odom
# 2021-01-14

library(tidyverse)
library(lubridate)
library(readxl)

linelist_df <- read_csv(
	file = "../../data/deaths/Case_Data_arcGIS_20210124.csv"
)


###  Questions for MDC  ###
mdcLinelist_df <-
	linelist_df %>%
	filter(County == "Dade") %>% 
	rename(CaseDate = Case_) %>% 
	# Regular expression to remove " 5:00" from the end of the date
	mutate(CaseDate  = str_remove(CaseDate, pattern = " .*")) %>%
	mutate(CaseDate  = as.Date(CaseDate, format = "%m/%d/%Y")) %>% 
	mutate(CaseDate  = as_date(CaseDate))

# Christine Rogers hs asked 3 questions concerning the state linelist:
#   1. Number of FL resident cases per day (the linelist data will not enable us
#      to indicate if the case was the first, or if the person tested positive
#      multiple days in a row). FLDoH measures this as "percent positivity for
#      new cases in FL residents".
#   2. Number of non-FL resident cases per day (similar comments to above)
#   3. Number of FL resident deaths daily (we are aware that this number is 
#      delayed, up to weeks or months at a time)



######  Question 1: FL Resident Cases / Day  ##################################
mdcLinelist_df %>% 
	pull(Jurisdiction) %>% 
	table(useNA = "always")

dailyMDCflResCount_df <- 
	mdcLinelist_df %>% 
	filter(Jurisdiction != "Non-FL resident") %>% 
	group_by(CaseDate) %>% 
	summarise(
		DailyCount = n()
	)

# write_csv(dailyMDCflResCount_df, "mdcDailyResCases_20210114.csv")

ggplot(data = dailyMDCflResCount_df) +
	aes(x = CaseDate, y = DailyCount) +
	geom_col()



######  Question 2: Non-FL Resident Cases / Day  ##############################

dailyMDCflNonResCount_df <- 
	mdcLinelist_df %>% 
	filter(Jurisdiction == "Non-FL resident") %>% 
	group_by(CaseDate) %>% 
	summarise(
		DailyCount = n()
	)

# write_csv(dailyMDCflNonResCount_df, "mdcDailyNonResCases_20210114.csv")

ggplot(data = dailyMDCflNonResCount_df) +
	aes(x = CaseDate, y = DailyCount) +
	geom_col()



######  Question 3: FL Resident Deaths / Day  #################################

dailyMDCflDeathsCount_df <- 
	mdcLinelist_df %>% 
	filter(Died %in% c("Recent", "Yes")) %>% 
	group_by(CaseDate) %>% 
	summarise(
		DailyCount = n()
	)

# write_csv(dailyMDCflDeathsCount_df, "mdcDailyDeaths_20210114.csv")

ggplot(data = dailyMDCflDeathsCount_df) +
	aes(x = CaseDate, y = DailyCount) +
	geom_col() +
	# Add line chart
	stat_smooth(method = "gam")



######  Question 4: Proportion of Cases in SFL  ###############################
# These few questions are from Dan Gelber (2021-01-24)

###  State Disease Burden  ###
linelist_df %>% 
	pull(Jurisdiction) %>% 
	table(useNA = "always")
linelist_df %>% 
	filter(Jurisdiction != "Non-FL resident") %>% 
	tally()
# 1609953 resident cases in FL; 7.35% disease burden
1609953 / 21900000


###  County Disease Burden  ###
linelist_df %>% 
	filter(Jurisdiction != "Non-FL resident") %>% 
	group_by(County) %>% 
	tally() %>% 
	arrange(desc(n))
# 352745, 163081, and 100152 cases in MD, Broward, and PB counties, respectively
352745 / 2719030 # 13.0% for Miami-Dade
163081 / 1959450 #  8.3% for Broward
100152 / 1510660 #  6.6% for Palm Beach
