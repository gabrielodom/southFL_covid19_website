# Plot Deaths Over Time
# Gabriel Odom
# 2020-07-12
# updated 2021-01-24

library(tidyverse)
library(lubridate)
library(readxl)
#



######  Death Data from Roy's "Connections"  ##################################
# ###  OLD DATA SOURCE  ###
# Through some black magic, Roy was able to get the .xlsx file *behind* the PDF
#   report that Trepka sent out. This makes life so much easier. See this slug:
# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/state_linelist_20200902.xlsx
# NOTE: this link no longer works after 9/2.

# deaths_df <- read_excel(
# 	path = "../data/deaths/state_linelist_20200830.xlsx",
# 	sheet = "Deaths",
# 	skip = 4
# ) %>%
# 	# Clean up the dates
# 	mutate(Date = as_date(`Date case counted`)) %>%
# 	select(-`Date case counted`)
# 
# # Newly-added deaths?
# deaths_df %>% 
# 	filter(`Newly identified death` == "Yes") %>% 
# 	filter(County == "Dade") %>% 
# 	arrange(Date) %>% 
# 	View
# # New deaths are added even back a full month. We are getting more and more 
# #   backlogged deaths.
# # On 5 August, they added 51 deaths to Miami-Dade County. Of those, 23 were
# #   added between 16-31 July, and 21 were added between 1-15 July. These counts
# #   cannot be expected as accurate until a full month passes.
# # As of 19 August, we added 40 deaths--22 of which were over 1 month old (before
# #   19 July). 
# 
# deathsbyday_df <-
# 	deaths_df %>%
# 	arrange(County, Date) %>%
# 	# Group deaths by day+county and count how many
# 	group_by(County, Date) %>%
# 	add_tally(name = "Count") %>%
# 	# Remove duplicate rows
# 	select(County, Date, Count) %>%
# 	distinct() %>%
# 	ungroup()


###  NEW DATA SOURCE  ###
# UPDATE: we can get simular data from ArcGIS:
# https://www.arcgis.com/sharing/rest/content/items/4cc62b3a510949c7a8167f6baa3e069d/data
#   The source for this data is this repo:
# https://github.com/mbevand/florida-covid19-line-list-data
#   The data link is defined in 
# <REPO>/blob/master/data_fdoh/download
#   The date-of-death variables are defined in comments in 
# <REPO>/blob/master/gamma.py

# We are missing data files after the 2nd of September from our original slug,
#   so we switched to the ArcGIS slug for 2020-09-08's data.



######  New Data Source from FLDoH Open Data  #################################
# As of 11 April 2021, the state linelist has been split into two data files:
# - https://open-fdoh.hub.arcgis.com/datasets/1d8756918efd40258ae05723f1c4ece0_0
# - https://open-fdoh.hub.arcgis.com/datasets/florida-covid19-case-line-data-2021-1
# 
# Both links require "point-and-click" operations to download the data. So now,
#   I have to write code to join the two data sets in order for the rest of this
#   code to work.
c(
	"../data/deaths/Case_Data_2020_arcGIS_20210418.csv",
	"../data/deaths/Case_Data_2021_arcGIS_20210418.csv"
) %>% 
	map(read_csv) %>% 
	# This has two columns that may be duplicates: "OBJECTID" and "ObjectId2". I
	#   cracked open the original two data sets, and these columns are duplicated
	#   on the DoH end; our code didn't create them.
	bind_rows() %>% 
	write_csv(file = "../data/deaths/Case_Data_arcGIS_20210418.csv")
# FORMAT CHANGES (as of 2021-04-11):
#   - column names are truncated:
#      - "Jurisdiction" --> "Jurisdicti"
#      - "Hospitalized" --> "Hospitaliz"
#   - column name "Case" --> "Case_"
#   - column name "Case_" --> "Case1" WHY???????
#   - date format: "m/d/YY" --> "YYYY/MM/DD" (1/1/21 --> 2021/01/01)
#   - maybe others that I missed? who knows

deaths_df <- 
	read_csv(
		file = "../data/deaths/Case_Data_arcGIS_20210418.csv"
	) %>% 
	# NOTE 2021-01-14: WHAT THE HELL IS "Recent"??? There are 243 "Recent" rows
	#   for the 16th data, but only 95 for the 10th. This must be a new designation
	filter(Died %in% c("Yes", "Recent")) %>% 
	# filter(Jurisdiction == "FL resident") %>% 
	filter(Jurisdicti == "FL resident") %>% 
	rename(CaseDate = Case1)

deaths2_df <- 
	deaths_df %>% 
	mutate(
		CaseDate  = str_remove(CaseDate, pattern = " .*"),
		ChartDate = str_remove(ChartDate, pattern = " .*")
	) %>%
	mutate(
		# CaseDate  = as.Date(CaseDate, format = "%m/%d/%Y"),
		# ChartDate = as.Date(ChartDate, format = "%m/%d/%Y")
		CaseDate  = as.Date(CaseDate, format = "%Y/%m/%d"),
		ChartDate = as.Date(ChartDate, format = "%Y/%m/%d")
	) %>% 
	mutate(
		CaseDate  = as_date(CaseDate),
		EventDate = as_date(EventDate),
		ChartDate = as_date(ChartDate),
	) %>% 
	select(
		County, Age, Age_group, Gender, EventDate, ChartDate
	)

# deaths2_df %>%
# 	mutate(diffTime = ChartDate - CaseDate) %>%
# 	pull(diffTime) %>%
# 	as.numeric() %>%
# 	summary()
# # Ok, so CaseDate and ChartDate are identical
# # UPDATE 2021-04-11: still true
# deaths2_df$CaseDate <- NULL

# deaths2_df %>%
# 	mutate(diffTime = ChartDate - EventDate) %>%
# 	pull(diffTime) %>%
# 	as.numeric() %>%
# 	summary()
# deaths2_df %>%
# 	mutate(diffTime = ChartDate - EventDate) %>%
# 	pull(diffTime) %>%
# 	as.numeric() %>%
# 	density() %>%
# 	plot()
# # For most people, the ChartDate is after the EventDate. This corroborates
# #   what we see in Bevand's code (gamma.py). He comments that the EventDate
# #   column is the date of the onset of COVID-19, while the ChartDate column
# #   lists the "date the case was counted". I now assume that means the date
# #   of death.
# # UPDATE 2021-04-11: we found out and confirmed last week that this is NOT
# #   date of death. 


###  Counts by Age  ###
deaths2_df %>% 
	mutate(old = Age >= 65) %>% 
	group_by(old) %>% 
	# summarise(Count = n())
	summarise(Proportion = n() / nrow(deaths2_df))
# For the state, 83.1% (20918 / 25163) of deaths are the 65 and up group.
deaths2_df %>% 
	filter(County == "Palm Beach") %>% 
	mutate(old = Age >= 65) %>% 
	group_by(old) %>% 
	# summarise(Count = n())
	summarise(
		Proportion = n() / nrow(deaths2_df %>% filter(County == "Palm Beach"))
	)
# # DATE: 2021-01-24
# # For Miami-Dade, 81.8% (3845 / 4703) of the deaths are the 65 and up group.
# # For Broward, 75.6% (1526 / 2018) of the deaths are the 65 and up group.**
# # For Palm Beach, 84.6% (1770 / 2093) of the deaths are the 65 and up group.
# fisher.test(
# 	matrix(c(20918, 25163, 1526, 2018), nrow = 2, byrow = FALSE),
# 	alternative = "two.sided"
# )


###  Counts by Day  ###
deathsbyday_df <-
	deaths2_df %>%
	arrange(County, ChartDate) %>%
	# Group deaths by day+county and count how many
	group_by(County, ChartDate) %>%
	add_tally(name = "Count") %>%
	# Remove duplicate rows
	select(County, ChartDate, Count) %>%
	distinct() %>%
	rename(Date = ChartDate) %>% 
	ungroup()
# I compared the first 10 rows (for Alachua County) between the reported deaths
#   we accessed on August 30th (FLDH_COVID19_deathsbyday_bycounty_20200830 in
#   the ~/data/deaths/ directory). They match exactly.

###  Save  ###
write_csv(
	x = deathsbyday_df,
	file = "../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20210418.csv"
)



######  New Deaths Added  #####################################################
# The data in the new format does not mark "newly-added" deaths, so we don't
#   know how delayed the reporting is. In order to estimate this approximately
#   on a weekly basis, we will import the same data from last week, anti-join
#   the sets, and inspect the date distribution for both FL and Miami-Dade
#   county.


# deathsOld_df <- read_excel(
# 	path = "../data/deaths/state_linelist_20200830.xlsx",
# 	sheet = "Deaths",
# 	skip = 4
# ) %>%
# 	# Clean up the dates
# 	mutate(Date = as_date(`Date case counted`)) %>%
# 	select(-`Date case counted`)

# deathsOld_df <- 
# 	read_csv(
# 		file = "../data/deaths/Case_Data_arcGIS_20210404.csv"
# 	) %>% 
# 	filter(Died %in% c("Yes", "Recent")) %>% 
# 	filter(Jurisdiction == "FL resident") %>% 
# 	rename(CaseDate = Case_) %>% 
# 	mutate(
# 		CaseDate  = str_remove(CaseDate, pattern = " .*"),
# 		ChartDate = str_remove(ChartDate, pattern = " .*")
# 	) %>%
# 	mutate(
# 		CaseDate  = as.Date(CaseDate, format = "%m/%d/%Y"),
# 		ChartDate = as.Date(ChartDate, format = "%m/%d/%Y")
# 	) %>% 
# 	mutate(
# 		CaseDate  = as_date(CaseDate),
# 		EventDate = as_date(EventDate),
# 		ChartDate = as_date(ChartDate),
# 	) %>% 
# 	select(
# 		County, Age, Age_group, Gender, EventDate, ChartDate
# 	)

deathsOld_df <- 
	read_csv(
		file = "../data/deaths/Case_Data_arcGIS_20210411.csv"
	) %>% 
	filter(Died %in% c("Yes", "Recent")) %>% 
	filter(Jurisdicti == "FL resident") %>% 
	rename(CaseDate = Case1) %>% 
	mutate(
		CaseDate  = str_remove(CaseDate, pattern = " .*"),
		ChartDate = str_remove(ChartDate, pattern = " .*")
	) %>%
	mutate(
		CaseDate  = as.Date(CaseDate, format = "%Y/%m/%d"),
		ChartDate = as.Date(ChartDate, format = "%Y/%m/%d")
	) %>% 
	mutate(
		CaseDate  = as_date(CaseDate),
		EventDate = as_date(EventDate),
		ChartDate = as_date(ChartDate),
	) %>% 
	select(
		County, Age, Age_group, Gender, EventDate, ChartDate
	)

deathsNew_df <- 
	read_csv(
		file = "../data/deaths/Case_Data_arcGIS_20210418.csv"
	) %>% 
	filter(Died %in% c("Yes", "Recent")) %>% 
	filter(Jurisdicti == "FL resident") %>% 
	rename(CaseDate = Case1) %>% 
	mutate(
		CaseDate  = str_remove(CaseDate, pattern = " .*"),
		ChartDate = str_remove(ChartDate, pattern = " .*")
	) %>%
	mutate(
		CaseDate  = as.Date(CaseDate, format = "%Y/%m/%d"),
		ChartDate = as.Date(ChartDate, format = "%Y/%m/%d")
	) %>% 
	mutate(
		CaseDate  = as_date(CaseDate),
		EventDate = as_date(EventDate),
		ChartDate = as_date(ChartDate),
	) %>% 
	select(
		County, Age, Age_group, Gender, EventDate, ChartDate
	)

newlyAddedDeaths_df <- 
	anti_join(
  	deathsNew_df %>% 
  		select(-EventDate, -Age_group) %>% 
  		rename(Date = ChartDate),
  	# Remove this wrangling step for deaths data in old (08-30) format
  	deathsOld_df %>% 
  		select(-EventDate, -Age_group) %>% 
  		rename(Date = ChartDate),
  	by = c("County", "Age", "Gender", "Date")
  )
nrow(deathsNew_df) - nrow(deathsOld_df)
# Between 30 August and 8 September, we added 795 new deaths, but 752 show up
#   as "present in New, but absent in Old". I'm willing to bet that this is
#   dropping repeated rows (i.e., there could be more than one 91 F from Dade
#   that died on the same day; it's unlikely, but possible). I don't know how
#   to fix this, but I can probably estimate the reporting delay without this
#   information.
# Between 8 September and 14 September, we added 727 new deaths, but only 648
#   show up in the anti-join.
# Between 14 September and 21 September, we added 675 new deaths, but only 658
#   show up in the anti-join.
# Between 21 September and 28 September, we added 720 new deaths, but only 691
#   show up in the anti-join.
# Between 28 September and 06 October, we added 730 new deaths, but 822 show up
#   in the anti-join.
# Between 06 October and 15 October, we added 969 new deaths, but 948 show up
#   in the anti-join. (NOTE: this was a 9-day window; that's 737 deaths/week.)
# Between 15 October and 20 October, we added 369 new deaths, but 363 show up
#   in the anti-join. (NOTE: this was a 5-day window; that's 517 deaths/week.)
# Between 20 October and 25 October, we added 324 new deaths, but 329 show up
#   in the anti-join. (NOTE: this was a 5-day window; that's 461 deaths/week.)
# Between 25 October and 1 November, we added 360 new deaths, but 370 show up
#   in the anti-join. 
# Between 1 November and 8 November, we added 332 new deaths, but 346 show up
#   in the anti-join. 
# Between 8 November and 15 November, we added 397 new deaths, but 399 show up
#   in the anti-join. 
# Between 15 November and 22 November, we added 412 new deaths, but 424 show up
#   in the anti-join. 
# Between 22 November and 29 November, we added 570 new deaths, but 552 show up
#   in the anti-join. 
# Between 29 November and 6 December, we added 677 new deaths, but 673 show up
#   in the anti-join. 
# Between 6 December and 13 December, we added 626 new deaths, but 624 show up
#   in the anti-join. 
# Between 13 December and 20 December, we added 686 new deaths, but 695 show up
#   in the anti-join. 
# Between 20 December and 27 December, we added 606 new deaths, but 611 show up
#   in the anti-join. 
# Between 27 December and 3 January, we added 785 new deaths, but 784 show up
#   in the anti-join. 
# Between 3 January and 10 January, we added 937 new deaths, but 949 show up
#   in the anti-join. 
# Between 10 January and 16 January, we added 1092 new deaths, but 1067 show up
#   in the anti-join. (NOTE: this was a 6-day window; that's 1245 deaths/week.)
# Between 16 January and 24 January, we added 1160 new deaths, but 1173 show up
#   in the anti-join. (NOTE: this was an 8-day window; that's 1026 deaths/week.)
# Between 24 January and 31 January, we added 1196 new deaths, but 1247 show up
#   in the anti-join. 
# Between 31 January and 7 February, we added 1239 new deaths, but 1281 show up
#   in the anti-join. 
# Between 7 February and 14 February, we added 1180 new deaths, but 1217 show up
#   in the anti-join. 
# Between 14 February and 21 February, we added 1034 new deaths, but 1057 show up
#   in the anti-join. 
# Between 21 February and 28 February, we added 1039 new deaths, but 1077 show up
#   in the anti-join. 
# Between 28 February and 7 March, we added 768 new deaths, but 770 show up
#   in the anti-join. 
# Between 7 March and 14 March, we added 635 new deaths, but 740 show up
#   in the anti-join. 
# Between 14 March and 21 March, we added 487 new deaths, but 489 show up
#   in the anti-join.
# Between 21 March and 28 March, we added 436 new deaths, but 493 show up
#   in the anti-join.
# Between 28 March and 4 April, we added 496 new deaths, but 578 show up
#   in the anti-join. This is a very large discrepancy.
# Between 4 April and 11 April, we updated to the new data format (with split
#   line list over 2020 and 2021). We added 403 new deaths, but 628 show up
#   in the anti-join. This is an even larger discrepancy than last week.
# Between 11 April and 18 April, we added 102 new deaths, but 413 show up
#   in the anti-join. This is the third week in a row with such a huge
#   difference. What is going on?



######  Reporting Delays  #####################################################
# County
newlyAddedDeaths_df %>% 
	filter(County == "Dade") %>% # %in% c("Escambia", "Santa Rosa")
	pull(Date) %>% 
	summary()

# State
newlyAddedDeaths_df %>% 
	pull(Date) %>% 
	summary()

newlyAddedDeaths_df %>%
	arrange(Date) %>%
	pull(Date) %>%
	`-`(Sys.Date()) %>%
	as.integer() %>%
	hist(main = "Days Since Diagnosis")
newlyAddedDeaths_df %>% 
	filter(Date < "2020-11-01") %>% 
	pull(Age) %>% 
	summary()


###  Reporting Delay 2020-09-08  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-20" "2020-07-13" "2020-07-26" "2020-07-25" "2020-08-10" "2020-09-07" 
# On 8 September, over half of the newly-added deaths in Miami-Dade county were
#   recorded on or before 26 July.
# 
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-20" "2020-07-20" "2020-08-06" "2020-08-02" "2020-08-19" "2020-09-07" 
# On 8 September, over half of the newly-added deaths in the State of Florida
#   were recorded on or before 6 August. This means that FL reporting is only
#   a month behind, while MDC is 6 weeks behind.


###  Reporting Delay 2020-09-14  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-30" "2020-07-15" "2020-07-29" "2020-07-27" "2020-08-10" "2020-09-10"
# Something is off here. These deaths are supposed to go up to 09-14, but the
#   most recent death we have is from the 10th? Regardless, the median death
#   reporting date for the newly-added deaths is 29 July, almost 7 weeks back.
# Also, we just added a death on 30 March? Holy crap
# 
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-30" "2020-07-22" "2020-08-07" "2020-08-05" "2020-08-23" "2020-09-13" 
# The state looks to be in a better position than Miami-Dade County. The deaths
#   are added up until the 13th of September, and median reporting date is only
#   5 weeks back.


###  Reporting Delay 2020-09-21  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-02" "2020-07-26" "2020-08-08" "2020-08-06" "2020-08-19" "2020-09-19"
# Something is off here. These deaths are supposed to go up to 09-14, but the
#   most recent death we have is from the 10th? Regardless, the median death
#   reporting date for the newly-added deaths is 29 July, almost 7 weeks back.
# Also, we just added a death on 30 March? Holy crap
# 
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-02" "2020-07-29" "2020-08-18" "2020-08-12" "2020-08-28" "2020-09-19" 
# The state looks to be in a better position than Miami-Dade County. The deaths
#   are added up until the 13th of September, and median reporting date is only
#   5 weeks back.


###  Reporting Delay 2020-09-28  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-03" "2020-07-21" "2020-08-02" "2020-07-29" "2020-08-13" "2020-09-11" 
# 10-week delay for 75th percentile; 8-week delay for 50th percentile
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-03" "2020-07-23" "2020-08-07" "2020-08-07" "2020-08-27" "2020-09-27" 
# The state looks to be in a better position than Miami-Dade County for recent
#   deaths, but the quantiles are still being dragged back.
# 9.5-week delay for 75th percentile; 7-week delay for 50th percentile


###  Reporting Delay 2020-10-06  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-11" "2020-07-16" "2020-08-04" "2020-08-05" "2020-08-27" "2020-10-05" 
# 12-week delay for 75th percentile; 9-week delay for 50th percentile
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-11" "2020-07-21" "2020-08-12" "2020-08-11" "2020-09-05" "2020-10-05"  
# The state looks to be in a better position than Miami-Dade County for recent
#   deaths, but the quantiles are still being dragged back.
# 11-week delay for 75th percentile; 8-week delay for 50th percentile


###  Reporting Delay 2020-10-15  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-17" "2020-07-24" "2020-08-10" "2020-08-12" "2020-09-10" "2020-10-06" 
# 12-week delay for 75th percentile; 9-week delay for 50th percentile
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-15" "2020-07-24" "2020-08-13" "2020-08-16" "2020-09-13" "2020-10-14"  
# 12-week delay for 75th percentile; 9-week delay for 50th percentile


###  Reporting Delay 2020-10-20  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-10" "2020-07-29" "2020-09-01" "2020-08-28" "2020-09-29" "2020-10-19"  
# 12-week delay for 75th percentile; 7-week delay for 50th percentile
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-19" "2020-08-05" "2020-09-03" "2020-08-29" "2020-09-25" "2020-10-19"  
# 11-week delay for 75th percentile; 7-week delay for 50th percentile


###  Reporting Delay 2020-10-25  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-23" "2020-08-23" "2020-09-11" "2020-09-03" "2020-09-29" "2020-10-22"  
# 9-week delay for 75th percentile; 6-week delay for 50th percentile
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-26" "2020-08-30" "2020-09-19" "2020-09-09" "2020-10-03" "2020-10-23"  
# 8-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Delay 2020-11-01  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-26" "2020-07-30" "2020-09-18" "2020-08-30" "2020-10-04" "2020-10-27"
# 13-week delay for 75th percentile; 6-week delay for 50th percentile. Something
#   wild happened here: our 75th percentile delay jumped by 5 weeks. I guess we
#   are getting caught up on deaths from the second wave?
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-26" "2020-09-13" "2020-09-28" "2020-09-20" "2020-10-10" "2020-10-29"
# 7-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Delay 2020-11-08  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-26" "2020-09-15" "2020-10-01" "2020-09-26" "2020-10-18" "2020-11-04" 
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-20" "2020-09-22" "2020-10-07" "2020-09-30" "2020-10-19" "2020-11-06"
# 7-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Delay 2020-11-15  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-03" "2020-08-04" "2020-10-01" "2020-09-06" "2020-10-17" "2020-11-04"
# 15-week delay for 75th percentile; 6-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-03" "2020-09-17" "2020-10-12" "2020-09-29" "2020-10-26" "2020-11-14"
# 8-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Delay 2020-11-22  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-07-13" "2020-08-13" "2020-10-08" "2020-09-24" "2020-10-29" "2020-11-19"
# 14-week delay for 75th percentile; 6-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-27" "2020-10-04" "2020-10-25" "2020-10-12" "2020-11-04" "2020-11-20" 
# 7-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Delay 2020-11-29  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-07-07" "2020-10-14" "2020-10-29" "2020-10-17" "2020-11-11" "2020-11-27"
# 7-week delay for 75th percentile; 4-week delay for 50th percentile. 
# THIS IS THE SHORTEST REPORTING DELAY WE'VE SEEN IN MIAMI-DADE SINCE SEPTEMBER.
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-02" "2020-08-31" "2020-10-22" "2020-10-03" "2020-11-07" "2020-11-27"
# 13-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Delay 2020-12-06  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-06-01" "2020-10-31" "2020-11-08" "2020-10-30" "2020-11-16" "2020-12-04" 
# 5-week delay for 75th percentile; 4-week delay for 50th percentile. 
# THIS IS THE SHORTEST REPORTING DELAY WE'VE SEEN IN MIAMI-DADE SINCE SEPTEMBER.
#   Last week was the shortest reporting delay, and now this week is even
#   shorter. Perhaps the current infrastructure is more capable of keeping pace
#   with the new deaths?
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-08" "2020-10-24" "2020-11-07" "2020-10-25" "2020-11-18" "2020-12-05" 
# 6-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2020-12-13  ###
# I met with DoH (Sarah Suarez and Dr. Villalta) this past week. They explained
#   that this delay we see isn't a "reporting" delay anymore (it hasn't been a
#   reporting delay since some time in August or September when a policy change
#   allowed physicians to mark COVID-19 as a cause of death; this removed the
#   medical examiner bottleneck that horribly delayed reporting over the second
#   wave). This delay is a "certification" and/or "quality assurance" delay:
#   the State offices in Tallahassee take extra time verifying that these deaths
#   are indeed COVID-19 deaths. The individual counties usually have all the
#   deaths data from nursing homes, EMS, long-term care, hospice, hospitals, and
#   the medical examiners office within 2 weeks. Then that data is sent to the
#   state office for QA (which can add quite a bit of delay).
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-05" "2020-11-02" "2020-11-13" "2020-11-07" "2020-11-24" "2020-12-05" 
# 6-week delay for 75th percentile; 4-week delay for 50th percentile.
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-22" "2020-10-28" "2020-11-14" "2020-10-29" "2020-11-24" "2020-12-10"  
# 7-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2020-12-20  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-07-09" "2020-11-12" "2020-11-22" "2020-11-12" "2020-11-30" "2020-12-17" 
# 5-week delay for 75th percentile; 4-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-10" "2020-11-10" "2020-11-22" "2020-11-09" "2020-12-02" "2020-12-18" 
# 6-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2020-12-27  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-25" "2020-11-17" "2020-11-26" "2020-11-19" "2020-12-08" "2020-12-21"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-07" "2020-11-15" "2020-11-28" "2020-11-19" "2020-12-08" "2020-12-24"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2021-01-03  ###
# happy new year...
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-07-13" "2020-11-25" "2020-12-06" "2020-11-30" "2020-12-14" "2020-12-28"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-17" "2020-11-23" "2020-12-06" "2020-11-25" "2020-12-15" "2020-12-31"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2021-01-10  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-16" "2020-11-25" "2020-12-10" "2020-11-27" "2020-12-17" "2021-01-06"
# 7-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-17" "2020-12-04" "2020-12-14" "2020-12-05" "2020-12-23" "2021-01-07"
# 5-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2021-01-16  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-19" "2020-12-06" "2020-12-14" "2020-12-02" "2020-12-21" "2021-01-12"
# 6-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-16" "2020-12-10" "2020-12-21" "2020-12-12" "2020-12-30" "2021-01-15"
# 5-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2021-01-24  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-26" "2020-12-13" "2020-12-25" "2020-12-13" "2020-12-31" "2021-01-21"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-04" "2020-12-16" "2020-12-28" "2020-12-18" "2021-01-05" "2021-01-22"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2021-01-31  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-27" "2020-12-16" "2020-12-28" "2020-12-14" "2021-01-05" "2021-01-28"
# 7-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-27" "2020-12-20" "2021-01-02" "2020-12-20" "2021-01-10" "2021-01-29"
# 6-week delay for 75th percentile; 4-week delay for 50th percentile


###  Reporting Certification Delay 2021-02-07  ###
# UPDATE: When we met with DoH (Sarah Suarez and Dr. Villalta) in early December,
#   we had a grave miscommunication--which I'm not sure how that was possible, 
#   as we were literally speaking on video. Regardless, NIETHER the "Case_" nor
#   the "EventDate" column in the linelist are updated to show the date of death.
#   This means that Case_ (renamed to "CaseDate") *still* measures when the 
#   subjects became ill. Therefore, this delay is the time for the virus to kill
#   the patient PLUS the certification delay. We emailed back to Sarah to find
#   out if there is any way to count the number of deaths per day.
#      To be completely honest, I thought it was very strange that the daily
#   "deaths" counts would happen at the same time (or even 1-2 days prior) to
#   the case counts. I brushed off my own worries because we had confirmed with
#   DoH that "Case_" was updated to date of death. I questioned myself, and I
#   shouldn't have ignored my instincts. 
#      All in all, here is what this means: the CDC has previously reported that
#   it takes COVID-19 about 15 days to kill people. If everyone that would die
#   died 15 days after infection, this would mean that 1/3rd of the delay is
#   the time it takes for the virus to kill the subjects, and the remaining time
#   is the certification delay. However, because we (on 7 February) have deaths
#   recorded in MDC on 3 February and on 5 February for the state, we know that
#   the certification delay is not fixed. Also, because of basic medicine, we
#   know that the time the SARS-Cov-2 virus takes to kill people will vary as
#   well. If we can estimate the distribution of the time it takes people to die
#   of COVID-19, then we can subtract the distribution of these "delays" to 
#   estimate the distribution of the certification delay.

# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-15" "2020-12-15" "2020-12-31" "2020-12-08" "2021-01-11" "2021-02-03"
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-04" "2020-12-24" "2021-01-06" "2020-12-25" "2021-01-15" "2021-02-05"
# 6-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Certification Delay 2021-02-14  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-17" "2020-12-22" "2021-01-09" "2020-12-21" "2021-01-19" "2021-02-12"
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-16" "2020-12-31" "2021-01-12" "2020-12-31" "2021-01-21" "2021-02-12" 
# 6-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Certification Delay 2021-02-21  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-06-29" "2021-01-01" "2021-01-14" "2021-01-06" "2021-01-25" "2021-02-15"
# 7-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-16" "2021-01-06" "2021-01-18" "2021-01-11" "2021-01-28" "2021-02-19"
# 7-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Certification Delay 2021-02-28  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-14" "2020-12-26" "2021-01-16" "2021-01-02" "2021-01-30" "2021-02-26"
# 9-week delay for 75th percentile; 6-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-14" "2021-01-05" "2021-01-20" "2021-01-11" "2021-01-31" "2021-02-26" 
# 8-week delay for 75th percentile; 6-week delay for 50th percentile


###  Reporting Certification Delay 2021-03-07  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-02" "2021-01-13" "2021-01-30" "2021-01-21" "2021-02-10" "2021-03-02" 
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-02" "2021-01-14" "2021-01-29" "2021-01-21" "2021-02-09" "2021-03-05" 
# 7-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Certification Delay 2021-03-14  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-27" "2021-01-07" "2021-01-30" "2021-01-03" "2021-02-13" "2021-03-08" 
# 9-week delay for 75th percentile; 6-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-10" "2021-01-16" "2021-02-03" "2021-01-20" "2021-02-16" "2021-03-09" 
# 8-week delay for 75th percentile; 6-week delay for 50th percentile


###  Reporting Certification Delay 2021-03-21  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-05-01" "2021-01-26" "2021-02-11" "2021-02-02" "2021-02-20" "2021-03-19" 
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-16" "2021-01-27" "2021-02-12" "2021-02-01" "2021-02-24" "2021-03-19" 
# 8-week delay for 75th percentile; 5-week delay for 50th percentile


###  Reporting Certification Delay 2021-03-28  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-20" "2021-02-01" "2021-02-19" "2021-01-28" "2021-03-01" "2021-03-26" 
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-03-20" "2021-01-19" "2021-02-16" "2021-01-22" "2021-03-01" "2021-03-26"
# 10-week delay for 75th percentile; 6-week delay for 50th percentile


###  Reporting Certification Delay 2021-04-04  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-17" "2021-01-15" "2021-02-26" "2021-01-22" "2021-03-07" "2021-03-31" 
# 11-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-14" "2021-01-04" "2021-02-16" "2021-01-17" "2021-03-07" "2021-04-01"
# 13-week delay for 75th percentile; 7-week delay for 50th percentile
#
# There is something seriously wrong with this data. The delay shouldn't be
#   moving backwards unless we just had a huge spike in deaths. We see from the
#   ICU/vents data that "heads in beds" has dropped by 20%+ for the third week
#   in a row, so maybe that's a bunch of deaths? Still, this feels wrong.


###  Reporting Certification Delay 2021-04-11  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-08-02" "2021-02-18" "2021-03-06" "2021-02-25" "2021-03-17" "2021-04-09" 
# 7-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-06-29" "2021-01-29" "2021-03-02" "2021-02-11" "2021-03-19" "2021-04-09"
# 10-week delay for 75th percentile; 6-week delay for 50th percentile
#
# This is a huge improvement from last week! I had emailed Sarah Suarez about
#   the crazy stuff I was seeing in the data, and they changed the whole data
#   format. However, some dates got messed up: last week we had 290 COVID-19
#   deaths on 2020-12-31, this week we have 497. I emailed Sarah about that too.


###  Reporting Certification Delay 2021-04-18  ###
# MIAMI-DADE COUNTY:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-07-15" "2021-02-23" "2021-03-12" "2021-02-28" "2021-03-24" "2021-04-15" 
# 8-week delay for 75th percentile; 5-week delay for 50th percentile. 
#  
# STATE OF FLORIDA:
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2020-04-17" "2021-02-01" "2021-03-09" "2021-02-13" "2021-03-25" "2021-04-15"
# 11-week delay for 75th percentile; 6-week delay for 50th percentile



######  Plots of Deaths  ######################################################
###  Import Cleaned Deaths Data  ###
deathsbyday_df <- read_csv(
	"../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20210411.csv"
)

# deathsbyday_df %>% 
# 	filter(County == "Dade")
# # The first 10 rows for Miami-Dade County also match exactly. I think I'm
# #   willing to put out this data.


###  Plot County Deaths over Time  ###
whichCounty <- "Dade" # "Palm Beach" # "Broward"

ggplot(
	deathsbyday_df %>% 
		filter(County == whichCounty) %>% # %in% c("Escambia", "Santa Rosa")
		# Only 25% of newly added deaths are on or before this date. See comments
		#   on newly-added deaths in previous section
		filter(Date <= "2021-02-18")
) +
	
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	aes(x = Date, y = Count) +
	# scale_y_log10() +
	scale_x_date(
		date_breaks = "1 month",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = paste("Deaths by Day for", whichCounty, "County")
	) +
	
	geom_point() +
	stat_smooth(method = "gam")


###  Plot State Deaths over Time  ###
ggplot(
	deathsbyday_df %>% 
		group_by(Date) %>% 
		summarise(Count = sum(Count)) %>% 
	  # See comments on newly-added deaths in previous section
	  filter(Date <= "2021-01-29")
) +
	
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	aes(x = Date, y = Count) +
	scale_x_date(
		date_breaks = "1 month",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = "Deaths by Day for the State of Florida"
	) +
	
	geom_point() +
	stat_smooth(method = "gam")


