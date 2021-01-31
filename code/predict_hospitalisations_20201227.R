# Lag Between Case Counts and Hospitalisations
# Gabriel Odom
# 2020-12-27

# We have been asked by Mayor Dan Gelber to find the relationship between cases
#   hospitalisations. On our own impetus, we attempted a version of this in
#   "miami_dade_composite_prediction_20200718.R".
# UPDATE 2020-12-27: we met with Gelber last week, and we found out that they
#   have access to (slightly) different data than we do. In particular, they
#   have a form of hospitalisation data going all the way back to April 5th.
#   This data format also includes testing data for the same period.



######  Setup  ################################################################
library(tidyverse)
library(lubridate)
library(readxl)

newHospAdmits_df <-
	read_excel(
		path = "../data/DEM_raw/2020.12.31 Daily COVID Case Matrix.xlsx",
		sheet = "EO 18-20 Hospital Stats",
		skip = 2
	) %>% 
	select(Date, `New COVID Patients`) %>% 
	mutate(Date = as_date(Date)) %>% 
	group_by(Date) %>% 
	summarise(COVID_admits = max(`New COVID Patients`))

casesMiamiDade_df <- read_csv(
	file = "../data/cases/FLDH_COVID19_cases_miamidade_20210103.csv"
) %>% 
	mutate(Date = as_date(Date)) 

# Based on what we discovered in "predict_hospitalisations_20201219.R", there
#   is a strong relationship between proportion of positive tests and new
#   hospital admissions 3 days later (Miami-Dade) // 7 days later (Broward and
#   Palm Beach).



######  Data Wrangling  #######################################################
raw_df <- 
	casesMiamiDade_df %>% 
	full_join(newHospAdmits_df) %>% 
	arrange(Date)

allDates_df <- tibble(
	Date = seq(
		from = raw_df$Date[1],
		to = raw_df$Date[nrow(raw_df)],
		by = "day"
	)
)

fullDates_df <- 
	allDates_df %>% 
	full_join(raw_df)

rm(casesMiamiDade_df, newHospAdmits_df, raw_df, allDates_df)


###  Lag Data  ###
lag_int <- 0L
numDays_int <- nrow(fullDates_df)

lagged_df <- tibble(
	Date      = fullDates_df$Date[1:(numDays_int - lag_int)],
	Positive  = fullDates_df$Positive[1:(numDays_int - lag_int)],
	PropPos   = fullDates_df$PropPositive[1:(numDays_int - lag_int)],
	NewAdmits = fullDates_df$COVID_admits[(lag_int + 1):numDays_int]
) %>% 
	mutate(nExpCases = ceiling(10000 * PropPos / 100)) 



######  Plots  ################################################################

###  Predict New Admits from Expected Counts per 10k  ###
lm(NewAdmits ~ nExpCases, data = lagged_df) %>% 
	summary()
# Adj R2 = 51%
ggplot(data = lagged_df) + 
	aes(x = nExpCases, y = NewAdmits, colour = Date) +
	geom_point()
# There is a date effect to the slope.
lagged_df %>% 
	mutate(early = Date < "2020-06-01") %>% 
	ggplot() + 
	aes(x = nExpCases, y = NewAdmits, colour = early) +
	geom_point()

# Let's include an interaction:
lm(NewAdmits ~ nExpCases * Date, data = lagged_df) %>% 
	summary()

# So, just like we saw before, there is an insane lockdown effect.
lagged2_df <- 
	lagged_df %>% 
	mutate(
		Wave = case_when(
			# 3 days after end of lockdown
			Date < "2020-05-21" ~ "1st",
			# 3 days after DeSantis exec order
			Date >= "2020-05-21" & Date < "2020-09-28" ~ "2nd",
			Date >= "2020-09-28" ~ "3rd"
		)
	) %>% 
	mutate(Wave = factor(Wave, levels = c("1st", "2nd", "3rd")))

lm(NewAdmits ~ nExpCases * Wave, data = lagged2_df) %>% 
	summary()
# Adj R2 isn't as high as it was for census, but 81% ain't bad. From this, we
#   see that in the 2nd wave, for every 100 people per 10k to test positive, we
#   would see 8 new admissions 3 days later. During the 3rd wave, for every 100
#   people per 10k to test positive, we would see 11 new admissions 3 days
#   later (the proper slope is 0.004889 + 0.111129).
# Mary Jo mentioned that, because of the shelf effect we saw, we can probably
#   model the hospitalisations without a lag with nearly the same effect. In
#   this case, the wave 2 slope is 0.081086 and the wave 3 slope is 0.108719
lagged2_df %>% 
	ggplot() + 
	theme_bw() +
	aes(x = nExpCases, y = NewAdmits, group = Wave, colour = Wave) +
	scale_color_manual(values = c("#404040", "#E69F00", "#56B4E9")) +
	labs(
		title = "COVID-19 Testing and Hospital Admissions in Miami-Dade County",
		subtitle = "Expected Cases Predicts New Hospitalisations",
		caption = "2nd Wave slope = 0.08 (May 21st to Sept. 28th); 3rd Wave slope = 0.11 (after Sept. 28th)",
		x = "Positive Tests per 10,000 Tests Administered",
		y = "Count of Daily COVID-19 Hospital Admissions"
	) +
	geom_point() +
	stat_smooth(method = "lm", se = FALSE)


###  Raw Case Count  ###
lm(NewAdmits ~ Positive, data = lagged2_df) %>% 
	summary()
# Adj R2 = 62%
ggplot(data = lagged2_df) + 
	aes(x = Positive, y = NewAdmits, colour = Wave) +
	scale_color_manual(values = c("#404040", "#E69F00", "#56B4E9")) +
	geom_point() +
	stat_smooth(method = "lm", se = FALSE)
# WILD!! It looks like there is a mixed effect for date. During the second wave,
#   admissions were much higher, and increasing the rate of positive tests was
#   strongly associated with increased admissions 3 days later. However, later
#   in the pandemic, there were fewer admissions relative to cases, and an
#   increase in positive tests was not as strongly associated with increasing
#   new admissions.
lm(NewAdmits ~ Positive * Wave, data = lagged2_df) %>% 
	summary()
# Because the "baseline" first wave doesn't have a significant slope, the other
#   two slope effects show up as significant (but they are almost the same
#   value). I'm going to ignore these absolutely tiny differences in slope 
#   between the 2nd and 3rd wave (for future questions, the ratio of the wave-
#   specific slopes are beta_W2 / beta_W3 = 0.054127 / 0.038627 = 1.4; I'd 
#   chalk this up to the fact that we are testing a WHOLE lot more people in
#   the 3rd wave). Anyway, let's go back to the simpler model:
lm(NewAdmits ~ Positive + Wave, data = lagged2_df) %>% 
	summary()
# Keep It Simple, Stupid. For every 100 additional positive tests, we will have
#   4-5 people walk into the hospital 3 days later. However, this is *REALLY*
#   affected by the number of people who get tested. I still like the figure
#   above.


###  Bar Charts  ###

lagged2_df %>% 
	filter(Wave != "1st") %>% 
	mutate(nExpCases = nExpCases / 5) %>% 
	ggplot() + 
	theme_bw() +
	aes(x = Date) +
	scale_x_date(
		date_breaks = "1 month",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = "COVID-19 Community Spread and Hospital Admissions after May 21st",
		subtitle = "Grey: Expected Positive COVID-19 Tests per 2,000 People Tested; Yellow: COVID-19 Daily Hospital Admissions.",
		caption = "Expected positive counts estimated as the product of the daily proportion positivity and 2,000.",
		x = "Date",
		y = "Count"
	) +
	geom_col(aes(y = nExpCases)) +
	geom_col(aes(y = NewAdmits), colour = "yellow", alpha = 0.25)

lagged2_df %>% 
	filter(Wave != "1st") %>% 
	ggplot() + 
	theme_bw() +
	aes(x = Date) +
	# scale_y_log10() +
	scale_x_date(
		date_breaks = "1 month",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = "COVID-19 Community Spread and Hospital Admissions after May 21st",
		subtitle = "Grey: Positive COVID-19 Tests; Yellow: COVID-19 Daily Hospital Admissions.",
		# caption = "Maximal lag between cases and hospital admissions in Miami-Dade detected at 3 days.",
		x = "Date",
		y = "Count"
	) +
	geom_col(aes(y = Positive)) +
	geom_col(aes(y = NewAdmits), colour = "yellow")

