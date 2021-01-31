# Lag Between Case Counts and Hospitalisations
# Gabriel Odom
# 2020-12-19

# We have been asked by Mayor Dan Gelber to find the relationship between cases
#   hospitalisations. On our own impetus, we attempted a version of this in
#   "miami_dade_composite_prediction_20200718.R"



######  Setup  ################################################################
library(tidyverse)
library(lubridate)

cases_df <- read_csv(
	# file = "../data/cases/FLDH_COVID19_cases_broward_20201220.csv"
	# file = "../data/cases/FLDH_COVID19_cases_miamidade_20201220.csv"
	file = "../data/cases/FLDH_COVID19_cases_palmbeach_20201220.csv"
) %>%
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>%
	mutate(Date = as_date(Date))

casesBroward_df <- read_csv(
	file = "../data/cases/FLDH_COVID19_cases_broward_20201220.csv"
) %>% 
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date)) %>% 
	mutate(County = "BROWARD")
casesMiamiDade_df <- read_csv(
	file = "../data/cases/FLDH_COVID19_cases_miamidade_20201220.csv"
) %>% 
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date)) %>% 
	mutate(County = "MIAMI-DADE")
casesPalmBeach_df <- read_csv(
	file = "../data/cases/FLDH_COVID19_cases_palmbeach_20201220.csv"
) %>% 
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date)) %>% 
	mutate(County = "PALM BEACH")
cases_df <- bind_rows(casesBroward_df, casesMiamiDade_df, casesPalmBeach_df)
rm(casesBroward_df, casesMiamiDade_df, casesPalmBeach_df)

hosp_df <- read_csv(
	file = "../data/ESS_processed/ESS_southFL_summary_20201219.csv"
) %>% 
	# filter(County == "BROWARD") %>% 
	# filter(County == "MIAMI-DADE") %>% 
	filter(County == "PALM BEACH") %>% 
	select(Date, County, Hospitalized, AdmitPrevDay)

raw_df <- 
	cases_df %>% 
	full_join(hosp_df) %>% 
	arrange(Date)



######  Wrangling  ############################################################
# Date Range
# there are some dates that we have no data for at all. We want to make sure
#   this doesn't interfere with our lagging operation.
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

# Summarise Duplicate Days
# Some ESS data we have for morning and evening.
clean_df <- 
	fullDates_df %>% 
	# group_by(Date) %>% 
	group_by(Date, County) %>% 
	summarise(across(everything(), max)) %>%
	select(-County) %>% 
	group_by(Date) %>% 
	summarise(
		tPos = sum(Positive, na.rm = TRUE),
		tNeg = sum(Negative, na.rm = TRUE),
		tHosp = sum(Hospitalized, na.rm = TRUE),
		tNewAdmit = sum(AdmitPrevDay, na.rm = TRUE)
	) %>% 
	mutate(tHosp = case_when(
		tHosp == 0 ~ NA_real_,
		TRUE ~ tHosp
	)) %>% 
	mutate(tNewAdmit = case_when(
		tNewAdmit == 0 ~ NA_real_,
		TRUE ~ tNewAdmit
	)) %>% 
	mutate(tPropPos = tPos / (tPos + tNeg))

rm(allDates_df, cases_df, fullDates_df, hosp_df, raw_df)



######  Explore Correlations  #################################################
# We expect that the hospitalisations will be correlated with case counts from
#   one week or so prior

numDays_int <- nrow(clean_df)
lags_int <- 1:28
names(lags_int) <- paste0("lag_", lags_int)


###  Cases to Hospitalisations  ###
plot(
	x = clean_df$tPos[1:(numDays_int - 15)],
	y = clean_df$tHosp[(15 + 1):numDays_int]
)

map_dbl(
	.x = lags_int,
	.f = ~{
		cor(
			clean_df$tPropPos[1:(numDays_int - .x)],
			clean_df$tHosp[(.x + 1):numDays_int],
			use = "pairwise.complete.obs",
			method = "spearman"
		)
	}
) %>% 
	plot(main = "Correlations")
# So, it looks like the correlation for Miami-Dade could be 6-7 days, 10 days,
#   or 12-13 days (all between 58-60%). For Broward, this correlation is much
#   stronger, maxing out at 75% for 13 days. For Palm Beach, this correlation
#   is weaker than in Broward, but stronger than Miami-Dade; the maximum 
#   correlation could be at day 6 (64%) or at days 12-13 (65%).
# Based on the information for these three counties, (and also after "zooming
#   out" to 28 day lags), it looks like the overall maximumally-correlated lag
#   between positive case count and hospitalisation census is 14 days. Also,
#   because the signal strength is weakest in the Miami-Dade county data, I
#   think I need to re-import the data as grouped across all three counties.
# I combined all three counties, and the lag with the max correlation is 13
#   with 65.5%.
# Update: because the volume of testing is so highly variable, I went back and
#   evaluated the relationship between the proportion of positive tests and
#   hospitalisations (so that the indicator of community spread is not
#   heteroskedastic).

# Now we repeat for admissions:
map_dbl(
	.x = lags_int,
	.f = ~{
		cor(
			clean_df$tPropPos[1:(numDays_int - .x)],
			clean_df$tNewAdmit[(.x + 1):numDays_int],
			use = "pairwise.complete.obs",
			method = "spearman"
		)
	}
) %>% 
	plot(main = "Correlations")
# For all three counties, we are basically flat at 80%+ correlation from day 1
#   to day 8, but the max is at day 3 (0.8288912). For Miami-Dade, the shelf
#   does not last as long, and the max is at day 3 as well (0.7908782). For
#   Broward, the shelf is plainly visible, and the max is at day 8 (0.8544540).
#   There's a strong shelf effect for Palm Beach too, but the max is at day 7
#   (0.7759206).



######  Modelling  ############################################################
# We found that the 13-day lag is the maximally-correlated lag. 
plot(
	x = clean_df$tPropPos[1:(numDays_int - 15)],
	y = clean_df$tHosp[(15 + 1):numDays_int]
)

# How many tests happen?
clean_df %>% 
	mutate(nTot = tPos + tNeg) %>% 
	pull(nTot) %>% 
	plot()

lag_int <- 3L
lagged_df <- tibble(
	Date    = clean_df$Date[1:(numDays_int - lag_int)],
	PropPos = clean_df$tPropPos[1:(numDays_int - lag_int)],
	nHosp   = clean_df$tHosp[(lag_int + 1):numDays_int]
) %>% 
	mutate(nExpCases = ceiling(10000 * PropPos)) %>% 
	select(-PropPos)

head(lagged_df)
tail(lagged_df)

# Ok, so this is a serious problem. At the start of the pandemic, we tested
#   very few people, so we had very few cases. I think I need to use expected
#   cases per 10k tests, that way we have a "standardised" measurement of cases.
lm(nHosp ~ nExpCases, data = lagged_df) %>% 
	summary()
# R2 = 39%
ggplot(data = lagged_df) + 
	aes(x = nExpCases, y = nHosp, colour = Date) +
	geom_point()
# Holy crap. There is a totally different relationship between the cases ~ hosp
#   at the start of the pandemic. I need to filter to some date later? But when?
lagged_df %>% 
	mutate(early = Date < "2020-06-01") %>% 
	ggplot() + 
	aes(x = nExpCases, y = nHosp, colour = early) +
	geom_point()
# Well, June 1st it is. (Technically, June 2nd is 15 days after the economy was
#   reopened.)

lagged2_df <- 
	lagged_df %>% 
	mutate(early = Date < "2020-06-02")

lm(nHosp ~ nExpCases * early, data = lagged2_df) %>% 
	summary()
# R2 = 90% OMG!!!
# Accounting for the switch from first to second wave is massive. So, it looks
#   like every positive case per 10k yields 2.3 additional hospitalisations 15
#   days later. During the lockdowns of the first wave, every positive case per
#   10k yielded 0.1 additional hospitalisations 15 days later.
# To predict recent hospitalisations, here is the linear model:
#   nHosp = -191.4 + 2.3 * nExpCases,
# where nExpCases is the expected number of cases per 10k, calculated by taking
#   the product of the proportion of positive cases and 10,000. However, this
#   is only applicable after the effects of the first wave lockdown ended.
lm(
	nHosp ~ nExpCases,
	data = lagged2_df %>% filter(Date > "2020-06-01")
) %>% 
	summary()

###  Scatterplot  ###
lagged2_df %>% 
	filter(Date > "2020-06-01") %>% 
	ggplot() + 
	theme_bw() +
	aes(x = nExpCases, y = nHosp) +
	labs(
		title = "COVID-19 Community Spread vs. Hospitalisations after June 1st",
		subtitle = "Maximal lag between cases and hospitalisations in South Florida detected at 15 days.",
		caption = "Linear Trend: Hospitalisations = +2.3 x (Case Count per 10k) - 191.4",
		x = "Count of Positive COVID-19 Tests per 10,000 People",
		y = "Census Hospitalisations 15 Days Later"
	) +
	geom_point() +
	geom_abline(slope = 2.303, intercept = -191.395, colour = "blue")


###  Layered Bar Chart  ###
# I need the "cases" bars to always be higher than the number of hospitalisations
#   so I probably need to do cases per 50k for the bars
lagged2_df %>% 
	filter(Date > "2020-06-01") %>% 
	mutate(nExpCases = nExpCases * 3) %>% 
	ggplot() + 
	  theme_bw() +
	  aes(x = Date) +
	  scale_x_date(
	  	date_breaks = "1 month",
	  	labels = scales::date_format("%d-%b")
	  ) +
	  labs(
	  	title = "COVID-19 Community Spread and Hospitalisations after June 1st",
	  	subtitle = "Grey: Expected Positive COVID-19 Tests per 30,000 People Tested; Yellow: COVID-19 Hospitalisations 15 days later.",
	  	caption = "Maximal lag between cases and hospitalisations in South Florida detected at 15 days.\n Miami-Dade County has had roughly 30,000 total tests per day in December.\n Expected positive counts estimated as the product of the daily proportion positivity and 30,000.",
	  	x = "Date",
	  	y = "Count"
	  ) +
	  geom_col(aes(y = nExpCases)) +
	  geom_col(aes(y = nHosp), colour = "yellow")


