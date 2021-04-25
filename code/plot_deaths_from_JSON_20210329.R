# Plot Deaths Over Time
# Deaths Data NOT from Linelist
# Gabriel Odom
# 2021-03-29
# updated 2021-03-29

library(lubridate)
library(jsonlite)
library(tidyverse)

ConvertJsonDate <- function(date_int) {
	# Function source: https://stackoverflow.com/questions/54450815/how-to-read-json-date-in-r
	# Checked against: https://www.freeformatter.com/epoch-timestamp-to-date-converter.html
	as.POSIXct(date_int / 1000, origin = "1970-01-01", tz = "GMT")
}

######  Introduction  #########################################################
# Last fall, when I was really digging in to the daily deaths data (as part of
#   our preparation to submit the "Lessons Learned" manuscript), I grew more and
#   more suspicious that the linelist actually contained the date of death for
#   the individuals contained therein. We sent some emails to our local DoH
#   contacts, and (after what we later found out was a serious communication
#   failure) they confirmed that the linelist contained the date of death.
# We shared our material on Twitter, and some of the other epidemiologists
#   around the state pointed out that the linelist marked when the people were
#   diagnosed with COVID-19 and if they eventually died. None of the date fields
#   would be updated with date of death. Therefore, our "deaths by day" table
#   that we have been reporting since September was wrong: it showed the date of
#   diagnosis for those who would eventually die. Therefore, even our estimation
#   of death certification delay was overly inflated because it included the 
#   time of disease progression. Therefore, the 8-week certification delays we
#   were reporting were probably around 4-5 weeks, maybe even less.
# Finally, in March, we were able to track down where the state was reporting 
#   daily deaths for the entire state (still no idea how to get this data for
#   individual counties). As of today, Tim Norris (UM) helped me figure out how
#   to get the data in .JSON format (no .CSV, so we'll convert it here). Now
#   that we have this data, I can compare the counts we estimated before with
#   the accurate counts (for the state) to see how many weeks off we are.


######  Step 1: Get the Data  #################################################
# This website
# <https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Florida_COVID_19_Deaths_by_Day/FeatureServer/0/query>
#   has a point-and-click query interface. We check the data dictionary by
#   removing the "/query" from the end. We want the following information:
#   - Where: "Deaths">-900
#   - Out Fields: "Date1,Deaths"
#   - Format: JSON
# This will open a new page with the data in JSON format. We need to save this
#   as a .JSON or .txt file in the new deaths_JSON/ directory.
deaths_json <- jsonlite::read_json(
	path = "../data/deaths_JSON/deaths_20210425.json",
	simplifyVector = TRUE
) 



######  Step 2: Wrangle the Data  #############################################
deaths_df <- 
	deaths_json$features %>%
	jsonlite::flatten() %>% 
	as_tibble() %>% 
	rename(
		Date1 = attributes.Date1,
		Deaths = attributes.Deaths
	) %>% 
	mutate(Date2 = ConvertJsonDate(Date1)) %>% 
	mutate(Date = as_date(Date2)) %>% 
	select(Date, Deaths) %>% 
	arrange(Date)

deathsClean_df <- 
	tibble(
		Date = seq(as.Date('2020-03-05'), Sys.Date(), by = "day")
	) %>% 
	left_join(deaths_df) %>% 
	replace_na(list(Deaths = 0)) 

write_csv(
	deathsClean_df,
	file = "../data/deaths_JSON/FLDH_COVID19_deathsbyday_20210425.csv"
)
# 



######  Step 3: Match to our Previous Estimates  ##############################
deathsbyday_df <-
	# This data is from the linelist and is wrangled in "plot_deaths_20210124.R"
	# We update it every Sunday, but I pulled Monday's data so we can have a
	#   better shot at measuring the difference between the two data sources.
	read_csv(
	  "../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20210329.csv"
  ) %>% 
	group_by(Date) %>% 
	summarise(Count = sum(Count))

# After inspecting the data, we have to "fill out" the missing days at the start
#   of the pandemic which have 0 deaths. Both data sets start on 5th March.
deathsCompare_df <- 
	tibble(
		Date = seq(as.Date('2020-03-05'), Sys.Date(), by = "day")
	) %>% 
	left_join(deaths_df, by = c("Date" = "DateClean")) %>% 
	rename(DailyDeaths = Deaths) %>% 
	left_join(deathsbyday_df, by = c("Date")) %>% 
	rename(LinelistDeaths = Count) %>% 
	replace_na(list(DailyDeaths = 0, LinelistDeaths = 0))



######  Step 4: Measure the Difference  #######################################
ggplot(data = deathsCompare_df) +
	aes(x = Date) +
	geom_point(aes(y = DailyDeaths), colour = "green") +
	geom_point(aes(y = LinelistDeaths), colour = "orange") 
	# scale_y_sqrt()
# well, there is a very strong relationship

ggplot(data = deathsCompare_df) +
	aes(x = LinelistDeaths, y = DailyDeaths) + 
	geom_point() 
	# scale_y_sqrt()
# and as I expected, it's heteroskedastic. Let's look at the residuals over time

deathsPredict_mod <- lm(
	sqrt(DailyDeaths) ~ sqrt(LinelistDeaths),
	data = deathsCompare_df
)
plot(deathsPredict_mod$residuals)

plot(deathsCompare_df$LinelistDeaths - deathsCompare_df$DailyDeaths)
difference_ts <- ts(
	sqrt(deathsCompare_df$LinelistDeaths) - sqrt(deathsCompare_df$DailyDeaths)
)
pacf(difference_ts)

difference_arima <- forecast::auto.arima(difference_ts)
plot(residuals(difference_arima))
Box.test(residuals(difference_arima), type = "Ljung-Box")
tseries::adf.test(residuals(difference_arima))
pacf(residuals(difference_arima))

difference_711arima <- arima(difference_ts, c(7, 1, 1))
plot(residuals(difference_711arima))
pacf(residuals(difference_711arima))
# We need p = 21, but a p = 7 model is more parsimonious. I can defend a 7-day
#   autoregressive process for deaths, but not a 21-day one.
tseries::adf.test(residuals(difference_711arima)) # Stationary
plot(fitted.values(difference_711arima))
plot(
	(sqrt(deathsCompare_df$LinelistDeaths) -
	 	fitted.values(difference_711arima)) ^ 2,
	xlab = "Day",
	ylab = "Deaths by Day in FL",
	main = "Predicted (Black) vs. Observed (Red) Daily Deaths"
)
lines(ts(deathsCompare_df$DailyDeaths), col = "red")

summary(difference_711arima)

difference_ets <- forecast::ets(difference_ts)
plot(residuals(difference_ets))
plot(fitted.values(difference_ets))



######  Step 5: Measure Death Certification Delay  ############################
# When I originally wrote this script, I only had access to the death-by-day
#   data for one week, so I compared it against the linelist data. I still don't
#   know how to estimate the lag between the linelist and state reported data
#   (recall that the linelist date is when the patients were marked as COVID-19
#   positive, and later an indicator is added if they died). Regardless, one 
#   week has passed, so I can compare the deaths-by-day data from last week to
#   this week in order to estimate the distribution of the certification delay.

dateSequence_df <- tibble(
	Date = seq(as.Date('2020-03-05'), as.Date('2021-04-18'), by = "day")
)

###  Last Week's Data  ###
deathsOld_json <- jsonlite::read_json(
	path = "../data/deaths_JSON/deaths_20210418.json",
	simplifyVector = TRUE
) 

deathsOld_df <- 
	deathsOld_json$features %>%
	jsonlite::flatten() %>% 
	as_tibble() %>% 
	rename(
		Date1 = attributes.Date1,
		Deaths = attributes.Deaths
	) %>% 
	mutate(Date2 = ConvertJsonDate(Date1)) %>% 
	mutate(Date = as_date(Date2)) %>% 
	select(Date, Deaths) %>% 
	arrange(Date)


###  Recent Data  ###
deathsNew_json <- jsonlite::read_json(
	path = "../data/deaths_JSON/deaths_20210425.json",
	simplifyVector = TRUE
)

deathsNew_df <- 
	deathsNew_json$features %>%
	jsonlite::flatten() %>% 
	as_tibble() %>% 
	rename(
		Date1 = attributes.Date1,
		Deaths = attributes.Deaths
	) %>% 
	mutate(Date2 = ConvertJsonDate(Date1)) %>% 
	mutate(Date = as_date(Date2)) %>% 
	select(Date, Deaths) %>% 
	arrange(Date)


###  Compare the Counts  ###
deathsCountsCompared_df <- 
	dateSequence_df %>% 
	left_join(deathsOld_df) %>% 
	rename(DeathsOld = Deaths) %>% 
	left_join(deathsNew_df) %>% 
	rename(DeathsNew = Deaths) %>% 
	replace_na(list(DeathsOld = 0, DeathsNew = 0)) %>% 
	mutate(NewlyAddedDeaths = DeathsNew - DeathsOld)

totalNewDeaths_int <- 
	deathsCountsCompared_df %>% 
	filter(NewlyAddedDeaths > 0) %>% 
	pull(NewlyAddedDeaths) %>% 
	sum()

# 75th Pctile Date: new deaths added after this date represent the most delayed
#   quarter of deaths
deathsCountsCompared_df %>% 
	mutate(CumlNewDeaths = cumsum(NewlyAddedDeaths)) %>% 
	filter(CumlNewDeaths < totalNewDeaths_int / 4) %>% 
	slice(n()) %>% 
	pull(Date)
# "2021-03-11" on 4 April; 3 weeks
# Our P75 estimate for the state based on line list data was 13 weeks this week. 
#   This means that something is very wrong with the how deaths are being
#   recorded in the line list.
# "2021-03-09" on 11 April; 5 weeks
# "2021-03-24" on 18 April; 4 weeks
# "2021-03-31" on 25 April; 4 weeks

# 50th Pctile Date: new deaths added after this date represent the most delayed
#   half of deaths
deathsCountsCompared_df %>% 
	mutate(CumlNewDeaths = cumsum(NewlyAddedDeaths)) %>% 
	filter(CumlNewDeaths < totalNewDeaths_int / 2) %>% 
	slice(n()) %>% 
	pull(Date)
# "2021-03-18" on 4 April; 2 weeks
# Our P50 estimate for the state based on line list data was 7 weeks this week.
# "2021-03-22" on 11 April; 3 weeks
# "2021-04-01" on 18 April; 2 weeks
# "2021-04-09" on 25 April; 2 weeks



###  Plot the Delay  ###
ggplot(
	data = deathsCountsCompared_df %>% 
		filter(NewlyAddedDeaths > 0)
) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	aes(x = Date, y = NewlyAddedDeaths) + 
	scale_x_date(
		date_breaks = "1 month",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	geom_col()

# We see that most of the deaths are added within the past month, but there are
#   a few that go back to last summer.



######  Step 6: Plot Deaths with Certainty  ###################################
# Now that we are confident that the data from 11 March and before is "stable" 
#   (as of 4 April anyway), we can build a plot of deaths over time.
deathsClean_df <- read_csv(
	file = "../data/deaths_JSON/FLDH_COVID19_deathsbyday_20210425.csv"
) %>% 
	filter(Date < "2021-03-31")

ggplot(data = deathsClean_df) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	aes(x = Date, y = Deaths) +
	scale_x_date(
		date_breaks = "1 month",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	geom_point()
