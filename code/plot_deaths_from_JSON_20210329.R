# Plot Deaths Over Time
# Deaths Data NOT from Linelist
# Gabriel Odom
# 2021-03-29
# updated 2021-03-29

library(lubridate)
library(jsonlite)
library(tidyverse)

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
# https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Florida_COVID_19_Deaths_by_Day/FeatureServer/0/query
#   has a point-and-click query interface. We check the data dictionary by
#   removing the "/query" from the end. We want the following information:
#   - Where: "Deaths">-900
#   - Out Fields: "Date1,Deaths"
#   - Format: JSON
# This will open a new page with the data in JSON format. We need to save this
#   as a .JSON or .txt file in the new deaths_JSON/ directory.
deaths_json <- jsonlite::read_json(
	path = "../data/deaths_JSON/deaths_20210329.json",
	simplifyVector = TRUE
) 



######  Step 2: Wrangle the Data  #############################################
ConvertJsonDate <- function(date_int) {
	# Function source: https://stackoverflow.com/questions/54450815/how-to-read-json-date-in-r
	# Checked against: https://www.freeformatter.com/epoch-timestamp-to-date-converter.html
	as.POSIXct(date_int / 1000, origin = "1970-01-01", tz = "GMT")
}

deaths_df <- 
	deaths_json$features %>%
	jsonlite::flatten() %>% 
	as_tibble() %>% 
	rename(
		Date = attributes.Date1,
		Deaths = attributes.Deaths
	) %>% 
	mutate(Date2 = ConvertJsonDate(Date)) %>% 
	mutate(DateClean = as_date(Date2)) %>% 
	select(DateClean, Deaths) %>% 
	arrange(DateClean)



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
