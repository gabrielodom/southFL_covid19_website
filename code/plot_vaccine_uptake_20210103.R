# Plot Vaccinations Over Time
# Gabriel Odom
# 2020-07-12

library(tidyverse)
library(lubridate)
library(readxl)


######  Helper Functions  #####################################################
RollUp <- function(.x, .w, .f = mean, ...){
	# Find "Rolling" values of a specified function
	# Inputs:
	#   .x: a vector (usually numeric) over which to "roll" the function .f
	#   .w: the window size
	#   .f: the fuction to "roll" over the values of .x
	#   ...: additional arguments to .f
	# Output: a vector the length of .x with the first (.w - 1) values set to NA
	#   and the remaining values equal to .f evaluated over the previous window.
	# Details: for a five-day moving average, set .w = 5. Then the moving average
	#   of .x at index 5 will be mean(.x[1], .x[2], ..., .x[5]).
	# Examples:
	#   # Rolling mean of first five integers
	#   RollUp(.x = 1:10, .w = 5)
	
	n <- length(.x)
	out <- rep(NA, times = n)
	class(out) <- class(.x)
	
	for(i in .w:n) {
		out[i] <- .f(.x[(i - .w + 1):i])
	}
	
	out
	
}



######  Data  #################################################################
dailyVaccinesFL_df <- read_excel(
	path = "../data/vaccinations/sfl_vaccinations_20210103.xlsx",
	sheet = "State Counts"
) %>% 
	mutate(Date = as_date(Date)) %>% 
	mutate(CountMA = RollUp(`First Dose`, .w = 3))

# We have weekly data at the county level, so no MA
cumlVaccinesSFL_df <- read_excel(
	path = "../data/vaccinations/sfl_vaccinations_20210103.xlsx",
	sheet = "County Counts"
) %>% 
	mutate(Date = as_date(Date)) %>% 
	rename(CumlCount = `First Dose (Cml)`)



######  Plot Statewide Vaccination  ###########################################
ggplot(data = dailyVaccinesFL_df) +
	theme_bw() +
	aes(x = Date, y = CountMA) +
	scale_x_date(
		date_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = "Daily Vaccinations for the State of Florida",
		subtitle = "3-Day Moving Average of First-Dose Counts",
		y = "Number of Persons Vaccinated per Day"
	) +
	geom_point(colour = "blue", size = 2)



######  Plot South Florida Cumulative Vaccinations  ###########################
cbPalette <- c(
	"#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
	"#999999"
)

ggplot(data = cumlVaccinesSFL_df) +
	theme(legend.position = "bottom") +
	aes(x = Date, y = CumlCount, group = County, fill = County) +
	scale_x_date(
		date_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) + 
	scale_fill_manual(values = cbPalette) +
	labs(
		title = "Weekly Cumulative Vaccinations for South Florida",
		y = "Number of Persons Vaccinated per Week"
	) +
	geom_col(position = "dodge", colour = "black")
