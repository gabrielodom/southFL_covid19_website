# Doubling Time
# Gabriel Odom
# 2020-07-01


######  Setup  ################################################################
# The DOH asked me today to estimate doubling time for new cases and proportion
#   of new cases

library(tidyverse)
library(lubridate)

mdCases_df <- read_csv(
	file = "../data/FLDH_COVID19_cases_miamidade_20200707.csv"
) 
mdCases2_df <- 
	mdCases_df %>% 
	mutate(PropPositive = 100 * Positive / (Positive + Negative)) %>% 
	mutate(PositivePer1k = (PropPositive / 100) * 1000) %>% 
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date)) 
	
	
	
######  Doubling Time Function Draft  #########################################

# Draft
# DoublingTime <- function(x, mostRecent = 14L) {
# 	# browser()
# 	
# 	# # Find the moving average
# 	# # From: https://stackoverflow.com/a/27986562/8366590
# 	# MovingAverage <- function(x, w){
# 	# 	res <- x
# 	# 	for(i in w:length(x)){
# 	# 		res[i] <- mean(x[(i - w):i])
# 	# 	}
# 	# 	res
# 	# }
# 	
# 	data_df <- data.frame(
# 		days = seq_len(mostRecent),
# 		x = x[(length(x) - mostRecent + 1):length(x)]
# 	)
# 	slope <- coefficients(
# 		lm(x ~ days, data = data_df)
# 	)[2]
# 	slope <- ifelse(test = slope < 0, yes = 0, no = slope)
# 	pctIncrease <- 100 * slope / data_df$x[1]
# 	# data_ls <- split(data_df, data_df$group)
# 	# growth_num <- vapply(
# 	# 	X = data_ls,
# 	# 	FUN = function(dataWindow_df){
# 	# 		
# 	# 		slope <- coefficients(
# 	# 			lm(x ~ days, data = dataWindow_df)
# 	# 		)[2]
# 	# 		# pctDelta <- (slope * window_int) / dataWindow_df$x[1]
# 	# 		# 
# 	# 		# ifelse(test = pctDelta > 0, yes = pctDelta, no = 0)
# 	# 	},
# 	# 	FUN.VALUE = numeric(1)
# 	# )
# 	
# 	unname(70 / pctIncrease)
# 	
# }


######  Doubling Time Function Clean  #########################################
DoublingTime <- function(x, mostRecent = 14L) {
	# Find the approximate doubling time for a vector with quasi-linear increase
	# Arguments:
	#   x: a vector of increasing values (not necessarily monotonic)
	#   mostRecent: how many periods should be used to estimate the doubling time?
	#     Defaults to 14.
	# Value: the estimated doubling time of an increasing vector over its last
	#   few entries.
	# Details: this function assumes that the increase observed in the most recent
	#   period can be approximated by a simple linear function. If your data show
	#   a quadratic or exponential increase, then use a smaller number of "most
	#   recent" values.
	
	# Make a data frame of the most recent number of days
	data_df <- data.frame(
		days = seq_len(mostRecent),
		x = x[(length(x) - mostRecent + 1):length(x)]
	)
	
	# Fit a linear model to the most recent values and extract the slope
	slope <- coefficients(
		lm(x ~ days, data = data_df)
	)[2]
	
	# In a perfect world, we wouldn't have negative values if we are asking about
	#   doubling time, but alas...
	slope <- ifelse(test = slope < 0, yes = 0, no = slope)
	
	# Find the percent increase that the slope represents over the first day in
	#   the "most recent" set.
	pctIncrease <- slope / data_df$x[1]
	
	# Find the doubling time.
	unname(
		log(2) / log(1 + pctIncrease)
	)
	
}

# Test
DoublingTime(1:20)
DoublingTime(mdCases2_df$PropPositive)
DoublingTime(mdCases2_df$Positive)
	
######  Values  ###############################################################
# The doubling time for proportion of positive cases is
DoublingTime(mdCases2_df$PropPositive)

# The doubling time for tests is
DoublingTime(mdCases2_df$Positive + mdCases2_df$Negative)

# The doubling time for positive cases is
DoublingTime(mdCases2_df$Positive)
