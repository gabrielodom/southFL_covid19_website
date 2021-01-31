# Plot Hospitalisations and Deaths for the West Florida Panhandle
# Gabriel Odom
# 2020-07-21
# UPDATE: 2020-11-11


library(tidyverse)

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



######  Hospitalisations  #####################################################
pandandleHosp_df <- 
	read_csv("../data/ESS_processed/ESS_panhandle_summary_20201213.csv") %>%
	group_by(Date) %>% 
	summarise(across(where(is.numeric), sum)) %>% 
	select(Date, Hospitalized, ICU, Ventilated) %>% 
	mutate(Hospitalized = RollUp(.x = Hospitalized, .w = 5, na.rm = TRUE)) %>% 
	mutate(ICU = RollUp(.x = ICU, .w = 5, na.rm = TRUE)) %>% 
	mutate(Ventilated = RollUp(.x = Ventilated, .w = 5, na.rm = TRUE)) %>% 
	pivot_longer(
		Hospitalized:Ventilated,
		names_to = "Type",
		values_to = "Count"
	)

startEndESS_date <- 
	pandandleHosp_df %>%
	slice(1, n()) %>%
	pull(Date) 

ggplot(
	data = pandandleHosp_df %>% 
		filter(Date >= "2020-10-01")
) +
	
	theme_bw() +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "bottom"
	) +
	aes(x = Date, y = Count, group = Type, colour = Type) +
	scale_x_date(
		date_breaks = "1 week",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	# scale_y_log10() +
	scale_color_manual(
		values = c(
			"Ventilated" = "#ff0000",
			"ICU" = "#ff7400",
			"Hospitalized" = "#ffc100"
		)
	) +
	labs(
		title = "Overall Hospital COVID-19 Census",
		subtitle = paste(
			"Escambia and Santa Rosa Counties;",
			"01 October",
			# format(startEndESS_date[1], "%d %B"),
			"to",
			format(startEndESS_date[2], "%d %B"),
			"2020"
		),
		y = "Counts (5-Day Moving Average)"
	) +
	
	geom_point(size = 2) # +
	# stat_smooth(se = FALSE)



######  Deaths  ###############################################################
deathsbyday_df <- read_csv(
	"../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20201108.csv"
) %>% 
	filter(County %in% c("Escambia", "Santa Rosa")) %>% 
	mutate(Count = RollUp(.x = Count, .w = 5, na.rm = TRUE)) %>% 
	# Ignore the last two weeks of deaths data because it is not complete
	filter(Date <= "2020-09-22")

ggplot(deathsbyday_df) +
	
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	aes(x = Date, y = Count) +
	# scale_y_log10() +
	scale_x_date(
		date_breaks = "1 week",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = paste("Deaths by Day for Escambia and Santa Rosa Counties"),
		y = "Counts (5-Day Moving Average)"
	) +
	
	geom_point() +
	stat_smooth(col = "red")

# Oh thank God