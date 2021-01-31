# Public Health Interventions Comparison
# Gabriel Odom
# 2020-11-12

# On 2020-11-11, Mayor Suarez called me; he asked me to create a report
#   comparing the "mask in public" rule to DeSantis' executive orders. After
#   conferring with Profs. Trepka and Bursac, Mr. Williams, as well as Miami
#   Fire-Rescue, we believe that the most effective argument will be to show
#   the proportion of positive COVID-19 tests in the six weeks following each
#   intervention (coupled with a slide of "fast facts").



######  Data and Packages  ####################################################
library(tidyverse)
library(lubridate)

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
		out[i] <- .f(.x[(i - .w + 1):i], ...)
	}
	
	out
	
}

cases_df <- read_csv(
	file = "../data/cases/FLDH_COVID19_cases_miamidade_20201115.csv"
) %>% 
	mutate(PropPositive = 100 * Positive / (Positive + Negative)) %>% 
	mutate(PropPositive = RollUp(PropPositive, .w = 7, na.rm = TRUE)) %>%  
	mutate(Positive = RollUp(Positive, .w = 7, na.rm = TRUE)) %>%  
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date))

startEnd_date <- 
	cases_df %>%
	slice(1, n()) %>%
	pull(Date) %>%
	format("%d %B")



######  Define Interventions  #################################################
# Full lockdown:
lockdownStart_Date <- as_date("2020-04-01")

# Remove full lockdown
lockdownEnd_Date <- as_date("2020-05-18")

# From MFR, the county started enforcing the "mask in public" rule on 20 June?
#   See: https://www.miamidade.gov/information/library/06.19.20-emergency-order-23-20-amendment-3.pdf
# Nope, 25 June:
# https://www.miamigov.com/files/sharedassets/public/news/2020/0625-emergency-order-20-16.pdf
maskStart_Date <- as_date("2020-06-25")

# Additionally, the county suspended enforcement of said rule on 26 September.
#   See: https://www.miamidade.gov/information/library/coronavirus-emergency-order-30-20.pdf
maskEnd_Date <- as_date("2020-09-26")

# Schools re-opened for all on MDC on 9 October
# See: http://pdfs.dadeschools.net/reopening/09-29-20_Approved_Schoolhouse_Return_Dates_for_Stage_II.pdf
schoolStart_Date <- as_date("2020-10-09")

# yesMasks_df <- 
# 	cases_df %>% 
# 	filter(Date >= maskStart_Date - 14) %>% 
# 	filter(Date < maskStart_Date + 42) %>% 
# 	rowwise() %>% 
# 	mutate(
# 		IncubationPeriodAlpha = if_else(
# 			condition = Date > maskStart_Date && Date <= maskStart_Date + 14,
# 			true = 0.1,
# 			false = 1
# 		)
# 	) 
# 
# yesSchool_df <- 
# 	cases_df %>% 
# 	filter(Date >= schoolStart_Date - 21) %>% 
# 	filter(Date < schoolStart_Date + 35) %>%  
# 	rowwise() %>% 
# 	mutate(
# 		IncubationPeriodAlpha = if_else(
# 			condition = Date > schoolStart_Date && Date <= schoolStart_Date + 14,
# 			true = 0.1,
# 			false = 1
# 		)
# 	) 



######  Plots v1  #############################################################
# # UPDATE 2020-11-16: these "zoom and enhance" plots are not what we want.
# ggplot(data = yesMasks_df) +
# 	
# 	theme_bw() +
# 	theme(legend.position = "none") +
# 	aes(x = Date) + 
# 	scale_y_continuous(limits = c(5, 25)) +
# 	labs(
# 		title = "Proportion of Positive COVID-19 Tests by Date",
# 		subtitle = "Miami-Dade County, June 11th to August 5th",
# 		caption = "Enforcement of a 'Mask in Public' rule effective June 25th (blue triangle).
# 		Two-week policy delay due to virus incubation shown by transparent dots.",
# 		y = "Proportion of Positive COVID-19 Tests (7-day Moving Average)"
# 	) +
# 	
# 	geom_point(
# 		aes(y = PropPositive, alpha = IncubationPeriodAlpha),
# 		size = 3
# 	) +
# 	geom_point(
# 		aes(x = maskStart_Date, y = 5),
# 		pch = 24, size = 3, fill = "blue"
# 	)
# 
# ggplot(data = yesSchool_df) +
# 
# 	theme_bw() +
# 	theme(legend.position = "none") +
# 	aes(x = Date) + 
# 	scale_y_continuous(limits = c(0, 10)) +
# 	labs(
# 		title = "Proportion of Positive COVID-19 Tests by Date",
# 		subtitle = "Miami-Dade County, September 12th to November 6th",
# 		caption = "Suspended enforcement of a 'Mask in Public' rule effective September 26th
# 		 (yellow triangle).	Schools resume operation October 9th (red triangle).
# 		Two-week policy delay due to virus incubation shown by transparent dots.",
# 		y = "Proportion of Positive COVID-19 Tests (7-day Moving Average)"
# 	) +
# 		
# 	geom_point(
# 		aes(x = Date, y = PropPositive, alpha = IncubationPeriodAlpha),
# 		size = 3
# 	) +
# 	geom_point(
# 		aes(x = maskEnd_Date, y = 0),
# 		pch = 24, size = 3, fill = "gold"
# 	) +
# 	geom_point(
# 		aes(x = schoolStart_Date, y = 0),
# 		pch = 24, size = 3, fill = "red"
# 	)



######  Additional Case Figures  ##############################################
# On 2020-11-13, Mayor Suarez spoke with me, Prof. Trepka, Prof. Bursac, and 
#   Dr. Hevia (from MFR). He would like to see the "overall effect" of the mask
#   in public rule, as well as include the hospitalizations as supplemental 
#   information. For the "overall effect", we aren't sure if he wants cases or
#   case positivity rate (he mentioned both and was not clear), so we will 
#   build and include both and let him decide.


cases2_df <- 
	cases_df %>% 
	rowwise() %>% 
	mutate(
		interventionAlpha = case_when(
			Date > lockdownStart_Date && Date <= lockdownStart_Date + 14 ~
				0.1,
			Date > lockdownEnd_Date && Date <= lockdownEnd_Date + 14 ~
				0.1,
			Date > maskStart_Date && Date <= maskStart_Date + 14 ~
				0.1,
			Date > schoolStart_Date && Date <= schoolStart_Date + 14 ~
				0.1,
			TRUE ~ 1
		)
	)


###  Cases  ###
ggplot(data = cases2_df) +
	
	theme_bw() +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "none"
	) +
	aes(x = Date, y = Positive) + 
	scale_x_date(
		date_breaks = "1 month",
		# date_minor_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) +
	# scale_y_log10() +
	labs(
		title = "Number of Positive COVID-19 Tests Results by Day",
		subtitle = paste(
			"Miami-Dade County;",
			startEnd_date[1], "to", startEnd_date[2], "2020"
		),
		# caption = "Enforcement of a 'Mask in Public' rule effective June 25th (blue triangle). Suspended enforcement of a 'Mask in Public' rule effective September 26th
		# (yellow triangle). Schools resume operation October 9th (red triangle). Two-week policy delay due to virus incubation shown by transparent dots.",
		y = "Count of Positive Tests (7-day Moving Average)"
	) + 
	
	geom_point(aes(alpha = interventionAlpha), size = 2) +
	geom_point(
		aes(x = lockdownStart_Date, y = 1),
		pch = 21, size = 3, fill = "blue"
	) +
	geom_point(
		aes(x = lockdownEnd_Date, y = 1),
		pch = 21, size = 3, fill = "red"
	) +
	geom_point(
		aes(x = maskStart_Date, y = 1),
		pch = 24, size = 3, fill = "blue"
	) +
	geom_point(
		aes(x = maskEnd_Date, y = 1),
		pch = 24, size = 3, fill = "gold"
	) +
	geom_point(
		aes(x = schoolStart_Date, y = 1),
		pch = 24, size = 3, fill = "red"
	)

# It is my opinion that the "number of cases" graph is not clear.


###  Proportion  ###
ggplot(data = cases2_df) +
	
	theme_bw() +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "none"
	) +
	aes(x = Date, y = PropPositive) + 
	scale_x_date(
		date_breaks = "1 month",
		# date_minor_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = "Proportion of Positive COVID-19 Tests Results by Day",
		subtitle = paste(
			"Miami-Dade County;",
			startEnd_date[1], "to", startEnd_date[2], "2020"
		),
		# caption = "Enforcement of a 'Mask in Public' rule effective June 25th (blue triangle). Suspended enforcement of a 'Mask in Public' rule effective September 26th
		# (yellow triangle). Schools resume operation October 9th (red triangle). Two-week policy delay due to virus incubation shown by transparent dots.",
		y = "Proportion of Positive Tests (7-day Moving Average)"
	) + 
	
	geom_point(aes(alpha = interventionAlpha), size = 2) +
	geom_point(
		aes(x = lockdownStart_Date, y = 0),
		pch = 21, size = 3, fill = "blue"
	) +
	geom_point(
		aes(x = lockdownEnd_Date, y = 0),
		pch = 21, size = 3, fill = "red"
	) +
	geom_point(
		aes(x = maskStart_Date, y = 0),
		pch = 24, size = 3, fill = "blue"
	) +
	geom_point(
		aes(x = maskEnd_Date, y = 0),
		pch = 24, size = 3, fill = "gold"
	) +
	geom_point(
		aes(x = schoolStart_Date, y = 0),
		pch = 24, size = 3, fill = "red"
	)



######  Additional Hospitalisation Figures  ###################################
# We will show the ICU use over time, while noting that the median time from
#   symptom onset to hospital admission is 8 days and ICU admission is 10 days.
#   See: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7189401/#Sec9title
# Of note, for patients who end up hospitalised, the median incubation time is
#   5 days, so our window is 13-15 days.


###  Data  ###
hospitalisations_df <- 
	read_csv(
		file = "../data/ESS_processed/ESS_southFL_summary_20201115.csv"
	) %>% 
	filter(County == "MIAMI-DADE")

hospitalisations_df[
	hospitalisations_df$Date == "2020-06-04", "Ventilated"
] <- NA_real_
# There is an error in the original data for June 4th. Rather than deleting the
#   whole row of the raw data, we are skipping this ventilation value

startEndESS_date <- 
	hospitalisations_df %>%
	slice(1, n()) %>%
	pull(Date) 

hospitalisations2_df <- 
	hospitalisations_df %>% 
	mutate(Hospitalized = RollUp(.x = Hospitalized, .w = 7, na.rm = TRUE)) %>% 
	rowwise() %>% 
	mutate(
		interventionAlpha = case_when(
			Date > lockdownStart_Date && Date <= lockdownStart_Date + 14 ~
				0.1,
			Date > lockdownEnd_Date && Date <= lockdownEnd_Date + 14 ~
				0.1,
			Date > maskStart_Date && Date <= maskStart_Date + 14 ~
				0.1,
			Date > schoolStart_Date && Date <= schoolStart_Date + 14 ~
				0.1,
			TRUE ~ 1
		)
	) %>% 
	select(Date, County, Hospitalized, interventionAlpha)

# hospitalisations2_df <- 
# 	hospitalisations_df %>% 
# 	mutate(Hospitalized = RollUp(.x = Hospitalized, .w = 7, na.rm = TRUE)) %>% 
# 	mutate(ICU = RollUp(.x = ICU, .w = 7, na.rm = TRUE)) %>% 
# 	mutate(Ventilated = RollUp(.x = Ventilated, .w = 7, na.rm = TRUE)) %>% 
# 	rowwise() %>% 
# 	mutate(
# 		interventionAlpha = case_when(
# 			Date > maskStart_Date && Date <= maskStart_Date + 14 ~
# 				0.1,
# 			Date > schoolStart_Date && Date <= schoolStart_Date + 14 ~
# 				0.1,
# 			TRUE ~ 1
# 		)
# 	)


# miamidadeCOVID_df <-
# 	hospitalisations2_df %>%
# 	select(-County, -AdmitPrevDay, -DischPrevDay, -DeltaAdmit) %>% 
# 	pivot_longer(
# 		Hospitalized:Ventilated,
# 		names_to = "Type",
# 		values_to = "Count"
# 	)


###  Figure  ###
ggplot(data = hospitalisations2_df) +
	# ggplot(data = miamidadeCOVID_df) +
	
	theme_bw() +
	theme(
		axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "top"
	) +
	# aes(x = Date, y = Count, group = Type, colour = Type) +
	aes(x = Date, y = Hospitalized) +
	scale_x_date(
		date_breaks = "1 month",
		# date_minor_breaks = "1 week",
		labels = scales::date_format("%d-%b")
	) +
	# scale_y_log10() +
	# scale_color_manual(
	# 	values = c(
	# 		"Ventilated" = "#ff0000",
	# 		"ICU" = "#ff7400",
	# 		"Hospitalized" = "#ffc100"
	# 	)
	# ) +
	labs(
		title = "Overall Miami-Dade County Hospital COVID-19 Census",
		subtitle = paste(
			"Miami-Dade County;",
			format(startEndESS_date[1], "%d %B"),
			"to",
			format(startEndESS_date[2], "%d %B"),
			"2020"
		),
		# caption = "Enforcement of a 'Mask in Public' rule effective June 25th (blue triangle). Suspended enforcement of a 'Mask in Public' rule effective September 26th
		# (yellow triangle). Schools resume operation October 9th (red triangle). Two-week policy delay due to virus incubation shown by transparent dots.",
		y = "Counts (7-day Moving Average)"
	) +
	
	geom_point(aes(alpha = interventionAlpha), size = 2) +
	guides(alpha = FALSE) +
	geom_point(
		aes(x = lockdownStart_Date, y = 250),
		pch = 21, size = 3, fill = "blue"
	) +
	geom_point(
		aes(x = lockdownEnd_Date, y = 250),
		pch = 21, size = 3, fill = "red"
	) +
	geom_point(
		aes(x = maskStart_Date, y = 250),
		pch = 24, size = 3, colour = "black", fill = "blue"
	) +
	geom_point(
		aes(x = maskEnd_Date, y = 250),
		pch = 24, size = 3, colour = "black", fill = "gold"
	) +
	geom_point(
		aes(x = schoolStart_Date, y = 250),
		pch = 24, size = 3, colour = "black", fill = "red"
	)
	
	
	