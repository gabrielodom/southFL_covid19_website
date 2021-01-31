# Plot Deaths Over Time
# Gabriel Odom
# 2020-07-12

library(tidyverse)
library(lubridate)
library(readxl)



######  Death Data from Roy's "Connections"  ##################################
# Through some black magic, Roy was able to get the .xlsx file *behind* the PDF
#   report that Trepka sent out. This makes life so much easier. See this slug:
# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/state_linelist_20200902.xlsx
# NOTE: this link no longer works after 9/2.
# NOTE: if you don't want to clean the data yourself, skip to the read_csv call
deaths_df <- read_excel(
	path = "../data/deaths/state_linelist_20200830.xlsx",
	sheet = "Deaths",
	skip = 4
) %>% 
	# Clean up the dates
	mutate(Date = as_date(`Date case counted`)) %>% 
	select(-`Date case counted`)

# Newly-added deaths?
deaths_df %>% 
	filter(`Newly identified death` == "Yes") %>% 
	filter(County == "Dade") %>% 
	arrange(Date) %>% 
	View
# New deaths are added even back a full month. We are getting more and more 
#   backlogged deaths.
# On 5 August, they added 51 deaths to Miami-Dade County. Of those, 23 were
#   added between 16-31 July, and 21 were added between 1-15 July. These counts
#   cannot be expected as accurate until a full month passes.
# As of 19 August, we added 40 deaths--22 of which were over 1 month old (before
#   19 July). 

deathsbyday_df <-
	deaths_df %>%
	arrange(County, Date) %>%
	# Group deaths by day+county and count how many
	group_by(County, Date) %>%
	add_tally(name = "Count") %>%
	# Remove duplicate rows
	select(County, Date, Count) %>%
	distinct() %>%
	ungroup()



###  Save  ###
write_csv(
	x = deathsbyday_df,
	path = "../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20200830.csv"
)


###  Import Cleaned Deaths Data  ###
deathsbyday_df <- read_csv(
	"../data/deaths/FLDH_COVID19_deathsbyday_bycounty_20200830.csv"
)


###  Plot County Deaths over Time  ###
whichCounty <- "Dade" # "Palm Beach" # "Broward"

ggplot(
	deathsbyday_df %>% 
		filter(County == whichCounty) %>% 
		# Ignore the last ~~2~~ 3/4 weeks of deaths data because it is not complete
		# As of 5 August, ignore the last ~~month~~ 5 weeks of deaths data (8/23)
		filter(Date <= "2020-07-19")
) +
	
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
		title = paste("Deaths by Day for", whichCounty, "County")
	) +
	
	geom_point() +
	stat_smooth()


###  Plot State Deaths over Time  ###
ggplot(
	deathsbyday_df %>% 
		group_by(Date) %>% 
		summarise(Count = sum(Count)) %>% 
	  # Ignore the last ~~2~~ ~~3 weeks~~ month of deaths data because it is not
		#   complete
		# On 23 August, we now have to ignore the last 6.5 weeks of deaths data.
	  filter(Date <= "2020-07-15")
) +
	
	theme_bw() +
	theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
	aes(x = Date, y = Count) +
	scale_x_date(
		date_breaks = "1 week",
		date_minor_breaks = "1 day",
		labels = scales::date_format("%d-%b")
	) +
	labs(
		title = "Deaths by Day for the State of Florida"
	) +
	
	geom_point() +
	stat_smooth()



######  Import All Deaths  ####################################################

###  Import All Raw Deaths Files  ###
end_Date <- ymd("20200830")
# From my exploration, dates at that slug/nugget start at July 1st
dates  <- format(seq(ymd("20200701"), end_Date, by = "days"), "%Y%m%d")
fileTo <- paste0("../data/deaths/florida_linelist_", dates, ".xlsx")

# slug   <- "http://ww11.doh.state.fl.us/comm/_partners/"
# nugget <- "covid19_report_archive/state_linelist_"
# urls   <- paste0(slug, nugget, dates, ".xlsx")

# library(httr)
# # This downloads the data at the website urls[1] into the file fileTo[1]; the
# #   write_disk() function tells GET() to store the data on the hard disk, not
# #   in RAM
# map(
# 	.x = seq_along(dates),
# 	.f = ~{ httr::GET(urls[.x], write_disk(fileTo[.x])) }
# )


###  Deaths July 1st and After  ###
deaths_ls <- 
	map(
		.x = fileTo,
		.f = ~{
			# browser()
			
			df <- read_excel(
				path = .x,
				sheet = "Deaths",
				skip = 4
			) %>% 
				# Clean up the dates
				mutate(Date = as_date(`Date case counted`)) %>% 
				select(-`Date case counted`)
			
			colName <- str_extract(.x, pattern = "[0-9]{8}")
			
			df %>% 
				arrange(County, Date) %>%
				# Group deaths by day+county and count how many
				group_by(County, Date) %>%
				add_tally(name = colName) %>%
				# Remove duplicate rows
				select(County, Date, one_of(colName)) %>%
				distinct() %>%
				ungroup()
			
		}
	)

newDeaths_df <- 
	reduce(
		.x = deaths_ls,
		.f = full_join,
		by = c("County", "Date")
	)

rm(deaths_ls)


###  Baseline Deaths (prior to July 1st)  ###
baselineDeaths_df <-
	read_excel(
	  path = fileTo[1],
	  sheet = "Deaths",
	  skip = 4
  ) %>% 
	# Clean up the dates
	mutate(Date = as_date(`Date case counted`)) %>% 
	select(-`Date case counted`) %>% 
	# Remove the newly-added dates on the first day to yield baseline deaths
	filter(is.na(`Newly identified deaths`)) %>% 
	arrange(County, Date) %>%
	# Group deaths by day+county and count how many
	group_by(County, Date) %>%
	add_tally(name = "Baseline") %>%
	# Remove duplicate rows
	select(County, Date, Baseline) %>%
	distinct() %>%
	ungroup() 


###  County x Date Grid  ###
start_Date <- min(
	min(newDeaths_df$Date), min(baselineDeaths_df$Date)
)
counties_char <- union(
	newDeaths_df$County, baselineDeaths_df$County
)

dateGrid_df <-
	full_join(
		tibble(County = counties_char),
		tibble(Date = seq(start_Date, end_Date, by = "days")),
		by = character()
	)


###  Full Data  ###
fullDeathByDay_df <-
	full_join(
		dateGrid_df,
		baselineDeaths_df,
		by = c("County", "Date")
	) %>% 
	full_join(
		newDeaths_df,
		by = c("County", "Date")
	)

# write_csv(
# 	fullDeathByDay_df,
# 	path = paste0(
# 		"../data/deaths/reported_deathsXday_",
# 		format(end_Date, "%Y%m%d"),
# 		".csv"
# 	)
# )



######  Plot Deaths by Day Reported  ##########################################
fullDeathByDay_df <-
	read_csv("../data/deaths/reported_deathsXday_20200830.csv")

# State of Florida
ggplot(
	data = fullDeathByDay_df %>% 
		group_by(Date) %>% 
		summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% 
		filter(Date <= "2020-07-01")
) +
	theme_bw() +
	aes(x = Date) +
	labs(
		title = "Deaths by Their Date Reported",
		subtitle = "State of Florida; March through July",
		y = "Recorded Deaths",
		caption =
			"Estimate Legend: Black = day of reporting; Red = reported two weeks later;\nOrange = reported one month later; Yellow = reported six week later; Green  = reported two months later"
	) +
  stat_smooth(aes(y = `20200701`), colour = "black", se = FALSE) +
	stat_smooth(aes(y = `20200715`), colour = "red", se = FALSE) +
	stat_smooth(aes(y = `20200730`), colour = "orange", se = FALSE) +
	stat_smooth(aes(y = `20200815`), colour = "yellow", se = FALSE) +
	stat_smooth(aes(y = `20200830`), colour = "green", se = FALSE)


# Miami-Dade County
ggplot(
	data = fullDeathByDay_df %>% 
		filter(County == "Dade") %>% 
		filter(Date <= "2020-07-01")
) +
	theme_bw() +
	aes(x = Date) +
	labs(
		title = "Deaths by Their Date Reported",
		subtitle = "Miami-Dade County; March through July",
		y = "Recorded Deaths",
		caption =
			"Estimate Legend: Black = day of reporting; Red = reported two weeks later;\nOrange = reported one month later; Yellow = reported six week later; Green  = reported two months later"
	) +
	stat_smooth(aes(y = `20200701`), colour = "black", se = FALSE) +
	stat_smooth(aes(y = `20200715`), colour = "red", se = FALSE) +
	stat_smooth(aes(y = `20200730`), colour = "orange", se = FALSE) +
	stat_smooth(aes(y = `20200815`), colour = "yellow", se = FALSE) +
	stat_smooth(aes(y = `20200830`), colour = "green", se = FALSE)
