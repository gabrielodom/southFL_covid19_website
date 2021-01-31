# Plot Deaths Over Time
# Gabriel Odom
# 2020-07-12

library(tidyverse)
library(lubridate)
library(readxl)



######  Death Data from Roy's "Connections"  ##################################
# Through some black magic, Roy was able to get the .xlsx file *behind* the PDF
#   report that Trepka sent out. This makes life so much easier. See this slug:
# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/state_linelist_20200830.xlsx
# NOTE: if you don't want to clean the data yourself, skip to the read_csv call
deaths_df <- read_excel(
	path = "../data/state_linelist_20200830.xlsx",
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
	path = "../data/FLDH_COVID19_deathsbyday_bycounty_20200830.csv"
)


###  Import Cleaned Deaths Data  ###
deathsbyday_df <- read_csv(
	"../data/FLDH_COVID19_deathsbyday_bycounty_20200830.csv"
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
