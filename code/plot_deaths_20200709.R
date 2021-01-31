# Plot Deaths Over Time
# Gabriel Odom
# 2020-07-06

library(tidyverse)
library(lubridate)
library(readxl)

######  Death Data from Dr. Villalta  #########################################
deaths_df <- read_csv("../data/miami_deaths_20200707.csv")
deathsbyday_df <- 
	deaths_df %>% 
	# Group deaths by day and count how many
	group_by(`Date case counted`) %>% 
	add_tally() %>% 
	# Remove duplicate rows
	select(`Date case counted`, n) %>% 
	distinct() %>% 
	# Clean up the dates
	mutate(Date_POSIX = mdy(`Date case counted`)) %>% 
	mutate(Date = as_date(Date_POSIX)) %>% 
	ungroup() %>% 
	select(Date, n)

ggplot(deathsbyday_df) +
	aes(x = Date, y = n) +
	geom_point() +
	stat_smooth()


######  FLDH Death Data  ######################################################
# We take the state linelist data:
# http://ww11.doh.state.fl.us/comm/_partners/covid19_report_archive/state_linelist_latest.pdf
#   We save that as a PDF, extract the first 60-80 pages for deaths only, then
#   export it as an excel file (using Adobe Acrobat Pro). Once we have an .xlsx
#   document, we need to go in and manually delete the repeated header row,
#   undo column merges, copy each column manually into another sheet (because
#   the merge and center that Adobe does in the export is atrocious), then
#   further reformat any text information.
# The intermediary pieces are in
#   - state_deaths_linelist_20200709.xlsx
#   - state_linelist_latest_deaths_clean-ish_20200709.xlsx
#   - state_linelist_latest_deaths_clean-ish2_20200709.xlsx
# The final product is
deathsFL_df <- read_excel("../data/FLDH_covid19_deaths_linelist_20200709.xlsx")


###  Re-format and "Roll Up" Dates  ###
deathLineByDayFL_df <- 
	deathsFL_df %>% 
	# Remove unnecessary columns
	select(County, Age, Gender, Date_counted) %>% 
	# Clean up the dates
	mutate(Date = as_date(Date_counted)) %>% 
	select(-Date_counted)

deathPerCountyByDayFL_df <- 
  deathLineByDayFL_df %>% 
	# Group deaths by day and count how many
	group_by(County, Date) %>% 
	add_tally() %>% 
	# Remove duplicate rows
	select(County, Date, n) %>% 
	rename(Count = n) %>% 
	distinct() %>% 
	ungroup()



###  Save  ###
write_csv(
	x = deathPerCountyByDayFL_df,
	path = "../data/FLDH_COVID19_deathsbyday_bycounty_20200709.csv"
)


###  Plot Deaths over Time  ###
whichCounty <- "Dade" # "Palm Beach" // "Broward"

ggplot(
	deathPerCountyByDayFL_df %>% 
		filter(County == whichCounty)
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
		title = paste("Deaths by Day for", whichCounty, "County")
	) +
	
	geom_point() +
	stat_smooth()
