# Plot Deaths Over Time
# Gabriel Odom
# 2020-07-06

library(tidyverse)
library(readxl)

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
