# Plot Vaccinations Over Time
# Gabriel Odom
# 2020-07-12

library(tidyverse)
library(lubridate)
library(readxl)
# library(httr)
# install.packages("tabulizer")
library(tabulizer)
# To try the extract_areas() interactive function from tabulzer::
# install.packages("miniUI")
library(miniUI)


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
# 



######  Download All Historic Vaccination Data  ###############################

# START AND END OF HISTORIC DATA
# start_Date <- ymd("20201220")
# end_Date <- ymd("20210116")
# dates  <- format(seq(start_Date, end_Date, by = "days"), "%Y%m%d")
# dates  <- dates[!(dates %in% c("20201225", "20210101"))]

# START AND END OF RECENT DATA
start_Date <- ymd("20210117")
end_Date <- ymd("20210123")

dates  <- format(seq(start_Date, end_Date, by = "days"), "%Y%m%d")
fileTo <- paste0(
	"../data/vaccinations/Daily Reports/vaccine_report_", dates, ".pdf"
)

slug   <- "http://ww11.doh.state.fl.us/comm/_partners/"
nugget <- "covid19_report_archive/vaccine/vaccine_report_"
urls   <- paste0(slug, nugget, dates, ".pdf")

# library(httr)
# This downloads the data at the website urls[1] into the file fileTo[1]; the
#   write_disk() function tells GET() to store the data on the hard disk, not
#   in RAM
map(
	.x = seq_along(dates),
	.f = ~{ httr::GET(urls[.x], write_disk(fileTo[.x])) }
)

# END data pull



######  Attempt to Scrape the PDFs  ###########################################
# Let's first attempt tabulizer. Blog post here:
# https://datascienceplus.com/extracting-tables-from-pdfs-in-r-using-the-tabulizer-package/
# Also see
# https://medium.com/@ketanrd.009/how-to-extract-pdf-tables-in-r-e994c0fe4e28

extract_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20201221.pdf",
	pages = 2L
)

locate_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20201221.pdf",
	pages = 2L
)
#       top      left    bottom     right 
# 112.16489  52.18085 757.77128 451.34043 

# The column headers are a bitch, so I think I'll set them manually.

vax20201221_ls <-
	extract_tables(
		file = "../data/vaccinations/Daily Reports/vaccine_report_20201221.pdf",
		pages = 2L,
		area = list(c(112.16489, 52.18085, 757.77128, 451.34043)),
		output = "data.frame"
	)

# Let's check to see if this program will still work on data a month later:
vax20210116_ls <-
	extract_tables(
		file = "../data/vaccinations/Daily Reports/vaccine_report_20210116.pdf",
		pages = 2L,
		area = list(c(112.16489, 52.18085, 757.77128, 451.34043)),
		output = "data.frame"
	)
# Doesn't look like it
locate_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20210116.pdf",
	pages = 2L
)
#       top      left    bottom     right 
# 105.84574  51.12766 763.03723 531.38298 

# Top, left, and bottom are similar, but right is way off. Let's make a window
#   that contains both, and see if we can get it to work.
tableBoundary_int <- c(100L, 50L, 775L, 535L)

testVax20201221_df <-
	extract_tables(
		file = "../data/vaccinations/Daily Reports/vaccine_report_20201221.pdf",
		pages = 2L,
		area = list(tableBoundary_int),
		output = "data.frame"
	)[[1]]
testVax20210116_df <-
	extract_tables(
		file = "../data/vaccinations/Daily Reports/vaccine_report_20210116.pdf",
		pages = 2L,
		area = list(tableBoundary_int),
		output = "data.frame"
	)[[1]]

# ok, so that actually worked.

# UPDATE: not for all the files
locate_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20201230.pdf",
	pages = 2L
)
#      top      left    bottom     right 
# 157.9785   78.6592 1164.1802  801.3097 
locate_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20201231.pdf",
	pages = 2L
)
#       top       left     bottom      right 
# 162.83940   68.93745 1183.62367  806.17054 
locate_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20210108.pdf",
	pages = 2L
)
#      top      left    bottom     right 
# 159.5988   78.6592 1175.5222  798.0691 

# I opened these PDFs, and they look to be in a slightly bigger font. Why?!



# END test



######  Apply the PDF Scraper  ################################################
start_Date <- ymd("20201220")
end_Date <- ymd("20210116")
dates  <- format(seq(start_Date, end_Date, by = "days"), "%Y%m%d")
dates  <- dates[!(dates %in% c("20201225", "20210101"))]
files_char <- paste0("vaccine_report_", dates, ".pdf")
filePaths_char <- paste0("../data/vaccinations/Daily Reports/", files_char)
	

# We have PDFs with 2 (hopefully only 2) font sizes
tableBoundary1_int <- c(100L, 50L, 775L, 535L)
tableBoundary2_int <- c(155L, 70L, 1175L, 800L)


###  Pass 1  ###
vaxTables1_ls <- 
	map(
		.x = filePaths_char,
		.f = ~{
			extract_tables(
				file = .x,
				pages = 2L,
				area = list(tableBoundary1_int),
				# I wanted to create a data frame, but some of the files get parsed
				#   correctly as integers, and others don't. Matrices are atomic, so 
				#   all the information is forced to be a character.
				output = "matrix"
			)[[1]]
		}
	)
names(vaxTables1_ls) <- files_char

# Well, this works for a third of the dates... It gives the 7 columns we need
#   for the 67 Florida counties plus "Out of state", "Unknown", and "Total"
# Let's go back up to the testing section to check out 12/30 and 01/10.

# Maybe we can make multiple passes over the data. These dimensions work for
#   some of the PDFs, so let's exclude the ones for which this works.
goodReads1_lgl <- map_dbl(vaxTables1_ls, nrow) >= 71L
vaxTables1_ls <- vaxTables1_ls[goodReads1_lgl]


###  Pass 2  ###
vaxTables2_ls <- 
	map(
		.x = filePaths_char[!goodReads1_lgl],
		.f = ~{
			extract_tables(
				file = .x,
				pages = 2L,
				area = list(tableBoundary2_int),
				output = "matrix"
			)[[1]]
		}
	)
names(vaxTables2_ls) <- files_char[!goodReads1_lgl]
# This second pass is a bit more problematic. I've played with the areas a lot,
#   but I can't seem to get the county column to import. Thankfully, it's the
#   same 70 columns in the same order as above, so we can add it back on. Also,
#   these data files have two extra columns of all NA, so we will have to clip
#   those out.
goodReads2_lgl <- map_dbl(vaxTables2_ls, nrow) >= 71L
vaxTables2_ls <- vaxTables2_ls[goodReads2_lgl]
# Only 1 problematic file left: December 30th.


###  The Straggler  ###
locate_areas(
	file = "../data/vaccinations/Daily Reports/vaccine_report_20201230.pdf",
	pages = 2L
)
#      top      left    bottom     right 
# 157.9785   78.6592 1164.1802  801.3097 

vax20201230_mat <-
	extract_tables(
		file = "../data/vaccinations/Daily Reports/vaccine_report_20201230.pdf",
		pages = 2L,
		area = list(c(157L, 78L, 1165L, 802L)),
		output = "matrix"
	)[[1]]
# What row is missing?
setdiff(vaxTables1_ls[[1]][, 1], vax20201230_mat[, 1])
setdiff(vax20201230_mat[, 1], vaxTables1_ls[[1]][, 1])
# So DeSoto county was parsed with a lower case "s" for the 30th. I checked the
#   original data, and that's how it is.
# Also, they just forgot that Glades county existed on the 30th.



######  Wrangle and Combine the Scraped Data  #################################
countyIDs_mat <- vaxTables1_ls[[1]][-1, 1, drop = FALSE]


###  First Pass  ###
vaxTablesClean1_df <- 
	map(
		.x = vaxTables1_ls,
		.f = ~{
			# browser()
			# Remove partial headers
			out <- .x[-1, ]
			# A few of the later data files have a small border between daily and
			#   cumulative; this is interpreted as a column of NAs
			if (ncol(out) == 8L) {
				out <- out[, -5]
			} 
			out2 <- matrix(
				str_remove_all(out, ","),
				nrow = 70, ncol = 7
			)
			
			out_df <- as.data.frame(out2)
			# I know I'm setting the same column names a million times, but it's
			#   easier here than later because of the .id column
			colnames(out_df) <-
				c(
					"County",
					"Yesterday_1stDoseOnly", "Yesterday_Complete", "Yesterday_Total",
					"Cumulative_1stDoseOnly", "Cumulative_Complete", "Cumulative_Total"
				)
			out_df
		}
	) %>% 
	bind_rows(.id = "File") 


###  Second Pass  ###
vaxTablesClean2_df <- 
	map(
		.x = vaxTables2_ls,
		.f = ~{
			# browser()
			# Remove Headers and empty columns; add county labels
			out1 <- .x[-1, ]
			out1 <- out1[, -c(4,8)]
			out2 <- cbind(countyIDs_mat, out1)
			out3 <- matrix(
				str_remove_all(out2, ","),
				nrow = 70, ncol = 7
			)
			
			out_df <- as.data.frame(out3)
			colnames(out_df) <-
				c(
					"County",
					"Yesterday_1stDoseOnly", "Yesterday_Complete", "Yesterday_Total",
					"Cumulative_1stDoseOnly", "Cumulative_Complete", "Cumulative_Total"
				)
			out_df
		}
	) %>% 
	bind_rows(.id = "File") 


###  The Straggler  ###
vax20201230_df <- 
	vax20201230_mat[-1, -5] %>% 
	str_remove_all(",") %>% 
	matrix(nrow = 69, ncol = 7) %>% 
	rbind(
		matrix(
			c("Glades", rep(NA_character_, 6)),
			nrow = 1
		),
		.
	) %>% 
	cbind(
		matrix(
			rep(files_char[10], 70),
			ncol = 1
		),
		.
	) %>% 
	as.data.frame()
colnames(vax20201230_df) <- 
	c(
		"File", "County",
		"Yesterday_1stDoseOnly", "Yesterday_Complete", "Yesterday_Total",
		"Cumulative_1stDoseOnly", "Cumulative_Complete", "Cumulative_Total"
	)

###  Putting it all together...  ###
vaxClean_df <- 
	bind_rows(
		vaxTablesClean1_df,
		vaxTablesClean2_df,
		vax20201230_df
	) %>% 
	as_tibble() %>% 
	mutate(File = str_remove(File, ".pdf")) %>% 
	mutate(File = str_remove(File, "vaccine_report_")) %>% 
	mutate(Date = as_date(File)) %>%
	select(Date, County, everything()) %>% 
	arrange(Date, County) %>% 
	select(-File) %>% 
	mutate(across(Yesterday_1stDoseOnly:Cumulative_Total, as.integer))

# IT WORKS!!!!! PRAISE THE LORD

write_csv(
	vaxClean_df,
	file = "../data/vaccinations/allFL_wrangled_daily_vaccinations_20210116.csv"
)


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
