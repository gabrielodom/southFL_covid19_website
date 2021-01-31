# Wrangle ESS Data for South Florida
# Gabriel Odom
# 2020-06-25


######  Import Data  ##########################################################
library(tidyverse)
library(lubridate)
library(readxl)
# From covid19_sfl_website/southFL_covid19_website/ in either the code/ or 
#   reports/ subdirectory. However, while running this code interactively, this
#   path is only one level up (../, not ../../).
data_dir <- "../data/"
dataFiles_char <- list.files(path = data_dir, pattern = "ESS_.*hrs")
# NOTE: on and after 10 June, they removed the "s" from "hrs". I've added it to
#   the file names manually (as I have to save the files manually from my email
#   anyway, I can add the "s" to the file name).

read_excel_safely <- safely(read_excel)

southFloridaHospitalised_ls <-
	map(
		.x = dataFiles_char,
		.f = ~{
			
			# The tab name changed in June (month 06)
			sheetName_char <- case_when(
				str_detect(
					.x, pattern = "Hospitals_04"
				) ~ "County_Bed_Availability_Report_",
				str_detect(
					.x, pattern = "Hospitals_05"
				) ~ "County_Bed_Availability_Report_",
				TRUE ~ "County_Bed_Avail_Report_for_Par"
			)
			
			df_ls <- read_excel_safely(
				path = paste0(data_dir, .x),
				sheet = sheetName_char,
				skip = 1
			)
			
			if(!is.null(df_ls$error)){
				NULL
			} else {
				
				df_ls$result %>%
					filter(County %in% c("MIAMI-DADE", "BROWARD", "PALM BEACH")) %>%
					select(County, contains("COVID")) %>%
					mutate(
						Date_char = str_remove(
							.x,
							pattern = "ESS_BedAvailabilityWithAddlinfo_Hospitals_"
						)
					) %>%
					mutate(
						Date_char = str_sub(Date_char, end = 10)
					) %>%
					mutate(
						Date_char = str_replace_all(
							Date_char,
							pattern = "\\.",
							replacement = "-"
						)
					) %>%
					mutate(Date = as.POSIXct(Date_char, format = "%m-%d-%Y")) %>%
					select(-Date_char) %>%
					select(Date, everything())
				
			}
			
		}
	)


######  Wrangle Data  #########################################################

###  Fix Errors  ###
# We had a data wrangling error, so I wrapped read_excel() in a safely() and
#   added an if() statement.
# "ESS_BedAvailabilityWithAddlinfo_Hospitals_04.23.2020_1045hrs.xlsx"
# I inspected this file manually and found the sheet's name was abbreviated
southFloridaHospitalised_ls[[30]] <-
	read_excel(
		path = paste0(data_dir, dataFiles_char[30]),
		sheet = "County",
		skip = 1
	) %>%
	filter(County %in% c("MIAMI-DADE", "BROWARD", "PALM BEACH")) %>%
	select(County, contains("COVID")) %>%
	mutate(Date_char = "2020-04-23") %>%
	mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
	select(-Date_char) %>%
	select(Date, everything())

southFloridaHospitalised_ls[[107]] <- 
	read_excel(
		path = paste0(data_dir, dataFiles_char[107]),
		sheet = "ESS_BedAvailabilityWithAddlinfo",
		skip = 1
	) %>%
	filter(County %in% c("MIAMI-DADE", "BROWARD", "PALM BEACH")) %>%
	select(County, contains("COVID")) %>%
	mutate(Date_char = "2020-07-05") %>%
	mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
	select(-Date_char) %>%
	select(Date, everything())
colnames(southFloridaHospitalised_ls[[107]]) <- 
	colnames(southFloridaHospitalised_ls[[106]])

###  SKIP = 1  ###
# # The sheet header changed on June 20th. I add back an empty heading line
#   in the original file
# skip0_lgl <- str_detect(.x, "06.20.2020") || str_detect(.x, "06.22.2020")
# ifelse(test = skip0_lgl, yes = 0, no = 1)
# # In order to avoid this foolishness, I add back in a blank row at the top of
# #   the original Excel file to match the format of the data before June 20th.


###  Wrangle  ###
southFloridaHospitalised_df <-
	southFloridaHospitalised_ls %>%
	bind_rows() %>%
	mutate(Date = as_date(Date)) %>% 
	mutate(Hospitalized = `COVID IN ICU` + `COVID NON ICU`) %>%
	rename(
		ICU = `COVID IN ICU`,
		Ventilated = `COVID ON VENT`,
		AdmitPrevDay = `COVID + Admits Day Before`,
		DischPrevDay = `COVID + Disch Day Before`
	) %>%
	mutate(DeltaAdmit = AdmitPrevDay - DischPrevDay) %>%
	select(
		Date, County, Hospitalized, ICU, Ventilated, AdmitPrevDay, DischPrevDay,
		DeltaAdmit
	)


######  Save Clean Data  ######################################################
# Calculate Dates and Save
startEndESS_date <- 
	southFloridaHospitalised_df %>%
	slice(1, n()) %>%
	pull(Date) 

write_csv(
	southFloridaHospitalised_df,
	path = paste0(
		data_dir,
		"ESS_southFL_summary_",
		format(startEndESS_date[2], "%Y%m%d"),
		".csv"
	)
)
