# Wrangle ESS Data for South Florida
# Gabriel Odom
# 2021-01-04


######  Import Data  ##########################################################
library(tidyverse)
library(lubridate)
library(readxl)
# From covid19_sfl_website/southFL_covid19_website/ in either the code/ or 
#   reports/ subdirectory. However, while running this code interactively, this
#   path is only one level up (../, not ../../).
data_dir <- "../data/ESS_raw/"
results_dir <- "../data/ESS_processed/"
dataFiles_char <- list.files(path = data_dir, pattern = "ESS_.*hrs")
# NOTE: on and after 10 June, they removed the "s" from "hrs". I've added it to
#   the file names manually (as I have to save the files manually from my email
#   anyway, I can add the "s" to the file name).
# UPDATE 20210103: because AHCA--in their infinite wisdom--decided to save the
#   file dates in MM-DD-YYYY format, the most recent data files (from January)
#   now appear *before* the data files from last April.
str_detect(dataFiles_char, pattern = "2021")
dataFiles_char <- c(
	dataFiles_char[str_detect(dataFiles_char, pattern = "2020")],
	dataFiles_char[str_detect(dataFiles_char, pattern = "2021")]
)


whichCounties_char <- c("MIAMI-DADE", "BROWARD", "PALM BEACH")
# whichCounties_char <- "MARTIN"
# whichCounties_char <- "ALACHUA"
# whichCounties_char <- c("ESCAMBIA", "SANTA ROSA")
# whichCounties_char <- "GRAND TOTAL"

###  Helper Functions  ###
ReadESS_safely <- function(file_char){
	
	# The tab name changed in June (month 06)
	sheetName_char <- case_when(
		str_detect(
			file_char, pattern = "Hospitals_04"
		) ~ "County_Bed_Availability_Report_",
		str_detect(
			file_char, pattern = "Hospitals_05"
		) ~ "County_Bed_Availability_Report_",
		TRUE ~ "County_Bed_Avail_Report_for_Par"
	)
	
	safely(read_excel)(
		path = paste0(data_dir, file_char),
		sheet = sheetName_char,
		skip = 1
	)
	
}

RangleESS <- function(df, fileName_char, county_char){
	
	df$result %>%
		mutate(County = str_to_upper(County)) %>% 
		filter(County %in% county_char) %>%
		select(County, contains("Beds"), contains("COVID")) %>%
		mutate(
			Date_char = str_remove(
				fileName_char,
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

southFloridaHospitalised_ls <-
	map(
		.x = dataFiles_char,
		.f = ~{
			
			df <- ReadESS_safely(file_char = .x)
			
			if(!is.null(df$error)){
				NULL
			} else {
				RangleESS(
					df,
					fileName_char = .x,
					county_char = whichCounties_char
				)
			}
			
		}
	)

map_lgl(southFloridaHospitalised_ls, is.null)




######  Wrangle Specific Data Files  ##########################################

###  April 23  ###
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
	mutate(County = str_to_upper(County)) %>% 
	filter(County %in% whichCounties_char) %>%
	select(County, contains("Beds"), contains("COVID")) %>%
	mutate(Date_char = "2020-04-23") %>%
	mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
	select(-Date_char) %>%
	select(Date, everything())


###  Data from Dr. Villalta: June 28 - July 5  ###
# After 28 June, we started to get ESS data from Dr. Villalta. She sent over 
#   data for Miami-Dade only, and in a different format. I had to change the 
#   format manually for these three dates. I also changed the column names to
#   match the those from Miami-Dade EMS
# The original column names were: County	FipsCode	LicensedBeds	TotalCensus
#   TotalAvailableBeds	AdultIcuCensus	"Pediatric Icu Census"	IcuOver65
#   IcuUnder65	VentsOnsite	VentsInUse	CovidOnVent	VentsCanConvert
#   AdultMedSurgCensus	PediatricMedSurgCensus	BurnCareCensus	LdrpCensus
#   IsolationBedCensus	GeneralAcuteCareCensus	AvailableAdultICU
#   AvailablePediatricICU	AvailableAdultMedSurg	AvailablePediatricMedSurg
#   AvailableBurnCare	AvailableLDRP	AvailableIsolation	AvailableGeneralAcuteCare
#   StaffAdultICU	StaffPediatricICU	StaffAdultMedSurge	StaffPediatricMedSurge
#   StaffBurnCare	StaffLDRP	StaffIsolation	StaffGeneralAcuteCare	"Covid In Icu"
#   "Covid In NonIcu"	"Covid Hospitalized"	CovidAdmitDayBefore
#   CovidAdmitFromLtcfDayBefore	CovidDischargeDayBefore	PuiToPositiveDayBefore
#   LtcfReadyPatients
# The new column names are: County	DOH FIPS Code	Licensed Beds	
#   Total Census/Patients	 Total Available Beds from Availability Screen
#   Adult ICU Census	Pediatric ICU Census	Icu Over 65	Icu Under 65
#   Vent Onsite	Vents Currently In Use (All)	COVID ON VENT
#   Vents from Converted Anesthesia Machines	Adult Med Surg Census
#   Pediatric Med Surg Census	Burn Care Census	LDRP Census	Isolation Bed Census
#   General Acute Care Census	Available Adult ICU	Available
#   Pediatric ICU	Available Adult MedSurg	Available Pediatric MedSurg	Available
#   Burn Care	Available LDRP	Available Isolation Beds
#   Available General AcuteCare	Staff Adult ICU	Staff Pediatric ICU 
#   Staff Adult MedSurg	Staff Pediatric MedSurg	Staff Burn Care	Staff LDRP
#   Staff Isolation Beds	Staff General Acute Care
#   COVID IN ICU	COVID NON ICU	COVID+ Hospitalized	COVID + Admits Day Before
#   COVID + Admits from LTCFs Day Before	COVID + Disch Day Before
#   PUI to + Day Before	LTCF Ready Patients
# I inserted the data for Miami-Dade county into the data format we had before
#   for those five days (28, 29 June and 1-3 July).

southFloridaHospitalised_ls[[112]] <- 
	read_excel(
		path = paste0(data_dir, dataFiles_char[112]),
		sheet = "ESS_BedAvailabilityWithAddlinfo",
		skip = 1
	) %>%
	mutate(County = str_to_upper(County)) %>% 
	filter(County %in% whichCounties_char) %>%
	select(County, contains("Beds"), contains("COVID")) %>%
	mutate(Date_char = "2020-07-05") %>%
	mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
	select(-Date_char) %>%
	select(Date, everything())


###  Data from MD Fire Rescue after July 6th ###
# On and after 6 July, the ESS data we got from the Fire Department was the
#   hospital data, rather than the county data. I don't think the columns are
#   the same, but they are close. This requires some extra steps
ReadHospitalLevelData <- function(fileName,
																	date_char,
																	county_char,
	                                sheetName = "ESS_Hospitals_FacilityResponses"){
	# browser()
	
	if (identical(county_char, "GRAND TOTAL")) {
		
		df <- read_excel(
			path = paste0(data_dir, fileName),
			sheet = sheetName,
			skip = 1
		) %>% 
			# EMS told me only to take "Class 1 Hospital", so I did. That's not correct
			# mutate(Class1 = str_detect(`Hospital Class`, "Class 1")) %>% 
			# filter(Class1) %>% 
			# After sleuthing the data, AHCA takes ALL hospitals except for the VA
			filter(HospitalClass != "No Class") %>% 
			select(contains("Beds"), contains("COVID"))  %>% 
			summarise(across(everything(), .fns = ~{sum(.x, na.rm = TRUE)}))
		
	} else {
		
		df <- read_excel(
			path = paste0(data_dir, fileName),
			sheet = sheetName,
			skip = 1
		) %>% 
			filter(County %in% county_char) %>%
			filter(HospitalClass != "No Class") %>% 
			select(County, contains("Beds"), contains("COVID"))  %>% 
			group_by(County) %>% 
			summarise(across(everything(), .fns = ~{sum(.x, na.rm = TRUE)}))
		
	}
	
	df %>%
		mutate(Date_char = date_char) %>%
		mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
		select(-Date_char) %>% 
		select(Date, everything()) %>% 
		rename(
			`Licensed Beds` = LicensedBeds,
			`Total Available Beds from Availability Screen` = TotalAvailableBeds,
			`COVID ON VENT` = CovidOnVent,
			`COVID IN ICU`  = CovidInIcu,
			`COVID NON ICU` = CovidInNonIcu,
			`COVID+ Hospitalized` = CovidHospitalized,
			`COVID + Admits Day Before` = CovidAdmitDayBefore,
			`COVID + Admits from LTCFs Day Before` = CovidAdmitFromLtcfDayBefore,
			`COVID + Disch Day Before` = CovidDischargeDayBefore
		)
	
}


southFloridaHospitalised_ls[113:148] <- 
	map2(
		.x = dataFiles_char[113:148],
		.y = c(
			"2020-07-06", "2020-07-08", "2020-07-09", "2020-07-10", "2020-07-12",
			"2020-07-13", "2020-07-14", "2020-07-15", "2020-07-16", "2020-07-17",
			"2020-07-19", "2020-07-21", "2020-07-22", "2020-07-23", "2020-07-24",
			"2020-07-25", "2020-07-26", "2020-07-27", "2020-07-28", "2020-07-29",
			"2020-07-30", "2020-08-01", "2020-08-02", "2020-08-03", "2020-08-04",
			"2020-08-06", "2020-08-07", "2020-08-08", "2020-08-09", "2020-08-10",
			"2020-08-11", "2020-08-12", "2020-08-13", "2020-08-14", "2020-08-16",
			"2020-08-17"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)

###  August 18th  ###
# The ESS data for 8/18 is in the old format, so ReadESS_safely() works.
#   However, the column names of the new file did not match the column names of
#   the data as originally formatted. I used the 6/24 data to confirm that the 
#   column orders were the same, then copied the old column names to the new 
#   data file for 8/18. Also, the sheet name did not match, so I had to rename
#   it to "County_Bed_Avail_Report_for_Par" from the original sheet title 
#   ("ESS_Hospitals_CountyRollup_BedA"). After all this, we stop using the
#   ReadHospitalLevelData() function for 18th, and then pick back up after. 
southFloridaHospitalised_ls[150:167] <- 
	map2(
		.x = dataFiles_char[150:167],
		.y = c(
			"2020-08-19", "2020-08-20", "2020-08-21", "2020-08-22", "2020-08-23",
			"2020-08-24", "2020-08-25", "2020-08-26", "2020-08-27", "2020-08-28",
			"2020-08-29", "2020-08-30", "2020-08-31", "2020-09-01", "2020-09-02",
			"2020-09-03", "2020-09-04", "2020-09-05"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)

###  September 6th  ###
# The ESS data for 9/06 is in the old format, so ReadESS_safely() works. Same
#   comment as above.
southFloridaHospitalised_ls[169:174] <- 
	map2(
		.x = dataFiles_char[169:174],
		.y = c(
			"2020-09-07", "2020-09-08", "2020-09-09", "2020-09-11", "2020-09-12",
			"2020-09-13"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)

###  September 15th  ###
# The ESS data for 9/15 is in the old format, so ReadESS_safely() works. Same
#   comment as above.

southFloridaHospitalised_ls[176:177] <-
	map2(
		.x = dataFiles_char[176:177],
		.y = c(
			"2020-09-16", "2020-09-17"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)


###  September 18th - 20th  ###
# The ESS data for 9/18 - 09/20 is in the old format, so ReadESS_safely() works.
#   Same comment as above.
southFloridaHospitalised_ls[181:195] <-
	map2(
		.x = dataFiles_char[181:195],
		.y = c(
			"2020-09-21", "2020-09-22", "2020-09-23", "2020-09-24", "2020-09-25",
			"2020-09-25", "2020-09-27", "2020-09-29", "2020-10-02", "2020-10-05",
			"2020-10-06", "2020-10-08", "2020-10-09", "2020-10-10", "2020-10-12"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)


# ###  October 14th, 17th  ###
# The ESS data for 10/14 is in the old format, so ReadESS_safely() works.
#   Same comment as above.
southFloridaHospitalised_ls[c(197, 199:209)] <-
	map2(
		.x = dataFiles_char[c(197, 199:209)],
		.y = c(
			"2020-10-16", "2020-10-18", "2020-10-19", "2020-10-20", "2020-10-22",
			"2020-10-23", "2020-10-24", "2020-10-25", "2020-10-26", "2020-10-27",
			"2020-10-28", "2020-10-29"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)

# There is a missing column (`CovidAdmitFromLtcfDayBefore`) in the 10-30 data.
#   After inspecting this data, there are a ton of missing columns comparing
#   before the 29th to after that date.


###  November Data Format Changes  ###
# There are missing columns in the ESS data starting on 30 October:
#    Dates: "2020-10-30", "2020-10-31", "2020-11-01", "2020-11-02"
# Also, recall that I manually save each data file in this format so that the
#   new data files match the format of the legacy data files (.xlsx with a row
#   of meta-data at the top). Even though I receive .csv files via email, I 
#   save them into .xlsx form and add an empty row at the top of the file. I 
#   know that this is a bad reason to do something ("because we've always done
#   it that way"), but I think it's better to have all the data in the same 
#   format, even if that format is less than ideal.
ReadHospitalLevelData2 <- function(fileName,
																	 date_char,
																	 county_char,
																	 sheetName = "ESS_Hospitals_FacilityResponses"){
	# browser()
	
	if (identical(county_char, "GRAND TOTAL")) {
		
		df <- read_excel(
			path = paste0(data_dir, fileName),
			sheet = sheetName,
			skip = 1
		) %>% 
			filter(HospitalClass != "No Class") %>% 
			select(contains("Beds"), contains("COVID"))  %>% 
			summarise(across(everything(), .fns = ~{sum(.x, na.rm = TRUE)}))
		
	} else {
		
		df <- read_excel(
			path = paste0(data_dir, fileName),
			sheet = sheetName,
			skip = 1
		) %>% 
			filter(County %in% county_char) %>%
			filter(HospitalClass != "No Class") %>% 
			select(County, contains("Beds"), contains("COVID"))  %>% 
			group_by(County) %>% 
			summarise(across(everything(), .fns = ~{sum(.x, na.rm = TRUE)}))
		
	}
	
	out_df <- df %>%
		mutate(Date_char = date_char) %>%
		mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
		select(-Date_char) %>% 
		select(Date, everything())
	
	# As of 30 October, there are two missing columns. We're going to check if
	#   those columns are truly gone, then fill them with missing values
	if (is.null(out_df$CovidAdmitFromLtcfDayBefore)) {
		out_df$CovidAdmitFromLtcfDayBefore <- NA_integer_
	}
	if (is.null(out_df$CovidDischargeDayBefore)) {
		out_df$CovidDischargeDayBefore <- NA_integer_
	}
	# As of 6 November, these columns are back. What?
	
	out_df %>% 
		rename(
			`Licensed Beds` = LicensedBeds,
			`Total Available Beds from Availability Screen` = TotalAvailableBeds,
			`COVID ON VENT` = CovidOnVent,
			`COVID IN ICU`  = CovidInIcu,
			`COVID NON ICU` = CovidInNonIcu,
			`COVID+ Hospitalized` = CovidHospitalized,
			`COVID + Admits Day Before` = CovidAdmitDayBefore,
			`COVID + Admits from LTCFs Day Before` = CovidAdmitFromLtcfDayBefore,
			`COVID + Disch Day Before` = CovidDischargeDayBefore
		)
	
}

southFloridaHospitalised_ls[210:242] <-
	map2(
		.x = dataFiles_char[210:242],
		.y = c(
			"2020-10-30", "2020-10-31", "2020-11-01", "2020-11-02", "2020-11-04",
			"2020-11-05", "2020-11-06", "2020-11-07", "2020-11-08", "2020-11-09",
			"2020-11-10", "2020-11-11", "2020-11-12", "2020-11-13", "2020-11-14",
			"2020-11-15", "2020-11-17", "2020-11-18", "2020-11-19", "2020-11-20",
			"2020-11-21", "2020-11-22", "2020-11-23", "2020-11-24", "2020-11-25",
			"2020-11-26", "2020-11-27", "2020-11-28", "2020-11-29", "2020-11-30",
			"2020-12-01", "2020-12-02", "2020-12-03"
		),
		.f = ReadHospitalLevelData2,
		county_char = whichCounties_char
	)


###  December 4th  ###
# We had a good run, but they sent us the Roll-Up data again on the 4th.
southFloridaHospitalised_ls[244:278] <-
	map2(
		.x = dataFiles_char[244:278],
		.y = c(
			"2020-12-05", "2020-12-06", "2020-12-07", "2020-12-08", "2020-12-09",
			"2020-12-10", "2020-12-11", "2020-12-12", "2020-12-13", "2020-12-14",
			"2020-12-15", "2020-12-16", "2020-12-18", "2020-12-19", "2020-12-20",
			"2020-12-21", "2020-12-22", "2020-12-23", "2020-12-24", "2020-12-25",
			"2020-12-26", "2020-12-27", "2020-12-28", "2020-12-29", "2020-12-30",
			# happy new year...
			"2020-12-31", "2021-01-01", "2021-01-02", "2021-01-03", "2021-01-04",
			"2021-01-05", "2021-01-06", "2021-01-07", "2021-01-08", "2021-01-09"
		),
		.f = ReadHospitalLevelData2,
		county_char = whichCounties_char
	)


###  January 10-11th  ###
southFloridaHospitalised_ls[281:298] <-
	map2(
		.x = dataFiles_char[281:298],
		.y = c(
			"2021-01-14", "2021-01-19", "2021-01-20", "2021-01-21", "2021-01-22",
			"2021-01-23", "2021-01-24", "2021-01-25", "2021-01-26", "2021-01-28",
			"2021-01-29", "2021-01-30", "2021-01-31", "2021-02-02", "2021-02-03",
			"2021-02-04", "2021-02-05", "2021-02-06"
		),
		.f = ReadHospitalLevelData2,
		county_char = whichCounties_char
	)


###  February 7th, 9th  ###
southFloridaHospitalised_ls[c(300, 302:310)] <-
	map2(
		.x = dataFiles_char[c(300, 302:310)],
		.y = c(
			"2021-02-08", "2021-02-10", "2021-02-11", "2021-02-12", "2021-02-13",
			"2021-02-14", "2021-02-15", "2021-02-16", "2021-02-17", "2021-02-18"
		),
		.f = ReadHospitalLevelData2,
		county_char = whichCounties_char
	)


######  Wrangle  ##############################################################
southFloridaHospitalised_df <-
	southFloridaHospitalised_ls %>%
	bind_rows() %>%
	mutate(Date = as_date(Date)) %>% 
	mutate(Hospitalized = `COVID IN ICU` + `COVID NON ICU`) %>%
	rename(
		TotalBeds = `Licensed Beds`,
		AvailableBeds = `Total Available Beds from Availability Screen`,
		ICU = `COVID IN ICU`,
		Ventilated = `COVID ON VENT`,
		AdmitPrevDay = `COVID + Admits Day Before`,
		DischPrevDay = `COVID + Disch Day Before`
	) %>%
	mutate(DeltaAdmit = AdmitPrevDay - DischPrevDay) %>%
	mutate(HospitalCapacity = AvailableBeds / TotalBeds) %>% 
	select(
		Date, County, TotalBeds, AvailableBeds, HospitalCapacity, Hospitalized, ICU,
		Ventilated, AdmitPrevDay, DischPrevDay, DeltaAdmit
	)



######  Incorporate DEM Data (MDC Only)  ######################################
# # We have data for MDC only through the Miami Beach DEM. In emergencies, when
# #   we do not have access to AHCA data, we can at least report on what is
# #   happening in Miami-Dade County.
# # Update: the DEM report is missing about 20 people hospitalized in MDC. I'll
# #   bet that they aren't using the AHCA definition (all non-VA beds).
# 
# dem_DF <- tibble(
# 	Date = as_date(c("2020-01-12", "2020-01-13")),
# 	County = c("MIAMI-DADE", "MIAMI-DADE"),
# 	Hospitalized = c(1208L, 1180L) + 20L
# )
# 
# # I'm not comfortable with this. I'd rather just ignore the DEM data for now,
# #   than add data using different definitions while assuming it's the same.

	  


######  Save Clean Data  ######################################################
# Calculate Dates and Save
startEndESS_date <- 
	southFloridaHospitalised_df %>%
	slice(1, n()) %>%
	pull(Date) 

fileSlug_char <- case_when(
	"ALACHUA" %in% whichCounties_char ~ "ESS_alachua_summary_",
	"ESCAMBIA" %in% whichCounties_char ~ "ESS_panhandle_summary_",
	"MARTIN" %in% whichCounties_char ~ "ESS_martin_summary_",
	"BROWARD" %in% whichCounties_char ~ "ESS_southFL_summary_",
	identical(whichCounties_char, "GRAND TOTAL") ~ "ESS_Florida_summary_"
)

write_csv(
	southFloridaHospitalised_df,
	file = paste0(
		results_dir,
		fileSlug_char,
		format(startEndESS_date[2], "%Y%m%d"),
		".csv"
	)
)
