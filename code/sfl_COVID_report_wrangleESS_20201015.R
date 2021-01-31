# Wrangle ESS Data for South Florida
# Gabriel Odom
# 2020-10-15


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

whichCounties_char <- c("MIAMI-DADE", "BROWARD", "PALM BEACH")
# whichCounties_char <- "MARTIN"
# whichCounties_char <- "ALACHUA"
# whichCounties_char <- c("ESCAMBIA", "SANTA ROSA")


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
	
	read_excel(
		path = paste0(data_dir, fileName),
		sheet = sheetName,
		skip = 1
	) %>% 
		filter(County %in% county_char) %>%
		# EMS told me only to take "Class 1 Hospital", so I did. That's not correct
		# mutate(Class1 = str_detect(`Hospital Class`, "Class 1")) %>% 
		# filter(Class1) %>% 
		# After sleuthing the data, AHCA takes ALL hospitals except for the VA
		filter(HospitalClass != "No Class") %>% 
		select(County, contains("Beds"), contains("COVID"))  %>% 
		group_by(County) %>% 
		summarise(across(everything(), .fns = ~{sum(.x, na.rm = TRUE)})) %>%
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
southFloridaHospitalised_ls[c(197, 199:213)] <-
	map2(
		.x = dataFiles_char[c(197, 199:213)],
		.y = c(
			"2020-10-16", "2020-10-18", "2020-10-19", "2020-10-20", "2020-10-22",
			"2020-10-23", "2020-10-24", "2020-10-25", "2020-10-26", "2020-10-27",
			"2020-10-28", "2020-10-29", "2020-10-30", "2020-10-31", "2020-11-01",
			"2020-11-02"
		),
		.f = ReadHospitalLevelData,
		county_char = whichCounties_char
	)

# There is a missing column (`CovidAdmitFromLtcfDayBefore`) in the 10-30 data.
#   After inspecting this data, there are a ton of missing columns comparing
#   before the 29th to after that date.



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


######  Save Clean Data  ######################################################
# Calculate Dates and Save
startEndESS_date <- 
	southFloridaHospitalised_df %>%
	slice(1, n()) %>%
	pull(Date) 

fileSlug_char <- case_when(
	"MARTIN" %in% whichCounties_char ~ "ESS_martin_summary_",
	"ALACHUA" %in% whichCounties_char ~ "ESS_alachua_summary_",
	"ESCAMBIA" %in% whichCounties_char ~ "ESS_panhandle_summary_",
	"BROWARD" %in% whichCounties_char ~ "ESS_southFL_summary_"
)

write_csv(
	southFloridaHospitalised_df,
	path = paste0(
		results_dir,
		fileSlug_char,
		format(startEndESS_date[2], "%Y%m%d"),
		".csv"
	)
)
