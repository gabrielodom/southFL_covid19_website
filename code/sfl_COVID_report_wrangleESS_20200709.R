# Wrangle ESS Data for South Florida
# Gabriel Odom
# 2020-07-05


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
					select(County, contains("Beds"), contains("COVID")) %>%
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
	filter(County %in% c("MIAMI-DADE", "BROWARD", "PALM BEACH")) %>%
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
	filter(County %in% c("MIAMI-DADE", "BROWARD", "PALM BEACH")) %>%
	select(County, contains("Beds"), contains("COVID")) %>%
	mutate(Date_char = "2020-07-05") %>%
	mutate(Date = as.POSIXct(Date_char, format = "%Y-%m-%d")) %>%
	select(-Date_char) %>%
	select(Date, everything())

# map(southFloridaHospitalised_ls, colnames)



###  SKIP = 1  ###
# # The sheet header changed on June 20th. I add back an empty heading line
#   in the original file
# skip0_lgl <- str_detect(.x, "06.20.2020") || str_detect(.x, "06.22.2020")
# ifelse(test = skip0_lgl, yes = 0, no = 1)
# # In order to avoid this foolishness, I add back in a blank row at the top of
# #   the original Excel file to match the format of the data before June 20th.



###  Data from MD Fire after July 6th ###
# On and after 6 July, the ESS data we got from the Fire Department was the
#   hospital data, rather than the county data. I don't think the columns are
#   the same, but they are close. This requires some extra steps
ReadHospitalLevelData <- function(fileName,
	                                sheetName = "ESS_Hospitals_FacilityResponses",
	                                date_char){
	# browser()
	
	read_excel(
		path = paste0(data_dir, fileName),
		sheet = sheetName,
		skip = 1
	) %>% 
		filter(County %in% c("MIAMI-DADE", "BROWARD", "PALM BEACH")) %>%
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

# # Test: test against the data from the 30th of June, because that's the last
# #   day we had both county and hospitalisation data in the same file
# dataFiles_char[108]
# ReadHospitalLevelData(
# 	dataFiles_char[108],
# 	sheetName = "Hospital_Acute_Care_Bed_Avail_R",
# 	date_char = "2020-06-30"
# )
# Solution: AHCA counts all beds not in the VA system

southFloridaHospitalised_ls[[113]] <- 
	ReadHospitalLevelData(dataFiles_char[113], date_char = "2020-07-06")
southFloridaHospitalised_ls[[114]] <- 
	ReadHospitalLevelData(dataFiles_char[114], date_char = "2020-07-08")
southFloridaHospitalised_ls[[115]] <- 
	ReadHospitalLevelData(dataFiles_char[115], date_char = "2020-07-09")
southFloridaHospitalised_ls[[116]] <- 
	ReadHospitalLevelData(dataFiles_char[116], date_char = "2020-07-10")
southFloridaHospitalised_ls[[117]] <- 
	ReadHospitalLevelData(dataFiles_char[117], date_char = "2020-07-12")
# This data point will be different because it has an extra ~500 beds in Miami
#   and an extra 300 beds in Palm Beach. The Broward data is the same.
# EDIT: Lt. Gomez told me I need to filter out any hospital that isn't Class 1.
# NEW EDIT: AHCA uses all beds not in the VA, not just class 1.


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

write_csv(
	southFloridaHospitalised_df,
	path = paste0(
		data_dir,
		"ESS_southFL_summary_",
		format(startEndESS_date[2], "%Y%m%d"),
		".csv"
	)
)
