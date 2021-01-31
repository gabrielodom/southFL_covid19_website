# Relating Cases to Hospitalisations
# Gabriel Odom
# 2020-07-18


######  Introduction  #########################################################
# We believe that hospitalisations lag cases by about 2 weeks. Lets find out.
#   Also, we want to find out if there is a strong relationship between ICU use
#   and deaths.
library(tidyverse)
library(lubridate)
library(readxl)


######  Import + Wrangle Data  ################################################
# Cases:
casesMiamiDade_df <- 
	read_csv("../data/FLDH_COVID19_cases_miamidade_20200714.csv") %>% 
	mutate(Date = as.POSIXct(strptime(Date, format = "%d-%b"))) %>% 
	mutate(Date = as_date(Date)) 

# Hospitalisations
hospMiamiDade_df <- 
	read_csv("../data/ESS_southFL_summary_20200717.csv") %>% 
	filter(County == "MIAMI-DADE") %>% 
	select(-County)

# Deaths
deathsMiamiDade_df <- 
	read_csv("../data/FLDH_COVID19_deathsbyday_bycounty_20200718.csv") %>% 
	filter(County == "Dade") %>% 
	select(-County) %>%
	rename(DeathCount = Count) %>% 
	# Remove the last two weeks of deaths because they are not complete
	filter(Date <= "2020-07-04")

# Joined Data
miamiDadeRaw_df <- 
	casesMiamiDade_df %>% 
	full_join(hospMiamiDade_df) %>% 
	full_join(deathsMiamiDade_df) %>% 
	arrange(Date)

# Date Range
# there are some dates that we have no data for at all. We want to make sure
#   this doesn't interfere with our lagging operation.
allDates_df <- tibble(
	Date = seq(
		from = miamiDade_df$Date[1],
		to = miamiDade_df$Date[nrow(miamiDade_df)],
		by = "day"
	)
)

miamiDadeFullDates_df <- 
	allDates_df %>% 
	full_join(miamiDadeRaw_df)

# Summarise Duplicate Days
# Some ESS data we have for morning and evening.
miamiDade_df <- 
	miamiDadeFullDates_df %>% 
	group_by(Date) %>% 
	summarise(across(everything(), max))



######  Test Appropriate Lags  ################################################
# We need to find which lags of case count are most related to hospitalisations
diff(miamiDade_df$Date, lag = 1)
# That doesn't work...

numDays_int <- nrow(miamiDade_df)
lags_int <- 1:28
names(lags_int) <- paste0("lag_", lags_int)

# map_dfc(
# 	.x = lags_int,
# 	.f = ~{
# 		
# 		# dropWhich_idx <- (numDays_int - .x + 1):numDays_int
# 		dropWhich_idx <- 1:.x
# 		c(
# 			miamiDade_df$Date[-dropWhich_idx],
# 			rep(NA_Date_, times = .x)
# 		)
# 		
# 	}
# )


###  Cases to Hospitalisations  ###
plot(
	x = miamiDade_df$Positive[1:(nrow(miamiDade_df) - 14)],
	y = miamiDade_df$Hospitalized[(14 + 1):nrow(miamiDade_df)]
)

map_dbl(
	.x = lags_int,
	.f = ~{
		cor(
			miamiDade_df$Positive[1:(nrow(miamiDade_df) - .x)],
			miamiDade_df$Hospitalized[(.x + 1):nrow(miamiDade_df)],
			use = "pairwise.complete.obs",
			method = "spearman"
		)
	}
)

plot(
	x = miamiDade_df$Positive[1:(nrow(miamiDade_df) - 6)],
	y = miamiDade_df$Hospitalized[(6 + 1):nrow(miamiDade_df)]
)
# Best lag from cases to hospitalisations is a week. Let's fit a model:
caseToHosp_df <- tibble(
	hospitalised = miamiDade_df$Hospitalized[(6 + 1):nrow(miamiDade_df)],
	cases = miamiDade_df$Positive[1:(nrow(miamiDade_df) - 6)]
)
ggplot(data = caseToHosp_df) +
	aes(x = (cases), y = (hospitalised)) +
	geom_point() +
	stat_smooth(method = "lm")

hospByCase_mod <- lm(
	(hospitalised) ~ (cases),
	data = caseToHosp_df
)
summary(hospByCase_mod)
par(mfrow = c(2, 2))
plot(hospByCase_mod)
par(mfrow = c(1, 1))


###  Hospitalisations to Vents  ###
map_dbl(
	.x = lags_int,
	.f = ~{
		cor(
			miamiDade_df$Hospitalized[1:(nrow(miamiDade_df) - .x)],
			miamiDade_df$Ventilated[(.x + 1):nrow(miamiDade_df)],
			use = "pairwise.complete.obs",
			method = "spearman"
		)
	}
)
# Oh thank God: there isn't a strong relationship between hospitalisations and
#   vent use.


###  Vents and Death  ###
map_dbl(
	.x = lags_int,
	.f = ~{
		cor(
			miamiDade_df$Ventilated[1:(nrow(miamiDade_df) - .x)],
			miamiDade_df$DeathCount[(.x + 1):nrow(miamiDade_df)],
			use = "pairwise.complete.obs",
			method = "spearman"
		)
	}
)

plot(
	x = miamiDade_df$Ventilated[1:(nrow(miamiDade_df) - 3)],
	y = miamiDade_df$DeathCount[(3 + 1):nrow(miamiDade_df)]
)
# That looks reasonable. Also, it seems to jive with what we know from the 
#   hospitals themselves: if you're going to die after being on a vent, it's
#   going to happen within about 5 days.

ventToDeath_df <- tibble(
	deaths = miamiDade_df$DeathCount[(3 + 1):nrow(miamiDade_df)],
	vents = miamiDade_df$Ventilated[1:(nrow(miamiDade_df) - 3)]
)
ggplot(data = ventToDeath_df) +
	aes(x = (vents), y = (deaths)) +
	geom_point() +
	stat_smooth(method = "lm")

deathsByVents_mod <- lm(
	(deaths) ~ (vents),
	data = ventToDeath_df
)
summary(deathsByVents_mod)
par(mfrow = c(2, 2))
plot(deathsByVents_mod)
par(mfrow = c(1, 1))
# There's a relationship here, but it's not a good idea to try to explain its
#   implications. This relationship is considerably more complex than the case 
#   to hospitalisation relationship.



######  Summary  ##############################################################
# We see a reasonable relationship between the date of a positive test and the
#   hospital admissions six (6) days later. This was also predicted in the CDC
#   report below (thanks Roy for sending it, see the table row for "Median
#   number of days from symptom onset to hospitalization (interquartile range)):
# https://www.cdc.gov/coronavirus/2019-ncov/hcp/planning-scenarios.html
#
# In contrast, we do not see a reasonable relationship between hospitalisations
#   and mechanical ventilation for any of the lags tested. I conjecture that
#   this is because of the incredibly heterogenous population hospitalised in
#   Miami-Dade county. There is not a clear-cut path from hospitalisation to
#   ventilator use for every patient.
#
# Finally, there is a moderate relationship between the date of mechanical 
#   ventilation and the death count three (3) days later, but lags for days 1,
#   2, 4, and 8 all had Spearman Rank correlations above 0.45. This is a bit
#   surprising, as the CDC report above expects the median time from ventilation
#   to death to be 6 days. However, lags 4 and 8 had reasonable fit statistics,
#   so we probably couldn't rule out 6 as the "true" population lag. That said,
#   we may have an older population on ventilators in Miami-Dade county, so
#   that could partially explain the discrepancy.
