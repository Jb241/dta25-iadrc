
**********************************************************************
* Recode IADRC data drop May, 2025 for dementia labeling paper
**********************************************************************

************************************************************************
************************************************************************
// Longitudinal data recodes
************************************************************************
************************************************************************

import excel "../Dta25 Data/raw and intermediate/IADC_clinical_2025-12-05.xlsx", sheet("Longitudinal Data") firstrow clear

************
// Organizational

*ID
codebook subid // 574 people with multiple waves
gen id = subid
label var id "Respondent ID"

* Wave
* Convert string date to Stata date format
gen visitdate_s = date(visitdate, "YMD")
format visitdate_s %td

* wave variable:
bysort id (visitdate_s): gen wave = _n
list id visitdate_s wave in 1/15
sum wave
label var wave "Wave"

* Years since 1st wave
bysort id (visitdate_s): gen days_since0 = visitdate_s - visitdate_s[1]
gen years_since0 = days_since0/365.25
hist years_since0
list id wave visitdate_s years_since0 in 1/10
label var years_since0 "Years since 1st visit"

* Years since previous wave
bysort id (visitdate_s): gen days_since_last = visitdate_s - visitdate_s[_n-1]
gen yrs_since_last = days_since_last / 365.25
list id wave visitdate_s yrs_since_last in 1/10

* Calendar year
gen study_year = year(visitdate_s)
label var study_year "Calendar year"

* Decade
gen decade = floor(study_year/10) * 10
label define decade_lbl 1990 "1990s" 2000 "2000s" 2010 "2010s" 2020 "2020s"
label values decade decade_lbl

************
// DV
codebook gds15
replace gds15 = . if gds15 == 88
label var gds15 "Geriatric Depression Scale-15"
by study_year, sort: count if missing(gds15)
by study_year, sort: count if !missing(gds15) // nothing before 2006

* Clinical depression
gen dep15_f2 = .
replace dep15_f2 = 1 if gds15 >= 5
replace dep15_f2 = 0 if gds15 < 5
tab dep15_f2, m // 634 observations with clinical depression ()
distinct id if dep15_f2 == 1 // 241 participants with at least one wave of clinical depression

************
// IVs

* CCI
local ccivars ccis01 ccis02 ccis03 ccis04 ccis05 ccis06 ccis07 ccis08 ccis09 ccis10 ccis11 ccis12 ccis13 ccis14 ccis15 ccis16 ccis17 ccis18 ccis19 ccis20
egen cci_tot = rowtotal(`ccivars')
egen miss_count = rowmiss(`ccivars')
replace cci_tot = . if miss_count > 0
sum cci_tot
label var cci_tot "CCI Self"

* alternative: 
gen memwors_bi = .
replace memwors_bi = 1 if memwors == 1 | memwors == 2
replace memwors_bi = 0 if memwors == 0
label var memwors_bi "Do you feel like your memory is getting worse?"
label define memwors_bi_lbl 0 "No" 1 "Yes"
label val memwors_bi memwors_bi_lbl

* CCI Informant
local cciinfvars ccii01 ccii02 ccii03 ccii04 ccii05 ccii06 ccii07 ccii08 ccii09 ccii10 ccii11 ccii12 ccii13 ccii14 ccii15 ccii16 ccii17 ccii18 ccii19 ccii20
egen cci_inftot = rowtotal(`cciinfvars')
egen miss_count_inf = rowmiss(`cciinfvars')
replace cci_inftot = . if miss_count_inf > 0
sum cci_inftot
label var cci_inftot "CCI Informant"

* MoCA
replace mocatots = . if mocatots == 88 // missing code
sum mocatots
label var mocatots "MoCA"

* Craft delayed recall
label var craftdre "Craft Story 21 Delayed Recall"

* Rey
label var reydecorab "Rey-AVLT Delayed Recall"

* Trails B time
gen trailbtime = trailb
replace trailbtime = . if trailb == 995 | trailb == 996 | trailb == 997 | trailb == 998
sum trailbtime

* Diagnosis // I think MBI and impaired not MCI should just be dropped
			// SCD as normal
			// dementia diagnosis or MCI = 1
gen MCIdem_iadc = .
replace MCIdem_iadc = 1 if diag == "Amnestic MCI-memory impairment"
replace MCIdem_iadc = 1 if diag == "Amnestic MCI-memory impairment Plus one or more domains"
replace MCIdem_iadc = 1 if diag == "Dementia"
replace MCIdem_iadc = 99 if diag == "Impaired, not MCI"
replace MCIdem_iadc = 99 if diag == "MBI"
replace MCIdem_iadc = 1 if diag == "MCI"
replace MCIdem_iadc = 1 if diag == "Non-amnestic MCI-multiple domains"
replace MCIdem_iadc = 1 if diag == "Non-amnestic MCI-single domain"
replace MCIdem_iadc = 0 if diag == "Normal"
replace MCIdem_iadc = 0 if diag == "SCD (particpant concern UDS4)"
label define MCIdemlbl 0 "Normal" 1 "MCI/Dementia" 99 "To drop: MBI/impaired"
label val MCIdem_iadc MCIdemlbl
label var MCIdem_iadc "Diagnosis (1=MCI/Dementia)"


************
// Time-varying controls

* Married 
	* 1 Married, 2 Widowed, 3 Divorced, 4 Separated, 5 Never married, 6 Living as 	married/domestic partner, 9 dk
tab maristat
recode maristat (1 6 = 1 "Married") (2 3 4 = 2 "No longer married") (5 = 3 "Never married") (9 = .), gen(married)
tab maristat married

* FAQ
local faqvars bills taxes shopping games stove mealprep events payattn remdates travel
egen miss_count_faq = rowmiss(`faqvars')
egen faq = rowtotal(`faqvars')
replace faq = . if miss_count_faq > 0
label var faq "Functional Activity Scale"

* Visitors
gen visitfreq = visitors_f
replace visitfreq = . if visitors_f==0 // 0 means no response/dk
label define visitlbl 1 "Once yr or less" 2 "Several per yr" 3 "Several per month" 4 "Several per week" 5 "Almost every day"
label val visitfreq visitlbl
label var visitfreq "Receive visitors frequency"

* Family/friend visit frequency
gen famfreq = famfriends_f
replace famfreq = . if famfriends_f==0 // 0 means no response/dk
label val famfreq visitlbl
label var famfreq "Visit family + friends frequency"

* Informant relation to subject
tab relsubj, m // 1 spouse/partner, 2 child, 3 sibling, 4 other relative, 5 friend/neighbor, 6 paid caregiver
gen relsubj_simple = .
replace relsubj_simple = . if relsubj == 0 | relsubj == 7 | relsubj == 8
replace relsubj_simple = 1 if relsubj == 1 | relsubj == 2
replace relsubj_simple = 2 if relsubj == 3 | relsubj == 4
replace relsubj_simple = 3 if relsubj == 5
replace relsubj_simple = 4 if relsubj == 6
label var relsubj_simple "Informant relationship to participant"
label define relsubjsim_lbl 1 "Spouse or child" 2 "Other relative" 3 "Friend/neighbor" 4 "Paid caregiver"
label val relsubj_simple relsubjsim_lbl

************
// Merge in BMI and depression history

* Add BMI from 2026 data
save "../Dta25 Data/raw and intermediate/iadc25_longitudinal_p1.dta", replace
import excel "../Dta25 Data/raw and intermediate/IADRC_clinical_2026-05-11.xlsx", sheet("Longitudinal Data") firstrow clear
gen id = subid
gen visitdate_s = date(visitdate, "YMD")
format visitdate_s %td
keep id visitdate_s bmi
merge 1:1 id visitdate_s using "../Dta25 Data/raw and intermediate/iadc25_longitudinal_p1.dta"
drop if _merge == 1 
* 114 obs in 2026 data not in 2025 data
drop _merge
label var bmi "BMI"

* Add depression history from 2024 data
save "../Dta25 Data/raw and intermediate/iadc25_longitudinal_p2.dta", replace
import delimited "../Dta25 Data/raw and intermediate/IADC_clinical_12APR2024.csv", clear
destring subid, gen(id) force
drop if id == .
gen strL v = trim(visitdate)
replace v = ustrregexra(v, "[^0-9]+", " ")
list v visitdate in 1/10
gen mm = real(word(v,1))
gen dd = real(word(v,2))
gen yy = real(word(v,3))
replace yy = yy + 2000 if yy >=0 & yy <= 25
replace yy = yy + 1900 if yy >=26 & yy <= 99
gen visitdate_s = mdy(mm, dd, yy)
format visitdate_s %td
keep id visitdate_s depothr
merge 1:1 id visitdate_s using "../Dta25 Data/raw and intermediate/iadc25_longitudinal_p2.dta"
keep if _merge == 3
drop _merge
* 459 observations in 2025 data not in 2024 data
tab depothr 
gen pastdep = depothr
replace pastdep = . if depothr == 9
label var pastdep "Depression episodes more than two years ago"

// Save longitudinal data
save "../Dta25 Data/raw and intermediate/iadc-longitud-recoded.dta", replace


************************************************************************
************************************************************************
// Cross-sectional
************************************************************************
************************************************************************

import excel "../Dta25 Data/raw and intermediate/IADRC_clinical_2026-05-11.xlsx", sheet("Cross-sectional Data") firstrow clear
rename subid id

************
// Time-constant controls

* Sex
gen female = .
replace female = 0 if sex == "M"
replace female = 1 if sex == "F"
label define sexlbl 0 "Male" 1 "Female" 
label val female sexlbl
label var female "Sex 1=Female"

* Education
label var grade "Highest grade education completed"

* Race
tab race // 1 = white, 2 = black, 5 = ?, 50 = ?
tab hispanic // 0 = not, 1 = yes
gen white = .
replace white = 1 if race == 1 & hispanic == 0
replace white = 0 if race == 2 | race == 5 | hispanic == 1
label define whitelbl 0 "Other race" 1 "White"
label val white whitelbl
label var white "Race 1=white"

* APOE
tab apoegenotype
recode apoegenotype (22 23 33 = 0 "No E4") (24 34 = 1 "Hetero E4") (44 = 2 "Homo E4"), gen(apoe)
label var apoegenotype "APOE genotype"

* Deal with one repeated row
duplicates drop

************
// Save cross-sectional data
save "../Dta25 Data/raw and intermediate/iadc-cross-sectional-recoded.dta", replace

************************************************************************
************************************************************************
// Merge
************************************************************************
************************************************************************

merge 1:m id using "../Dta25 Data/raw and intermediate/iadc-longitud-recoded.dta"

************
// Final merged recodes

* Age at each visit (in years)
gen birthdate = mdy(birthmnth, 15, birthyr) // set date to 15 for everyone
format birthdate %td
gen ageatvisit = (visitdate_s - birthdate) / 365.25
label var ageatvisit "Age at visit (years)"

* Age at baseline (first visit)
bysort id (visitdate_s): gen age_base = ageatvisit[1]
label var age_base "Age at baseline (years)"

************
// Standardize every continuous variable
local varlist gds15 cci_tot cci_inftot mocatots ///
	age_base grade faq bmi ///
	craftdre trailbtime reydecorab years_since0
foreach var in `varlist' {
	egen `var'_sd = std(`var')
}

************
// Lag everything

tsset id wave

* Standardized ones
local varlist bmi cci_inftot cci_tot craftdre faq ///
	mocatots reydecorab trailbtime
foreach var in `varlist' {
	gen L_`var'_sd = L.`var'_sd
}

* Unstandardized ones
local varlist2 famfreq gds15 married mastery1 mastery2 ///
	MCIdem_iadc memwors_bi relsubj_simple visitfreq yrs_since_last pastdep
foreach var in `varlist2' {
	gen L_`var' = L.`var'
}

************
// Drop invalid rows

*
misstable summarize gds15 MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd ///
	years_since0_sd age_base_sd faq_sd female white grade_sd married ///
	relsubj_simple mastery1 mastery2 visitfreq famfreq  ///
	yrs_since_last bmi_sd pastdep

drop if years_since0_sd == . // remove 149 observations where this is missing (no date for visit)
drop if MCIdem_iadc == 99 // remove 56 observations where diagnosis is MBI no MCI (not relevant)
	
misstable summarize	years_since0_sd age_base_sd female grade_sd

************
// Save final data set
save "../Dta25 Data/iadc_dta25.dta", replace
