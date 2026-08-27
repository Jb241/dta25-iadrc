**# Bookmark #6
********************************************************************************
********************************************************************************
****** Multiple Imputation
********************************************************************************
********************************************************************************	

********************************************************************************	
// MICE

use "../Dta25 Data/iadc_dta25.dta", clear
set seed 23414


* MICE set up
mi set wide
mi register regular years_since0_sd age_base_sd female grade_sd MCIdem_iadc
mi register imputed white cci_tot_sd L_cci_tot_sd cci_inftot_sd L_cci_inftot_sd ///
					L_MCIdem_iadc mocatots_sd L_mocatots_sd gds15 L_gds15 ///
					bmi_sd memwors_bi pastdep famfreq visitfreq mastery1 mastery2 ///
					relsubj_simple married faq_sd craftdre_sd reydecorab_sd trailbtime_sd

local nolagomit i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15
local cciomit i.L_MCIdem_iadc L_cci_inftot_sd L_mocatots_sd L_gds15
local infomit i.L_MCIdem_iadc L_cci_tot_sd L_mocatots_sd L_gds15
local mocaomit i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_gds15
local gdsomit i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd
local Lcciomit MCIdem_iadc cci_inftot_sd mocatots_sd gds15
local Linfomit MCIdem_iadc cci_tot_sd mocatots_sd gds15
local Lmocaomit MCIdem_iadc cci_tot_sd cci_inftot_sd gds15
local Lgdsomit MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd
local LMCIomit cci_tot_sd cci_inftot_sd mocatots_sd gds15
local marriedomit i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 i.mastery1 i.famfreq
	
* MICE (n=20)
mi impute chained /// 
	(regress, omit(`nolagomit')) bmi_sd ///
	(logit, omit(`nolagomit') augment) memwors_bi pastdep white relsubj_simple ///
	(ologit, omit(`nolagomit') augment) famfreq visitfreq mastery1 mastery2 ///
	(mlogit, omit(`marriedomit') augment) married /// -- to fix convergence/perfect prediction issues
	(pmm, knn(5) omit(`nolagomit')) faq_sd craftdre_sd reydecorab_sd trailbtime_sd ///
	(pmm, knn(5) omit(`cciomit')) cci_tot_sd ///
	(pmm, knn(5) omit(`infomit')) cci_inftot_sd ///
	(pmm, knn(5) omit(`mocaomit')) mocatots_sd ///
	(pmm, knn(5) omit(`gdsomit')) gds15 ///
	(pmm, knn(5) omit(`Lcciomit')) L_cci_tot_sd ///
	(pmm, knn(5) omit(`Linfomit')) L_cci_inftot_sd ///
	(pmm, knn(5) omit(`Lmocaomit')) L_mocatots_sd ///
	(pmm, knn(5) omit(`Lgdsomit')) L_gds15 ///
	(logit, omit(`LMCIomit') augment) L_MCIdem_iadc ///
	= years_since0_sd age_base_sd female grade_sd MCIdem_iadc, rseed(23414) dots add(20)

save "../Dta25 Data/raw and intermediate/mi_iadc_dta25.dta", replace

********************************************************************************	
// Passive Recodes

					
* Collect locals of original means/SDs
use "../Dta25 Data/iadc_dta25.dta", clear
set seed 23414
local vars_to_unstandardize age_base grade cci_tot cci_inftot ///
	mocatots bmi faq craftdre reydecorab trailbtime years_since0
foreach var of local vars_to_unstandardize {
    qui sum `var'
    local `var'_mean = r(mean)
    local `var'_sd = r(sd)
}

* Drop lagged and opposite standardized versions of vars
use "../Dta25 Data/raw and intermediate/mi_iadc_dta25.dta", clear
qui mi xeq: drop L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
	cci_tot cci_inftot mocatots bmi reydecorab craftdre trailbtime ///
	age_base faq grade years_since0

mi xtset id wave

* Passively unstandardize variables in the imputed datasets
qui mi passive: gen cci_tot = cci_tot_sd * `cci_tot_sd' + `cci_tot_mean'
qui mi passive: gen cci_inftot = cci_inftot_sd * `cci_inftot_sd' + `cci_inftot_mean'
qui mi passive: gen mocatots = mocatots_sd * `mocatots_sd' + `mocatots_mean'
qui mi passive: gen bmi = bmi_sd * `bmi_sd' + `bmi_mean'
qui mi passive: gen reydecorab = reydecorab_sd * `reydecorab_sd' + `reydecorab_mean'
qui mi passive: gen craftdre = craftdre_sd * `craftdre_sd' + `craftdre_mean'
qui mi passive: gen trailbtime = trailbtime_sd * `trailbtime_sd' + `trailbtime_mean'
qui mi passive: gen age_base = age_base_sd * `age_base_sd' + `age_base_mean'
qui mi passive: gen faq = faq_sd * `faq_sd' + `faq_mean'
qui mi passive: gen grade = grade_sd * `grade_sd' + `grade_mean'
qui mi passive: gen years_since0 = years_since0_sd * `years_since0_sd' + `years_since0_mean'

* Recreate binary varirables
qui mi passive: gen dep15_f2 = (gds15 >= 5)

qui mi passive: gen cci_tot_bi = (cci_tot >= `cci_tot_mean' + `cci_tot_sd')
label define ccilbl 0 "Normal (<SD above mu)" 1 "Worried (>=SD above mu)"
label val cci_tot_bi ccilbl
label var cci_tot_bi "CCI (1=Worried)"

qui mi passive: gen cci_inftot_bi = (cci_inftot >= `cci_inftot_mean' + `cci_inftot_sd')
label val cci_inftot_bi ccilbl
label var cci_inftot_bi "CCI Informant (1=Worried)"

qui mi passive: gen mocatots_bi = (mocatots <= 25)
label define mocalbl 0 "Normal (>25)" 1 "Poor (<=25)"
label val mocatots_bi mocalbl
label var mocatots_bi "MoCA (1=Poor)"

* Recreate lagged variables
mi xtset id wave
qui mi passive: gen L_MCIdem_iadc = L.MCIdem_iadc
qui mi passive: gen L_cci_inftot_sd = L.cci_inftot_sd
qui mi passive: gen L_cci_tot_sd = L.cci_tot_sd
qui mi passive: gen L_mocatots_sd = L.mocatots_sd
qui mi passive: gen L_gds15 = L.gds15
qui mi passive: gen L_cci_tot_bi = L.cci_tot_bi
qui mi passive: gen L_cci_inftot_bi = L.cci_inftot_bi
qui mi passive: gen L_mocatots_bi = L.mocatots_bi
qui mi passive: gen L_dep15_f2 = L.dep15_f2

********************************************************************************	
// Labels for variables that are not yet labeled or recreated
					
label var cci_tot "CCI"
label var cci_inftot "Informant CCI"
label var mocatots "MoCA"
label var bmi "BMI"
label var reydecorab "Rey AVLT"
label var craftdre "Craft delayed recall"
label var trailbtime "Trails-B time"
label var age_base "Age at 1st visit"
label var faq "Functional ability"
label var grade "Years of education"
label var L_cci_inftot_sd "Lagged Informant CCI (SD)"
label var L_cci_tot_sd "Lagged CCI (SD)"
label var L_mocatots_sd "Lagged MoCA (SD)"
label var L_gds15 "Lagged GDS-15"
label var years_since0 "Years since 1st visit"
label var age_base_sd "Age at 1st visit (SD)"
label var grade_sd "Years of education (SD)"
label var cci_tot_sd "CCI (SD)"
label var cci_inftot_sd "Informant CCI (SD)"
label var mocatots_sd "MoCA (SD)"
label var bmi_sd "BMI (SD)"
label var faq_sd "Functional ability (SD)"
label var craftdre_sd "Craft delayed recall (SD)"
label var reydecorab_sd "Rey AVLT (SD)"
label var trailbtime_sd "Trails-B time (SD)"

********************************************************************************	
// Save
save "../Dta25 Data/mi_iadc_dta25_recoded.dta", replace


