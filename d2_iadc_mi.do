**# Bookmark #6
********************************************************************************
********************************************************************************
****** Multiple Imputation
********************************************************************************
********************************************************************************	
use "../Dta25 Data/iadc_dta25.dta", clear
set seed 23414

drop if cci_tot < 20
drop if gds15 > 15 & gds15 !=. // people with "88" as value ??
drop if female == . // this is one person with only 1 wave

sum id
codebook id

misstable summarize gds15 MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd ///
	time_centered_sd age_base_sd faq_sd female white grade_sd married ///
	relsubj_simple mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
	days_since_last 
	/// bmi_sd

* MICE set up
mi set wide
mi register imputed cci_tot_sd cci_inftot_sd mocatots_sd MCIdem_iadc ///
	L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_MCIdem_iadc ///
	/// bmi_sd ///
	married ///
	gds15 L_gds15 ///
	reydecorab_sd craftdre_sd trailbtime_sd ///
	L_reydecorab_sd L_craftdre_sd L_trailbtime_sd ///
	mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
	L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
	/// pastdep ///
	memwors_bi L_memwors_bi yrs_since_last relsubj_simple
mi register regular time_centered_sd age_base_sd faq_sd female white grade_sd

* Dry run	
mi impute chained /// (regress, omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
					///	L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd ///
					///	L_visitors_f_sd L_famfriends_f_sd i.L_memwors_bi)) bmi_sd ///
	/// (logit, augment omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		///	L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		///	i.L_memwors_bi)) pastdep ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) married yrs_since_last relsubj_simple ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) gds15 ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_inftot_sd cci_tot_sd mocatots_sd ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_gds15 ///
	(logit, augment omit(cci_inftot_sd cci_tot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_MCIdem_iadc ///	
	(logit, omit(L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		)) memwors_bi ///
	(logit, omit(cci_inftot_sd cci_tot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		)) L_memwors_bi ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) mocatots_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_inftot_sd cci_tot_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_mocatots_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) cci_tot_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_cci_tot_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) cci_inftot_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_cci_inftot_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) trailbtime_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_trailbtime_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) craftdre_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_craftdre_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) reydecorab_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_reydecorab_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) mastery1_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_mastery1_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) mastery2_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_mastery2_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) visitors_f_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd famfriends_f_sd ///
		i.memwors_bi)) L_visitors_f_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd ///
		i.L_memwors_bi)) famfriends_f_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd ///
		i.memwors_bi)) L_famfriends_f_sd ///
	(logit, augment omit(L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) MCIdem_iadc ///
	= time_centered_sd age_base_sd faq_sd female white grade_sd, rseed(23414) dryrun
	
* Real initial imputation step
mi impute chained /// (regress, omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
					///	L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd ///
					///	L_visitors_f_sd L_famfriends_f_sd i.L_memwors_bi)) bmi_sd ///
	/// (logit, augment omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		///	L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		///	i.L_memwors_bi)) pastdep ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) married yrs_since_last relsubj_simple ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) gds15 ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_inftot_sd cci_tot_sd mocatots_sd ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_gds15 ///
	(logit, omit(L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		)) memwors_bi ///
	(logit, omit(cci_inftot_sd cci_tot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		)) L_memwors_bi ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) mocatots_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_inftot_sd cci_tot_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_mocatots_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) cci_tot_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_cci_tot_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) cci_inftot_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_cci_inftot_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) trailbtime_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_trailbtime_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) craftdre_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_craftdre_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) reydecorab_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_reydecorab_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) mastery1_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_mastery1_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) mastery2_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_mastery2_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) visitors_f_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd famfriends_f_sd ///
		i.memwors_bi)) L_visitors_f_sd ///
	(pmm, knn(5) omit(i.L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd ///
		i.L_memwors_bi)) famfriends_f_sd ///
	(pmm, knn(5) omit(i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd ///
		i.memwors_bi)) L_famfriends_f_sd ///
	(logit, augment omit(cci_inftot_sd cci_tot_sd mocatots_sd gds15 ///
		trailbtime_sd craftdre_sd reydecorab_sd mastery1_sd mastery2_sd visitors_f_sd famfriends_f_sd ///
		i.memwors_bi)) L_MCIdem_iadc ///	
	(logit, augment omit(L_cci_inftot_sd L_cci_tot_sd L_mocatots_sd L_gds15 ///
		L_trailbtime_sd L_craftdre_sd L_reydecorab_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd ///
		i.L_memwors_bi)) MCIdem_iadc ///
	= time_centered_sd age_base_sd faq_sd female white grade_sd, rseed(23414) add(20) force

save "mi20_iadc-long_2025.dta", replace

***********************
* Passive Recodes

* Unstandardize variables
use "iadc-long_2025.dta", clear
set seed 23414

drop if cci_tot < 20
drop if gds15 > 15 & gds15 !=.
drop if female == . 

* Define locals with original means and SDs
local vars_to_unstandardize cci_tot cci_inftot mocatots ///
	/// bmi ///
	reydecorab ///
    craftdre trailbtime mastery1 mastery2 visitors_f famfriends_f ///
    time_centered age_base faq grade

* Loop through and store means and SDs
foreach var of local vars_to_unstandardize {
    qui sum `var'
    local `var'_mean = r(mean)
    local `var'_sd = r(sd)
}

use "mi20_iadc-long_2025.dta", clear // switch to "mi_iadc-long.dta" for final analysis

qui mi xeq: drop L_MCIdem_iadc L_cci_inftot_sd L_cci_tot_sd L_reydecorab_sd L_craftdre_sd L_trailbtime_sd L_mastery1_sd L_mastery2_sd L_visitors_f_sd L_famfriends_f_sd L_mocatots_sd L2_MCIdem_iadc L2_cci_inftot_sd L2_cci_tot_sd L2_mocatots_sd L_gds15 L2_gds15 dep15_f2 L_memwors_bi cci_tot_bi cci_inftot_bi mocatots_bi craftdre_bi reydecorab_bi trailbtime_bi L_cci_tot_bi L_cci_inftot_bi L_mocatots_bi L_craftdre_bi L_reydecorab_bi L_trailbtime_bi cci_inftot L_cci_inftot cci_tot L_cci_tot yrs_since_last cci_mean_sd cci_within_sd L_cci_within_sd L_cci_within cci_mean cci_within L_cci_within gds15_mean gds15_within L_gds15_within gds15_sd gds15_mean_sd gds15_within_sd L_gds15_within_sd mocatots ///
	///bmi ///
	reydecorab craftdre trailbtime mastery1 mastery2 visitors_f famfriends_f time_centered age_base faq grade

mi xtset id wave

* Create years since last
gen yrs_since_last = days_since_last / 365.25

* Passively unstandardize variables in the imputed datasets
qui mi passive: gen cci_tot = cci_tot_sd * `cci_tot_sd' + `cci_tot_mean'
qui mi passive: gen cci_inftot = cci_inftot_sd * `cci_inftot_sd' + `cci_inftot_mean'
qui mi passive: gen mocatots = mocatots_sd * `mocatots_sd' + `mocatots_mean'
mi passive: gen bmi = bmi_sd * `bmi_sd' + `bmi_mean'
qui mi passive: gen reydecorab = reydecorab_sd * `reydecorab_sd' + `reydecorab_mean'
qui mi passive: gen craftdre = craftdre_sd * `craftdre_sd' + `craftdre_mean'
qui mi passive: gen trailbtime = trailbtime_sd * `trailbtime_sd' + `trailbtime_mean'
qui mi passive: gen mastery1 = mastery1_sd * `mastery1_sd' + `mastery1_mean'
qui mi passive: gen mastery2 = mastery2_sd * `mastery2_sd' + `mastery2_mean'
qui mi passive: gen visitors_f = visitors_f_sd * `visitors_f_sd' + `visitors_f_mean'
qui mi passive: gen famfriends_f = famfriends_f_sd * `famfriends_f_sd' + `famfriends_f_mean'
qui mi passive: gen time_centered = time_centered_sd * `time_centered_sd' + `time_centered_mean'
qui mi passive: gen age_base = age_base_sd * `age_base_sd' + `age_base_mean'
qui mi passive: gen faq = faq_sd * `faq_sd' + `faq_mean'
qui mi passive: gen grade = grade_sd * `grade_sd' + `grade_mean'

local cat_to_round mastery1 mastery2 visitors_f famfriends_f
foreach var of local cat_to_round {
    mi passive: replace `var' = round(`var')
}

* well with diagnosis
qui mi passive: gen mocadiag_off = .
qui mi passive: replace mocadiag_off = 0 if MCIdem_iadc != . & mocatots !=.
qui mi passive: replace mocadiag_off = 1 if MCIdem_iadc == 1 & mocatots > 24

* Lagged vars
mi xtset id wave
qui mi passive: gen L_MCIdem_iadc = L.MCIdem_iadc
qui mi passive: gen L_cci_inftot_sd = L.cci_inftot_sd
qui mi passive: gen L_cci_tot_sd = L.cci_tot_sd
qui mi passive: gen L_mocatots_sd = L.mocatots_sd
qui mi passive: gen L_reydecorab_sd = L.reydecorab_sd
qui mi passive: gen L_craftdre_sd = L.craftdre_sd
qui mi passive: gen L_trailbtime_sd = L.trailbtime_sd
qui mi passive: gen L_mastery1_sd = L.mastery1_sd
qui mi passive: gen L_mastery2_sd = L.mastery2_sd
qui mi passive: gen L_visitors_f_sd = L.visitors_f_sd
qui mi passive: gen L_famfriends_f_sd = L.famfriends_f_sd
qui mi passive: gen L2_MCIdem_iadc = L2.MCIdem_iadc
qui mi passive: gen L2_cci_inftot_sd = L2.cci_inftot_sd
qui mi passive: gen L2_cci_tot_sd = L2.cci_tot_sd
qui mi passive: gen L2_mocatots_sd = L2.mocatots_sd
qui mi passive: gen L_gds15 = L.gds15
qui mi passive: gen L2_gds15 = L2.gds15
qui mi passive: gen L_memwors_bi = L.memwors_bi
qui mi passive: gen L_cci_inftot = L.cci_inftot
qui mi passive: gen L_cci_tot = L.cci_tot
qui mi passive: gen L_faq_sd = L.faq_sd
qui mi passive: gen L_mocadiag_off = L.mocadiag_off

* Calculate means and SDs for binary cutoffs from original data
preserve
mi extract 0, clear
local vars_to_bi cci_tot cci_inftot reydecorab ///
    craftdre trailbtime
foreach var of local vars_to_bi {
    qui sum `var'
    local mean_`var' = r(mean)
    local sd_`var' = r(sd)
}
restore

* Binary vars
qui mi passive: gen dep15_f2 = (gds15 >= 5)

qui mi passive: gen cci_tot_bi = (cci_tot >= `mean_cci_tot' + `sd_cci_tot')
label define ccilbl 0 "Normal (<SD above mu)" 1 "Worried (>=SD above mu)"
label val cci_tot_bi ccilbl
label var cci_tot_bi "CCI (1=Worried)"

qui mi passive: gen cci_inftot_bi = (cci_inftot >= `mean_cci_inftot' + `sd_cci_inftot')
label val cci_inftot_bi ccilbl
label var cci_inftot_bi "CCI Informant (1=Worried)"

qui mi passive: gen mocatots_bi = (mocatots <= 25)
label define mocalbl 0 "Normal (>25)" 1 "Poor (<=25)"
label val mocatots_bi mocalbl
label var mocatots_bi "MoCA (1=Poor)"

qui mi passive: gen craftdre_bi = (craftdre <= `mean_craftdre' - `sd_craftdre')
label define coglbl 0 "Normal (>SD below mu)" 1 "Poor (<=SD below mu)"
label val craftdre_bi coglbl
label var craftdre_bi "Craft Story 21 Delayed Recall (1=Poor)"

qui mi passive: gen reydecorab_bi = (reydecorab <= `mean_reydecorab' - `sd_reydecorab')
label val reydecorab_bi coglbl
label var reydecorab_bi "Rey-AVLT (1=Poor)"

qui mi passive: gen trailbtime_bi = (trailbtime >= `mean_trailbtime' + `sd_trailbtime')
label define traillbl 0 "Normal (<SD above mu)" 1 "Poor (>=SD above mu)"
label val trailbtime_bi traillbl
label var trailbtime_bi "Trails-B Time (1=Poor)"

mi xtset id wave
qui mi passive: gen L_cci_tot_bi = L.cci_tot_bi
qui mi passive: gen L_cci_inftot_bi = L.cci_inftot_bi
qui mi passive: gen L_mocatots_bi = L.mocatots_bi
qui mi passive: gen L_reydecorab_bi = L.reydecorab_bi
qui mi passive: gen L_trailbtime_bi = L.trailbtime_bi
qui mi passive: gen L_craftdre_bi = L.craftdre_bi


* Labels
label var mocatots_sd "MoCA Score (in SD)"
label var cci_tot "CCI"
label var cci_inftot "CCI Informant"
label var L_cci_tot "Lag CCI"
label var L_cci_inftot "Lag CCI Informant"
label var L_MCIdem_iadc "Lag Diagnosis (1=MCI/dementia)"
label var L_gds15 "Lag GDS-15"
label var L_visitors_f "Lag Freq Receive Visitors"
label var L_famfriends_f "Lag Freq Visit Family/Friends"
label var L_craftdre_sd "Lag CRAFT Delayed Recall (in SD)"
label var L_cci_within "Lag CCI Within"
label var cci_mean "Person-level CCI Mean"
label var L_gds15_within "Lag GDS-15 Within"
label var gds15_mean "Person-level GDS-15 Mean"
label var L2_MCIdem_iadc "2 lags Diagnosis (1=MCI/dementia)"
label var L2_gds15  "2 lags GDS-15"
label var L_memwors_bi "Lag self feel memory getting worse"
label var L_cci_tot_bi "Lag CCI (1=Worried)"
label var L_cci_inftot_bi "Lag CCI Informant (1=Worried)"
label var L_mocatots_bi "Lag MoCA (1=Poor)"
label var L_craftdre_bi "Lag Craft Story 21 Delayed Recall (1=Poor)"
label var L_reydecorab_bi "Lag Rey-AVLT (1=Poor)"
label var L_trailbtime_bi "Lag Trails-B Time (1=Poor)"
label var L_cci_tot_sd "Lag CCI (in SD)"
label var L_cci_inftot_sd "Lag CCI Informant (in SD)"
label var L_visitors_f_sd "Lag Freq Receive Visits (in SD)"
label var L_famfriends_f_sd "Lag Freq Visit Family/Friends (in SD)"
label var L_trailbtime_sd "Lag Trails B Time (in SD)"
label var L_reydecorab_sd "Lag Rey-AVLT (in SD)"
label var L_craftdre_sd "Lag CRAFT Delayed Recall (in SD)"
label var L_mocatots_sd "Lag MoCA Score (in SD)"
label var cci_within_sd "CCI Within (in SD)"
label var L_cci_within_sd "Lag CCI Within (in SD)"
label var cci_mean_sd "Person-level CCI Mean (in SD)"
label var gds15_within_sd "GDS-15 Within (in SD)"
label var L_gds15_within_sd "Lag GDS-15 Within (in SD)"
label var gds15_mean_sd "Person-level GDS-15 Mean (in SD)"
label var L_mastery1_sd "Lag Mastery1: can do anything (in SD)"
label var L_mastery2_sd "Lag Mastery2: feel helpless dealing with problems (in SD)"
label var L2_cci_inftot_sd "2 lags CCI Informant (in SD)"
label var L2_cci_tot_sd "2 lags CCI (in SD)"
label var L2_mocatots_sd "2 lags MoCA (in SD)"
label var L_faq_sd "Lagged functional ability (in SD)"
label var cci_tot "Cognitive Change Index"
label var cci_inftot "Cognitive Change Index - Informant"
label var mocatots "MoCA"
label var MCIdem_iadc "Diagnosis (1=MCI/Dementia)"
label var bmi "Body Mass Index"
label var married "Marital Status (ref=Married)"
label var gds15 "Geriatric Depression Scale-15"
label var reydecorab "Rey-AVLT"
label var craftdre "Craft Story 21 Delayed Recall"
label var trailbtime "Trail Making Test B Time"
label var mastery1 "Mastery 1: I can do just about anything I really set my mind to."
label var mastery2 "Mastery 2: I often feel helpless in dealing with the problems of life."
label var visitors_f "Frequency Receive Visitors"
label var famfriends_f "Frequency Visit Family/Friends"
label var pastdep "Depression episode(s) >2 yrs ago"
label var yrs_since_last "Years Since Last Visit"
label var time_centered "Time Elapsed Since Baseline (Centered)"
label var age_base "Age at Baseline"
label var faq "Functional Activities Questionnaire"
label var grade "Years of Education"
label var mocadiag_off "Healthy MoCA with Diagnosis"
label var L_mocadiag_off "Lag Healthy MoCA with Diagnosis"
label define mocadiaglbl 0 "No" 1 "Yes"
label val mocadiag_off L_mocadiag_off mocadiaglbl


save "mi20_iadc-long-recoded_2025.dta", replace


