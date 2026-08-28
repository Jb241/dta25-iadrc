**********************************************************************
* Analysis for IADRC data drop May, 2025 for dementia labeling paper
**********************************************************************

********************************************************************************
********************************************************************************
****** Univariate Descriptives
********************************************************************************
********************************************************************************
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == . // do not impute dependent variable

* Recodes for univariate  descriptives
qui mi passive: gen mardum1 = (married==1)
qui mi passive: gen mardum2 = (married==2)
qui mi passive: gen mardum3 = (married==3)
qui mi passive: gen mastdum1 = (mastery1==1)
qui mi passive: gen mastdum2 = (mastery1==2)
qui mi passive: gen mastdum3 = (mastery1==3)
qui mi passive: gen mastdum4 = (mastery1==4)
qui mi passive: gen mast2dum1 = (mastery2==1)
qui mi passive: gen mast2dum2 = (mastery2==2)
qui mi passive: gen mast2dum3 = (mastery2==3)
qui mi passive: gen mast2dum4 = (mastery2==4)
qui mi passive: gen famdum1 = (famfreq==1)
qui mi passive: gen famdum2 = (famfreq==2)
qui mi passive: gen famdum3 = (famfreq==3)
qui mi passive: gen famdum4 = (famfreq==4)
qui mi passive: gen famdum5 = (famfreq==5)
qui mi passive: gen visdum1 = (visitfreq==1)
qui mi passive: gen visdum2 = (visitfreq==2)
qui mi passive: gen visdum3 = (visitfreq==3)
qui mi passive: gen visdum4 = (visitfreq==4)
qui mi passive: gen visdum5 = (visitfreq==5)
qui mi passive: gen reldum0 = (relsubj_simple==1)
qui mi passive: gen reldum1 = (relsubj_simple==2)
qui mi passive: gen reldum2 = (relsubj_simple==3)
qui mi passive: gen reldum3 = (relsubj_simple==4)
label var mardum1 "Married"
label var mardum2 "No longer married"
label var mardum3 "Never married"
label var mastdum1 "Strongly agree"
label var mastdum2 "Somewhat agree"
label var mastdum3 "Somewhat disagree"
label var mastdum4 "Strongly disagree"
label var mast2dum1 "Strongly agree"
label var mast2dum2 "Somewhat agree"
label var mast2dum3 "Somewhat disagree"
label var mast2dum4 "Strongly disagree"
label var famdum1 "Once a year or less"
label var famdum2 "Several times a year"
label var famdum3 "Several times a month"
label var famdum4 "Several times a week"
label var famdum5 "Every day or almost every day"
label var visdum1 "Once a year or less"
label var visdum2 "Several times a year"
label var visdum3 "Several times a month"
label var visdum4 "Several times a week"
label var visdum5 "Every day or almost every day"
label var reldum0 "Spouse or child"
label var reldum1 "Other relative"
label var reldum2 "Friend or neighbor"
label var reldum3 "Paid caretaker"

* Create list of all variables to summarize
local allvars gds15 MCIdem_iadc cci_tot cci_inftot ///
	mocatots faq bmi ///
	female white grade ///
	mardum1 mardum2 mardum3 ///
	reldum0 reldum1 reldum2 reldum3 ///
    age_base years_since0 yrs_since_last ///
	mastdum1 mastdum2 mastdum3 mastdum4 ///
	mast2dum1 mast2dum2 mast2dum3 mast2dum4 ///
	visdum1 visdum2 visdum3 visdum4 visdum5 ///
	famdum1 famdum2 famdum3 famdum4 famdum5 
	/// pastdep

* Get sample size from imputed datasets (m=1)
mi xeq 1: summarize cci_tot

local N = r(N)

* Calculate MI-based means and SDs
matrix means = J(1, `:word count `allvars'', .)
matrix sds = J(1, `:word count `allvars'', .)

local i = 1
foreach var of local allvars {
    * Get MI estimate of mean and SD using Rubin's rules
    quietly mi estimate, post: mean `var'
    matrix means[1, `i'] = _b[`var']
    
    * Calculate SD using Andy's formula: SE * sqrt(N)
    scalar v = e(V)[1,1]
    local sd = sqrt(v) * sqrt(e(N))
    matrix sds[1, `i'] = `sd'
    
    local i = `i' + 1
}

* Name the matrix columns
matrix colnames means = `allvars'
matrix colnames sds = `allvars'

* Run a simple estimation command to set up e() space
quietly regress cci_tot cci_inftot

* Clear and post  matrices
ereturn clear
ereturn post means
estadd matrix sds = sds

* Export table using esttab
esttab using "../../Output/Dta25 Output/Tables/raw/T1_descriptive_table.docx", ///
	replace ///
    cells("b(fmt(2) label(Mean/Proportion)) sds(fmt(2) label(SD))") ///
    label title("Descriptive Statistics") ///
    nonumbers nomtitles noobs ///
    addnotes("N = `N'")

// More sample info

* number of participants			// 575 unique participants
mi xeq 1: codebook id

* number of waves					// 19 waves max
preserve
mi extract 1, clear
bysort id: gen nobs = _N
tabulate nobs
restore

* year span
mi xeq 1: codebook visitdate_s 		// 2006-2026

// Cross diagonal counts
set seed 23414
scalar rand_m = ceil(runiform() * 20)
display rand_m

local outfile "../../Output/Dta25 Output/Tables/raw/crosstabs.doc"

preserve
mi extract 2, clear

asdoc tab cci_tot_bi cci_inftot_bi, save(`outfile') replace
asdoc tab cci_tot_bi MCIdem_iadc, save(`outfile') append
asdoc tab cci_tot_bi mocatots_bi, save(`outfile') append
asdoc tab cci_inftot_bi MCIdem_iadc, save(`outfile') append
asdoc tab cci_inftot_bi mocatots_bi, save(`outfile') append
asdoc tab mocatots_bi MCIdem_iadc, save(`outfile') append
restore

**# Bookmark #2
********************************************************************************
********************************************************************************
****** Main Effects
********************************************************************************
********************************************************************************

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear

drop if gds15 == .

mi xtset id wave

* Main Analyses
mi estimate, cmdok eform post: mepoisson gds15 ///
    MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd ///
    || id:, irr iterate(500) intpoints(20) difficult
est sto mi_main_contemp_red

mi estimate, cmdok eform post: mepoisson gds15 ///
	MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd faq_sd ///
	bmi_sd female white grade_sd ///
	age_base_sd years_since0_sd ///
	i.married relsubj_simple ///
    || id:, irr iterate(200) intpoints(12)
est sto mi_main_contemp_full
	* AMEs
est restore mi_main_contemp_full
mimrgns, dydx(cci_tot_sd) predict(mu)
mimrgns, dydx(MCIdem_iadc) predict(mu)

preserve
mi xtset id wave
save "../Dta25 Data/raw and intermediate/mi_iadc_2025_05_temp.dta", replace
mi convert flong
qui mi estimate: regress gds15 ///
	i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd
mivif
qui mi estimate: regress gds15 ///
	i.MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd ///
    years_since0_sd age_base_sd female white grade_sd faq_sd ///
	bmi_sd i.married ///
	relsubj_simple
mivif
restore 

* plot
mi xeq: sum cci_tot_sd, d
* first percentile across all 20 to 99th percentile: -1.2 to 2.9
graph set window fontface "Times New Roman"
est restore mi_main_contemp_full
mimrgns, at(cci_tot_sd=(-1.2(1)2.9)) predict(mu) cmdmargins
marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    plot1opts(lcolor(gs4) lwidth(medium) mcolor(gs4) msize(small)) ///
    ci1opts(color(gs7%40)) ///
    xtitle("CCI Score (in SD)", size(medium)) ///
    ytitle("Predicted GDS-15 Score", size(medium)) ///
    title("Predicted Number of Depressive Symptoms (GDS-15)" "by Self-Assessed Decline (CCI)", size(medlarge) color(black)) ///
    xlabel(, labsize(small)) ///
    ylabel(, labsize(small) angle(horizontal)) ///
    legend(off) ///
    graphregion(fcolor(white) lcolor(white) margin(medium)) ///
    plotregion(fcolor(white) lcolor(black)) ///
    xsize(4) ysize(4)
graph export "../../Output/Dta25 Output/Figures/F2_CCI_main.tif", replace width(3000)


* plot for diagnosis
graph set window fontface "Times New Roman"
est restore mi_main_contemp_full
mimrgns, at(MCIdem_iadc=(0 1)) predict(mu) cmdmargins
marginsplot, ///
    recast(bar) ///
    ciopts(lcolor(black) lwidth(medium)) ///
    plot1opts(fcolor(gs7) lcolor(black)) ///
    ylabel(0(1)4, labsize(small) angle(horizontal)) ///
    ytitle("Predicted GDS-15 Score", size(medium)) ///
    xtitle("Diagnostic Status", size(medium)) ///
    title("Predicted Number of Depressive Symptoms (GDS-15)" "by Diagnostic Status", ///
	size(medlarge) color(black)) ///
    xlabel(0 "No Diagnosis" 1 "MCI or Dementia Diagnosis", labsize(small)) ///
    legend(off) ///
    graphregion(fcolor(white) lcolor(white) margin(medium)) ///
    plotregion(fcolor(white) lcolor(black)) ///
    yscale(range(0 4.5)) ///
    xsize(4) ysize(4)
graph export "../../Output/Dta25 Output/Figures/F1_diagnosis_main.tif", replace width(3000)
	
* Tables
esttab mi_main_contemp_red mi_main_contemp_full using ///
	"../../Output/Dta25 Output/Tables/raw/T2_main_regression.rtf", ///
    replace ///
    eform ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    title("Table: Estimates From Multilevel Poisson Model for GDS-15 (IRRs)") ///
    mtitles("Model 1" "Model 2") ///
    label	

*****
// Lagged models + past depression

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .

mi xtset id wave

* IV Lagged
mi estimate, cmdok eform post: mepoisson gds15 ///
	L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd faq_sd ///
    bmi_sd female white grade_sd ///
	i.married relsubj_simple ///
	age_base_sd years_since0_sd ///
    || id:, irr iterate(200) intpoints(12) 
est sto lagiv

* IV lagged, DV lagged, and past depression history
mi estimate, cmdok eform post: mepoisson gds15 ///
	L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd ///
	faq_sd bmi_sd female white grade_sd ///
    i.married relsubj_simple ///
	age_base_sd years_since0_sd ///
	L_gds15 pastdep ///
    || id:, irr iterate(200) intpoints(12)
est sto lagpastdep

esttab lagiv lagpastdep ///
	using "../../Output/Dta25 Output/Tables/raw/T3_lagged_main_regression.rtf", ///
    replace ///
    eform ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    title("Table: Estimates From Multilevel Poisson Model for GDS-15 (IRRs)") ///
    mtitles("Model 1" "Model 2") ///
    label

*****
// Other objective measures
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .

mi xtset id wave

* Craft
mi estimate, cmdok eform post: mepoisson gds15 ///
	MCIdem_iadc cci_tot_sd cci_inftot_sd craftdre_sd ///
    faq_sd bmi_sd female white grade_sd ///
	i.married relsubj_simple ///
	age_base_sd years_since0_sd  ///
    || id:, irr iterate(200) intpoints(12) 
est sto mi_craft

* Rey
mi estimate, cmdok eform post: mepoisson gds15 ///
	MCIdem_iadc cci_tot_sd cci_inftot_sd reydecorab_sd ///
    faq_sd bmi_sd female white grade_sd  ///
	i.married relsubj_simple ///
	age_base_sd years_since0_sd ///
    || id:, irr iterate(200) intpoints(12) 
est sto mi_rey

* Trails
mi estimate, cmdok eform post: mepoisson gds15 ///
	MCIdem_iadc cci_tot_sd cci_inftot_sd trailbtime_sd faq_sd bmi_sd ///
    female white grade_sd ///
	i.married relsubj_simple ///
	age_base_sd years_since0_sd ///
    || id:, irr iterate(200) intpoints(12) 
est sto mi_trail

esttab mi_craft mi_rey mi_trail using ///
	"../../Output/Dta25 Output/Tables/raw/TA4_supplementary_objective.docx", ///
    replace ///
    eform ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    title("Table: Estimates From Multilevel Poisson Model for GDS-15 (IRRs)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    label

*****
// Fixed effects
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .

mi xtset id wave

mi estimate, cmdok eform post: xtpoisson gds15 ///
    MCIdem_iadc cci_tot_sd cci_inftot_sd mocatots_sd faq_sd ///
	bmi_sd years_since0_sd, ///
    fe irr iterate(200)
est sto m1_mi_fe

mi estimate, cmdok eform post: xtpoisson gds15 ///
    L_MCIdem_iadc L_cci_tot_sd L_cci_inftot_sd L_mocatots_sd faq_sd ///
	bmi_sd years_since0_sd, ///
    fe irr iterate(200)
est sto m2_mi_fe

esttab m1_mi_fe m2_mi_fe using ///
	"../../Output/Stage 2 Output/Tables/raw/T4_fixed_effects.docx", ///
    replace eform ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    title("Table: Estimates From Fixed Effects Multilevel Poisson for GDS-15 (IRRs)") ///
    mtitles("Model 1" "Model 2") ///
    label
	
**# Bookmark #3
********************************************************************************
********************************************************************************
****** Mediation
********************************************************************************
********************************************************************************	

********************************************************************************
// Contemporaenous mediation: hypothesized

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
qui drop if gds15 == .
qui mi passive: gen gds15_copy1 = gds15
qui mi passive: gen gds15_copy2 = gds15
qui mi passive: gen gds15_copy3 = gds15

mi xtset id wave

qui mi estimate, cmdok post dots: gsem ///
	/// No mediators ///
	(gds15 <- cci_tot_sd cci_inftot_sd ///
					mocatots_sd MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd faq_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple M1[id], poisson) ///
	/// Mastery added ///
	(gds15_copy1 <- cci_tot_sd cci_inftot_sd ///
					mocatots_sd MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd faq_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple M1[id] ///
					i.mastery1 i.mastery2, poisson) ///	
	/// Social connectedness added ///
	(gds15_copy2 <- cci_tot_sd cci_inftot_sd ///
					mocatots_sd MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd faq_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple M1[id] ///
					i.famfreq i.visitfreq, poisson) ///	
	/// Both added ///
	 (gds15_copy3 <- cci_tot_sd cci_inftot_sd ///
                    mocatots_sd MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple M1[id] ///
					i.mastery1 i.mastery2 ///
                    i.famfreq i.visitfreq ///
                    faq_sd , poisson) ///
	 , latent(M1) nocapslatent difficult iterate(400) 
* specified as two, they are almost identical because same outcome and similar covariates. supported by: https://www.stata.com/manuals/semexample42g.pdf
* does conflate within and between
	
estimates store both_models_re
esttab both_models_re both_models_re both_models_re both_models_re using ///
	"../../Output/Dta25 Output/Tables/raw/T5_focal_mediation.docx", replace ///
    equation(1 2 3 4) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
    eform b(3) se(3) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    label nobaselevels drop(_cons var(M1[id])) ///
    compress

* CCI
est restore both_models_re
mimrgns, dydx(cci_tot_sd) post predict(equation(gds15)) /// no mediators (1)
								predict(equation(gds15_copy1)) /// adds mastery (2)
								predict(equation(gds15_copy2)) /// adds social (3)
								predict(equation(gds15_copy3))
estimates save "../../Output/Dta25 Output/Statistics/mimrgns_CCI_mediation", replace
estimates use "../../Output/Dta25 Output/Statistics/mimrgns_CCI_mediation"
estimates replay
* mastery
mlincom 1 - 2, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[2._predict]) / _b[1._predict] * 100)
* social
mlincom 1 - 3, stat(est se p)
display %12.8f r(est)
nlcom (pct_mediated: (_b[1._predict] - _b[3._predict]) / _b[1._predict] * 100)
* both
mlincom 1 - 4, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[4._predict]) / _b[1._predict] * 100)

* Diagnosis
est restore both_models_re
mimrgns, dydx(MCIdem_iadc) post predict(equation(gds15)) /// no mediators (1)
								predict(equation(gds15_copy1)) /// adds mastery (2)
								predict(equation(gds15_copy2)) /// adds social (3)
								predict(equation(gds15_copy3))
estimates save "../../Output/Dta25 Output/Statistics/mimrgns_diagnosis_mediation", replace
estimates use "../../Output/Dta25 Output/Statistics/mimrgns_diagnosis_mediation"

* mastery
mlincom 1 - 2, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[2._predict]) / _b[1._predict] * 100)
* social
mlincom 1 - 3, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[3._predict]) / _b[1._predict] * 100)
* both
mlincom 1 - 4, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[4._predict]) / _b[1._predict] * 100)

********************************************************************************
// Contemporaenous mediation: not hypothesized (moca/faq)

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
qui drop if gds15 == .
qui mi passive: gen gds15_copy1 = gds15
qui mi passive: gen gds15_copy2 = gds15
qui mi passive: gen gds15_copy3 = gds15

mi xtset id wave

qui mi estimate, cmdok post dots: gsem ///
	/// No mediators ///
	(gds15 <- cci_tot_sd cci_inftot_sd ///
					MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple M1[id], poisson) ///
	/// MoCA added ///
	(gds15_copy1 <- cci_tot_sd cci_inftot_sd ///
					mocatots_sd MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple ///
					mocatots_sd M1[id], poisson) ///
	/// FAQ added ///
	(gds15_copy2 <- cci_tot_sd cci_inftot_sd ///
					MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple ///
					faq_sd M1[id], poisson) ///	
	/// Both added ///
	(gds15_copy3 <- cci_tot_sd cci_inftot_sd ///
					MCIdem_iadc ///
                    years_since0_sd age_base_sd female white grade_sd ///
					bmi_sd yrs_since_last i.married i.relsubj_simple ///
					mocatots_sd faq_sd  M1[id], poisson) ///	
	 , latent(M1) nocapslatent difficult iterate(400) 
* specified as two, they are almost identical because same outcome and similar covariates. supported by: https://www.stata.com/manuals/semexample42g.pdf
* does conflate within and between
	
estimates store both_models_re_moca
esttab both_models_re_moca both_models_re_moca ///
	both_models_re_moca both_models_re_moca using ///
	"../../Output/Dta25 Output/Tables/raw/TA6_alternative_mediation.docx", replace ///
    equation(1 2 3 4) ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
    eform b(3) se(3) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    label nobaselevels drop(_cons var(M1[id])) ///
    compress ///
	order(MCIdem_iadc cci_tot_sd cci_inftot_sd ///
          female white grade_sd ///
		  2.married 3.married 2.relsubj_simple 3.relsubj_simple 4.relsubj_simple ///
		  age_base_sd years_since0_sd yrs_since_last ///
          mocatots_sd faq_sd)
	
* CCI
est restore both_models_re_moca
mimrgns, dydx(cci_tot_sd) post predict(equation(gds15)) /// no mediators (1)
								predict(equation(gds15_copy1)) /// adds moca (2)
								predict(equation(gds15_copy2)) /// adds faq (3)
								predict(equation(gds15_copy3)) /// adds both
estimates save "../../Output/Dta25 Output/Statistics/mimrgns_CCI_alternative_mediation", replace
estimates use "../Output/Dta25 Output/Statistics/mimrgns_CCI_alternative_mediation"
estimates replay
* moca
mlincom 1 - 2, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[2._predict]) / _b[1._predict] * 100)
* faq
mlincom 1 - 3, stat(est se p)
display %12.8f r(est)
nlcom (pct_mediated: (_b[1._predict] - _b[3._predict]) / _b[1._predict] * 100)
* both
mlincom 1 - 4, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[4._predict]) / _b[1._predict] * 100)

* Diagnosis
est restore both_models_re_moca
mimrgns, dydx(MCIdem_iadc) post predict(equation(gds15)) /// no mediators (1)
								predict(equation(gds15_copy1)) /// adds mastery (2)
								predict(equation(gds15_copy2)) /// adds social (3)
								predict(equation(gds15_copy3))
estimates save "../../Output/Dta25 Output/Statistics/mimrgns_diagnosis_alternative_mediation", replace
estimates use "../../Output/Dta25 Output/Statistics/mimrgns_diagnosis_alternative_mediation"
* moca
mlincom 1 - 2, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[2._predict]) / _b[1._predict] * 100)
* faq
mlincom 1 - 3, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[3._predict]) / _b[1._predict] * 100)
* both
mlincom 1 - 4, stat(est se p)
nlcom (pct_mediated: (_b[1._predict] - _b[4._predict]) / _b[1._predict] * 100)

**# Bookmark #4
********************************************************************************
********************************************************************************
****** Interactions
********************************************************************************
********************************************************************************

**# Bookmark #4
********************************************************************************
********************************************************************************
****** Interactions
********************************************************************************
********************************************************************************

/* Descriptive combinations

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
mi xtset id wave

mi passive: gen cci_x_inf_bi = .
mi passive: replace cci_x_inf_bi = 0 if cci_tot_bi == 0 & cci_inftot_bi == 0
mi passive: replace cci_x_inf_bi = 1 if cci_tot_bi == 1 & cci_inftot_bi == 0
mi passive: replace cci_x_inf_bi = 2 if cci_tot_bi == 0 & cci_inftot_bi == 1
mi passive: replace cci_x_inf_bi = 3 if cci_tot_bi == 1 & cci_inftot_bi == 1

mi passive: gen cci_x_diag_bi = .
mi passive: replace cci_x_diag_bi = 0 if cci_tot_bi == 0 & MCIdem_iadc == 0
mi passive: replace cci_x_diag_bi = 1 if cci_tot_bi == 1 & MCIdem_iadc == 0
mi passive: replace cci_x_diag_bi = 2 if cci_tot_bi == 0 & MCIdem_iadc == 1
mi passive: replace cci_x_diag_bi = 3 if cci_tot_bi == 1 & MCIdem_iadc == 1

mi passive: gen diag_x_inf_bi = .
mi passive: replace diag_x_inf_bi = 0 if MCIdem_iadc == 0 & cci_inftot_bi == 0
mi passive: replace diag_x_inf_bi = 1 if MCIdem_iadc == 1 & cci_inftot_bi == 0
mi passive: replace diag_x_inf_bi = 2 if MCIdem_iadc == 0 & cci_inftot_bi == 1
mi passive: replace diag_x_inf_bi = 3 if MCIdem_iadc == 1 & cci_inftot_bi == 1

mi estimate: mean gds15, over(cci_x_inf_bi)
mi estimate: mean gds15, over(cci_x_diag_bi)
mi estimate: mean gds15, over(diag_x_inf_bi)

mi estimate, eform cmdok post: mepoisson gds15 ///
	i.cci_x_inf_bi MCIdem_iadc mocatots_sd ///
    years_since0_sd age_base_sd female white grade_sd faq_sd ///
	bmi_sd i.married ///
    || id:, irr iterate(400) intpoints(12) 
mi estimate, eform cmdok post: mepoisson gds15 ///
	i.cci_x_diag_bi cci_inftot_sd mocatots_sd ///
    years_since0_sd age_base_sd female white grade_sd faq_sd ///
	bmi_sd i.married ///
    || id:, irr iterate(400) intpoints(12) 
mi estimate, eform cmdok post: mepoisson gds15 ///
	i.diag_x_inf_bi cci_tot_sd mocatots_sd ///
    years_since0_sd age_base_sd female white grade_sd faq_sd ///
	bmi_sd i.married ///
    || id:, irr iterate(400) intpoints(12) 
*/

**********************
* Diagnosis x CCI

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
mi xtset id wave

mi estimate, eform cmdok post: mepoisson gds15 ///
	i.MCIdem_iadc##c.cci_tot_sd cci_inftot_sd mocatots_sd faq_sd ///
	bmi_sd female white grade_sd  ///
	i.married  i.relsubj_simple ///
	age_base_sd years_since0_sd    ///
    || id:, irr iterate(400) intpoints(12) 
est sto m_int_diag

	
* second difference table
est restore m_int_diag
gen insample = !missing(gds15, MCIdem_iadc, cci_tot_sd, cci_inftot_sd, ///
               mocatots_sd, faq_sd, female, white, grade_sd, ///
               married, relsubj_simple, age_base_sd, years_since0_sd, bmi_sd)
mimrgns, dydx(MCIdem_iadc) at(cci_tot_sd=(-1.2 3.5)) predict(mu) subpop(insample) post
matrix b = e(b_mi)
matrix V = e(V_mi)
matrix df = e(df_mi)
local me1: di %5.3f b[1,1]
local me2: di %5.3f b[1,2]
local se1 = sqrt(V[1,1])
local se2 = sqrt(V[2,2])
local se1_fmt: di %5.3f `se1'
local se2_fmt: di %5.3f `se2'
local t1 = b[1,1] / `se1'
local t2 = b[1,2] / `se2'
local df1 = df[1,1]
local df2 = df[1,2]
local p_me1 = 2*ttail(`df1', abs(`t1'))
local p_me2 = 2*ttail(`df2', abs(`t2'))
local sig1 = cond(`p_me1'<0.001, "***", cond(`p_me1'<0.01, "**", cond(`p_me1'<0.05, "*", "")))
local sig2 = cond(`p_me2'<0.001, "***", cond(`p_me2'<0.01, "**", cond(`p_me2'<0.05, "*", "")))
mlincom 2 - 1, stat(est p se)
local est3: di %5.3f r(est)
local se3: di %5.3f r(se)
local p3: di %5.3f r(p)
local sig3 = cond(r(p)<0.001, "***", cond(r(p)<0.01, "**", cond(r(p)<0.05, "*", "")))
file open myfile using "../../Output/Dta25 Output/Tables/raw/CCI_diagnosis_interaction.docx", write replace
file write myfile "{\rtf1\ansi\deff0" _n
file write myfile "{\b Table 6: Diagnosis Marginal Effect on GDS-15 by CCI}\par\par" _n
file write myfile "\trowd\trgaph100" _n
file write myfile "\cellx4000\cellx8000" _n
file write myfile "{\b Effect}\intbl\cell" _n
file write myfile "{\b Estimate}\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Diagnosis effect at CCI = -1.2 SD\intbl\cell" _n
file write myfile "`me1'`sig1'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se1_fmt')\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Diagnosis effect at CCI = 3.5 SD\intbl\cell" _n
file write myfile "`me2'`sig2'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se2_fmt')\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Difference (interaction)\intbl\cell" _n
file write myfile "`est3'`sig3'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se3')\intbl\cell\row" _n
file write myfile "{\i Note:} * p<0.05, ** p<0.01, *** p<0.001\par" _n
file write myfile "}" _n
file close myfile

**
* Get predictions in empirical range

* CCI at L_MCIdem_iadc==0 (normal)
mi xeq: sum cci_tot_sd if MCIdem_iadc == 0, d
* min cci across all 20: -1.212761   
* smallest max cci across 20: 4.147753

* CCI at L_MCIdem_iadc==1 (diagnosed)
mi xeq: sum cci_tot_sd if MCIdem_iadc == 1, d
* min cci across all 20: -1.212761 
* smallest max cci across 20: 3.529232

* Bounds: -1.2 to 3.5

**
* Plot

est restore m_int_diag
mimrgns, dydx(MCIdem_iadc) at(cci_tot_sd=(-1.2 3.5)) subpop(insample) predict(mu) cmdmargins
marginsplot, ///
    recast(scatter) ///
    recastci(rcap) ///
    plot1opts(mcolor(black) msize(large)) ///
    ci1opts(color(black) lwidth(medium)) ///
    ytitle("Average Marginal Effect of MCI/Dementia Diagnosis", size(medium)) ///
    xtitle("CCI Score (in SD)", size(medium)) ///
    title("Effect of Diagnosis on Predicted Number of Depressive" "Symptoms (GDS-15) by Self-Assessed Decline (CCI)", ///
			size(medlarge) color(black)) ///
    xlabel(-1.2 `"-1.2 (Low/Normal)"' 3.5 `"3.5 (High/Worried)"', labsize(small)) ///
    xscale(range(-3 5)) ///
    ylabel(-3(1)1, labsize(small) angle(horizontal)) ///
    yline(0, lpattern(dash) lcolor(gray)) ///
    legend(off) ///
    graphregion(fcolor(white) lcolor(white) margin(medium)) ///
    plotregion(fcolor(white) lcolor(black)) ///
    xsize(5) ysize(4)
graph export "../../Output/Stage 2 Output/Figures/F5_CCI_diagnosis_interaction.tif", replace
	

**********************	
* Informant x CCI

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
mi xtset id wave


mi estimate, eform cmdok post: mepoisson gds15 ///
	i.MCIdem_iadc c.cci_tot_sd##c.cci_inftot_sd c.mocatots_sd ///
    years_since0_sd age_base_sd female white grade_sd faq_sd ///
	bmi_sd i.married i.relsubj_simple ///
    || id:, irr iterate(400) intpoints(12)
est sto m_int_inf_rel	


* second difference table
est restore m_int_inf_rel	
mimrgns, dydx(cci_tot_sd) at(cci_inftot_sd=(-0.8 2)) predict(mu) post
matrix b = e(b_mi)
matrix V = e(V_mi)
matrix df = e(df_mi)
local me1: di %5.3f b[1,1]
local me2: di %5.3f b[1,2]
local se1 = sqrt(V[1,1])
local se2 = sqrt(V[2,2])
local se1_fmt: di %5.3f `se1'
local se2_fmt: di %5.3f `se2'
local t1 = b[1,1] / `se1'
local t2 = b[1,2] / `se2'
local df1 = df[1,1]
local df2 = df[1,2]
local p_me1 = 2*ttail(`df1', abs(`t1'))
local p_me2 = 2*ttail(`df2', abs(`t2'))
local sig1 = cond(`p_me1'<0.001, "***", cond(`p_me1'<0.01, "**", cond(`p_me1'<0.05, "*", "")))
local sig2 = cond(`p_me2'<0.001, "***", cond(`p_me2'<0.01, "**", cond(`p_me2'<0.05, "*", "")))
mlincom 2 - 1, stat(est se p)
local est3: di %5.3f r(est)
local se3: di %5.3f r(se)
local p3: di %5.3f r(p)
local sig3 = cond(r(p)<0.001, "***", cond(r(p)<0.01, "**", cond(r(p)<0.05, "*", "")))
file open myfile using "../../Output/Dta25 Output/Tables/raw/CCI_informant_interaction.docx", write replace
file write myfile "{\rtf1\ansi\deff0" _n
file write myfile "{\b Table: CCI Marginal Effect on GDS-15 by CCI Informant}\par\par" _n
file write myfile "\trowd\trgaph100" _n
file write myfile "\cellx4000\cellx8000" _n
file write myfile "{\b Effect}\intbl\cell" _n
file write myfile "{\b Estimate}\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "CCI effect at CCI Informant = -0.8 SD\intbl\cell" _n
file write myfile "`me1'`sig1'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se1_fmt')\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "CCI effect at CCI Informant = 2 SD\intbl\cell" _n
file write myfile "`me2'`sig2'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se2_fmt')\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Difference (interaction)\intbl\cell" _n
file write myfile "`est3'`sig3'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se3')\intbl\cell\row" _n
file write myfile "{\i Note:} * p<0.05, ** p<0.01, *** p<0.001\par" _n
file write myfile "}" _n
file close myfile

**

mi xeq: sum cci_tot_sd , d
mi xeq: sum cci_inftot_sd, d
* lowest 10%/highest 90% cci across all 20:  -1.075312 / 1.604945
* rounded: -1 to 1.6

* CCI Informant at 90% CCI
mi xeq: sum cci_inftot_sd if cci_tot_sd > 1.6
* min/max across all 20:  -.8124925   3.132262

* CCI at low values of CCI Informant (not worried)
mi xeq: sum cci_inftot_sd if cci_tot_sd < -1
* min across all 20/ smallest max in 20: -.8124925 1.998145
* rounded: -0.8 to 2
est restore m_int_inf_rel	

mimrgns, at(cci_tot_sd=(-1(0.5)1.6) cci_inftot_sd=(-0.8 2)) predict(mu) cmdmargins
marginsplot, ///
    recast(line) ///
    recastci(rarea) ///
    plot1opts(lcolor(gs4) mcolor(gs4) lwidth(medium) lpattern(solid)) ///
    ci1opts(color(gs4%30)) ///
    plot2opts(lcolor(gs4) mcolor(gs4) lwidth(medium) lpattern(dash)) ///
    ci2opts(color(gs10%30)) ///
    xtitle("CCI Score (in SD)", size(medium)) ///
    ytitle("Predicted GDS-15 Score", size(medium)) ///
    title("Predicted Number of Depressive Symptoms (GDS-15) by" "Self-Assessed (CCI) and Informant-Perceived Decline (Informant CCI)", size(medlarge) color(black)) ///
    xlabel(, labsize(small)) ///
    ylabel(, labsize(small) angle(horizontal)) ///
    legend(order(3 "-0.8 (Low/Normal)" 4 "2 (High/Worried)") ///
            size(small) title("Informant CCI Score (in SD)", size(small))) ///
    graphregion(fcolor(white) lcolor(white) margin(medium)) ///
    plotregion(fcolor(white) lcolor(black)) ///
    xsize(6) ysize(4)         
graph export "../../Output/Stage 2 Output/Figures/F7_CCI_informant_interaction.tif", replace

**********************	
* Diagnosis x Informant

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
mi xtset id wave

mi estimate, eform cmdok post: mepoisson gds15 ///
	c.cci_inftot_sd##i.MCIdem_iadc c.cci_tot_sd c.mocatots_sd ///
    years_since0_sd age_base_sd female white grade_sd faq_sd ///
	bmi_sd i.married i.relsubj_simple ///
    || id:, irr iterate(400) intpoints(12)
est sto m_int_inf_diag_rel	


* second difference table
est restore m_int_inf_diag_rel
mimrgns, dydx(MCIdem_iadc) at(cci_inftot_sd=(-.8 2.8)) predict(mu) post
matrix b = e(b_mi)
matrix V = e(V_mi)
matrix df = e(df_mi)
local me1: di %5.3f b[1,1]
local me2: di %5.3f b[1,2]
local se1 = sqrt(V[1,1])
local se2 = sqrt(V[2,2])
local se1_fmt: di %5.3f `se1'
local se2_fmt: di %5.3f `se2'
local t1 = b[1,1] / `se1'
local t2 = b[1,2] / `se2'
local df1 = df[1,1]
local df2 = df[1,2]
local p_me1 = 2*ttail(`df1', abs(`t1'))
local p_me2 = 2*ttail(`df2', abs(`t2'))
local sig1 = cond(`p_me1'<0.001, "***", cond(`p_me1'<0.01, "**", cond(`p_me1'<0.05, "*", "")))
local sig2 = cond(`p_me2'<0.001, "***", cond(`p_me2'<0.01, "**", cond(`p_me2'<0.05, "*", "")))
mlincom 2 - 1, stat(est se p)
local est3: di %5.3f r(est)
local se3: di %5.3f r(se)
local p3: di %5.3f r(p)
local sig3 = cond(r(p)<0.001, "***", cond(r(p)<0.01, "**", cond(r(p)<0.05, "*", "")))
file open myfile using "../../Output/Dta25 Output/Tables/raw/diagnosis_informant_interaction.docx", write replace
file write myfile "{\rtf1\ansi\deff0" _n
file write myfile "{\b Table: Diagnosis Marginal Effect on GDS-15 by CCI Informant}\par\par" _n
file write myfile "\trowd\trgaph100" _n
file write myfile "\cellx4000\cellx8000" _n
file write myfile "{\b Effect}\intbl\cell" _n
file write myfile "{\b Estimate}\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Diagnosis effect at CCI Informant = -0.8 SD\intbl\cell" _n
file write myfile "`me1'`sig1'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se1_fmt')\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Diagnosis effect at CCI Informant = 2.8 SD\intbl\cell" _n
file write myfile "`me2'`sig2'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se2_fmt')\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "Difference (interaction)\intbl\cell" _n
file write myfile "`est3'`sig3'\intbl\cell\row" _n
file write myfile "\trowd\trgaph100\cellx4000\cellx8000" _n
file write myfile "\intbl\cell" _n
file write myfile "(`se3')\intbl\cell\row" _n
file write myfile "{\i Note:} * p<0.05, ** p<0.01, *** p<0.001\par" _n
file write myfile "}" _n
file close myfile

**
* Get predictions in empirical range

* CCI at L_MCIdem_iadc==0 (normal)
mi xeq: sum cci_inftot_sd if MCIdem_iadc == 0, d
* min cci across all 20: -.8124925 
* smallest max cci across 20: 2.836405

* CCI at L_MCIdem_iadc==1 (diagnosed)
mi xeq: sum cci_inftot_sd if MCIdem_iadc == 1, d
* min cci across all 20: -.8124925
* smallest max cci across 20: 3.132262

* Bounds: -0.8 to 2.8

**
* Plots
/*est restore m_int_inf_diag_rel	
mimrgns, at(cci_inftot_sd=(-0.8(0.5)2.8) MCIdem_iadc=(0 1)) predict(mu) cmdmargins
marginsplot, ///
    recast(line) ///
	recastci(rarea) ///
    plot1opts(lcolor("49 54 149") mcolor("49 54 149") lwidth(medium)) ///
    ci1opts(color("49 54 149%30")) ///
    plot2opts(lcolor("215 48 39") mcolor("215 48 39") lwidth(medium)) ///
    ci2opts(color("215 48 39%30")) ///
    xtitle("CCI Informant in SD (high=worried)") ///
    ytitle("Predicted GDS15 Score") ///
    title("Predicted GDS15 by CCI Informant and Diagnosis") ///
	legend(order(3 "No MCI/Dementia" 4 "MCI/Dementia") ///
           size(small) title("MCI/dementia diagnosis", size(small)))       
graph export "../Figures/mi_int-diag-inf-twoci_2025.jpg", replace
*/

est restore m_int_inf_diag_rel
mimrgns, dydx(MCIdem_iadc) at(cci_inftot_sd=(-0.8 2.8)) predict(mu) cmdmargins
marginsplot, ///
    recast(scatter) ///
    recastci(rcap) ///
    plot1opts(mcolor(black) msize(large)) ///
    ci1opts(color(black) lwidth(medium)) ///
    ytitle("Average Marginal Effect of MCI/Dementia Diagnosis", size(medium)) ///
    xtitle("Informant CCI (in SD)", size(medium)) ///
    title("Effect of Diagnosis on Predicted Number of Depressive" "Symptoms by Informant-Perceived Decline (Informant CCI)", ///
			size(medlarge) color(black)) ///
    xlabel(-0.8 `"-0.8 (Low/Normal)"' 2.8 `"2.8 (High/Worried)"', labsize(small)) ///
    xscale(range(-3 5)) ///
    ylabel(-1(0.5)1, labsize(small) angle(horizontal)) ///
    yline(0, lpattern(dash) lcolor(gray)) ///
    legend(off) ///
    graphregion(fcolor(white) lcolor(white) margin(medium)) ///
    plotregion(fcolor(white) lcolor(black)) ///
    xsize(5) ysize(4)
graph export "../../Output/Dta25 Output/Figures/F6_diagnosis_informant_interaction.tif", replace

**********************
* Focal interactions table
esttab m_int_diag m_int_inf_diag_rel m_int_inf_rel ///
	using "../../Output/Dta25 Output/Tables/raw/TA10_interaction_regression.docx", ///
    replace ///
    eform ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    title("Table: Estimates From Multilevel Negative Binomial for GDS-15 (IRRs)") ///
    mtitles("Model 1" "Model 2" "Model 3") ///
    label

********
// Education interaction (not significant)

use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear

drop if gds15 == .
mi xtset id wave


mi estimate, cmdok eform post: mepoisson gds15 ///
	c.cci_tot_sd##c.grade_sd MCIdem_iadc cci_inftot_sd mocatots_sd ///
    years_since0_sd age_base_sd female white faq_sd ///
	bmi_sd i.married ///
    || id:, irr iterate(200) intpoints(12) 
est sto edint_cci

mi estimate, cmdok eform post dots: mepoisson gds15 ///
	c.cci_tot_sd i.MCIdem_iadc##c.grade_sd cci_inftot_sd mocatots_sd ///
    years_since0_sd age_base_sd female white faq_sd ///
	bmi_sd i.married ///
    || id:, irr iterate(200) intpoints(12) 
est sto edint_diag
	
est restore edint_cci
mimrgns, dydx(cci_tot_sd) at(grade_sd=(-1.5 1.5)) predict(mu) post
mlincom 2 - 1, stat(est p se)

est restore edint_diag
mimrgns, dydx(MCIdem_iadc) at(grade_sd=(-1.5 1.5)) predict(mu) post
mlincom 2 - 1, stat(est p se)


	
**# Bookmark #5
********************************************************************************
********************************************************************************
****** Reverse Causality
********************************************************************************
********************************************************************************

******************
// CCI
*******************
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear

drop if gds15 == .
drop if cci_tot == .
*keep if yrs_since_last >= 0.5 & yrs_since_last <= 1.5

// Model
mi estimate, cmdok post: gsem (gds15 <- L_MCIdem_iadc L_cci_tot ///
			 L_cci_inftot L_mocatots_sd ///
			faq_sd bmi_sd ///
			female white grade_sd i.married i.relsubj_simple ///
			age_base_sd years_since0_sd ///
			L_gds15 M1[id], family(poisson)) ///
    (cci_tot <- L_gds15 ///
			faq_sd bmi_sd ///
			female white grade_sd ///
			i.married i.relsubj_simple ///
			age_base_sd years_since0_sd ///
			L_cci_tot M2[id], family(poisson)), ///
			cov(M1[id]*M2[id]) ///
    latent(M1 M2) nocapslatent ///
    difficult iterate(400) intpoints(12) technique(nr)

est sto clpm_cci
mibeta L_gds15 L_cci_tot
display sqrt(.2205448) //.46962198
estimates restore clpm_cci

// Extract Coefs (exponentiated for IRRs)
* Cross-lagged: CCI → Depression
scalar c1 = exp(_b[gds15:L_cci_tot])
scalar se1 = _se[gds15:L_cci_tot]
scalar z1 = _b[gds15:L_cci_tot] / se1
scalar p1 = 2 * normal(-abs(z1))

* Cross-lagged: Depression → CCI
scalar c2 = exp(_b[cci_tot:L_gds15])
scalar se2 = _se[cci_tot:L_gds15]
scalar z2 = _b[cci_tot:L_gds15] / se2
scalar p2 = 2 * normal(-abs(z2))

* Autoregressive: Depression → Depression
scalar c3 = exp(_b[gds15:L_gds15])
scalar se3 = _se[gds15:L_gds15]
scalar z3 = _b[gds15:L_gds15] / se3
scalar p3 = 2 * normal(-abs(z3))

* Autoregressive: CCI → CCI
scalar c4 = exp(_b[cci_tot:L_cci_tot])
scalar se4 = _se[cci_tot:L_cci_tot]
scalar z4 = _b[cci_tot:L_cci_tot] / se4
scalar p4 = 2 * normal(-abs(z4))

// Path diagram with pooled estimates
clear
set obs 4
gen str30 path = ""
gen float coef = .
gen float se = .
gen float pvalue = .
gen float ci_lower = .
gen float ci_upper = .

* Fill in the extracted values
replace path = "Cog_to_Dep" in 1
replace coef = c1 in 1
replace se = se1 in 1
replace pvalue = p1 in 1
replace ci_lower = exp(_b[gds15:L_cci_tot] - 1.96*se1) in 1
replace ci_upper = exp(_b[gds15:L_cci_tot] + 1.96*se1) in 1

replace path = "Dep_to_Cog" in 2
replace coef = c2 in 2
replace se = se2 in 2
replace pvalue = p2 in 2
replace ci_lower = exp(_b[cci_tot:L_gds15] - 1.96*se2) in 2
replace ci_upper = exp(_b[cci_tot:L_gds15] + 1.96*se2) in 2

replace path = "Dep_to_Dep" in 3
replace coef = c3 in 3
replace se = se3 in 3
replace pvalue = p3 in 3
replace ci_lower = exp(_b[gds15:L_gds15] - 1.96*se3) in 3
replace ci_upper = exp(_b[gds15:L_gds15] + 1.96*se3) in 3

replace path = "Cog_to_Cog" in 4
replace coef = c4 in 4
replace se = se4 in 4
replace pvalue = p4 in 4
replace ci_lower = exp(_b[cci_tot:L_cci_tot] - 1.96*se4) in 4
replace ci_upper = exp(_b[cci_tot:L_cci_tot] + 1.96*se4) in 4

* Add significance stars
gen str5 sig = ""
replace sig = "***" if pvalue < 0.001
replace sig = "**" if pvalue < 0.01 & pvalue >= 0.001
replace sig = "*" if pvalue < 0.05 & pvalue >= 0.01

* Create label with coefficient and stars
gen str20 label = string(coef, "%5.3f") + sig

list

* Export to CSV
export delimited using "../../Output/Dta25 Output/Statistics/clpm_cci_coefs.csv", replace

* Get fully standardized cross-lagged paths
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
drop if cci_tot == .
est restore clpm_cci
preserve
mi extract 0, clear
summarize gds15
local sd_gds15   = r(sd)
local mean_gds15 = r(mean)
summarize cci_tot
local sd_cci_tot   = r(sd)
local mean_cci_tot = r(mean)
summarize L_gds15
local sd_Lgds15 = r(sd)
summarize L_cci_tot
local sd_Lcci_tot = r(sd)
di "SD gds15:     " `sd_gds15'
di "SD cci_tot:   " `sd_cci_tot'
di "SD L_gds15:   " `sd_Lgds15'
di "SD L_cci_tot: " `sd_Lcci_tot'

restore
est restore clpm_cci
local b_cci_to_gds = _b[gds15:L_cci_tot]
local b_gds_to_cci = _b[cci_tot:L_gds15]
local se_cci_to_gds = _se[gds15:L_cci_tot]
local se_gds_to_cci = _se[cci_tot:L_gds15]
di "Raw b  CCI->GDS: " `b_cci_to_gds'
di "Raw b  GDS->CCI: " `b_gds_to_cci'

local b_std_cci_to_gds = `b_cci_to_gds' * `sd_Lcci_tot' / `sd_gds15'
local b_std_gds_to_cci = `b_gds_to_cci' * `sd_Lgds15'  / `sd_cci_tot'
local se_std_cci_to_gds = `se_cci_to_gds' * `sd_Lcci_tot' / `sd_gds15'
local se_std_gds_to_cci = `se_gds_to_cci' * `sd_Lgds15'  / `sd_cci_tot'
di `b_std_cci_to_gds'
di `b_std_gds_to_cci'

/*
di `b_std_cci_to_gds'
.0420091

. di `b_std_gds_to_cci'
.0007861

*/

******************
// CCI Informant
*******************
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
drop if cci_inftot == .

* Model with mi estimate
mi estimate, cmdok post dots: gsem (gds15 <- L_MCIdem_iadc L_cci_tot ///
		L_cci_inftot L_mocatots_sd faq_sd ///
		bmi_sd female white grade_sd ///
		i.married i.relsubj_simple ///
		age_base_sd years_since0_sd ///
		L_gds15 M1[id], family(poisson)) ///
    (cci_inftot <- L_gds15 ///
		faq_sd bmi_sd ///
		female white grade_sd ///
		i.married i.relsubj_simple ///
		age_base_sd years_since0_sd ///
		L_cci_inftot M2[id], family(poisson)), ///
	cov(M1[id]*M2[id]) ///
    latent(M1 M2) nocapslatent ///
    difficult iterate(200) intpoints(12) technique(nr)

est sto clpm_inf

mibeta L_gds15 L_cci_inftot
display sqrt(.0787856) // .28068773

estimates restore clpm_inf

// Extract Coefs (exponentiated for IRRs)
* Cross-lagged: CCI → Depression
scalar c1 = exp(_b[gds15:L_cci_inftot])
scalar se1 = _se[gds15:L_cci_inftot]
scalar z1 = _b[gds15:L_cci_inftot] / se1
scalar p1 = 2 * normal(-abs(z1))

* Cross-lagged: Depression → CCI
scalar c2 = exp(_b[cci_inftot:L_gds15])
scalar se2 = _se[cci_inftot:L_gds15]
scalar z2 = _b[cci_inftot:L_gds15] / se2
scalar p2 = 2 * normal(-abs(z2))

* Autoregressive: Depression → Depression
scalar c3 = exp(_b[gds15:L_gds15])
scalar se3 = _se[gds15:L_gds15]
scalar z3 = _b[gds15:L_gds15] / se3
scalar p3 = 2 * normal(-abs(z3))

* Autoregressive: CCI → CCI
scalar c4 = exp(_b[cci_inftot:L_cci_inftot])
scalar se4 = _se[cci_inftot:L_cci_inftot]
scalar z4 = _b[cci_inftot:L_cci_inftot] / se4
scalar p4 = 2 * normal(-abs(z4))

// Path diagram with pooled estimates
clear
set obs 4
gen str30 path = ""
gen float coef = .
gen float se = .
gen float pvalue = .
gen float ci_lower = .
gen float ci_upper = .

* Fill in the extracted values
replace path = "Cog_to_Dep" in 1
replace coef = c1 in 1
replace se = se1 in 1
replace pvalue = p1 in 1
replace ci_lower = exp(_b[gds15:L_cci_inftot] - 1.96*se1) in 1
replace ci_upper = exp(_b[gds15:L_cci_inftot] + 1.96*se1) in 1

replace path = "Dep_to_Cog" in 2
replace coef = c2 in 2
replace se = se2 in 2
replace pvalue = p2 in 2
replace ci_lower = exp(_b[cci_inftot:L_gds15] - 1.96*se2) in 2
replace ci_upper = exp(_b[cci_inftot:L_gds15] + 1.96*se2) in 2

replace path = "Dep_to_Dep" in 3
replace coef = c3 in 3
replace se = se3 in 3
replace pvalue = p3 in 3
replace ci_lower = exp(_b[gds15:L_gds15] - 1.96*se3) in 3
replace ci_upper = exp(_b[gds15:L_gds15] + 1.96*se3) in 3

replace path = "Cog_to_Cog" in 4
replace coef = c4 in 4
replace se = se4 in 4
replace pvalue = p4 in 4
replace ci_lower = exp(_b[cci_inftot:L_cci_inftot] - 1.96*se4) in 4
replace ci_upper = exp(_b[cci_inftot:L_cci_inftot] + 1.96*se4) in 4

* Add significance stars
gen str5 sig = ""
replace sig = "***" if pvalue < 0.001
replace sig = "**" if pvalue < 0.01 & pvalue >= 0.001
replace sig = "*" if pvalue < 0.05 & pvalue >= 0.01

* Create label with coefficient and stars
gen str20 label = string(coef, "%5.3f") + sig

list

* Export to CSV
export delimited using "../../Output/Stage 2 Output/Statistics/clpm_inf_coefs.csv", replace

			
******************
// Diagnosis
*******************
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
drop if MCIdem_iadc == .

* --- Poisson piece alone ---
mi estimate, cmdok post dots: gsem (gds15 <- L_MCIdem_iadc L_cci_tot L_cci_inftot L_mocatots_sd faq_sd ///
    bmi_sd female white grade_sd i.married i.relsubj_simple ///
    age_base_sd years_since0_sd L_gds15 M1[id], family(poisson)), ///
    latent(M1) nocapslatent intpoints(7)

estimates store m1only

* --- Bernoulli piece alone ---
mi estimate, cmdok post dots: gsem (MCIdem_iadc <- L_gds15 faq_sd bmi_sd female white grade_sd ///
    i.married i.relsubj_simple age_base_sd years_since0_sd ///
    L_MCIdem_iadc M2[id], family(bernoulli)), ///
    latent(M2) nocapslatent intpoints(7)  technique(bhhh 10 nr 100) iterate(200) difficult

* Model with mi estimate
mi estimate, cmdok post dots: gsem (gds15 <- L_MCIdem_iadc L_cci_tot ///
		L_cci_inftot L_mocatots_sd faq_sd ///
		bmi_sd female white grade_sd ///
		i.married i.relsubj_simple ///
		age_base_sd years_since0_sd ///
		L_gds15 M1[id], family(poisson)) ///
    (MCIdem_iadc <- L_gds15 ///
		faq_sd bmi_sd ///
		female white grade_sd ///
		 i.married i.relsubj_simple ///
		age_base_sd years_since0_sd ///
		L_MCIdem_iadc M2[id], family(bernoulli)), ///
    cov(M1[id]*M2[id]) ///
    latent(M1 M2) nocapslatent ///
    difficult iterate(500) intpoints(12) technique(bhhh 10 nr 100)
est sto clpm_diag

/*
matrix b0 = e(b)
preserve
keep if yrs_since_last >= 0.5 & yrs_since_last <= 1.5
mi estimate, cmdok post dots: gsem (gds15 <- L_cci_tot ///
		L_cci_inftot L_mocatots_sd L_MCIdem_iadc ///
		years_since0_sd age_base_sd female white grade_sd faq_sd ///
		/// bmi_sd ///
		i.married ///
		L_gds15 M1[id], family(poisson)) ///
    (MCIdem_iadc <- L_gds15 ///
		years_since0_sd age_base_sd female white grade_sd faq_sd ///
		/// bmi_sd ///
		i.married i.apoe ///
		L_MCIdem_iadc M2[id], family(bernoulli)), ///
    latent(M1 M2) nocapslatent ///
    from(b0, skip) difficult iterate(200) intpoints(12) technique(nr)
restore
*/

mibeta L_gds15 L_MCIdem_iadc
display sqrt(.0575179) // .2398289

estimates restore clpm_diag

// Extract Coefs (exponentiated for IRRs/ORs)
* Cross-lagged: Diagnosis → Depression
scalar c1 = exp(_b[gds15:L_MCIdem_iadc])
scalar se1 = _se[gds15:L_MCIdem_iadc]
scalar z1 = _b[gds15:L_MCIdem_iadc] / se1
scalar p1 = 2 * normal(-abs(z1))

* Cross-lagged: Depression → Diagnosis
scalar c2 = exp(_b[MCIdem_iadc:L_gds15])
scalar se2 = _se[MCIdem_iadc:L_gds15]
scalar z2 = _b[MCIdem_iadc:L_gds15] / se2
scalar p2 = 2 * normal(-abs(z2))

* Autoregressive: Depression → Depression
scalar c3 = exp(_b[gds15:L_gds15])
scalar se3 = _se[gds15:L_gds15]
scalar z3 = _b[gds15:L_gds15] / se3
scalar p3 = 2 * normal(-abs(z3))

* Autoregressive: Diagnosis → Diagnosis
scalar c4 = exp(_b[MCIdem_iadc:L_MCIdem_iadc])
scalar se4 = _se[MCIdem_iadc:L_MCIdem_iadc]
scalar z4 = _b[MCIdem_iadc:L_MCIdem_iadc] / se4
scalar p4 = 2 * normal(-abs(z4))

// Path diagram with pooled estimates
clear
set obs 4
gen str30 path = ""
gen float coef = .
gen float se = .
gen float pvalue = .
gen float ci_lower = .
gen float ci_upper = .

* Fill in the extracted values
replace path = "Cog_to_Dep" in 1
replace coef = c1 in 1
replace se = se1 in 1
replace pvalue = p1 in 1
replace ci_lower = exp(_b[gds15:L_MCIdem_iadc] - 1.96*se1) in 1
replace ci_upper = exp(_b[gds15:L_MCIdem_iadc] + 1.96*se1) in 1

replace path = "Dep_to_Cog" in 2
replace coef = c2 in 2
replace se = se2 in 2
replace pvalue = p2 in 2
replace ci_lower = exp(_b[MCIdem_iadc:L_gds15] - 1.96*se2) in 2
replace ci_upper = exp(_b[MCIdem_iadc:L_gds15] + 1.96*se2) in 2

replace path = "Dep_to_Dep" in 3
replace coef = c3 in 3
replace se = se3 in 3
replace pvalue = p3 in 3
replace ci_lower = exp(_b[gds15:L_gds15] - 1.96*se3) in 3
replace ci_upper = exp(_b[gds15:L_gds15] + 1.96*se3) in 3

replace path = "Cog_to_Cog" in 4
replace coef = c4 in 4
replace se = se4 in 4
replace pvalue = p4 in 4
replace ci_lower = exp(_b[MCIdem_iadc:L_MCIdem_iadc] - 1.96*se4) in 4
replace ci_upper = exp(_b[MCIdem_iadc:L_MCIdem_iadc] + 1.96*se4) in 4

* Add significance stars
gen str5 sig = ""
replace sig = "***" if pvalue < 0.001
replace sig = "**" if pvalue < 0.01 & pvalue >= 0.001
replace sig = "*" if pvalue < 0.05 & pvalue >= 0.01

* Create label with coefficient and stars
gen str20 label = string(coef, "%5.3f") + sig

list

* Export to CSV
export delimited using "../../Output/Dta25 Output/Statistics/clpm_diagnosis_coefs.csv", replace			
		

******************
// MoCA
*******************
use "../Dta25 Data/mi_iadc_dta25_recoded.dta", clear
drop if gds15 == .
drop if mocatots_sd == .

* Model with mi estimate
mi estimate, cmdok post dots: gsem (gds15 <- L_MCIdem_iadc L_cci_tot ///
		L_cci_inftot L_mocatots_sd faq_sd ///
		bmi_sd female white grade_sd ///
		i.married i.relsubj_simple ///
		age_base_sd years_since0_sd ///
		L_gds15 M1[id], family(poisson)) ///
    (mocatots_sd <- L_gds15 faq_sd ///
		bmi_sd female white grade_sd ///
		i.married i.relsubj_simple ///
		age_base_sd years_since0_sd ///
		L_mocatots_sd M2[id], family(gaussian)), ///
    cov(M1[id]*M2[id]) ///
    latent(M1 M2) nocapslatent ///
    difficult iterate(500) intpoints(10) technique(nr 100 bhhh 10)

est sto clpm_moca

mibeta L_gds15 L_mocatots_sd
display sqrt(.0772001) // .27784906

estimates restore clpm_moca

// Extract Coefs (raw coefficients for continuous outcome)
* Cross-lagged: MoCA → Depression
scalar c1 = exp(_b[gds15:L_mocatots_sd])
scalar se1 = _se[gds15:L_mocatots_sd]
scalar z1 = _b[gds15:L_mocatots_sd] / se1
scalar p1 = 2 * normal(-abs(z1))

* Cross-lagged: Depression → MoCA (raw coefficient, not IRR)
scalar c2 = _b[mocatots_sd:L_gds15]
scalar se2 = _se[mocatots_sd:L_gds15]
scalar z2 = c2 / se2
scalar p2 = 2 * normal(-abs(z2))

* Autoregressive: Depression → Depression
scalar c3 = exp(_b[gds15:L_gds15])
scalar se3 = _se[gds15:L_gds15]
scalar z3 = _b[gds15:L_gds15] / se3
scalar p3 = 2 * normal(-abs(z3))

* Autoregressive: MoCA → MoCA (raw coefficient, not IRR)
scalar c4 = _b[mocatots_sd:L_mocatots_sd]
scalar se4 = _se[mocatots_sd:L_mocatots_sd]
scalar z4 = c4 / se4
scalar p4 = 2 * normal(-abs(z4))

// Path diagram with pooled estimates
clear
set obs 4
gen str30 path = ""
gen float coef = .
gen float se = .
gen float pvalue = .
gen float ci_lower = .
gen float ci_upper = .

* Fill in the extracted values
replace path = "Cog_to_Dep" in 1
replace coef = c1 in 1
replace se = se1 in 1
replace pvalue = p1 in 1
replace ci_lower = exp(_b[gds15:L_mocatots_sd] - 1.96*se1) in 1
replace ci_upper = exp(_b[gds15:L_mocatots_sd] + 1.96*se1) in 1

replace path = "Dep_to_Cog" in 2
replace coef = c2 in 2
replace se = se2 in 2
replace pvalue = p2 in 2
replace ci_lower = c2 - 1.96*se2 in 2
replace ci_upper = c2 + 1.96*se2 in 2

replace path = "Dep_to_Dep" in 3
replace coef = c3 in 3
replace se = se3 in 3
replace pvalue = p3 in 3
replace ci_lower = exp(_b[gds15:L_gds15] - 1.96*se3) in 3
replace ci_upper = exp(_b[gds15:L_gds15] + 1.96*se3) in 3

replace path = "Cog_to_Cog" in 4
replace coef = c4 in 4
replace se = se4 in 4
replace pvalue = p4 in 4
replace ci_lower = c4 - 1.96*se4 in 4
replace ci_upper = c4 + 1.96*se4 in 4

* Add significance stars
gen str5 sig = ""
replace sig = "***" if pvalue < 0.001
replace sig = "**" if pvalue < 0.01 & pvalue >= 0.001
replace sig = "*" if pvalue < 0.05 & pvalue >= 0.01

* Create label with coefficient and stars
gen str20 label = string(coef, "%5.3f") + sig

list

* Export to CSV
export delimited using "../../Output/Dta25 Output/Statistics/clpm_moca_coefs.csv", replace		


*****
* Table

esttab clpm_diag clpm_cci clpm_inf clpm_moca using "../../Output/Dta25 Output/Tables/raw/TA5_CLPM.rtf", ///
    replace ///
    eform ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    title("Table: Estimates From Multilevel CLPM for GDS-15 and Cognitive Indicators") ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
    label ///
    addnote("Note: Poisson for GDS15, CCI, and CCI Informant. Logistic regression for diagnosis. Linear regression for MoCA.")
