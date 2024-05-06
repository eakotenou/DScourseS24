clear all
cap log close
set more off
version 18

*============================================================================*
*                       COLLABORATION ON THE CODE                            *
*----------------------------------------------------------------------------*
* Define project root directory
global projectroot "/path/to/project/root"

* Set user-specific paths relative to project root
global user 1   // change the number to your user number
if $user == 1 { 
    global path "${projectroot}/Data/Source"
}
if $user == 2 {
    global path "${projectroot}/Data/Source"  
}

* Define other directories relative to path
global do "${path}/do"
global data "${path}/data" 
global outtex "${path}/outtex"
global figures "${path}/figures"

* Create output directories if they don't exist
foreach dir in do data outtex figures {
    cap mkdir "${path}/`dir'"
}

* Start a log file
cap log close
log using "${do}/internet_hf.txt", replace

*----------------------------------------------------------------------------*
*        DATA LOADING AND CLEANING                                           *
*----------------------------------------------------------------------------*

* Load and append data files
use "${data}/sdi_kenya_2018_module1_anonymized.dta", clear

*----------------------------------------------------------------------------*
*        CREATE VARIABLES                                                    *
*----------------------------------------------------------------------------*

* Create binary indicator for more than 100 births
gen morethan100birth = (num_births == 2) 
replace morethan100birth = . if num_births == 0
replace morethan100birth = . if num_births == .

* Create birth rate variable and label it
gen birth_rate = morethan100birth / num_inpatient
replace birth_rate = 1 if birth_rate == 0.5 
rename birth_rate high_successful_births
label var successful_births "High_successful_births"
label define high_births_lab 1 "More than 100" 0 "Less than 100"  
label values High_successful_births High_births_lab

global outcomes num_births num_inpatient successful_births

* Variables of interest
global varofinter internet_avb internet_f

* Recode internet access variables 
recode internet_a (0 = 0 "No") (1 2 = 1 "Yes"), gen(internet_avb)
label var internet_avb "Internet access available"

recode internet_b (0 = 0 "No") (1 2 = 1 "Yes"), gen(internet_f)  
label var internet_f "Internet is functional"

* Control variables
recode ruralurban (1 = 0 "Rural") (2 = 1 "Urban"), gen(urban)
label var urban "Facility is located in urban area"

recode publicprivate (1 = 1 "Public") (2 3 4 5 = 0 "Private"), gen(public)
label var public "Public health facility"

foreach var of varlist fridge_temp2 water_a power_a {
    replace `var' = . if inlist(`var', 99, 9)
}

recode has_fridge (2 3 = 1 "Yes") (0 1 = 0 "No"), gen(has_fridge)
label var has_fridge "Facility has a functional refrigerator"
tab power_a, gen(p)
tab water_a, gen(w)

tab1 num_outpatient beds_total 
tab num_outpatient, gen(outp)

tab beds_total, gen(bed)

tab1 staff_fp staff_fp_num_a trained_imci

tab1 power_b water_b Has_fridge vaccine_store1 has_maternity_a has_maternity_b has_inpatient outp1-outp5, mi

tab1 has_bloodtrans has_csection has_lab

* Define global macros for control variables
glo x public urban facility_level  num_outpatient beds_total 

global facility_vars "g1 g2 g3 urban public bed1 bed2 bed3 "
sum $facility_vars if has_births == 1 & year == 2018

global staff_vars "staff_fp staff_fp_num_a trained_imci"

sum $staff_vars if has_births == 1 & year == 2018
*power_b 
global infrastructure_vars "p1-p5 power_b w1-w13 water_b fridge_temp2 Has_fridge vaccine_store1 has_maternity_a has_maternity_b has_inpatient outp1-outp5"

sum $infrastructure_vars if has_births == 1 & year == 2018

global $service_vars "has_bloodtrans has_csection has_lab"

des $service_vars
sum $service_vars if has_births == 1 & year == 2018


****for channel: search for health related info
glo channels ict_*
sum $channels if has_births == 1 & year == 2018

glo chls ict_a  ict_d ict_e ict_g ict_i ict_j
sum $chls if has_births == 1 & year == 2018
des $chls 

*----------------------------------------------------------------------------*
*        DESCRIPTIVE STATISTICS                                              *
*----------------------------------------------------------------------------*

* Summarize key variables
estpost tabstat $outcomes $varofinter $facility_vars $staff_vars ///
    $infrastructure_vars  $service_vars if has_births == 1 & year == 2018, ///
    c(stat) stat(mean sd min max n)

* Output summary statistics table  
#delimit ;
esttab using "${outtex}/des_stats.tex", replace
    cells("mean(fmt(2)) sd min max count") 
    noobs nonumber label booktabs
    collabels("Mean" "SD" "Min" "Max" "N")
    title("Summary statistics") 
    addnotes("Sample: Health facilities with births in 2018.");
#delimit cr

*----------------------------------------------------------------------------*
*        BALANCE TABLES                                                      *
*----------------------------------------------------------------------------*

* Balance table for internet access
iebaltab $facility_vars $staff_vars $infrastructure_vars $service_vars ///
    if has_births == 1 & year == 2018, grpvar(internet_avb) ///
    savetex("${outtex}/balance_table_internet.tex") replace 

*----------------------------------------------------------------------------*
*        PROPENSITY SCORE MATCHING                                           *
*----------------------------------------------------------------------------*

* Check overlap of propensity scores
twoway (kdensity pscore if internet_f == 0, color(blue%30)) ///
       (kdensity pscore if internet_f == 1, color(red%30)) ///
       if has_births == 1 & year == 2018, ///
       legend(order(1 "No internet" 2 "Has internet")) ///
       title("Overlap of propensity scores") ///
       xtitle("Propensity score") ytitle("Density") ///
       xlabel(0(0.2)1) ylabel(, angle(0))

graph export "${figures}/pscore_overlap.png", replace       

*----------------------------------------------------------------------------*
*        REGRESSION MODELS                                                   *
*----------------------------------------------------------------------------*

* Create interaction terms 
global interactions
foreach var1 of varlist $staff_vars $infrastructure_vars $service_vars {
    foreach var2 of varlist $staff_vars $infrastructure_vars $service_vars {
        gen `var1'_X_`var2' = `var1' * `var2'
        global interactions $interactions `var1'_X_`var2'
    }
}

* Post-double selection lasso
eststo clear
eststo: pdslasso successful_births internet_avb ///
    $facility_vars $staff_vars $infrastructure_vars $service_vars $interactions
eststo: pdslasso successful_births internet_avb pub_X_internet_avb ///
    $facility_vars $staff_vars $infrastructure_vars $service_vars $interactions

* Output pdslasso results    
esttab using "${outtex}/pdslasso_results.tex", replace 
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) 
    star(* 0.10 ** 0.05 *** 0.01) 
    stats(N  r2, fmt(%9.0f %9.3f) labels("Observations" "R-squared"))
    label booktabs nocons nomtitles          

* Instrumental variables lasso
ivlasso successful_births (internet_avb = $facility_vars $staff_vars ///
        $infrastructure_vars $service_vars), first

* Interaction of public facility and internet
ivlasso successful_births (internet_avb pub_X_internet_avb = ///
        $facility_vars $staff_vars $infrastructure_vars $service_vars), first        

* Interaction of using ICT for info and other factors        
ivlasso successful_births (ict_i = $facility_vars $staff_vars /// 
        $infrastructure_vars $service_vars), first        

* Output ivlasso results
eststo clear        
foreach var in internet_avb "internet_avb pub_X_internet_avb" ict_i {
    eststo: ivlasso successful_births (`var' = $facility_vars $staff_vars ///
            $infrastructure_vars $service_vars), first
}

esttab using "${outtex}/ivlasso_results.tex", replace
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))
    star(* 0.10 ** 0.05 *** 0.01)
    stats(N  rkf, fmt(%9.0f %9.3f) labels("Observations" "KP Wald F-stat"))
    label booktabs nocons nomtitles
    mtitles("Internet" "Int-X-public" "ICT info")

* Close log
log close
