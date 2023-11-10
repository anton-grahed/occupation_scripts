/*
TITLE: OCCUPATIONAL PERSISTENCE
DESCRIPTION: THIS DO-FILE LOOKS AT TRENDS AND PATTERNS REGARDING OCCUPATIONAL PERSISTENCE, I.E. WHETHER THERE IS PERSISTENCE IN THE CHOICE OF OCCUPATION BY FAMILY
PROJECT: ZERO-SUM
AUTHOR: ANTON GRAHED
DATE CREATED: 07/11/2023
DATE LAST MODIFIED: 09/11/2023
*/


** STATA INITIALIZATION **
set more off
set varabbrev off
clear all
macro drop _all
version 18

set trace on
set tracedepth 1

** DATA ** 
di "`c(username)'"

if "`c(username)'" == "GRAHED" {
	global ROOT "C:/Users/Grahed/Desktop/Projects/Zero-Sum"
} 

if "`c(username)'" == "anton_grahed" {
	global ROOT "/Users/anton_grahed/Desktop/Projects/Zero-Sum"
}


** LOAD DATA ** 
use ${ROOT}/analysis.dta, clear 


** DESCRIBE DATA OF INTEREST ** 
// create smaller version - with variables of potential interest 
global svars = "race male age born_us born_country born_state born_county born_fips liberal biden democrat"
local np1 "mum dad mgmum mgdad pgmum pgdad"
local np2 "job_cat immigrated age"

global parvars ""
foreach var in `np1'{
	foreach term in `np2' {
		
		local x `var'_`term'
		
		global parvars = "${parvars} `x'"
		
		
	}
	
}

global svars = "${svars} ${parvars}"


/*
qui log using ${ROOT}/codebook.txt, text replace
	codebook $svars
qui log close 
*/

** GENERATE OCCUPATIONAL PERSISTENCE VARIABLES **

/*
v.0: no gender restriction
- have to remove any missing job category - so . == . is F 
- job_cat == 2 signifies don't know - so have removed it 
- decision:: keep job classified as other as a valid comparison criteria - this 
means we are likely to *overestimate* the occupational persistence
*/


// create indicators if a job category is not missing and not labelled as dont know (ADD POTENTIALLY: NOT LABELLED AS OTHER)

foreach var of varlist *job_cat {
	
	g `var'_nm_dk = (`var' != . & `var' != 2)
	
}


* create v0 indicators *
gen same_job_par0 = inlist(job_cat, mum_job_cat, dad_job_cat) if job_cat_nm_dk == 1 
* if both mum job is missing and dad job is missing - set to missing as no valid comparison can be made
replace same_job_par0 = . if mum_job_cat_nm_dk == 0 & dad_job_cat_nm_dk == 0

* mum has the same job as at least one of their grandparents
gen same_job_mpar_gpar0 = inlist(mum_job_cat, mgmum_job_cat, mgdad_job_cat) if mum_job_cat_nm_dk == 1
* if both gpar jobs are missing - set to missing as no valid comparison can be made 
replace same_job_mpar_gpar0 = . if mgmum_job_cat_nm_dk == 0 & mgdad_job_cat_nm_dk == 0


* dad has the same job as at least one of their grandparents
gen same_job_dpar_gpar0 = inlist(dad_job_cat, pgmum_job_cat, pgdad_job_cat) if dad_job_cat_nm_dk == 1
* if both gpar jobs are missing - set to missing as no valid comparison can be made
replace same_job_dpar_gpar0 = . if pgmum_job_cat_nm_dk == 0 & pgdad_job_cat_nm_dk == 0

* at least one parent has the same job as their parent (and at least one is non-missing)
gen same_job_par_gpar0 = (same_job_mpar_gpar0 == 1 | same_job_dpar_gpar0 == 1) if (same_job_mpar_gpar0 != . | same_job_dpar_gpar0 != .)

* if you have the same job as at least one of your grandparents
gen same_job_gpar0 = inlist(job_cat, mgmum_job_cat, mgdad_job_cat, pgmum_job_cat, pgdad_job_cat) if job_cat_nm_dk == 1
* if all gpar jobs missing - set to missing as no valid comparison can be made 
replace same_job_gpar0 = . if (mgmum_job_cat_nm_dk == 0 & mgdad_job_cat_nm_dk == 0 & pgmum_job_cat_nm_dk == 0 & pgdad_job_cat_nm_dk == 0)

** CHECK OVERLAPS AND MISSINGNESS **

qui log using ${ROOT}/job_cat_overlap.txt, text replace 
	tab job_cat 
	tab mum_job_cat
	tab dad_job_cat
	tab mgmum_job_cat
	tab mgdad_job_cat
	tab pgdad_job_cat
	tab pgmum_job_cat
qui log close 

* child variable only has ~5k non-missing - so the highest we could match is 5k


/*
 if your parents had the same job as their grandparent - how likely are you to have the same job as your parent 
*/

** very low **

tab same_job_gpar0


** rename to main 
frames rename default main 


** create bar graph frame 
frames create bar str15(variable) double(esto se)

foreach x of varlist same_job_par0 same_job_gpar0 same_job_par_gpar0 {
	reg `x'
	frame post bar ("`x'") (_b[_cons]) (_se[_cons])
}


cwf bar 
gen ci_upper = esto + 1.96 * se
gen ci_lower = esto - 1.96 * se

encode variable, gen(varn)
label define varn_lbl 1 "Same Job Grandparent" 2 "Same Job Parent" 3 "Parent Same Job Grandparent"
label values varn varn_lbl

twoway (bar esto varn if varn == 1, barwidth(.8) color(red%40)) ///
	   (bar esto varn if varn == 2, barwidth(.8) color(blue%40)) /// 
	   (bar esto varn if varn == 3, barwidth(.8) color(yellow%40)) ///
	   (rcap ci_upper ci_lower varn), /// 
	   yscale(range(0 1)) xlabel(1 "Same Job Grandparent" 2 "Same Job Parent" 3 "Parent Same Job Grandparent", noticks) ylabel(0(0.1)1) ///
	   legend(off) ///
	   xtitle("Occupational Persistence") ytitle("Probability")

graph export ${ROOT}/persistence_bar.png, replace

** CORRELATIONS **

// change back to main 
cwf main

** REGRESSION **
/*
Regress: what predicts occupational persistence: gender, income, education, age, race, state FE 

*/

// rescale so coef not too small
gen hhincome_10k = hhincome / 10000


// remove 4% of the data with american indian / alaskan native, native hawaiian / other 


reghdfe same_job_par0 hhincome_10k, absorb(current_state)

// collapse education
decode education, gen(educ_str)
gen high_school = (education == 5 | education == 10 | education == 12) if education != .
gen college = (education == 13 | education == 14 | education == 16) if education != .
gen master_phd = (education == 18 | education == 21) if education != .

// collapse race
decode race, gen(race_str)
gen aframerican = (race == 1) if race != .
gen asian = (race == 3) if race != .
gen white = (race == 4) if race != .
gen latino = (race == 5) if race != .


/*
check the gen is correct
tab high_school educ_str 
tab college educ_str 
tab master_phd educ_str 

tab aframerican race_str 
tab asian race_str
tab white race_str
tab latino race_str
*/


// = 1 if in any of the 4 categories and = 0 otherwise
gen race_exist = inlist(1, aframerican, asian, white, latino) if race != .

// label vars
label var same_job_par0 "Occupational Persistence"
lab var born_us "Born U.S"
lab var male "Male"
lab var hhincome_10k "HH Income (10K)"
lab var college "Highest Level College"
lab var master_phd "Highest Level Masters / PhD"
lab var age "Age"
lab var aframerican "African American"
lab var asian "Asian American"
lab var latino "Latin American"




reghdfe same_job_par0 born_us male hhincome_10k college master_phd age aframerican asian latino if race_exist == 1, absorb(current_state) cluster(current_state)

gen _sample = e(sample)

global spec ""born_us""male""hhincome_10k""college master_phd""age""aframerican asian latino""born_us male hhincome_10k college master_phd age aframerican asian latino""

cap est clear 
forv i = 1/7 {
	
	local ivs : word `i' of $spec
	
	eststo mod`i' : reghdfe same_job_par0 `ivs' if _sample == 1, absorb(current_state) cluster(current_state)
	estadd local FE "Yes"
	estadd local AR2 e(r2_a)
}


esttab mod* using "${ROOT}/occup_persistence_regtable.tex", replace /// 
				   b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
				   booktabs ///
				   addnotes("Standard errors clustered at state level.") ///
				   scalars("FE Fixed Effects")



