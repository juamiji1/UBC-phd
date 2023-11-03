/*------------------------------------------------------------------------------
PROJECT: 
AUTHOR: JMJR
TOPIC: PSET 5 
DATE:

NOTES:
------------------------------------------------------------------------------*/

clear all 

*Setting directories 
if c(username) == "juami" {
	gl localpath "C:\Users/`c(username)'\Dropbox\UBC-PhD\Metrics III\Applied Metrics\Problem Set 5"	
}
else {
	*gl path "C:\Users/`c(username)'\Dropbox\"
}

gl do "${localpath}\Code"
gl data "${localpath}\Data"
gl results "${localpath}\Results"

cd "${data}"

*Setting a pre-scheme for plots
set scheme s2mono
grstyle init
grstyle title color black
grstyle color background white
grstyle color major_grid dimgray

*-------------------------------------------------------------------------------
* b.
*-------------------------------------------------------------------------------
use "$data\morg_cleaned_1979-occ.dta", clear
append using "$data\morg_cleaned_1997-occ.dta"

* New variable lnhr wage
gen lnhr_wage=log(hr_wage)
lab var lnhr_wage "Log(hourly wage)"

* Restricted sample
keep if hr_wage_sample==1

* Save data
save "$data\data_pset5", replace

* kdensities
local label : variable label lnhr_wage
twoway (kdensity lnhr_wage [aweight=wgt_hrs] if year==1979)(kdensity lnhr_wage [aweight=wgt_hrs] if year==1997), legend(order(1 "1979" 2 "1997")) xtitle("`label'") ytitle("kdensity")
gr export "$results\Fig1.png", replace

*-------------------------------------------------------------------------------
* d.
*-------------------------------------------------------------------------------
preserve 

	* Keep black men age 25-50
	keep if (race == 2 & sex==1 & age>=25 & age<=50)

	* kdensities
	twoway (kdensity lnhr_wage [aweight=wgt_hrs] if year==1979)(kdensity lnhr_wage [aweight=wgt_hrs] if year==1997), legend(order(1 "1979" 2 "1997")) xtitle("`label'") ytitle("kdensity")
	graph export "$results\Fig2_d.png", replace
	
restore 

preserve
	* Keep white women age 25-50
	keep if (race == 1 & sex==2 & age>=25 & age<=50)

	* kdensities
	twoway (kdensity lnhr_wage [aweight=wgt_hrs] if year==1979)(kdensity lnhr_wage [aweight=wgt_hrs] if year==1997), legend(order(1 "1979" 2 "1997")) xtitle("`label'") ytitle("kdensity")
	graph export "$results\Fig3_d.png", replace
restore

*-------------------------------------------------------------------------------
* e.
*-------------------------------------------------------------------------------
* 1997 dummy variable
gen d97 = (year==1997)
gen dblck = (race==2) if race<3

drop dblck

* Logit estimation
logit d97 i.sex i.race c.age##c.age [pweight=wgt_hrs]
predict ps_97_v1

logit d97 i.sex i.dblck c.age##c.age [pweight=wgt_hrs]
predict ps_97_v2

twoway scatter ps_97_v1 ps_97_v2

*-------------------------------------------------------------------------------
* f.
*-------------------------------------------------------------------------------




