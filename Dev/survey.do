
clear all 

*Setting directories 
if c(username) == "juami" {
	gl localpath "C:\Users/`c(username)'\Dropbox\My-Research\Guerillas_Development"
	gl overleafpath "C:\Users/`c(username)'\Dropbox\Overleaf\GD-draft-slv"
	gl do "C:\Github\Guerrilla-development\code"
	
}
else {
	*gl path "C:\Users/`c(username)'\Dropbox\"
}

gl data "${localpath}\2-Data\Salvador"
gl maps "${localpath}\5-Maps\Salvador"
gl tables "${overleafpath}\tables"
gl plots "${overleafpath}\plots"

cd "${data}"

*Setting a pre-scheme for plots
grstyle init
grstyle title color black
grstyle color background white
grstyle color major_grid dimgray


use "${data}/temp\slvShp_segm_brks.dta", clear
ren (SEG_ID cntrl50 cntr100 cntr200) (segm_id control_break_fe_50 control_break_fe_100 control_break_fe_200)
keep segm_id control_break_fe_*

tempfile Bks
save `Bks', replace 


use "C:\Users\juami\Dropbox\My-Research\Guerillas_Development\Replication Package\Data (noGIS)\SurveyAnalysis.dta", clear

merge m:1 segm_id using `Bks', keep(1 3) gen(mergebks200)


gl controls "within_control i.within_control#c.z_run_cntrl z_run_cntrl"
gl if "if abs(z_run_cntrl)<=${h}"


recode S3 (10731=1) (10732=0) 

gen p181_high=(p181>0) if p181!=.
gen p182_high=(p182>0) if p182!=.
gen p183_high=(p183>0) if p183!=.
gen p184_high=(p184>0) if p184!=.
gen p185_high=(p185>0) if p185!=.

gen p14a=(p14>2) if p14!=.
gen p13a=(p13>2) if p13!=.

gen p21a=(p21>2) if p21!=.
gen p21b=(p21>3) if p21!=.

gen p13b=(p13>1) if p13!=.
gen p14b=(p14>1) if p14!=.
drop p15b
gen p15b=(p15>1) if p15!=.


gen p17a_high=(p17a>0) if p17a!=.
gen p17b_high=(p17b>0) if p17b!=.
gen p17c_high=(p17c>0) if p17c!=.


reg p15a i1.within_control#c.p17b_high p17b_high within_control, r
reg p15b i1.within_control#c.p17b_high p17b_high within_control, r

reg S3 within_control, r
reg p9 within_control, r
reg p16 within_control, r


replace dst_cnt=dst_cnt/1000
replace dst_cnt = -dst_cnt if within_control==0


foreach var of varlist p17a p17b p17c p181 p182 p183 p184 p185 {
	summ `var'
	gen `var'_high_v2=(`var'>=`r(mean)') if `var'!=.
}
 
 
gl dictator "p17a p17b p17c p82 p181 p182 p183 p184 p185 p181_high_v2 p182_high_v2 p183_high_v2 p184_high_v2 p185_high_v2"
gl averineq "p30 p31a p31b p33 p34 p35_recode p36_recode S3"
gl cooperat "p13a p14a p15a p13b p14b p15b p37_recode"

*Global of border FE for all estimates
gl breakfe "control_break_fe_50"
gl controls "within_control i.within_control#c.dst_cnt dst_cnt"


foreach var of global dictator {

	*RDD with break fe and triangular weights 
	rdrobust `var' dst_cnt, all kernel(triangular) 
	gl h=e(h_l)
	gl b=e(b_l)

	*Conditional for all specifications
	gl if "if abs(dst_cnt)<=${h}"

	*Replicating triangular weights
	cap drop tweights
	gen tweights=(1-abs(dst_cnt/${h})) ${if}

	reghdfe `var' ${controls} likert_SD_Index [aw=tweights] ${if}, vce(r) a(i.${breakfe}) 
	*qui: summ `var' if e(sample)==1 & within_control==0, d
	*gl mean_y=round(r(mean), .001)
	*outreg2 using "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.tex", nor2 tex(frag) keep(within_control) addstat("Bandwidth (Km)", ${h}, "Dependent mean", ${mean_y}) label nonote nocons append 
	
}


foreach var of global cooperat {

	*RDD with break fe and triangular weights 
	rdrobust `var' dst_cnt, all kernel(triangular) 
	gl h=e(h_l)
	gl b=e(b_l)

	*Conditional for all specifications
	gl if "if abs(dst_cnt)<=${h}"

	*Replicating triangular weights
	cap drop tweights
	gen tweights=(1-abs(dst_cnt/${h})) ${if}

	reghdfe `var' ${controls} likert_SD_Index [aw=tweights] ${if}, vce(r) a(i.${breakfe}) 
	*qui: summ `var' if e(sample)==1 & within_control==0, d
	*gl mean_y=round(r(mean), .001)
	*outreg2 using "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.tex", nor2 tex(frag) keep(within_control) addstat("Bandwidth (Km)", ${h}, "Dependent mean", ${mean_y}) label nonote nocons append 
	
}

foreach var of global averineq {

	*RDD with break fe and triangular weights 
	rdrobust `var' dst_cnt, all kernel(triangular) 
	gl h=e(h_l)
	gl b=e(b_l)

	*Conditional for all specifications
	gl if "if abs(dst_cnt)<=${h}"

	*Replicating triangular weights
	cap drop tweights
	gen tweights=(1-abs(dst_cnt/${h})) ${if}

	reghdfe `var' ${controls} likert_SD_Index [aw=tweights] ${if}, vce(r) a(i.${breakfe}) 
	*qui: summ `var' if e(sample)==1 & within_control==0, d
	*gl mean_y=round(r(mean), .001)
	*outreg2 using "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.tex", nor2 tex(frag) keep(within_control) addstat("Bandwidth (Km)", ${h}, "Dependent mean", ${mean_y}) label nonote nocons append 
	
}


gen p17_high_v2=1 if p17b_high_v2==1 | p17c_high_v2==1
replace p17_high_v2=0 if p17_high_v2==.

gen p18_high_v2=1 if p181_high_v2==1 | p182_high_v2==2
replace p18_high_v2=0 if p18_high_v2==.


foreach var of global cooperat {

	*RDD with break fe and triangular weights 
	rdrobust `var' dst_cnt, all kernel(triangular) 
	gl h=e(h_l)
	gl b=e(b_l)

	*Conditional for all specifications
	gl if "if abs(dst_cnt)<=${h}"

	*Replicating triangular weights
	cap drop tweights
	gen tweights=(1-abs(dst_cnt/${h})) ${if}

	reghdfe `var' i1.within_control#i1.p17_high_v2 p17_high_v2 i1.p17_high_v2#c.dst_cnt ${controls} likert_SD_Index [aw=tweights] ${if}, vce(r) a(i.${breakfe}) 
	qui: summ `var' if e(sample)==1 & within_control==0, d
	gl mean_y=round(r(mean), .001)
	*outreg2 using "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.tex", nor2 tex(frag) keep(within_control) addstat("Bandwidth (Km)", ${h}, "Dependent mean", ${mean_y}) label nonote nocons append 
	
}

foreach var of global cooperat {

	*RDD with break fe and triangular weights 
	rdrobust `var' dst_cnt, all kernel(triangular) 
	gl h=e(h_l)
	gl b=e(b_l)

	*Conditional for all specifications
	gl if "if abs(dst_cnt)<=${h}"

	*Replicating triangular weights
	cap drop tweights
	gen tweights=(1-abs(dst_cnt/${h})) ${if}

	reghdfe `var' i1.within_control#i1.p18_high_v2 p18_high_v2 i1.p18_high_v2#c.dst_cnt ${controls} likert_SD_Index [aw=tweights] ${if}, vce(r) a(i.${breakfe}) 
	qui: summ `var' if e(sample)==1 & within_control==0, d
	gl mean_y=round(r(mean), .001)
	*outreg2 using "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.tex", nor2 tex(frag) keep(within_control) addstat("Bandwidth (Km)", ${h}, "Dependent mean", ${mean_y}) label nonote nocons append 
	
}





gl final "p17a p17c p15b p37_recode"

cap erase "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.tex"
cap erase "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\agro.txt"

label var within_control "Guerrilla control"

foreach var of global final {

	*RDD with break fe and triangular weights 
	rdrobust `var' dst_cnt, all kernel(triangular) 
	gl h=e(h_l)
	gl b=e(b_l)

	*Conditional for all specifications
	gl if "if abs(dst_cnt)<=${h}"

	*Replicating triangular weights
	cap drop tweights
	gen tweights=(1-abs(dst_cnt/${h})) ${if}

	reghdfe `var' ${controls} likert_SD_Index [aw=tweights] ${if}, vce(r) a(i.${breakfe}) 
	qui: summ `var' if e(sample)==1 & within_control==0, d
	gl mean_y=round(r(mean), .001)
	outreg2 using "C:\Users\juami\Dropbox\UBC-PhD\Dev II\Ideas\Idea3\survey.tex", nor2 tex(frag) keep(within_control) addstat("Bandwidth (Km)", ${h}, "Dependent mean", ${mean_y}) label nonote nocons append 
	
}









*END





