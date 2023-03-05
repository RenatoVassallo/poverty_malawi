* Poverty Assesment in Malawi - by: Giovanna Chaves, Eric Frey and Renato Vassallo
* Development Economics - March 2023
* Analysis of household survey data - Do-file

*************************************
******  Directories and Paths  ******
*************************************
clear
drop _all
graph drop _all
set more off

* Set global paths (data and figures)

global malawi_data "[INSERT YOUR PATH]\data"
global malawi_figures "[INSERT YOUR PATH]\figures"

*************************************
***** Q2. Macroeconomic Context *****
*************************************

global wdi_data	"$malawi_data\wdi_data"

import delimited "$wdi_data\cd504e96-92d6-4ca9-b094-ca8379dfceba_Data.csv", clear

reshape long yr, i(countrycode) j(year) 

rename yr gdpc

gen group = "High" if gdpc > 1000
replace group = "Low" if missing(group)

* Figure 1: GDP per capita by income group

twoway (line gdpc year if countrycode=="MWI", sort) (line gdpc year if countrycode=="BDI", sort) (line gdpc year if countrycode=="CAF", sort) (line gdpc year if countrycode=="MDG", sort) (line gdpc year if countrycode=="MUS", sort) (line gdpc year if countrycode=="BWA", sort) (line gdpc year if countrycode=="GAB", sort), ytitle("GDP per capita (constant 2015 US$)", size(medsmall)) xtitle("Year", size(medsmall)) by(, title("GDP per capita by income group", size(medlarge))) by(group, yrescale note("Source: World Bank, World Development Indicators", size(tiny))) ylab(, labsize(vsmall)) xlab(, labsize(vsmall)) legend(ring(0) size(vsmall) row(2) label(1 "Malawi") label(2 "Burundi") label(3 "Central African Republic") label(4 "Madagascar") label(5 "Mauritius") label(6 "Botswana") label(7 "Gabon"))
graph export "$malawi_figures\1_gdppc.png", width(1000) height(750) replace

frame create fh
frame change fh

import delimited "$wdi_data\fh.csv", clear

gen group = "High" if fh_country == "Mauritius" | fh_country == "Botswana" | fh_country == "Gabon"
replace group = "Low" if fh_country == "Malawi" | fh_country == "Burundi" | fh_country == "Central African Republic"  | fh_country == "Madagascar"
drop if missing(group)

drop if year < 1980

* Figure 2: political and civil liberties by income group

twoway (line fh_total year if fh_country=="Malawi", sort) (line fh_total year if fh_country=="Burundi", sort) (line fh_total year if fh_country=="Central African Republic", sort) (line fh_total year if fh_country=="Madagascar", sort) (line fh_total year if fh_country=="Mauritius", sort) (line fh_total year if fh_country=="Botswana", sort) (line fh_total year if fh_country=="Gabon", sort), ytitle("Freedom House (Composite Score)", size(medsmall)) xtitle("Year", size(medsmall)) by(, title("Political and civil liverties by income group", size(medlarge))) by(group, note("Source: Freedom House", size(tiny))) ylab(, labsize(vsmall)) xlab(, labsize(vsmall)) legend(ring(0) size(vsmall) row(2) label(1 "Malawi") label(2 "Burundi") label(3 "Central African Republic") label(4 "Madagascar") label(5 "Mauritius") label(6 "Botswana") label(7 "Gabon"))
graph export "$malawi_figures\2_liberties.png", width(1000) height(750) replace


*************************************
***** Q3. Poverty Assesment *****
*************************************

* Set paths for IHS16/17 and IHS19/20 data

global MW_2016	"$malawi_data\2016"
global MW_2019	"$malawi_data\2019"

*************************************
******  Fourth IHS 2016-2017   ******
*************************************

* Gender and Head (individual data)
use "$MW_2016\hh_mod_b.dta", clear
gen female = hh_b03==2
gen head = hh_b04==1
keep case_id pid female head
tempfile input_2016
save "input_2016", replace

* Own business (individual data)
use "$MW_2016\hh_mod_e.dta", clear
gen business = hh_e06_2==1
merge 1:1 case_id pid using "input_2016", nogen
keep case_id pid female head business
save "input_2016", replace

* Education (individual data)
use "$MW_2016\hh_mod_c.dta", clear
ren hh_c08 educ_years
gen min_educ = 1 
replace min_educ = 0 if educ_years<=7
keep case_id pid min_educ 
merge 1:1 case_id pid using "input_2016", nogen
drop if head != 1
ren female female_head
ren min_educ mineduc_head
keep case_id female_head mineduc_head business
save "input_2016", replace

* Water and rooms (household level data)
use "$MW_2016\hh_mod_f.dta", clear
ren hh_f10 nrooms
gen water = hh_f36==1
keep case_id nrooms water
merge 1:1 case_id using "input_2016", nogen
save "input_2016", replace

* Consumption Aggregate (household level data)
use "$MW_2016\ihs4_consumption_aggregate.dta", clear
gen rural = urban==2
keep case_id region rural hhsize rexp_cat011 rexpagg rexpaggpc upline pline poor upoor
merge 1:1 case_id using "input_2016", nogen

* Regions
gen south =0
replace south=1 if region == 3
gen central = 0 
replace central = 1 if region == 2
gen north = 0 
replace north = 1 if region == 1
drop region

* Categorical variable for Poverty
gen poor_ind = 3 if upoor == 100
replace poor_ind = 2 if upoor == 0 & poor == 100
replace poor_ind = 1 if upoor == 0 & poor == 0
drop poor upoor

* Number of persons per room
gen persons_pr = hhsize/nrooms

* Consumption to food ratio
gen food_ratio = rexp_cat011/rexpagg

* Save in local folder
save "input_2016", replace


*************************************
******  Fifth IHS 2019-2020   ******
*************************************

* Gender and Head (individual data)
import delimited using "$MW_2019\HH_MOD_B.csv", clear
gen female = hh_b03=="FEMALE"
ren hh_b04 head
keep case_id pid female head
tempfile input_2019
save "input_2019", replace

* Own business (individual data)
import delimited using "$MW_2019\HH_MOD_E.csv", clear
gen business = hh_e06_2=="YES"
merge 1:1 case_id pid using "input_2019", nogen
keep case_id pid female head business
save "input_2019", replace

* Education (individual data)
import delimited using "$MW_2019\HH_MOD_C.csv", clear
ren hh_c08 educ_years
gen min_educ = 1 
replace min_educ = 0 if educ_years=="PRIMARY STND 7" | educ_years=="PRIMARY STND 6" | educ_years=="PRIMARY STND 5" | educ_years=="PRIMARY STND 4" | educ_years=="PRIMARY STND 3"
keep case_id pid min_educ 
merge 1:1 case_id pid using "input_2019", nogen
drop if head != "HEAD"
ren female female_head
ren min_educ mineduc_head
keep case_id female_head mineduc_head business
save "input_2019", replace

* Water and rooms (household level data)
import delimited using "$MW_2019\HH_MOD_F.csv", clear
ren hh_f10 nrooms
gen water = hh_f36_1=="PIPED INTO DWELLING"
keep case_id nrooms water
merge 1:1 case_id using "input_2019", nogen
save "input_2019", replace

* Consumption Aggregate (household level data)
import delimited using "$MW_2019\ihs5_consumption_aggregate.csv", clear
gen rural = urban==2
keep case_id region rural hhsize rexp_cat011 rexpagg rexpaggpc upline pline poor upoor
merge 1:1 case_id using "input_2019", nogen


* Regions
gen south =0
replace south=1 if region == 3
gen central = 0 
replace central = 1 if region == 2
gen north = 0 
replace north = 1 if region == 1

* Categorical variable for Poverty
gen poor_ind = 3 if upoor == 1
replace poor_ind = 2 if upoor == 0 & poor == 1
replace poor_ind = 1 if upoor == 0 & poor == 0

* Number of persons per room
gen persons_pr = hhsize/nrooms

* Consumption to food ratio
gen food_ratio = rexp_cat011/rexpagg

* Save in local folder
save "input_2019", replace



*************************************
***** 3.1 Correlates of Poverty *****
*************************************

local bases 2016 2019
local nbases : word count `bases'

foreach base of local bases{
	use input_`base', clear
	egen nind = sum(hhsize)
	gen wt = hhsize/nind
	local variables rexpaggpc food_ratio female_head mineduc_head rural south central north hhsize business water persons_pr
	local nvar : word count `variables'
	matrix output_`base' = J(`nvar', 9,.)
	matrix rowname output_`base' = `variables'
	matrix colname output_`base' = non-poor poor ultra-poor a-b a-b_test a-b_pval b-c b-c_test b-c_pval

	forvalues i=1/3{ 
		local j = 1
		foreach var in `variables' {
			quie summ `var' if poor_ind == `i'
			//quie summ `var' [aw=wt] if poor_ind == `i' // for weighted analysis
			matrix output_`base'[`j', `i'] = r(mean)
			if `i' == 1 {
				if (`j'==1 |`j'==2|`j'==9|`j'==12){
					quie ttest `var' if poor_ind==`i'|poor_ind==`i'+1, by(poor_ind)
					matrix output_`base'[`j', 5] = r(t)
					matrix output_`base'[`j', 6] = r(p)
				}
				else {
					quie prtest `var' if poor_ind==`i'|poor_ind==`i'+1, by(poor_ind)
					matrix output_`base'[`j', 5] = r(z)
					matrix output_`base'[`j', 6] = r(p)
				}
			}
			else if `i' == 2 {
				if (`j'==1 |`j'==2|`j'==9|`j'==12){
					matrix output_`base'[`j', 4] = output_`base'[`j', 1] - output_`base'[`j', 2]
					quie ttest `var' if poor_ind==`i'|poor_ind==`i'+1, by(poor_ind)
					matrix output_`base'[`j', 8] = r(t)
					matrix output_`base'[`j', 9] = r(p)
				}
				else {
					matrix output_`base'[`j', 4] = output_`base'[`j', 1] - output_`base'[`j', 2]
					quie prtest `var' if poor_ind==`i'|poor_ind==`i'+1, by(poor_ind)
					matrix output_`base'[`j', 8] = r(z)
					matrix output_`base'[`j', 9] = r(p)
				}
			}
			else if `i' == 3 {
				matrix output_`base'[`j', 7] = output_`base'[`j', 2] - output_`base'[`j', 3]
			}
			local ++j
		}
	}
	matrix list output_`base', format(%10.3f)
}


*************************************
*******  3.2 Poverty Profile  *******
*************************************

use input_2016, clear
gen lconsumppc_2016 = log(rexpaggpc)
egen nind = sum(hhsize)
gen wt16 = hhsize/nind
keep case_id lconsumppc_2016 wt16
tempfile lconpc_2016
save "lconpc_2016", replace

use input_2019, clear
gen lconsumppc_2019 = log(rexpaggpc)
egen nind = sum(hhsize)
gen wt19 = hhsize/nind
keep lconsumppc_2019 wt19
append using lconpc_2016
gen povline_2016 = log(137425)
gen povline_2019 = log(165879)

cumul lconsumppc_2016 [aw=wt16], gen(cdf_2016)
cumul lconsumppc_2019 [aw=wt19], gen(cdf_2019)
cumul povline_2016, gen(cdf_povline_2016)
cumul povline_2019, gen(cdf_povline_2019)

stack cdf_2016 lconsumppc_2016 cdf_2019 lconsumppc_2019 cdf_povline_2016 povline_2016 cdf_povline_2019 povline_2019, into(c consump) wide clear

*ssc install palettes, replace

line cdf_2016 cdf_povline_2016 cdf_2019 cdf_povline_2019 consump if (consump <= 15 & consump >=10), sort ylab(, grid glwidth(thin) glpattern(dash) glcolor(grey%10)) lcolor(cranberry cranberry%30 ebblue ebblue%30) xtitle("Welfare aggregate (log PC consumption)") ytitle("Cumulative Distribution") legend(pos(5) ring(0) col(1) region(col(white%40)) size(small) order(1 "2016" 2 "Poverty Line 2016" 3 "2019"  4 "Poverty line 2019")) graphregion(color(white)) 
*title("Poverty Incidence Curve") subtitle("Malawi IHS 2016/2019")
graph export "$malawi_figures\3_poverty.png", width(1000) height(750) replace


*************************************
****** 3.3 Poverty Indicators  ******
*************************************

local bases 2016 2019
matrix poverty_ind = J(3,4,.)
matrix rowname poverty_ind = p_0 p_1 p_2
matrix colname poverty_ind = mean_16 sd_16 mean_19 sd_19

local j = 1

foreach base of local bases{
	
	use input_`base', clear
	egen nind = sum(hhsize)
	gen wt = hhsize/nind
	
	gen pind = rexpaggpc <= pline
	qui summ pind [aweight=wt]
	matrix poverty_ind[1, `j'] = r(mean)
	matrix poverty_ind[1, `j'+1] = r(sd)
	
	gen df=(1-rexpaggpc/pline)*pind
	qui summ df [aweight=wt]
	matrix poverty_ind[2, `j'] = r(mean)
	matrix poverty_ind[2, `j'+1] = r(sd)
	
	gen df2=(1-rexpaggpc/pline)^2*pind
	qui summ df2 [aweight=wt]
	matrix poverty_ind[3, `j'] = r(mean)
	matrix poverty_ind[3, `j'+1] = r(sd)
	
	local j = 3
}
matrix list poverty_ind


local bases 2016 2019
matrix poverty_group = J(12,4,.)
matrix rowname poverty_group = p0_rural_0 p1_rural_0 p2_rural_0 p0_rural_1 p1_rural_1 p2_rural_1 p0_mineduc_0 p1_mineduc_0 p2_mineduc_0 p0_mineduc_1 p1_mineduc_1 p2_mineduc_1
matrix colname poverty_group = mean_16 sd_16 mean_19 sd_19

local j = 1
foreach base of local bases{
	
	use input_`base', clear
	egen nind = sum(hhsize)
	gen wt = hhsize/nind
	gen pind = rexpaggpc <= pline
	gen df=(1-rexpaggpc/pline)*pind
	gen df2=(1-rexpaggpc/pline)^2*pind
	
	local variables rural mineduc_head
	local jump = 0
	foreach variable of local variables{
		local row = 1
		forvalues i=0/1{
			
			qui summ pind [aweight=wt] if `variable' == `i'
			matrix poverty_group[`row'+`jump', `j'] = r(mean)
			matrix poverty_group[`row'+`jump', `j'+1] = r(sd)
			
			local ++row
			qui summ df [aweight=wt] if `variable' == `i'
			matrix poverty_group[`row'+`jump', `j'] = r(mean)
			matrix poverty_group[`row'+`jump', `j'+1] = r(sd)
			
			local ++row
			qui summ df2 [aweight=wt] if `variable' == `i'
			matrix poverty_group[`row'+`jump', `j'] = r(mean)
			matrix poverty_group[`row'+`jump', `j'+1] = r(sd)
			
			local ++row
		}
		local jump = 6
	}
	local j = 3
}
matrix list poverty_group


*************************************
***** 4. Gini and Lorenz Curve  *****
*************************************

* Install packages
*ssc install ineqdeco
*ssc install glcurve

local bases 2016 2019
local nbases : word count `bases'
matrix gini_theil = J(4,`nbases',.)
matrix rowname gini_theil = gini_urban gini_rural theil_urban theil_rural
matrix colname gini_theil = 2016 2019

local j = 1
foreach base of local bases{
	
	use input_`base', clear
	egen nind = sum(hhsize)
	gen wt = hhsize/nind
	
	* Gini and Theil Index by region
	qui ineqdeco rexpaggpc [aw=wt], by(rural)
	matrix gini_theil[1,`j'] = r(gini_0)
	matrix gini_theil[2,`j'] = r(gini_1)
	matrix gini_theil[3,`j'] = r(ge1_0)
	matrix gini_theil[4,`j'] = r(ge1_1)
	
	* Lorenz Curves
	if `j' == 1 {
		glcurve rexpaggpc [aw=wt], by(rural) split lorenz plot(function equality = x, lcolor(balck) lpattern(dash)) title("IHS 2016/17") ytitle("Cumulative % of Consumption") xtitle("Cumulative % of Population") ylab(, grid glwidth(thin) glpattern(dash) glcolor(grey%10)) lcolor(cranberry ebblue) lpattern(solid solid) legend(pos(5) ring(0) col(1) region(col(white%40)) size(small) order(1 "Urban" 2 "Rural" 3 "Equality")) graphregion(color(white)) saving(lc_`j', replace)
	}
	else if `j' == 2 {
		glcurve rexpaggpc [aw=wt], by(rural) split lorenz plot(function equality = x, lcolor(balck) lpattern(dash)) title("IHS 2019/20") ytitle("") xtitle("Cumulative % of Population") ylab(, grid glwidth(thin) glpattern(dash) glcolor(grey%10)) lcolor(cranberry ebblue) lpattern(solid solid) legend(pos(5) ring(0) col(1) region(col(white%40)) size(small) order(1 "Urban" 2 "Rural" 3 "Equality")) graphregion(color(white)) saving(lc_`j', replace)
	}

	local ++j
}
* Table with Gini and Theil Coefficients
matrix list gini_theil

* Combined Lorenz Curves
gr combine lc_1.gph lc_2.gph, ycommon graphregion(color(white)) 
graph export "$malawi_figures\4_lorenz.png", width(1000) height(750) replace
