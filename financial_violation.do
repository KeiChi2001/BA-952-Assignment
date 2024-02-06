******************* Paper 2: financial convenant violation and financial policy ****************


***************** Data Preparation **********************

clear all
cd "D:\Documents\study\MSyear1\Empirical Corp Fin\Assignment 1"
log using "finviol.log", replace

*** restrict the data range
use "secfilling", replace
* generate year and quarter number
gen fyear = substr(datacqtr, 1, 4)
destring fyear, replace
keep if fyear<=2005
gen fqtr = substr(datacqtr, 6, 6)
destring fqtr, replace
save "secfilling", replace


*** pre-processing for compustat
use "Compustat_finviolation", replace
format datadate %tdDDNNCCYY // change date format
gen year=year(datadate)
gen month=month(datadate)
gen day=day(datadate)
ren (fyearq) (fyear)

preserve
use "fyr_prefstock", replace
drop if indfmt == "FS"
save "fyr_prefstock", replace
restore

merge m:1 gvkey fyear using "fyr_prefstock", keepusing(fyr pstkl) keep(1 3)
drop _m
duplicates tag fqtr fyear gvkey, gen(tag)
drop if tag==1 & mi(datafqtr)
save "Compustat_finviolation", replace

*************** Generate Financial variables **************
use "Compustat_finviolation", replace
drop tag

* set panel
destring gvkey, replace
gen fqtr_n = 10*fyear+fqtr

* fqtr_n cannot be identified as continuous time. Need to use the rank.
preserve
keep fqtr_n
duplicates drop
egen q_num = rank(fqtr_n), track
tempfile rank
save "`rank'"
restore

merge m:1 fqtr_n using "`rank'", nogen
ren q_num rank
xtset gvkey rank

*** capital structure variables
* lag total asset
gen atql1 = L.atq
* book debt
gen bdebt = dlttq + dlcq
* net debt issuance
gen netdebt = (bdebt - L.bdebt)/atql1
* net equity issuance
gen neteq = (sstky - prstkcy)/atql1
* book debt - assets ratio
gen bdat = bdebt/atq

* Covenant control variables
* net worth-assets ratio
gen nworth = atq-ltq
gen nwat = nworth/atq
* net working capital-assets ratio
gen nwc = actq - lctq
gen nwcat = nwc/atq
* cash-assets ratio
gen cashat = cheq/atq
* EBITDA-lag assets ratio
gen ebitda = oibdpq/atql1
* Cash flow-lag assets ratio
gen cf = ibq+dpq
gen cfat = cf/atql1
* net income-lag assets ratio
gen niat = niq/atql1
* interest expense-lag assets ratio
gen intat = xintq/atql1

*** Other controls
* market-to-book
gen mv = prccq*cshoq
gen bv = actq - lctq - pstkl + txditcq
gen mbr = mv/bv
* tangible asset ratio
gen tatr = ppentq/atq
* ln assets
gen lnat = ln(atq)

* generate lags
foreach v in netdebt neteq bdat nwat nwcat cashat ebitda cfat niat intat mbr tatr lnat{
	gen `v'l1 = L.`v'
}

foreach v in saleq ppentq ltq nworth cheq nwc oibdpq cf niq xintq bv mv{
	gen `v'l1 = L.`v'
}

duplicates drop gvkey fqtr fyear, force
save "Compustat_finviolation", replace


***********************  Merge violation with Compustat  ***********************
*** since the datadate in financial convenant violation is calendar date, I want to convert it into fiscal year and quarter.
use "financial convenant violation", replace

* generate time variables
gen year=substr(datadate,5,4)
gen month=substr(datadate,1,2)
gen day=substr(datadate,3,2)
destring year month day, replace

/*
* gvkey format
gen gvkey_str = string(gvkey, "%06.0f")
drop gvkey
ren gvkey_str gvkey
*/

* merge with compustat to get fiscal year-quarter
merge 1:m year month day gvkey using "Compustat_finviolation", keepusing(datafqtr fyr fyear) keep(1 3) 
gen fqtr = substr(datafqtr, 6, 1)
destring fqtr, replace
drop _m
save "financial convenant violation_f", replace


*** Merge with Compustat
use "financial convenant violation_f", replace
keep if fyear<=2006
drop if missing(datafqtr)
merge 1:1 gvkey fqtr fyear using "Compustat_finviolation", keep(1 3) force
drop if fyear>=2006

*** restrict the sample with nonmissing data for both thecontemporaneous and lagged value of financial variables
xtset gvkey rank

foreach v in saleq ppentq ltq nworth cheq nwc oibdpq cf niq xintq bv mv mbr atq{
	keep if !mi(`v') & !mi(`v'l1)
}

*** drop the firms without at least four consecutive quarters of records
* Generate a variable indicating the consecutive status for each firm-quarter
sort gvkey rank
bysort gvkey: gen consec = (rank == rank[_n-1]+1)
bysort gvkey: replace consec = 1 if _n==1
bysort gvkey: gen drop_tag = 1 if consec == 0 & (consec[_n-1]==0 | consec[_n-2]==0 | consec[_n-3]==0 | consec[_n-4]==0 | consec[_n+1]==0 | consec[_n+2]==0 | consec[_n+3]==0 | consec[_n+4]==0)
* Keep only the observations where consec_count is at least 4
drop if drop_tag==1


* winsorize the variables
foreach v in netdebtl1 neteql1 bdatl1 nwatl1 nwcatl1 cashatl1 ebitdal1 cfatl1 niatl1 intatl1 mbrl1 tatrl1 lnatl1{
	winsor `v', gen(`v'W) p(0.05)
}

* change into basis point
replace netdebt = 10000*netdebt
replace neteq = 10000*neteq
replace netdebtl1W = 10000*netdebtl1W
replace neteql1W = 10000*neteql1W

replace neteql1W = 0 if mi(neteql1W)
replace netdebtl1W = 0 if mi(netdebtl1W)

tabstat netdebtl1W neteql1W bdatl1W nwatl1W nwcatl1W cashatl1W ebitdal1W cfatl1W niatl1W intatl1W mbrl1W tatrl1W lnatl1W, s(mean p50 sd) col(stat) f(%7.3f)

*** Table II
eststo clear
estpost sum netdebtl1W neteql1W bdatl1W nwatl1W nwcatl1W cashatl1W ebitdal1W cfatl1W niatl1W intatl1W mbrl1W tatrl1W lnatl1W, d
eststo

label var netdebtl1W "Net debt issuance (basis points)"
label var neteql1W "Net equity issuance (basis points)"
label var bdatl1W "Book debt$_t$/assets$_t$"
label var nwatl1W "Net worth$_t$/assets$_t$"
label var nwcatl1W "Net working capital$_t$/assets$_t$"
label var cashatl1W "Cash$_t$/assets$_t$"
label var ebitdal1W "EBITDA$_t$/assets$_{t-1}$"
label var cfatl1W "Cash flow$_t$/assets$_{t-1}$"
label var niatl1W "Net income$_t$/assets$_{t-1}$"
label var intatl1W "Interest expense$_t$/assets$_{t-1}$"
label var mbrl1W "Market-to-book ratio$_t$"
label var tatrl1W "Tangibble assets$_t$/assets$_t$"
label var lnatl1W "Ln(assets$_t$)"


esttab * using "financial_viol_table2.csv", ///
	cells("mean(fmt(%9.3fc)) sd(fmt(%9.3fc)) p50(fmt(%9.3fc))") nonumber ///
	replace nomtitle nonote noobs label collabels("Mean" "SD" "Median")



* prepare for 'Has S&P ratings' 
preserve
use "Compustat_finviolation", replace
destring gvkey, replace
keep gvkey fyr
duplicates drop gvkey, force
duplicates report gvkey
tempfile fyr
save "`fyr'"
restore

preserve
use "ratings", replace
gen HasRatingmonth = (!mi(splticrm) | !mi(spsdrm) | !mi(spsticrm))
*** there is no fiscal quarter and fiscal year in ratings, so I need to infer these using the datadate
gen year=year(datadate)
gen month=month(datadate)
gen day=day(datadate)
destring gvkey, replace
merge m:1 gvkey using "`fyr'", keep(1 3)

gen fqtr = 1
replace fqtr = 2 if (month<fyr & 4 <= month+12-fyr <= 6) | (month>fyr & 4 <= month-fyr <= 6)
replace fqtr = 3 if (month<fyr & 7 <= month+12-fyr <= 9) | (month>fyr & 7 <= month-fyr <= 9)
replace fqtr = 4 if (month<fyr & 10 <= month+12-fyr < 12) | (month==fyr)
gen fyear=.
replace fyear = year if month>fyr
replace fyear = year-1 if month<=fyr

bys gvkey fyear fqtr: egen HasRating = max(HasRatingmonth)

drop if mi(fyr)
keep gvkey fyear fqtr HasRating
duplicates drop gvkey fyear fqtr, force
tempfile ratings
save "`ratings'"
restore

drop _m
merge m:1 gvkey fyear fqtr using "`ratings'", keep(1 3) force
replace HasRating=0 if mi(HasRating)

******** Table III
xtset gvkey rank
* generate calendar quarter
gen cqtr=.
replace cqtr = 10*year+1 if 1 <= month & month <= 3
replace cqtr = 10*year+2 if 4 <= month & month <= 6
replace cqtr = 10*year+3 if 7 <= month & month <= 9
replace cqtr = 10*year+4 if 10 <= month & month <= 12
winsor netdebt, gen(netdebtW) p(0.05)

*** Panel A
* Col 1
eststo clear
reghdfe netdebtW viol L.viol, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "None"
eststo

* Col 2
reghdfe netdebtW viol L.viol L.HasRating lnatl1W tatrl1W mbrl1W bdatl1W nwatl1W cashatl1W ebitda ebitdal1W cfat cfatl1W niat niatl1W intat intatl1W, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "Covenant control variables"
eststo


* Col 3
gen bdatl1W_cfatl1W = bdatl1W*cfatl1W
gen bdatl1W_ebitdal1W = bdatl1W*ebitdal1W
gen bdatl1W_nwatl1W = bdatl1W*nwatl1W
gen ebitdal1W_intatl1W = ebitdal1W*intatl1W

reghdfe netdebtW viol L.viol L.HasRating lnatl1W tatrl1W mbrl1W bdatl1W nwatl1W cashatl1W ebitda ebitdal1W cfat cfatl1W niat niatl1W intat intatl1W bdatl1W_cfatl1W bdatl1W_ebitdal1W bdatl1W_nwatl1W ebitdal1W_intatl1W, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "Covenant control variables, Covenant interaction control variables"
eststo


* Col 4
foreach v in bdatl1W nwatl1W cashatl1W ebitda ebitdal1W cfat cfatl1W niat niatl1W intat intatl1W bdatl1W_cfatl1W bdatl1W_ebitdal1W bdatl1W_nwatl1W ebitdal1W_intatl1W{
	gen `v'_sq = `v'^2
	gen `v'_cb = `v'^3
	egen `v'_tile = xtile(`v'), nq(5)
}

reghdfe netdebtW viol L.viol L.HasRating lnatl1W tatrl1W mbrl1W bdatl1W nwatl1W cashatl1W ebitda ebitdal1W cfat cfatl1W niat niatl1W intat intatl1W bdatl1W_cfatl1W bdatl1W_ebitdal1W bdatl1W_nwatl1W ebitdal1W_intatl1W *_sq *_cb i.*_tile, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "Control variables, control variables squared, control variables to the third power, and quintile indicators for each control"
eststo

esttab using "financial_viol_table3_A.csv", ///
	compress replace nonotes keep(viol L.viol) nobaselevels ///
	r2 se star(* 0.10 ** 0.05 *** 0.01) nomtitles nogap ///
	coeflabels(viol "Covenant violation$_t$" L.viol "Covenant violation$_{t-1}$") ///
	stats(Covenant_Control_Variables N r2, labels("Covenant Control Variables" "N" "$ R^2$ ")) ///
	prehead("{\begin{tabular}{l*{4}{wc{0.1\textwidth}}}\hline\hline&\multicolumn{4}{c}{Dependent variable: net debt issuance$_t$/assets$_{t-1}$(Basis Points)}\\")




*** Panel B
* Col 1
eststo clear
reghdfe D.netdebtW viol L.viol, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "None"
eststo


* Col 2
reghdfe D.netdebtW viol L.viol LD.HasRating D.lnatl1W D.tatrl1W D.mbrl1W D2.bdatl1W D2.nwatl1W D.cashatl1W D.ebitda D.ebitdal1W D.cfat D.cfatl1W D.niat D.niatl1W D.intat D.intatl1W, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "Covenant control variables"
eststo


* Col 3
reghdfe D.netdebtW viol L.viol LD.HasRating D.lnatl1W D.tatrl1W D.mbrl1W D.bdatl1W D.nwatl1W D.cashatl1W D.ebitda D.ebitdal1W D.cfat D.cfatl1W D.niat D.niatl1W D.intat D.intatl1W D2.bdatl1W_cfatl1W D2.bdatl1W_ebitdal1W D2.bdatl1W_nwatl1W D.ebitdal1W_intatl1W, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "Covenant control variables, Covenant interaction control variables"
eststo


* Col 4
reghdfe D.netdebtW viol L.viol LD.HasRating D.lnatl1W D.tatrl1W D.mbrl1W D.bdatl1W D.nwatl1W D.cashatl1W D.ebitda D.ebitdal1W D.cfat D.cfatl1W D.niat D.niatl1W D.intat D.intatl1W D2.bdatl1W_cfatl1W D2.bdatl1W_ebitdal1W D2.bdatl1W_nwatl1W D.ebitdal1W_intatl1W D.*_sq D.*_cb i.*_tile, vce(cluster gvkey) absorb(rank cqtr)
estadd local Covenant_Control_Variables "Control variables, control variables squared, control variables to the third power, and quintile indicators for each control"
eststo

esttab using "financial_viol_table3_B.csv", ///
	compress replace nonotes keep(viol L.viol) nobaselevels ///
	r2 se star(* 0.10 ** 0.05 *** 0.01) nomtitles nogap ///
	coeflabels(viol "Covenant violation$_t$" L.viol "Covenant violation$_{t-1}$") ///
	stats(Covenant_Control_Variables N r2, labels("Covenant Control Variables" "N" "$ R^2$ ")) ///
	prehead("{\begin{tabular}{l*{4}{wc{0.1\textwidth}}}\hline\hline&\multicolumn{4}{c}{Dependent variable: Change in net debt issuance$_t$/assets$_{t-1}$(Basis Points)}\\")



log close

