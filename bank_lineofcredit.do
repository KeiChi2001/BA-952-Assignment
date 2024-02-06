*********************  Compustat Data Processing *************************

*** import raw Compustat
clear all
cd "D:\Documents\study\MSyear1\Empirical Corp Fin\Assignment 1"

log using "bankline.log", replace

use "Compustat_93-05.dta", replace

*** Solve the duplication problem
duplicates report gvkey fyear
duplicates tag gvkey fyear, gen(tag)
* the duplicates are because the rows with indfmt=="FS" has missing variables but indfmt==="INDL" don't.
drop if indfmt=="FS"
duplicates report gvkey fyear
save "Compustat_93-05_nodup.dta", replace

use "Compustat_93-05_nodup.dta", replace

*** Adding the approximate ipo date
preserve
use "Compustat_ipo.dta", clear
sort gvkey fyear
drop if mi(prcc_f)
* define the IPO year as the first year that a firm has record of closing price in Compustat 
bysort gvkey: egen ipo_yr = min(fyear)
keep gvkey ipo
duplicates drop
tempfile ipo
save "`ipo'"
restore

merge m:1 gvkey using "`ipo'", keep(1 3)
drop _m

* Adding the S&P indicator: how does he define this indicator? (firm, or firm-year) Here I just follow this: if a firm has been a S&P member, then it has sp==1.
preserve
use "SPMIM_data", replace
tostring gvkey, replace format(%06.0f)
gen sp = (!mi(spmim))
bys gvkey: egen max_sp = max(sp)
keep if max_sp == 1
keep gvkey max_sp
duplicates drop
tempfile spmim
save "`spmim'"
restore

merge m:1 gvkey using "`spmim'"
drop if _m==2
replace max_sp = 0 if max_sp == 1
replace max_sp = 1 if mi(max_sp)
ren max_sp sp
drop _m

/*
* Adding the tangible asset data
preserve
use "Compustat_ppe.dta", clear
drop if indfmt=="FS"
tempfile ppe
save "`ppe'"
restore
merge 1:1 gvkey fyear using "`ppe'", keepusing(ppent) nogen keep(3)



* Adding the stock exchange information
preserve
use "exchg.dta", clear
duplicates drop exchg gvkey fyear, force
gen otc = (exchg==19)
keep gvkey fyear otc
tempfile otc
save "`otc'"
restore

merge 1:1 gvkey fyear using "`otc'", keep(3) nogen
*/


/*
* Adding SIC information
preserve
use "SIC", replace
keep gvkey fyear sic
duplicates drop
tempfile sic
save "`sic'"
restore

merge 1:1 gvkey fyear using "`sic'", keep(3) nogen
*/

********************** fill in missing values to avoid missing mblc ***********
replace txditc=0 if mi(txditc)
replace pstkl=0 if mi(pstkl)

****************************** Generating variables **************************
destring gvkey, replace
xtset gvkey fyear
gen age = fyear-ipo_yr
gen asslc = at - che
gen cflc = oibdp/asslc
gen bd = (dlc+dltt)/at
gen tanglc = ppent/asslc
gen nwlc = (asslc-lt)/asslc
gen bveq = at - lt - pstkl + txditc
gen mkeq = csho * prcc_f
gen mblc = (at - bveq + mkeq - che)/asslc
gen otc = (exchg==19)


foreach v in asslc cflc bd tanglc nwlc mblc{
	gen `v'l1 = L.`v'
}


******************** Merge sufi_data with Compustat **************************
/*
preserve
use "sufi_rfs_linesofcredit20070221data", replace
ren yeara fyear
tostring gvkey, replace format(%06.0f)
save "sufi_rfs_linesofcredit20070221data", replace
restore
*/

tostring gvkey, replace format(%06.0f)
merge 1:1 gvkey fyear using "sufi_rfs_linesofcredit20070221data"
keep if _m==3
drop _m

*** Construct industry sales 
gen sic3 = substr(sic,1,3)

preserve
use "Compustat_quarter_sales", replace
destring gvkey, replace
gen fyq = 100*fyearq+fqtr
drop if mi(datafqtr)
keep fyq
duplicates drop
egen q_num = rank(fyq), track
tempfile rank
save "`rank'"
restore

preserve
use "Compustat_quarter_sales", replace
destring gvkey, replace
gen fyq = 100*fyearq+fqtr
drop if mi(datafqtr)
merge m:1 fyq using "`rank'", keep(3) nogen
xtset gvkey q_num
* changes of sales
gen chg_sales = saleq - L.saleq
* calculate the standard deviation of the changes in sales within each SIC 3-digit industry
bys gvkey fyear: egen avg_asset = mean(at)
bys gvkey fyear: egen sd_csales = sd(chg_sales)
gen sd_csales_at = sd_csales/avg_asset
gen sic3 = substr(sic,1,3)
bys sic3 fyear: egen sd_csales_med = median(sd_csales_at)
ren fyearq fyear
keep sic3 fyear sd_csales_med
duplicates drop
tempfile ind_var
save "`ind_var'"
restore

merge m:1 sic3 fyear using "`ind_var'", keep(1 3) nogen
ren sd_csales_med q_salesvar

*** Construct cash-flow volatility
preserve
use "Compustat_lag", replace
keep gvkey fyear oibdp at che
foreach v in oibdp at che{
	replace `v' = 0 if mi(`v') 
}
duplicates drop gvkey fyear, force
* generate noncash asset
gen noncash_at = at-che

destring gvkey, replace
xtset gvkey fyear
* change of ebitda
gen chg_ebitda = oibdp - L.oibdp
* fill the empty values
replace chg_ebitda = 0 if mi(chg_ebitda)

*** calculate the mean of changes in ebitda
gen mean_4yr = (L.chg_ebitda + L2.chg_ebitda + L3.chg_ebitda + L4.chg_ebitda)/4
* standard deviation of changes in ebitda
gen var_4yr = ((L.chg_ebitda - mean_4yr)^2 + (L2.chg_ebitda - mean_4yr)^2 + (L3.chg_ebitda - mean_4yr)^2 + (L4.chg_ebitda - mean_4yr)^2)/4
gen sd_4yr = sqrt(var_4yr)
gen avg_noncash = (L.noncash_at + L2.noncash_at + L3.noncash_at + L4.noncash_at)/4
gen sd_4yr_at = sd_4yr/avg_noncash
tostring gvkey, replace format(%06.0f)
ren sd_4yr_at cfvar
tempfile cfv
save "`cfv'"
restore

merge 1:1 gvkey fyear using "`cfv'", keep(3) nogen
replace cfvar=0 if mi(cfvar) // fill the missing values

* winsorize the variables
foreach v in cflcl1 tanglcl1 nwlcl1 asslcl1 mblcl1 cfvar q_salesvar{
	winsor `v', generate(`v'W) p(0.05)
	sum `v'W, d
}

************************* Summary Statistics (Table 1) **********************
*** full sample, using winsorized variables
eststo clear
estpost sum lineofcredit bd cflcl1W tanglcl1W nwlcl1W asslcl1 mblcl1W q_salesvarW cfvarW sp otc age, d
eststo

label var lineofcredit "Has line of credit [0,1]"
label var bd "Book debt/assets"
label var cflcl1W "EBITDA/(assets - cash)"
label var tanglcl1W "Tangible assets/(assets - cash)"
label var nwlcl1W "Net worth, cash adjusted"
label var asslcl1 "Assets - cash"
label var mblcl1W "Market-to-book, cash adjusted"
label var q_salesvarW "Industry sales volatility"
label var cfvarW "Cash-flow volatility"
label var sp "Not in an S&P index {0,1}"
label var otc "Traded over the counter {0,1}"
label var age "Firm age (years since IPO)"


esttab * using "bankline_t1_full.csv", ///
	cells("mean(fmt(%9.3fc)) sd(fmt(%9.3fc)) p50(fmt(%9.3fc))") nonumber ///
	replace nomtitle nonote noobs label collabels("Mean" "SD" "p50")



*** random sample
* generate descriptive variables
gen ra_line = line/at if randomsample==1
gen ra_lineun = lineun/at if randomsample==1
gen ra_linetot = linetot/at if randomsample==1
gen liq_linetot = linetot/(linetot+che) if randomsample==1
gen liq_lineun = lineun/(lineun+che) if randomsample==1


eststo clear
estpost sum lineofcredit_rs ra_linetot ra_lineun ra_line liq_linetot liq_lineun def bd cflcl1W tanglcl1W nwlcl1W asslcl1 mblcl1W q_salesvarW cfvarW sp otc age if randomsample==1, d
eststo

label var lineofcredit_rs "Has line of credit [0,1]"
label var ra_linetot "Total line of credit/assets"
label var ra_lineun "Unused line of credit/assets"
label var ra_line "Used line of credit/assets"
label var liq_linetot "Total line/(total line + cash)"
label var liq_lineun "Unused line/(unused line + cash)"
label var def "Violation of financial covenant {0,1}"
label var bd "Book debt/assets"
label var cflcl1W "EBITDA/(assets - cash)"
label var tanglcl1W "Tangible assets/(assets - cash)"
label var nwlcl1W "Net worth, cash adjusted"
label var asslcl1 "Assets - cash"
label var mblcl1W "Market-to-book, cash adjusted"
label var q_salesvarW "Industry sales volatility"
label var cfvarW "Cash-flow volatility"
label var sp "Not in an S&P index {0,1}"
label var otc "Traded over the counter {0,1}"
label var age "Firm age (years since IPO)"


esttab * using "bankline_t1_rs.csv", ///
	cells("mean(fmt(%9.3fc)) sd(fmt(%9.3fc)) p50(fmt(%9.3fc))") nonumber ///
	replace nomtitle nonote noobs label collabels("Mean" "SD" "p50")


*********************************** Table 3 *********************************
gen sic1 = substr(sic,1,1)
gen lasslcl1 = ln(asslcl1)
gen lfirmage = ln(age)
winsor(lasslcl1), generate(lasslcl1W) p(0.05)

*** create year fix effect
forvalues i=1997/2003{
	gen yd`i' = (fy==`i')
}

* Col 1
eststo clear
xi: dprobit lineofcredit yd* i.sic1 cflcl1W tanglcl1W lasslcl1W nwlcl1W mblcl1W q_salesvar cfvar sp otc lfirmage, cluster(gvkey)
eststo

* Col 2
xi: dprobit lineofcredit_rs yd* i.sic1 cflcl1W tanglcl1W lasslcl1W nwlcl1W mblcl1W q_salesvar cfvar sp otc lfirmage if randomsample==1, cluster(gvkey)
eststo

* Col 3
xi: regress liq_linetot yd* i.sic1 cflcl1W tanglcl1W lasslcl1W nwlcl1W mblcl1W q_salesvar cfvar sp otc lfirmage if randomsample==1, cluster(gvkey)
eststo

* Col 4
xi: regress liq_linetot yd* i.sic1 cflcl1W tanglcl1W lasslcl1W nwlcl1W mblcl1W q_salesvar cfvar sp otc lfirmage if randomsample==1 & lineofcredit==1, cluster(gvkey)
eststo

* Col 5
xi: regress liq_lineun yd* i.sic1 cflcl1W tanglcl1W lasslcl1W nwlcl1W mblcl1W q_salesvar cfvar sp otc lfirmage if randomsample==1, cluster(gvkey)
eststo

* Col 6
xi: regress liq_lineun yd* i.sic1 cflcl1W tanglcl1W lasslcl1W nwlcl1W mblcl1W q_salesvar cfvar sp otc lfirmage if randomsample==1 & lineofcredit==1, cluster(gvkey)
eststo

esttab using "bankline_t3.csv", ///
	compress replace nonotes drop(_cons _Isic1* yd*) nobaselevels ///
	r2 se star(* 0.10 ** 0.05 *** 0.01) nomtitles nogap ///
	coeflabels(cflcl1W "EBITDA/(assets-cash)" tanglcl1W "Tangible assets/(assets-cash)" lasslcl1W "Ln(assets-cash)" nwlcl1W "Net worth, cash adjusted" mblcl1W "Market-to-book, cash adjusted" q_salesvar "Industry sales volatility" cfvar "Cash flow volatility" sp "Not in an S&P index" otc "Traded over the counter" lfirmage "Ln(firm age)") ///
	stats(N r2, labels("N" "$ R^2$ "))	///
	prehead("{\begin{tabular}{l*{6}{wc{0.1\textwidth}}}\hline\hline&\multicolumn{1}{c}{Full}&\multicolumn{1}{c}{Random}&\multicolumn{1}{c}{Random}&\multicolumn{1}{c}{With line of credit}&\multicolumn{1}{c}{Random}&\multicolumn{1}{c}{With line of credit}\\")



********************************** Figure 1 **********************************
* generate the decile variable for cash flow
egen cfcat = xtile(cflcl1), nq(10)
gen cashat = che/at
label variable cashat "cash-to-asset ratio"

collect clear
table cfcat, stat(mean lineofcredit cashat) nformat(%9.4f) name(figure1)
collect preview
collect export figure1.tex, replace
collect export figure1.xlsx, replace

*** plot the graph
preserve
import excel figure1.xlsx, firstrow clear
ren (Haslineofcreditfullsample cashtoassetratio) (lineofcredit cashat)
destring cfcat, replace

#delimit ;
twoway (connected cashat cfcat, mcolor(black) msymbol(lgx) lcolor(black) lpattern(tight_dot)) 
(connected lineofcredit cfcat, yaxis(2) mcolor(black) msymbol(square) lcolor(blue) lpattern(solid)), 
ytitle("{bf:Cash/assets}", axis(1) size(small)) ylabel(0(0.1)0.6, format(%9.1f) grid glcolor(black) axis(1) labsize(small)) yscale(range(0 (0.1) 0.6) axis(1))
ytitle("{bf:Fraction with line of credit}", axis(2) size(small)) ylabel(0(0.1)1, format(%9.1f) nogrid axis(2) labsize(small)) yscale(range(0 (0.1) 1) axis(2))
xtitle("{bf: Deciles of EBITDA/(assets-cash)}", size(small)) xlabel(1(1)10, nogrid) 
legend(size(*0.7) order(1 "average cash/assets (left axis)" 2 "Fraction with line of credit (right axis)") position(6)) plotregion(lcolor(black));
#delimit cr

log close

