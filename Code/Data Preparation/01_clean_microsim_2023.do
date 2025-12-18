********************************************************************************
//				Prepare Non-Labor Income Vector: Microsim 2023				//
********************************************************************************
//Paths
//Marta 
global data "C:\Users\wb562318\OneDrive - WBG\Documents\POV-SAR\SL\PA\Analysis\Data"
global lfs  $data/LFS
global hies $data/HIES
global output "C:\Users\wb562318\OneDrive - WBG\Documents\POV-SAR\SL\PA\Analysis\Out"


********************************************************************
//			Labor Income 
********************************************************************
use "$data/poverty_simulations_10022023.dta", clear 

drop ncpi23

*2019 to 2023
gen ncpi23=203.8/88.034
label var ncpi23 "Correction of 2023 prices to 2019 terms" //series rebased in 2022

gen ind_wages2023_real =   ind_wage2023_nom / ncpi23

egen inc_selfemp_mon23 = rowtotal(agri_profit2023 industry_profit2023 services_profit2023) , missing
gen inc_selfemp_mon23_real = inc_selfemp_mon23/ncpi23 

su 	ind_wages2023_real if ind_wages2023_real >0, d  
su inc_selfemp_mon23_real , d	

// Using total labor income for now: we might need to refine to use the s2s imputation definition 
egen labor_income23_real = rowtotal(inc_selfemp_mon23_real ind_wages2023_real) , missing

keep district sector month psu snumber hhno nhh result person_serial_no ind_wages2023_real inc_selfemp_mon23_real labor_income23

save "$data/labor_income_microsim23.dta", replace 

********************************************************************
//			Expenditure: latest sims 
********************************************************************
use "$data/MPO_srilanka_AM25.dta" , clear 
gen welfare23 = rpccons23

keep district sector month psu snumber hhno nhh result person_serial_no welfare23

save "$data/welfare23" , replace 
********************************************************************
//			Non-Labor Income 
********************************************************************

use "$data/poverty_simulations_05012024.dta", clear

merge 1:1 district sector month psu snumber hhno nhh result person_serial_no using "$data/labor_income_microsim23.dta", nogen

merge 1:1 district sector month psu snumber hhno nhh result person_serial_no using "$data/welfare23.dta", nogen 


svy: mean elder if ben_elderallow==1
scalar mean_elder=e(b)[1,1]
svy: mean tb if ben_kidallow==1
scalar mean_ckd=e(b)[1,1]
svy: mean disab if ben_disaballow==1
scalar mean_disab=e(b)[1,1]
svy: mean pension if pension!=. & pension>0 & (main_activity==2 | main_activity==4)
scalar mean_pension=e(b)[1,1]
svy: mean samurdhi_pc if samurdhi_hh==1
scalar mean_samurdhi=e(b)[1,1]

**2019 to 2020
gen ncpi20=137.6/129.6 //correction of 2020 prices to 2019 terms
label var ncpi20 "Correction of 2020 prices to 2019 terms"

**2019 to 2021
gen ncpi21=147.2/129.6 //correction of 2021 prices to 2019 terms
label var ncpi21 "Correction of 2021 prices to 2019 terms"

*2019 to 2022
gen ncpi22=221.5/129.6 //correction of 2022 prices to 2019 terms
label var ncpi22 "Correction of 2022 prices to 2019 terms"

*2019 to 2023
gen ncpi23=203.8/88.034
label var ncpi23 "Correction of 2023 prices to 2019 terms" //series rebased in 2022

**2019 - expanded beneficiary list**
gen elder_allowance2019=elder 
replace elder_allowance2019=mean_elder if ben_elder19==1   & ben_elderallow==0

gen ckd_allowance2019=tb
replace ckd_allowance2019=mean_ckd if ben_ckd19==1   & ben_kidallow==0

gen disab_allowance2019=disab
replace disab_allowance2019=mean_disab if ben_disab19==1   & ben_disaballow==0

gen samurdhi_2019=samurdhi_pc
replace samurdhi_2019=mean_samurdhi if samurdhi_hh19==1 & samurdhi_hh==0

gen pension2019 = pension

egen total_transfers2019 = rowtotal(elder_allowance2019 ckd_allowance2019 disab_allowance2019 samurdhi_2019 pension2019)
label var total_transfers2019 "Total transfers from social protection, including pensions, 2019"

egen inocct_m19 = rowtotal(elder_allowance2019 ckd_allowance2019 disab_allowance2019 samurdhi_2019)
label var inocct_m19 "Total transfers from social protection, 2019 (nominal)"

gen ind_remit19 = remit_monthly

gen ind_othinc19 = ind_othincome - pension - disab - samurdhi  -elder - tb - remit_monthly
su ind_othinc19  , d 

egen icap19	=rowtotal(property dividend) 
su icap19 , d

egen labor_income19 = rowtotal(ind_wages agri_profit industry_profit services_profit)

**2020 - further expansion for CKD, growth in pension**
gen elder_allowance2020=elder 
replace elder_allowance2020=mean_elder if ben_elder19==1   & ben_elderallow==0

gen ckd_allowance2020=tb
replace ckd_allowance2020=mean_ckd if ben_ckd19==1   & ben_kidallow==0
replace ckd_allowance2020=mean_ckd if ben_ckd20==1   & ben_ckd19==0

gen disab_allowance2020=disab
replace disab_allowance2020=mean_disab if ben_disab19==1   & ben_disaballow==0

gen samurdhi_2020=samurdhi_pc
replace samurdhi_2020=mean_samurdhi if samurdhi_hh19==1 & samurdhi_hh==0

gen pension2020 = pension*1.0949

gen oneoff_transfers2020 = total_extra_transfers_pc

egen total_transfers2020 = rowtotal(elder_allowance2020 ckd_allowance2020 disab_allowance2020 samurdhi_2020 pension2020 oneoff_transfers2020)
label var total_transfers2020 "Total transfers from social protection, including pensions and one-off payments, 2020 (nominal)"

gen ind_othinc20 = ind_othinc19 //other incomes stay fixed in nominal terms
gen icap20 = icap19 

foreach var of varlist elder_allowance2020 ckd_allowance2020 disab_allowance2020 samurdhi_2020 pension2020  oneoff_transfers2020 total_transfers2020 ind_othinc20 icap20{
	gen `var'_real = `var'/ncpi20
}

//ind_remit20 is already in 2019 terms

label var total_transfers2020_real "Total transfers from social protection, including pensions and one-off payments, 2020 (in 2019 Rs)"

**2021 - no changes other than growth in pension**
gen elder_allowance2021=elder_allowance2020

gen ckd_allowance2021=ckd_allowance2020

gen disab_allowance2021=disab_allowance2020

gen samurdhi_2021=samurdhi_2020

gen pension2021 = pension*1.0949*1.0303

gen oneoff_transfers2021 = total_extra_transfers_pc21 

egen total_transfers2021 = rowtotal(elder_allowance2021 ckd_allowance2021 disab_allowance2021 samurdhi_2021 pension2021 oneoff_transfers2021)
label var total_transfers2021 "Total transfers from social protection, including pensions and one-off payments, 2021 (nominal)"

gen ind_othinc21 = ind_othinc20 //other incomes stay fixed in nominal terms
gen icap21 = icap20 
//ind_remit21 is already in 2019 terms


foreach var of varlist elder_allowance2021 ckd_allowance2021 disab_allowance2021 samurdhi_2021 pension2021 oneoff_transfers2021 total_transfers2021 ind_othinc21 icap21 {
	gen `var'_real = `var'/ncpi21
}

label var total_transfers2021_real "Total transfers from social protection, including pensions and one-off payments, 2021 (in 2019 Rs)"


**2022 - no changes for first four months, then different benefit amounts from May to October, then revert to previous for Nov/Dec
gen elder_allowance2022=elder_allowance2021 if month<4 | month>10
replace elder_allowance2022=5002 if ben_elder22==1   & month>=5 & month<=10

gen ckd_allowance2022=ckd_allowance2021 if month<4 | month>10
replace ckd_allowance2022=7178 if ben_ckd22==1     & month>=5 & month<=10

gen disab_allowance2022=disab_allowance2021 if month<4 | month>10
replace disab_allowance2022=6339 if ben_disab22==1   & month>=5 & month<=10

gen samurdhi_2022=samurdhi_2021 if month<4 | month>10
replace samurdhi_2022=5879/hhmem if samurdhi_hh22==1 & month>=5 & month<=10

gen pension2022 = pension2021

gen ind_othinc22 = ind_othinc21 //other incomes stay fixed in nominal terms

egen total_transfers2022 = rowtotal(elder_allowance2022 ckd_allowance2022 disab_allowance2022 samurdhi_2022 pension2022)
label var total_transfers2022 "Total transfers from social protection, including pensions, 2022 (nominal)"

egen inocct_m22 = rowtotal(elder_allowance2022 ckd_allowance2022 disab_allowance2022 samurdhi_2022)
label var inocct_m22 "Total transfers from social protection, including pensions, 2022 (nominal)"

gen icap22 = icap21 

egen inc_selfemp_mon22 = rowtotal(agri_profit2022 industry_profit2022 services_profit2022)

egen labor_income22 = rowtotal(ind_wage2022_nom inc_selfemp_mon22 )

foreach var of varlist elder_allowance2022 ckd_allowance2022 disab_allowance2022 samurdhi_2022 pension2022 total_transfers2022 ind_othinc22 inocct_m22 icap22 labor_income22 {
	gen `var'_real = `var'/ncpi22
	
}
label var total_transfers2022_real "Total transfers from social protection, including pensions and one-off payments, 2022 (in 2019 Rs)"


local months "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec"
local remit_2022 "-59.40 -63.25 -32.44 -22.35 18.88 3.36 11.28 31.18 82.49 102.22 154.94 163.75"
local remit_2023 "203.61 256.73 129.89 83.50 36.29 45.03 71.25 36.65 19.58 30.18 26.26 7.78"
local remit_2024 "-1.24 0.92 -6.56 11.59 9.66 10.15 -0.32 8.16 7.46 2.77 -12.24 -3.82"

**2023** - pensions and other income don't change, four main transfers are same as status quo until June 2023, then nothing from July onwards as switch to Aswesuma

gen samurdhi_2023 = samurdhi_2021 if month<7
replace samurdhi_2023 = 0 if month>=7

gen elder_allowance2023 = elder_allowance2021 if month<7
replace elder_allowance2023 = 0 if month>=7

gen ckd_allowance2023 = ckd_allowance2021 if month<7
replace ckd_allowance2023 = 0 if month>=7

gen disab_allowance2023 = disab_allowance2021 if month<7
replace disab_allowance2023 = 0 if month>=7

gen pension2023 = pension2022

gen ind_othinc23 = ind_othinc22

gen icap23 = icap22 

egen total_transfers2023 = rowtotal(elder_allowance2023 ckd_allowance2023 disab_allowance2023 samurdhi_2023 pension2023)
label var total_transfers2023 "Total transfers from social protection, including pensions, 2023 (nominal)"


foreach var of varlist elder_allowance2023 ckd_allowance2023 disab_allowance2023 samurdhi_2023 pension2023 ind_othinc23 total_transfers2023 icap23 {
	gen `var'_real = `var'/ncpi23
}
label var total_transfers2023_real "Total transfers from social protection, including pensions and one-off payments, 2023 (in 2019 Rs)"

//Remittances for 2022/23

drop ind_remit22
//Apply rupee growth rate then apply deflator for 2020 and 2021 values
gen ind_remit22_nom=.
gen ind_remit23_nom=.
gen ind_remit22=.
gen ind_remit23=.

forvalues j = 1/12 {
	local z : word `j' of `remit_2022'
	local u : word `j' of `remit_2023'
	
	replace ind_remit22_nom     = (ind_remit21_nom) *(1+`z'/100)  if month==`j'
	replace ind_remit23_nom     = (ind_remit22_nom) *(1+`u'/100)  if month==`j'

	replace ind_remit22     = ind_remit22_nom    /(ncpi22) 
	replace ind_remit23     = ind_remit23_nom  /(ncpi23) 
	}

 
	
//keep district sector month psu snumber hhno nhh result person_serial_no ncpi20-total_transfers2023_real ind_remit19 ind_remit20 ind_remit21 ind_remit22 ind_remit23
//rename to match SARMD variables 
gen 	ijubi22 	= pension2022_real
drop icap22
gen 	icap22  	= icap22_real
gen 	itranext_m22= ind_remit22
drop inocct_m22 
gen 	inocct_m22 	= inocct_m22_real 
ren rpccons_imperf_2022b welfare22

gen 	ijubi23 	= pension2023_real
drop 	icap23 
gen 	icap23  	= icap23_real
gen 	itranext_m23= ind_remit23
egen 	inocct_m23 	=rowtotal(elder_allowance2023_real ckd_allowance2023_real disab_allowance2023_real samurdhi_2023_real)

////////////////////////////////////////////////////////////////////////////////
// 		Check shares in 2019
////////////////////////////////////////////////////////////////////////////////
egen ynl19 			=  rowtotal (pension2019 inocct_m19 ind_othinc19 ind_remit19 icap19 ) 

gen sh_pensions19 	= pension2019	/ rpccons
gen sh_inocct_m19 	= inocct_m19 / rpccons
gen sh_icap19		= icap19 /rpccons 
gen sh_remit19 		= ind_remit19 / rpccons
gen sh_ynl19 		= ynl19 / rpccons 

xtile quintiles19= rpccons [aw=weight], nq(5)

tabstat sh_pensions19 , by(quintiles19) //2% b20, 6% t20, avg 3.6%
tabstat sh_inocct_m19 , by(quintiles19) //10%b20, 0.4% t20, avg 4%
tabstat sh_icap19 , by(quintiles19) // 1.7 %b20, 6%t20 , avg 2.3%
tabstat sh_remit19 , by(quintiles19) //1.5%b20, 4.2% t20 , avg 2.9%
tabstat sh_ynl19 , by(quintiles19) //21%b20 27%t20 , avg 20.2% 

////////////////////////////////////////////////////////////////////////////////
// 		Check shares in 2022
////////////////////////////////////////////////////////////////////////////////

egen ynl22_real =  rowtotal (pension2022_real inocct_m22_real icap22_real ind_remit22)
gen sh_pensions22 = pension2022_real	/ welfare22 
gen sh_inocct_m22 = inocct_m22_real / welfare22
gen sh_icap22 	= icap22_real  /welfare22 
gen sh_remit22 	= ind_remit22 		/welfare22 
gen sh_ynl22 = ynl22_real 				/ welfare22 

xtile quintiles22 = welfare22 [aw=weight], nq(5) 
tabstat sh_pensions22 , by(quintiles22) // avg 3.3 %
tabstat sh_inocct_m22 , by(quintiles22) // avg 4.9%
tabstat sh_icap22 , by(quintiles22) // avg 2% 
tabstat sh_remit22 , by(quintiles22) //avg 3.5 
tabstat sh_ynl22 , by(quintiles22) // avg 13.9%  

////////////////////////////////////////////////////////////////////////////////
// 		Check shares in 2023
////////////////////////////////////////////////////////////////////////////////

egen ynl23_real =  rowtotal (pension2023_real inocct_m23 icap23_real ind_remit23)
gen sh_pensions23 = pension2023_real	/  welfare23 
gen sh_inocct_m23 = inocct_m23 /  welfare23
gen sh_icap23 	= icap23_real  / welfare23 
gen sh_remit23 	= ind_remit23 		/ welfare23 
gen sh_ynl23 = ynl23_real 				/  welfare23 

xtile quintiles23 =  welfare23 [aw=weight], nq(5) 
tabstat sh_pensions23 , by(quintiles23) // avg 2.6 %
tabstat sh_inocct_m23 , by(quintiles23) // avg 1.4%
tabstat sh_icap23 , by(quintiles23) // avg 1.6% 
tabstat sh_remit23 , by(quintiles23) //avg 3.5 
tabstat sh_ynl23 , by(quintiles23) // avg 13.9%  

tabstat sh_inocct_m19 if inocct_m19>0, by(quintiles19)
tabstat sh_inocct_m22 if inocct_m22>0, by(quintiles22)
tabstat sh_inocct_m23  if inocct_m23>0, by(quintiles23)

/*

///////////////////////////////////////////////////////////////////////////////
// 			As shares of labor income 
///////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// 		Check shares in 2019
////////////////////////////////////////////////////////////////////////////////

egen ynl19 			=  rowtotal (pension2019 inocct_m19 ind_othinc19 ind_remit19 icap19 )
gen sh_pensions19 	= pension2019	/ labor_income19
gen sh_inocct_m19 	= inocct_m19 / labor_income19
gen sh_icap19		= icap19 /labor_income19 
gen sh_remit19 		= ind_remit19 / labor_income19
gen sh_ynl19 		= ynl19 / labor_income19 

xtile quintiles19= labor_income19 if labor_income19>0 [aw=weight], nq(5)

tabstat sh_pensions19 if labor_income19>0, by(quintiles19) //2% b20, 6% t20, avg 3.6%
tabstat sh_inocct_m19 if labor_income19>0, by(quintiles19) //10%b20, 0.4% t20, avg 4%
tabstat sh_icap19 if labor_income19>0, by(quintiles19) // 1.7 %b20, 6%t20 , avg 2.3%
tabstat sh_remit19 if labor_income19>0, by(quintiles19) //1.5%b20, 4.2% t20 , avg 2.9%
tabstat sh_ynl19 if labor_income19>0 , by(quintiles19) //21%b20 27%t20 , avg 20.2% 

////////////////////////////////////////////////////////////////////////////////
// 		Check shares in 2022
////////////////////////////////////////////////////////////////////////////////

egen ynl22_real =  rowtotal (pension2022_real inocct_m22_real icap22_real ind_remit22)
gen sh_pensions22 = pension2022_real / labor_income22_real
gen sh_inocct_m22 = inocct_m22_real / labor_income22_real
gen sh_icap22 	= icap22_real  		/labor_income22_real 
gen sh_remit22 	= ind_remit22 		/labor_income22_real 
gen sh_ynl22 = ynl22_real 			/ labor_income22_real 

xtile quintiles22 = labor_income22_real if labor_income22_real>0[aw=weight], nq(5) 
tabstat sh_pensions22 if labor_income22_real>0, by(quintiles22) // avg 3.3 %
tabstat sh_inocct_m22 if labor_income22_real>0, by(quintiles22) // avg 4.9%
tabstat sh_icap22 if labor_income22_real>0, by(quintiles22) // avg 2% 
tabstat sh_remit22 if labor_income22_real>0, by(quintiles22) //avg 3.5 
tabstat sh_ynl22 if labor_income22_real>0, by(quintiles22) // avg 13.9%  

////////////////////////////////////////////////////////////////////////////////
// 		Check shares in 2023
////////////////////////////////////////////////////////////////////////////////

egen ynl23_real =  rowtotal (pension2023_real inocct_m23 icap23_real ind_remit23)
gen sh_pensions23 = pension2023_real	/  labor_income23_real 
gen sh_inocct_m23 = inocct_m23 /  labor_income23_real
gen sh_icap23 	= icap23_real  / labor_income23_real 
gen sh_remit23 	= ind_remit23 		/ labor_income23_real 
gen sh_ynl23 = ynl23_real 				/  labor_income23_real 

xtile quintiles23 =  labor_income23_real if labor_income23_real>0 [aw=weight], nq(5) 
tabstat sh_pensions23 if labor_income23_real>0 , by(quintiles23) // avg 2.6 %
tabstat sh_inocct_m23  if labor_income23_real>0, by(quintiles23) // avg 1.4%
tabstat sh_icap23  if labor_income23_real>0, by(quintiles23) // avg 1.6% 
tabstat sh_remit23 if labor_income23_real>0 , by(quintiles23) //avg 3.5 
tabstat sh_ynl23  if labor_income23_real>0, by(quintiles23) // avg 13.9%  
*/ 



keep district sector month psu snumber hhno nhh result person_serial_no ijubi2* icap2* itranext_m2* inocct_m2* welfare2* labor_income23_real

destring district sector month psu snumber hhno nhh result person_serial_no , replace

save "$data/inc_microsim23.dta" , replace



