********************************************************************************
// Prepare Vectors for RScript
********************************************************************************
//Marta 
global data ../../Data
global lfs2019  $data/LFS
global hies2019 $data/HIES

global output ../../Out 

********************************************************************************
// 				HIES
********************************************************************************
use "$data/hies2019_clean" , clear 
xtile quintiles = welfare [aw=popwt], nq(5)
gen missing_inc =  rpcinc1==. 
gen zero_inc = rpcinc1 ==0 

tabstat missing_inc, by(quintiles)
tabstat zero_inc, by(quintiles)
drop quintiles missing_inc zero_inc

/*// Windorize incomes 
foreach var in rpcinc1 rpcinc2 rpcwage1 rpcwage2 rpcself1 rpcself2{
	sum `var' , d 
	scalar p1 =r(p1)
	scalar p99 = r(p99)
	replace `var' = p1 if `var'<p1
	replace `var' = p99 if `var'>p99
}
*/

glo spatial district urban rural
//Dependency ratio goes to zero for hh with elderly head : keep only share of kids and share of old 
glo demo hhsize age_avg num_deps num_* /*dep_ratio*/ share_dep share_kids sh_mem* edu_sh_* has_in_school sh_in_school
glo hhh buddhist_hh married_hh sinhala_hh female_hh age_hh edu_hhh*
glo assets cellphone 
glo sector hh_main_* have_agri_emp have_constr_emp have_serv_emp have_ind_emp
glo disab has_*_disab *disab_hhh
glo empstat have_public_emp have_pvt_emp have_skilled_worker have_semiskilled_worker employee employer self_employed familyworker public_hhh private_hhh  sh_selfempl sh_employee sh_ecactive
glo incomes hh_inc_primary_nc_pc hh_wages_primary_pc hh_selfemp_primary_pc /*hh_inc_sec_nc_pc hh_wages_sec_pc hh_selfemp_sec_pc */ rpcinc1 rpcwage1 rpcself1 /*rpcinc2 rpcwage2 rpcself2*/ sh_wages_pc sh_selfemp_pc
glo complit  hh_aware* hh_can* hh_use* hh_internet* hh_emailed* 
glo expenditure welfare 
glo ynl19  sh_pensionyl19 sh_icapyl19 sh_inocct_myl19 sh_remittancesyl19 sh_ynyl19 rnlincpc19 rnlincpc23 
glo microsim  sh_pensionyl23 sh_icapyl23 sh_inocct_myl23 sh_remittancesyl23 sh_ynyl23 welfare23 rlaborincpc23  

mdesc  $spatial $demo $disab $hhh $assets $sector $disab $empstat $incomes $expenditure $microsim 

svy: mean  dist_* urban rural $demo 
svy: mean $disab $hhh $assets 
svy: mean $sector $empstat 
svy: mean hh_inc_primary_nc_pc rpcinc1 
svy: mean welfare 

//To avoid dropping obs: replace missing incomes with zeros
foreach var in $incomes $ynl19 $microsim {

	gen ln_`var' = ln(`var')
	replace ln_`var' = 0 if ln_`var' ==. 
	replace `var' = 0 if `var' ==. 
	
	
/*
	sum ln_`var' , d 
	scalar p1 =r(p1)
	scalar p99 = r(p99)
	replace `var' = p1 if `var'<p1
	replace `var' = p99 if `var'>p99
*/
	}

sum sh_ynyl19 , d
sum rnlincpc19 , d 
 	
//To avoid dropping obs: replace missing if no household member in that category? Check this 
foreach var in edu_sh_1564_none have_public_emp have_pvt_emp have_skilled_worker have_semiskilled_worker sh_employee sh_selfempl {
	replace `var' = 0 if `var' ==. 
	}

//Shares of labor income from primary occupation 
gen sh_wages = rpcwage1 /  rpcinc1 
gen sh_selfemp = rpcself1 / rpcinc1

sum sh_wages sh_selfemp
replace sh_wages=0 if sh_wages<0 | sh_wages==.
replace sh_selfemp=0 if sh_selfemp<0 | sh_selfemp==. 


global incomes sh_wages sh_selfemp rpcinc1 rpcwage1 rpcself1 ln_rpcinc1 ln_rpcwage1 ln_rpcself1 rpcinc1 rpcwage1 rpcself1 

keep hhid psu weight popwt sector $spatial $demo $hhh $assets $sector $disab $empstat $incomes $expenditure $assets $ynl19 $microsim 

gen hhsize_sq = hhsize^2
gen avg_age_sq = age_avg^2

//share of consumption to labor income from 2023 microsim 
gen r23 = welfare23 / rlaborincpc23

mdesc *
sum *

gen estate = sector==3
tab sector estate 
 
save "$data/cleaned/hies2019_clean" , replace 

********************************************************************************
// 				LFS
********************************************************************************
use "$data/lfs2019_clean" , clear 

//To avoid dropping obs: replace missing incomes with zeros
foreach var in hh_paidemp_pc hh_selfemp_pc hh_selfemp_primary_pc hh_inc_pc ///
hh_wages_pc hh_wages_primary_pc hh_inc_nc_pc hh_inc_primary_nc_pc ///
rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot {

//Winsorize positive values 
	sum `var' if `var'>0 , d 
	scalar p1 =r(p1)
	scalar p99 = r(p99)
	replace `var' = p1 if `var'<p1 		& `var'!=. 
	replace `var' = p99 if `var'>p99 	& `var'!=. 
//Replace missings with zero 	
	replace `var' = 0 if `var' ==. 
//Create logs 
	gen ln_`var' = ln(`var')
	replace ln_`var' = 0 if ln_`var' ==. 


	}
	gen flag6_income = rpcinc1==0
	tabstat  flag6_income

	gen flag6_income2 = rpcinc_tot==0 
	tabstat  flag6_income2
//To avoid dropping obs: replace missing if no household member in that category? Check this 
foreach var in edu_sh_1564_none have_public_emp have_pvt_emp have_skilled_worker ///
have_semiskilled_worker sh_employee sh_selfempl ///
hh_can_activities hh_can_for_edu hh_can_for_work hh_can_use_phone {
	replace `var' = 0 if `var' ==. 
	}

//Shares of labor income from primary occupation 
gen sh_wages = rpcwage1 /  rpcinc1 
gen sh_selfemp = rpcself1 / rpcinc1

sum sh_wages sh_selfemp
replace sh_wages=0 if sh_wages<0 | sh_wages==.
replace sh_selfemp=0 if sh_selfemp<0 | sh_selfemp==. 

global incomes sh_wages sh_selfemp ln_rpcinc1 ln_rpcwage1 ln_rpcself1 rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot

keep hhid psu weight popwt sector $spatial $demo $hhh $assets $sector $disab $empstat $incomes $assets flag6_income* $complit

mdesc *
gen hhsize_sq = hhsize^2
gen avg_age_sq = age_avg^2
sum *
gen hhb_year = 2019-age_hh

gen estate = sector==3
tab sector estate 

tab district , nol
save "$data/cleaned/lfs2019_clean" , replace

********************************************************************************
// 				2023
********************************************************************************
use "$data/lfs2023_clean" , clear 
mdesc  /// labor income missing in 19% cases :/ 

//To avoid dropping obs: replace missing incomes with zeros
foreach var in hh_paidemp_pc hh_selfemp_pc hh_selfemp_primary_pc hh_inc_pc hh_wages_pc hh_wages_primary_pc hh_inc_nc_pc hh_inc_primary_nc_pc ///
rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot {

//Winsorize positive values 
	sum `var' if `var'>0 , d 
	scalar p1 =r(p1)
	scalar p99 = r(p99)
	replace `var' = p1 if `var'<p1 		& `var'!=. 
	replace `var' = p99 if `var'>p99 	& `var'!=. 

//Replace missings with zero 	
	replace `var' = 0 if `var' ==. 
//Create logs 
	gen ln_`var' = ln(`var')
	replace ln_`var' = 0 if ln_`var' ==. 

	}

	gen flag6_income = rpcinc1==0 
	tabstat  flag6_income
	gen flag6_income2 = rpcinc_tot==0 
	tabstat  flag6_income2
//To avoid dropping obs: replace missing if no household member in that category? Check this 
foreach var in edu_sh_1564_none have_public_emp have_pvt_emp have_skilled_worker ///
have_semiskilled_worker sh_employee sh_selfempl ///
hh_can_activities hh_can_for_edu hh_can_for_work hh_can_use_phone {
	replace `var' = 0 if `var' ==. 
	}

//Shares of labor income from primary occupation 
gen sh_wages = rpcwage1 /  rpcinc1 
gen sh_selfemp = rpcself1 / rpcinc1

sum sh_wages sh_selfemp
replace sh_wages=0 if sh_wages<0 | sh_wages==. 
replace sh_selfemp=0 if sh_selfemp<0 | sh_selfemp==. 

global incomes sh_wages sh_selfemp ln_rpcinc1 ln_rpcwage1 ln_rpcself1 rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot

keep hhid psu weight popwt sector $spatial $demo $hhh $assets $sector $disab $empstat $incomes $assets flag6_income* $complit

mdesc *

gen hhsize_sq = hhsize^2

gen avg_age_sq = age_avg^2
sum *

gen hhb_year = 2023-age_hh
tabstat flag6_income

gen estate = sector==3
tab sector estate 
tab district, nol 
save "$data/cleaned/lfs2023_clean" , replace

********************************************************************************
// 				2024
********************************************************************************
use "$data/lfs2024_clean" , clear 
mdesc  /// labor income missing in 19% cases :/ 

//To avoid dropping obs: replace missing incomes with zeros
foreach var in hh_paidemp_pc hh_selfemp_pc hh_selfemp_primary_pc hh_inc_pc hh_wages_pc hh_wages_primary_pc hh_inc_nc_pc hh_inc_primary_nc_pc ///
rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot {

//Winsorize positive values 
	sum `var' if `var'>0 , d 
	scalar p1 =r(p1)
	scalar p99 = r(p99)
	replace `var' = p1 if `var'<p1 		& `var'!=. 
	replace `var' = p99 if `var'>p99 	& `var'!=. 

//Replace missings with zero 	
	replace `var' = 0 if `var' ==. 
//Create logs 
	gen ln_`var' = ln(`var')
	replace ln_`var' = 0 if ln_`var' ==. 

	}

	gen flag6_income = rpcinc1==0 
	tabstat  flag6_income
	gen flag6_income2 = rpcinc_tot==0 
	tabstat  flag6_income2
//To avoid dropping obs: replace missing if no household member in that category? Check this 
foreach var in edu_sh_1564_none have_public_emp have_pvt_emp have_skilled_worker ///
have_semiskilled_worker sh_employee sh_selfempl ///
hh_can_activities hh_can_for_edu hh_can_for_work hh_can_use_phone {
	replace `var' = 0 if `var' ==. 
	}

//Shares of labor income from primary occupation 
gen sh_wages = rpcwage1 /  rpcinc1 
gen sh_selfemp = rpcself1 / rpcinc1

sum sh_wages sh_selfemp
replace sh_wages=0 if sh_wages<0 | sh_wages==. 
replace sh_selfemp=0 if sh_selfemp<0 | sh_selfemp==. 

global incomes sh_wages sh_selfemp ln_rpcinc1 ln_rpcwage1 ln_rpcself1 rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot

keep hhid psu weight popwt sector $spatial $demo $hhh $assets $sector $disab $empstat $incomes $assets flag6_income* $complit

mdesc *

gen hhsize_sq = hhsize^2

gen avg_age_sq = age_avg^2
sum *

gen hhb_year = 2023-age_hh
tabstat flag6_income

gen estate = sector==3
tab sector estate 
tab district, nol 
save "$data/cleaned/lfs2024_clean" , replace


********************************************************************************
// 				2016
********************************************************************************
use "$data/lfs2016_clean" , clear 
mdesc  /// labor income missing in 19% cases :/ 

glo spatial district urban rural

//Dependency ratio goes to zero for hh with elderly head : keep only share of kids and share of old 
glo demo hhsize age_avg num_deps num_* /*dep_ratio*/ share_dep share_kids sh_mem* edu_sh_* has_in_school sh_in_school
glo hhh buddhist_hh married_hh sinhala_hh female_hh age_hh edu_hhh*
glo sector hh_main_* have_agri_emp have_constr_emp have_serv_emp have_ind_emp
//glo disab has_*_disab *disab_hhh
glo empstat have_public_emp have_pvt_emp have_skilled_worker have_semiskilled_worker employee employer self_employed familyworker public_emp_hhh private_emp_hhh  sh_selfempl sh_employee sh_ecactive
glo incomes hh_inc_primary_nc_pc hh_wages_primary_pc hh_selfemp_primary_pc /*hh_inc_sec_nc_pc hh_wages_sec_pc hh_selfemp_sec_pc */ rpcinc1 rpcwage1 rpcself1 /*rpcinc2 rpcwage2 rpcself2*/ sh_wages_pc sh_selfemp_pc
glo complit  hh_aware* hh_can* hh_use* hh_internet* hh_emailed* 
glo expenditure welfare 

//To avoid dropping obs: replace missing incomes with zeros
foreach var in hh_paidemp_pc hh_selfemp_pc hh_selfemp_primary_pc hh_inc_pc ///
hh_wages_pc hh_wages_primary_pc hh_inc_nc_pc hh_inc_primary_nc_pc ///
rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot {

//Winsorize positive values 
	sum `var' if `var'>0 , d 
	scalar p1 =r(p1)
	scalar p99 = r(p99)
	replace `var' = p1 if `var'<p1 		& `var'!=. 
	replace `var' = p99 if `var'>p99 	& `var'!=. 
//Replace missings with zero 	
	replace `var' = 0 if `var' ==. 
//Create logs 
	gen ln_`var' = ln(`var')
	replace ln_`var' = 0 if ln_`var' ==. 
	}

	gen flag6_income = rpcinc1<=1 
	tabstat  flag6_income

	gen flag6_income2 = rpcinc_tot<=1 
	tabstat  flag6_income2
	
//To avoid dropping obs: replace missing if no household member in that category? Check this 
foreach var in edu_sh_1564_none have_public_emp have_pvt_emp have_skilled_worker ///
have_semiskilled_worker sh_employee sh_selfempl 								///
hh_can_activities hh_can_for_edu hh_can_for_work hh_can_use_phone 				///
hh_aware_activities hh_aware_for_edu hh_aware_for_work 							///
 hh_used_internet_12m hh_used_phone hh_emailed_12m   {
	replace `var' = 0 if `var' ==. 
	}

//Shares of labor income from primary occupation 
gen sh_wages = rpcwage1 /  rpcinc1 
gen sh_selfemp = rpcself1 / rpcinc1 

sum sh_wages sh_selfemp
replace sh_wages=0 if sh_wages<0 | sh_wages==.
replace sh_selfemp=0 if sh_selfemp<0 | sh_selfemp==. 

global incomes sh_wages sh_selfemp ln_rpcinc1 ln_rpcwage1 ln_rpcself1 rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot

keep hhid psu weight popwt sector $spatial $demo $hhh $sector $empstat $complit $incomes flag6_income* *_tot 

mdesc *

gen hhsize_sq = hhsize^2

gen avg_age_sq = age_avg^2
sum *

gen hhb_year = 2016-age_hh
tabstat flag6_income

gen estate = sector==3
tab sector estate 

save "$data/cleaned/lfs2016_clean" , replace