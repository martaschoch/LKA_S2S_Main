***********************************************************************
*	Clean 2019 HIES 
***********************************************************************
set more off

//data folders 
//Tilokas
/*global lfs2019  "C:\Users\User\OneDrive - University of Moratuwa\WB\Sri Lanka - poverty\LFS"
global hies2019 "C:\Users\User\OneDrive - University of Moratuwa\WB\Sri Lanka - poverty\HIES 2019\rundata"
global data ../data
global output ../output 

global data "C:\Users\User\OneDrive - University of Moratuwa\WB\Sri Lanka - poverty\LFS HIES SWIFT\data"
*/ 

****************************************************
//Deflators 
****************************************************
import excel using "$data/NCPI_series.xlsx", sheet("data") firstrow clear 

save "$data/NCPI_series", replace 

*************************************************************
//HIES 2019: NON-LABOR INCOME AGGREGATES AT HOUSEHOLD LEVEL 
*************************************************************
// Harmonized non-labor income components from 2019 HIES  
use "$hies/SARMD/HIES_2019_inc"

keep hhid pid ijubi icap itranext_m inocct_m ipcf

save "$hies/SARMD/HIES_2019_inc_hh" , replace 

/* I think we can use the SARMD income total
*************************************************************
//AGGREGATES FROM NSO  
*************************************************************
use  $hies2019/RAW/rundata/aggregates , clear 

destring district sector psu snumber hhno , replace

save  $hies2019/RAW/rundata/aggregates_clean, replace
*/
*************************************************************
//COMBINE ALL DATASETS  
*************************************************************

use "$hies/RAW/LKA_2019_HIES_v01_M", clear 
/*
// Merge income and consumption aggregates from NSO 
merge m:1 district sector  psu snumber hhno using $hies2019/RAW/rundata/aggregates_clean , nogen keepusing(hhexppm hhincomepm)
*/
// Merge consumption aggregate from harmonized file 
merge 1:1 pid hhid using "$hies/SARMD/HIES_2019" , nogen keepusing(welfare subnatid*)

// Merge non-labor income components from harmonized file 
merge 1:1 pid hhid using "$hies/SARMD/HIES_2019_inc_hh"  , nogen 



********************************************************************************
//Code variables in a consistent way with LFS//
********************************************************************************
*Provinces
egen province = group(subnatid1)
tab province

*District dummies
tab district, gen(dist_)
tab sector, gen(sector_)

gen urban=(sector==1)
gen rural=(sector==2)

*ethnicity 
label define  ethn 1"Sinhala" 2"Tamil" 3"Indian Tamil" 4"Moor/Muslim" 5"Burgher" 6"Malay" 9"Other"
label values ethnicity ethn 

gen sinhala=ethnicity==1

*religion 
label define rel 1"Buddhist" 2 "Hindu" 3"Islam" 4"Catholic" 5"Christian" 9"Other"
label values religion rel 
gen buddhist=religion==1 

*sex
recode sex (2=0), gen(male)

*marital_status
recode marital_status (1=1) (2/3=2) (4=3) (5=4) (6/7=5), gen(marstat)

tab marital_status
gen married = marstat==2


********************************************************************************
*education 
********************************************************************************

recode education (19 = 1) (0/5 = 2) (6 = 3) (7/10 = 4) (11/14 = 5) (18=6) (15/16=7)  (17=8) (*=.) if age>=5, g(educat7)
note educat7: For LKA_2019_LFS, to be consistent with LKA_2016_HIES we categorized "Special Education learning / learnt" as educat7 = 6 "Higher than secondary but not university".

//Education
* educat5 = Highest level of education completed (5 categories)
tab educat7 education
 
recode educat7 (0=0) (1=1) (2=2) (3/4=3) (5=4) (6/8=5), g(educat5)
note educat5: For LKA_2019_LFS, to be consistent with LKA_2016_HIES we categorized "Special Education learning / learnt" as educat5 = 5 "some tertiary/post-secondary".

gen noedu=educat5==1

gen atleast_sec=(educat7==5 | educat7==7)

gen schoolage_noschool=(age>=5 & age<=16 & curr_educ==9)

bysort hhid: egen have_atleast_secedu=max(atleast_sec)

bysort hhid: egen have_schoolage_noschl=max(schoolage_noschool)

//Currently in education 
tab curr_educ

gen curr_school =  curr_educ<=2
gen curr_edu_other = curr_educ >3 & curr_educ<9

bys hhid : egen has_in_school = max(curr_school)
gen sh_in_school = has_in_school / hhsize 

*******************************************************************************
*Head of household Characteristics 
*******************************************************************************

gen femaleHHH=(relationship==1 & male==0)
bysort hhid: egen female_hhh=max(femaleHHH)

gen age_hhh = age if relationship==1

*******************************************************************************
*Household Characteristics 
*******************************************************************************
bys hhid: egen age_avg=mean(age)

gen dep=age<15 | age>=65

gen child=age<15
gen old= age>=65

gen labor = age>=15 & age<65 

bysort hhid: egen num_deps=total (dep) , missing 
bysort hhid: egen num_kids=total (child) , missing 
bysort hhid: egen num_old=total (old) , missing 
bysort hhid: egen num_labor=total (labor) , missing 

gen dep_ratio=num_deps/num_labor
gen share_dep=num_deps/hhsize
gen share_kids=num_kids/hhsize

mdesc hhsize 
mdesc dep_ratio
mdesc share_*
tab hhsize if dep_ratio==.  
tab num_deps if dep_ratio==. 

	*Sex structure 
		g aux_fem 	= sex==2
		g aux_male	= sex==1
		bys hhid: egen aux_fem_tot = total(aux_fem)
		bys hhid: egen aux_male_tot = total(aux_male)
		g sex_ratio = aux_male_tot/aux_fem_tot  
		lab var sex_ratio "Ratio men to women among all HH members"
	* Age structure 
		g aux_014		= age<15
		g aux_1564 		= age>=15 & age<=64
		g aux_65plus 	= age>=65 & age<.
		g aux_0			= age==0
		g aux_1			= age==1
		g aux_2			= age==2
		g aux_3			= age==3
		g aux_4 		= age==4
		
		foreach var in _014 _1564 _0 _1 _2 _3 _4 _male _fem {
		bys hhid: egen hh_mem`var' = total(aux`var')
		g sh_mem`var' = hh_mem`var'/ hhsize
		}
		
* Edu structure 
		
	g aux_edu_hhh_none		= educat5==1 if relationship==1
	g aux_edu_hhh_prim		= educat5==2 if relationship==1
	g aux_edu_hhh_secincomp	= educat5==3 if relationship==1
	g aux_edu_hhh_sec		= educat5==4 if relationship==1
	g aux_edu_hhh_high 		= educat5==5 if relationship==1
	g aux_edu_yr_hhh = 		educat5 if relationship==1
	g aux_edu_yr_max = 		educat5
	
	loc tomax edu_hhh_none edu_hhh_prim edu_hhh_secincomp edu_hhh_sec edu_hhh_high edu_yr_hhh edu_yr_max
	foreach var of loc tomax {
	
	bys hhid: egen `var'=max(aux_`var')
	assert `var'!=.
	drop aux_`var'
	
	}
	
	g aux_edu_sh_5plus_none 	= educat5==1 if (age>=5 & age<.)
	g aux_edu_sh_1564_none 		= educat5==1 if (age>=15 & age<65)
	g aux_edu_yr_25plus_avg 	= educat5 if age>=25 & age<.
		
	loc tomean edu_sh_5plus_none edu_sh_1564_none edu_yr_25plus_avg
	
	foreach var of loc tomean{
	bys hhid: egen `var'=mean(aux_`var')
	drop aux_`var'
	}
	
	//assert edu_yr_25plus_avg!=. // Problem. Includes 235 missings for HH without 25+ years old
		
	//g aux_edu_hhh_iliterate =
	//g aux_edu_hhh_literate  =
	//g aux_edu_sh_5plus_iliterate = 
	//g aux_edu_sh_5plus_literate = 
	//g aux_edu_sh_1564_literate = 
	
	lab var edu_yr_max 			"Household head highest years of education"
	lab var edu_yr_hhh 			"Household max. years of education by members"
	//lab var edu_hhh_iliterate 	"Household head literacy: literate"
	//lab var edu_hhh_literate 	"Household head literacy: iliterate"
	lab var edu_yr_25plus_avg 			"Household avg. years of education by members" 
	//lab var edu_sh_5plus_literate 	"Household share of members 5+ literate"
	//lab var edu_sh_5plus_iliterate 	"Household share of members 5+ iliterate"
	//lab var edu_sh_1564_literate 	"Household share of members 15-64 literate"
	lab var edu_hhh_none 			"Household head: no education"
	lab var edu_hhh_prim			"Household head with primary or below (incl primary completed)"
	lab var edu_hhh_secincomp 		"Household head with some secondary education but not completed"
	lab var edu_hhh_sec				"Household head with completed lower secondary to completed upper secondary"
	lab var edu_hhh_high			"Household head with higher education completed or special education"
	lab var edu_sh_5plus_none 		"Share of hh members (5+) with no education"
	lab var edu_sh_1564_none 		"Share of hh members (15-64) with no education"
	lab var edu_yr_25plus_avg 		"Avg years of schooling among hh members 25+"	

*******************************************************************************
//Disability
*******************************************************************************

* eye_dsablty = Difficulty seeing
gen eye_dsablty=s3b_a 

* hear_dsablty = Difficulty hearing
gen hear_dsablty=s3b_6 

* walk_dsablty = Difficulty walking or climbing steps
*Not compatible with LFS

* conc_dsord = Difficulty remembering or concentrating
gen conc_dsord=s3b_10 

* slfcre_dsablty = Difficulty with self-care
gen slfcre_dsablty=s3b_11 

* comm_dsablty = Difficulty communicating
gen comm_dsablty=s3b_12

tab eye_dsablty 
tab hear_dsablty 
tab conc_dsord 
tab comm_dsablty 


gen no_eye_disab=eye_dsablty==1
gen eye_disab=eye_dsablty>=2

gen no_hear_disab=hear_dsablty==1
gen hear_disab=hear_dsablty>=2 

gen no_conc_disab=conc_dsord==1
gen conc_disab=conc_dsord>=2

gen no_comms_disab=comm_dsablty==1
gen comms_disab=comm_dsablty>=2 

foreach var in eye hear conc comms{
	bys hhid: egen has_`var'_disab = max(`var'_disab)
	gen `var'_disab_hhh = `var'_disab if relationship==1 
	}

*******************************************************************************
// Firewood/Water 
*******************************************************************************
gen distance_to_water  	= s8_6b2_distance 
gen time_to_water 		= s8_6b2_premises_time
gen collected_water 	= s8_6b1_inside_outside==2 
bys hhid: egen collects_water = max(collected_water)

tab is_collect_firewood 
gen collected_firewood = is_collect_firewood==1 
bys hhid: egen collects_firewood = max(collected_firewood)


*******************************************************************************
//Digital device ownership - HH level
*******************************************************************************

gen cellphone=(telephone_mobile==1)

gen computer=(computers==1)

********************************************************************************
						//LABOR MARKET// 
********************************************************************************

*labor force status 
tab is_active 

recode is_active (2=0), gen (lstat_active) 

gen lstat_active_hhh = lstat_active==1 if relationship==1 

*employment status
tab employment_status 
label drop empstat 
label define empstat 1"Public Employee" 2 "Private sector employee" 3 "Family Worker" 4 "Employer" 5 "Self-employed" 
recode employment_status (1/2=1) (3=2) (6=3) (4=4) (5=5), gen (empstat)
label values empstat empstat 
tab empstat, gen(empstat_)

tab empstat if lstat_active ==0
tab empstat if relationship ==1  & lstat_active==0 , missing

// Will use employment status to better refine income definitions and match LFS definition selfemployed+employer
gen public 				= empstat ==1 
gen private 			= empstat ==2

gen employee			=empstat<=2 
gen ownaccount_employer=empstat>=4 
gen familyworker=empstat==3 
gen employer				=empstat==4 
gen self_employed           =empstat==5

tab employee empstat 
tab ownaccoun empstat 
tab familyworker empstat 

foreach var in public private employee ownaccount familyworker employer self_employed {
	gen `var'_hhh = `var' if relationship==1 
}


*Industry
tostring industry, replace
replace industry="0"+industry if strlen(industry)==4

gen main_industry=substr(industry, 1, 2)
destring main_industry, replace

gen broad_industry=1 if main_industry<3
replace broad_industry=2 if main_industry>=5 & main_industry<40
replace broad_industry=3 if main_industry>=41 & main_industry<=43
replace broad_industry=4 if broad_industry==. & main_industry!=.

//label define broad_ind 1 "Agri" 2 "Manufacturing (excl construction)" 3 "Construction" 4 "Services"
label values broad_industry broad_ind

tab broad_industry, gen(broad_ind_)

*Occupation
tostring main_occupation, gen(occupation)
replace occupation="0"+occupation if strlen(occupation)==3

gen main_occ=substr(occupation,1,1)
destring main_occ, replace

gen skill_level=3 if main_occ>=1 & main_occ<=3
replace skill_level=2 if main_occ>=4 & main_occ<=8
replace skill_level=1 if main_occ==9
replace skill_level=0 if main_occ==0

//label define skill_level 0 "Armed forces" 1 "Skill level 1" 2 "Skill level 2" 3 "Skill level 3"
label values skill_level skill_level

tab skill_level, gen(skill_)

********************************************************************************
						// INCOMES // 
********************************************************************************


*Income from paid employment: harmonized to monthly see Do/help/LKA_2019_HIES_v01_M.do 


egen ind_wages			= rowtotal (wages_salaries_1 wages_salaries_2), missing 
egen ind_allowances 	= rowtotal (allowences_1 allowences_2), missing 
egen ind_bonus 			= rowtotal (bonus_1  bonus_2), missing 

egen inc_paidemp_mon	=	rowtotal(ind_wages ind_allowances ind_bonus), missing 
egen inc_emp_excl_bonus	=	rowtotal(ind_wages ind_allowances), missing 

sum ind_wages, d 
sum ind_allowances, d 
sum inc_paidemp_mon, d
sum inc_emp_excl_bonus, d

//primary occupation only 
gen wages_primary = wages_salaries_1 
gen wages_secondary = wages_salaries_2

********************************************************************************
*Income from self-employment - not sure if this is net of input costs
********************************************************************************
// Income variables in harmonized dataset don't match those we were using - consider using raw data 
*Income from agri/non-agri activities - profits
su agricultural_*, d 
egen ind_agritotal_profit	= rowtotal(agricultural_1 agricultural_2 ) 			, missing 	// year 
egen ind_nonagri_profit		= rowtotal(non_agricultural_1 non_agricultural_2)	, missing  //year

sum ind_agritotal_profit, d  //negative values because this is calculated as net 
sum ind_nonagri_profit, d  

//Cleaning self-employed incomes to match LFS definition: drop zeros 
replace ind_agritotal_profit=. if ind_agritotal_profit <0 // 197 changes 
replace ind_nonagri_profit=. if ind_nonagri_profit <0     //43 changes 
sum 	ind_nonagri_profit if ind_nonagri_profit ==0 

egen 	inc_selfemp_primary =rowtotal (agricultural_1 non_agricultural_1) , missing 
egen 	inc_selfemp_sec = rowtotal (agricultural_2 non_agricultural_2)    , missing 

egen 	inc_selfemp_mon 	=rowtotal(ind_agritotal_profit ind_nonagri_profit), missing 

egen 	labor_income_total	=rowtotal	(inc_paidemp_mon inc_selfemp_mon), missing   
egen 	labor_income_nc 	=rowtotal (ind_wages inc_selfemp_mon) 	, missing  				//non-contributory 

//Main variable 
egen 	labor_income_primary_nc	= rowtotal (wages_primary inc_selfemp_primary) 	, missing  //non-contributory 
egen 	labor_income_sec_nc	= rowtotal (wages_secondary inc_selfemp_sec) 	, missing  //non-contributory 



sum 	inc_selfemp_mon labor_income_total, d 
mdesc labor_income_total 


********************************************************************************
*Dummies for income source  
********************************************************************************

gen has_wages    		= ind_wages>0 & ind_wages!=. 
gen has_emp_inc  		= inc_paidemp_mon>0 & inc_paidemp_mon!=.
gen has_selfemp_inc 	= inc_selfemp_mon>0 & inc_selfemp_mon!=. 

corr has_wages employee
tab has_wages familyworker

tab has_selfemp_inc employee
tab has_selfemp_inc familyworker


********************************************************************************
*Total Labor Income 
********************************************************************************

bys	hhid: egen 	hh_wages 	  = 	total (ind_wages) , missing 

bys hhid: egen hh_inc_paidemp = 	total (inc_paidemp_mon) , missing
bys hhid: egen hh_inc_selfemp = 	total (inc_selfemp_mon)    , missing 

bys hhid: egen hh_mon_inc = 	total (labor_income_total)     , missing 
bys hhid: egen hh_mon_inc_nc = 	total (labor_income_nc)     , missing 

********************************************************************************
*Primary Employment Totals  
********************************************************************************
bys 	hhid: egen hh_wages_primary	=	total (wages_primary)  	, missing 
bys hhid: egen hh_inc_selfemp_primary = 	total (inc_selfemp_primary)    , missing 
bys hhid: egen hh_mon_inc_primary_nc = 	total (labor_income_primary_nc)     , missing 

********************************************************************************
*Secondary Employment Totals  
********************************************************************************
bys hhid: egen hh_wages_sec 		=	total (wages_secondary)  		, missing 
bys hhid: egen hh_inc_selfemp_sec 	= 	total (inc_selfemp_sec)    	, missing 
bys hhid: egen hh_mon_inc_sec_nc 	= 	total (labor_income_sec_nc) , missing 
 

//All in per-capita terms 
gen hh_inc_pc=hh_mon_inc						/hhsize
gen hh_inc_nc_pc=hh_mon_inc_nc					/hhsize
gen hh_selfemp_pc = hh_inc_selfemp 				/hhsize 
gen hh_paidemp_pc = hh_inc_paidemp 				/hhsize 
gen hh_wages_pc= hh_wages						/hhsize 

//Primary Occ
gen hh_inc_primary_nc_pc=hh_mon_inc_primary_nc  /hhsize
gen hh_selfemp_primary_pc = hh_inc_selfemp_primary /hhsize 
gen hh_wages_primary_pc = hh_wages_primary 		/ hhsize 

//Secondary Occ
gen hh_inc_sec_nc_pc	=	hh_mon_inc_sec_nc  		/hhsize
gen hh_selfemp_sec_pc 	=	hh_inc_selfemp_sec 		/hhsize 
gen hh_wages_sec_pc		= 	hh_wages_sec			/ hhsize 

//Shares of labor income from primary occupation pc : check

gen sh_wages_pc 	= hh_wages_primary_pc /  hh_inc_primary_nc_pc
gen sh_selfemp_pc   = hh_selfemp_primary_pc / hh_inc_primary_nc_pc

gen sh_wages2_pc 	= hh_wages_sec_pc /  hh_inc_sec_nc_pc
gen sh_selfemp2_pc   = hh_selfemp_sec_pc / hh_inc_sec_nc_pc

gen have_wages = (hh_wages_pc>0 & hh_wages_pc!=.)
gen have_emp_inc=(hh_inc_pc>0 & hh_inc_pc!=.)
gen have_selfemp_inc 	= hh_inc_selfemp>0 & hh_inc_selfemp!=. 

gen has_inc_sec = hh_inc_sec_nc_pc > 0 

// Household head 
gen wages_hhh 			= ind_wages if relationship==1 
gen wages_primary_hhh 	= wages_primary if relationship==1 
gen incself_hhh 		= inc_selfemp_mon if relationship==1 

********************************************************************************
*Non-Labor Incomes   
********************************************************************************

bys hhid: egen hh_pensions	 = total(ijubi) , missing
bys hhid: egen hh_capital	 = total(icap) , missing
bys hhid: egen hh_remittances = total(itranext_m) , missing
bys hhid: egen hh_inocct_m	= total(inocct_m) , missing

foreach var in pensions capital remittances inocct_m{
	gen hh_`var'_pc = hh_`var'/hhsize 
}
 
********************************************************************************
*Temporal and Spatial Deflation  
********************************************************************************
tab year 
tab month

merge m:1 year month using "$data/NCPI_series", keepusing(cpi_base2013) 
keep if _merge==3 
drop _merge 

//merge m:1 district using "$data/HIES/RAW/spatial_priceindex.dta",  nogen
bys year: egen avg_cpi = mean(cpi_base2013)
tab avg_cpi 

********************************************************************************
* Income in real terms 
********************************************************************************
xtile decile = welfare [aw=finalweight] , nq(10)
tabstat welfare hh_inc_primary_nc_pc , by(decile)

gen rpcinc1  = (hh_inc_primary_nc_pc*avg_cpi)/cpi_base2013
gen rpcwage1 = (hh_wages_primary_pc*avg_cpi)/cpi_base2013
gen rpcself1 = (hh_selfemp_primary_pc*avg_cpi)/cpi_base2013

gen rpcinc2  = (hh_inc_sec_nc_pc*avg_cpi)/cpi_base2013
gen rpcwage2 = (hh_wages_sec_pc*avg_cpi)/cpi_base2013
gen rpcself2 = (hh_selfemp_sec_pc*avg_cpi)/cpi_base2013

//spatial 
foreach var in rpcinc1 rpcwage1 rpcself1 rpcinc2 rpcwage2 rpcself2 {
	replace `var' = `var'*lpindex1
}

foreach var in pensions capital remittances inocct_m {
	
	gen r`var'pc = ((hh_`var'_pc * avg_cpi)/cpi_base2013)*lpindex1
	su r`var'pc, d 
}

//Deflate Total income per capita: from SARMD
gen ripcfpc = ((ipcf * avg_cpi)/cpi_base2013)*lpindex1
su ripcfpc, d 

tabstat welfare hh_inc_primary_nc_pc rpcinc* cpi_base2013, by(month)

xtile quintile19 = welfare [aw=finalweight] , nq(5)

tabstat welfare hh_inc_primary_nc_pc rpcinc* , by(quintile19)
sum welfare hh_inc_primary_nc_pc rpcinc* 

tabstat welfare hh_inc_primary_nc_pc rpcinc1, by(decile)

tabstat welfare hh_inc_pc hh_inc_primary_nc_pc , by(decile)

*******************************************************************
//Replace negative incomes with zeros 
*******************************************************************
foreach var in rpcinc1 rpcwage1 rpcself1 ///
				hh_inc_primary_nc_pc hh_wages_primary_pc hh_selfemp_primary_pc ///
				hh_pensions hh_capital hh_remittances hh_inocct_m ///
				rpensionspc rcapitalpc rremittancespc rinocct_mpc ripcfpc {
				//winsorize all income variables 
				sum `var' if `var'>0, d 
				scalar p1_`var'=r(p1)
				scalar p99_`var' = r(p99)
				replace `var'=p1_`var' if `var'<p1_`var' & `var'>0 & `var'!=.
				replace `var'=p99_`var' if `var'>p99_`var' & `var'>0 & `var'!=.
				
				replace `var' = 0 if `var'<0
}

*******************************************************************
// Propensity to Consume 
*******************************************************************
gen theta = welfare/ ripcfpc 

tabstat theta , by(quintile19)
tabstat welfare ripcfpc , by(quintile19)

*******************************************************************
// Non - Labor Income Components as share of welfare (?) 
*******************************************************************
gen sh_pension = 	rpensionspc/welfare
gen sh_capital =  	rcapitalpc/welfare 
gen sh_remittances =  	rremittancespc/welfare 
gen sh_inocct_m =  	rinocct_mpc/welfare 

egen rnlincpc19 = rowtotal (rpensionspc rcapitalpc rremittancespc rinocct_mpc) , missing 
sum rnlincpc19 if rnlincpc19>0 ,d 
scalar p1 =r(p1)
scalar p99 = r(p99)
replace rnlincpc19=p1 if rnlincpc19<p1 & rnlincpc19>0 & rnlincpc19!=.
replace rnlincpc19=p99 if rnlincpc19>p99 & rnlincpc19>0  & rnlincpc19!=.
replace rnlincpc19 =0 if rnlincpc19<0 & rnlincpc19!=.

gen sh_nl19 = rnlincpc19 / welfare 
tabstat sh_nl19 , by(quintile19)
tabstat sh_pension , by(quintile19) 
tabstat sh_remittances , by(quintile19) 
tabstat sh_inocct_m , by(quintile19) 

*******************************************************************************
// As a share of labor income for methodological ease in s2s 
*******************************************************************************
gen sh_ynyl19 		= rnlincpc19 /rpcinc1
gen sh_pensionyl19 	= rpensionspc /rpcinc1
gen sh_remittancesyl19 = rremittancespc /rpcinc1
gen sh_inocct_myl19 = rinocct_mpc /rpcinc1
gen sh_icapyl19 = rcapitalpc /rpcinc1

xtile quintileyl19 = rpcinc1 [aw=finalweigh] , nq(5)

tabstat sh_ynyl19 if rpcinc1>0 , by(quintileyl19)
tabstat sh_pensionyl19 if rpcinc1>0, by(quintileyl19) 
tabstat sh_remittancesyl19 if rpcinc1>0, by(quintileyl19) 
tabstat sh_inocct_myl19  if rpcinc1>0, by(quintileyl19) 
tabstat sh_icapyl19 if rpcinc1>0, by(quintileyl19)

*******************************************************************************
//2023 : already in 2019 prices, only need to spatially deflate 
*******************************************************************************

merge 1:1 district sector month psu snumber hhno nhh result person_serial_no  using "$data/inc_microsim23.dta", nogen

bys hhid: egen hh_pensions23	 	= total(ijubi23) , missing
bys hhid: egen hh_capital23	 		= total(icap23) , missing
bys hhid: egen hh_remittances23 	= total(itranext_m23) , missing
bys hhid: egen hh_inocct_m23		= total(inocct_m23) , missing
bys hhid: egen hh_laborinc23        = total(labor_income23_real), missing 

foreach var in pensions capital remittances inocct_m laborinc {
	gen 	r`var'pc23 = hh_`var'23	/	hhsize 
	replace r`var'pc23 = r`var'pc23 *	lpindex1
}

gen sh_pension23 	= 	rpensionspc23/welfare23
gen sh_capital23 	=  	rcapitalpc23/welfare23
gen sh_remittances23=  	rremittancespc23/welfare23 
gen sh_inocct_m23   =  	rinocct_mpc23/welfare23 

egen rnlincpc23 = rowtotal (rpensionspc23 rcapitalpc23 rremittancespc23 rinocct_mpc23) , missing 
sum rnlincpc23 if rnlincpc23>0,d 
scalar p1 =r(p1)
scalar p99 = r(p99)
replace rnlincpc23=p1 if rnlincpc23<p1 & rnlincpc23>0 & rnlincpc23!=.
replace rnlincpc23=p99 if rnlincpc23>p99 & rnlincpc23>0 & rnlincpc23!=.
replace rnlincpc23 =0 if rnlincpc23<0 & rnlincpc23!=.

su *23 , d 
gen sh_nl23 = rnlincpc23 / welfare 

xtile quintile23 = welfare23 [aw=finalweight] , nq(5)
tabstat sh_nl23 , by(quintile23)
tabstat sh_pension23 , by(quintile23) 
tabstat sh_remittances23 , by(quintile23) 
tabstat sh_inocct_m23 , by(quintile23) 

tabstat sh_pension if rpensionspc	>0 , by(quintile19) 
tabstat sh_pension23 if rcapitalpc23 >0 , by(quintile23) 

tabstat sh_capital if rcapitalpc    >0 , by(quintile19) 
tabstat sh_capital23 if rcapitalpc23 >0 , by(quintile23) 

tabstat sh_remittances if rremittancespc>0 , by(quintile19) 
tabstat sh_remittances23 if rremittancespc23 >0 , by(quintile23) 

tabstat sh_inocct_m if inocct_m>0 , by(quintile19) 
tabstat sh_inocct_m23 if inocct_m23 >0 , by(quintile23) 


*******************************************************************************
// As a share of labor income for methodological ease in s2s 
*******************************************************************************

gen sh_pensionyl23 	= 	rpensionspc23/rlaborincpc23
gen sh_icapyl23 	=  	rcapitalpc23/rlaborincpc23
gen sh_remittancesyl23=  	rremittancespc23/rlaborincpc23 
gen sh_inocct_myl23   =  	rinocct_mpc23/rlaborincpc23 

gen sh_ynyl23 = rnlincpc23 / rlaborincpc23 

xtile quintileyl23 = rlaborincpc23 [aw=finalweigh] , nq(5)

tabstat sh_ynyl23 		if labor_income23_real>0 , by(quintileyl23)
tabstat sh_pensionyl23 	if labor_income23_real>0, by(quintileyl23) 
tabstat sh_remittancesyl23 if labor_income23_real>0, by(quintileyl23) 
tabstat sh_inocct_myl23  if labor_income23_real>0, by(quintileyl23) 
tabstat sh_icapyl23		 if labor_income23_real>0, by(quintileyl23)

*******************************************************************************
//MPC 
*******************************************************************************

gen sh_yl19 = rpcinc1 / welfare 
tabstat sh_yl19  if rpcinc1>0 , by(quintile19)

*******************************************************************
*Income by sector
*******************************************************************

bysort hhid: egen hh_agri_inc=total (labor_income_total) if broad_ind_1==1 , missing
bysort hhid: egen hh_inc_agri=total (hh_agri_inc)  , missing
drop hh_agri_inc

bysort hhid: egen hh_ind_inc=total (labor_income_total) if broad_ind_2==1 | broad_ind_3==1  , missing
bysort hhid: egen hh_inc_ind=total (hh_ind_inc)  , missing
drop hh_ind_inc

bysort hhid: egen hh_serv_inc=total(labor_income_total) if broad_ind_4==1  , missing
bysort hhid: egen hh_inc_serv=mean(hh_serv_inc)
drop hh_serv_inc

egen hh_max_inc=rowmax(hh_inc_agri hh_inc_ind hh_inc_serv)

gen hh_maininc_agri=(hh_max_inc==hh_inc_agri)
replace hh_maininc_agri=0 if hh_max_inc==.
gen hh_maininc_ind=(hh_max_inc==hh_inc_ind)
replace hh_maininc_ind=0 if hh_max_inc==.
gen hh_maininc_serv=(hh_max_inc==hh_inc_serv)
replace hh_maininc_serv=0 if hh_max_inc==.

bysort hhid: egen have_agri_emp=max(broad_ind_1)
bysort hhid: egen have_ind_emp=max(broad_ind_2)
bysort hhid: egen have_constr_emp=max(broad_ind_3)
bysort hhid: egen have_serv_emp=max(broad_ind_4)
recode have_agri_emp have_ind_emp have_constr_emp have_serv_emp (.=0)

bysort hhid: egen num_agri_emp=total (broad_ind_1) , missing 
bysort hhid: egen num_indexcons_emp=total (broad_ind_2) , missing 
bysort hhid: egen num_cons_emp=total (broad_ind_3) , missing 
egen num_ind_emp=rowtotal(num_indexcons_emp num_cons_emp) , missing 
bysort hhid: egen num_serv_emp=total (broad_ind_4) , missing 

egen most_emp=rowmax(num_agri_emp num_ind_emp num_serv_emp)

gen hh_main_agri=(most_emp==num_agri_emp)
gen hh_main_ind=(most_emp==num_ind_emp)
gen hh_main_serv=(most_emp==num_serv_emp)

replace hh_main_agri=0 if most_emp==. | most_emp==0
replace hh_main_ind=0 if most_emp==. | most_emp==0
replace hh_main_serv=0 if most_emp==. | most_emp==0

bysort hhid: egen have_skilled_worker=max(skill_4)
bysort hhid: egen have_semiskilled_worker=max(skill_3)

//bysort hhid: egen have_member_disab=max(have_disabilities)
mdesc empstat 
bysort hhid: egen num_public_emp  		=total (empstat_1) , missing  
bysort hhid: egen num_pvt_emp     		=total (empstat_2)  , missing 
bysort hhid: egen num_family_worker 	=total (empstat_3) , missing  
bysort hhid: egen num_employer			=total (empstat_4) , missing 
bysort hhid: egen num_self_emp			=total (empstat_5) , missing 

bysort hhid: egen have_public_emp  		=max(empstat_1)
bysort hhid: egen have_pvt_emp     		=max(empstat_2)
bysort hhid: egen have_family_worker 	=max(empstat_3)
bysort hhid: egen have_employer			=max(empstat_4)
bysort hhid: egen have_self_emp			=max(empstat_5) 

bysort hhid: egen num_ecactive=total (lstat_active) , missing 

gen hh_lfpr=num_ecactive/(hhsize-num_kids)

gen sh_employee =  (num_public_emp+num_pvt_emp) / num_ecactive
gen sh_selfempl =  num_self_emp / num_ecactive
gen sh_ecactive = num_ecactive / hhsize 

*******************************************************************************
// Summary stats relevant variables 
*******************************************************************************

tab district 
tab ethnicity
tab religion
tab sex

tab educat5
tab educat7

tab eye_dsablty
tab hear_dsablty
tab conc_dsord
tab slfcre_dsablty
tab comm_dsablty

tab empstat 
tab lstat_active
tab broad_industry
tab skill_level
ren finalweight weight

mean dist_* sector_* sin buddhist male age married hhsize  noedu atleast_sec schoolage_noschool no_* cellphone computer [aw=weight]

mean lstat_active empstat_* broad_ind_* skill_*  [aw=weight]

mean inc_paidemp_mon [aw=weight] if inc_paidemp_mon!=0
mean inc_selfemp_mon [aw=weight] if inc_selfemp_mon!=0
mean labor_income_total [aw=weight] if labor_income_total!=0
mean inc_emp_excl_bonus [aw=weight] if inc_emp_excl_bonus!=0

*******************************************************************************
*******************************************************************************

*keep hhid relationship person weight dist* sector urban sector_* hhsize district ethnicity sin age sex male religion buddhist educat5 educat7 noedu atleast_sec schoolage_noschool marstat married empstat* lstat_active inc* rpccons hhexppm cellphone computer eye_dsablty hear_dsablty conc_dsord slfcre_dsablty comm_dsablty broad_industry skill_level broad_ind_* skill_* no_* major_*  inc_emp_excl_bonus

//ren person pid

gen data="HIES2019"

//only keep head of household 
keep if relationship==1

//recode have_public_emp have_pvt_emp have_family_worker have_employer have_self_emp have_skilled_worker have_semiskilled_worker (.=0)

*Household level variables
mean dist_* sector_* female_hh sin buddhist age married hhsize cellphone computer have_atleast_secedu have_schoolage_noschl share_dep share_kids num_kids have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv  have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp hh_lfpr have_emp_inc hh_inc_pc [aw=weight]

mean hh_inc_pc if hh_inc_pc!=0 [aw=weight]
mean hh_inc_paidemp if hh_inc_paidemp!=0 [aw=weight]
mean hh_inc_selfemp if hh_inc_selfemp!=0 [aw=weight]

sum hh_inc_pc if hh_inc_pc!=0 [aw=weight]
sum hh_inc_paidemp if hh_inc_paidemp!=0 [aw=weight]
sum hh_inc_selfemp if hh_inc_selfemp!=0 [aw=weight]

//Per-capita values 
sum hh_inc_pc if hh_inc_pc!=0 [aw=weight]
sum hh_paidemp_pc if hh_paidemp_pc!=0 [aw=weight]
sum hh_selfemp_pc if hh_selfemp_pc!=0 [aw=weight]

gen ln_welfare=ln(welfare)

reg ln_welfare dist_* sector_* female_hh sin buddhist age married hhsize cellphone computer have_atleast_secedu have_schoolage_noschl share_dep have_agri_emp have_ind_emp have_constr_emp have_serv_emp have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp have_emp_inc hh_inc_pc

gen popwt=hhsize*weight
svyset [pw=popwt] , psu(psu)
svy: total popwt 

foreach var in married sinhala buddhist {
	rename `var' `var'_hhh
}

//Percentiles of the income distribution by type of income 
pctile ptile_selfemp 		= hh_selfemp_primary_pc [pw=weight] , nq(100)
pctile ptile_wages 			= hh_wages_primary_pc [pw=weight] , nq(100)
pctile ptile_hh_inc_prim_nc = hh_inc_primary_nc_pc [pw=weight], nq(100)

keep hhid psu weight popwt province district dist_* sector* urban rural age_avg hhsize share_dep num_dep num_kids num_old share_kids dep_ratio cellphone computer have_atleast_secedu have_schoolage_noschl have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv  have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp have_emp_inc ln_welfare welfare data hh_lfpr sex_ratio *mem* edu* ///
 hh_wages have_* wages_hhh *_pc ptile* rpcinc1 rpcwage1 rpcself1 rpcinc2 rpcwage2 rpcself2 /// 
 labor_income* collects_* sh_in_school have_schoolage_noschl has_in_school has_*_disab *hhh ///
 sh_selfempl sh_employee sh_ecactive sh_pensionyl* sh_icapyl* sh_inocct_myl* sh_remittancesyl* sh_ynyl* welfare23 rlaborincpc23 rnlincpc19 rnlincpc23
 
save "$data/hies2019_clean" , replace
sum sh_ynyl19 , d
sum rnlincpc19 , d 
mdesc * 
sum *
