***********************************************************************
*	Clean 2023 LFS
***********************************************************************
clear
set more off

//data folders 
//Tilokas
/*global lfs2019  "C:\Users\User\OneDrive - University of Moratuwa\WB\Sri Lanka - poverty\LFS"
global hies2019 "C:\Users\User\OneDrive - University of Moratuwa\WB\Sri Lanka - poverty\HIES 2019\rundata"
global data ../data
global output ../output 

global data "C:\Users\User\OneDrive - University of Moratuwa\WB\Sri Lanka - poverty\LFS HIES SWIFT\data"
*/ 

//Marta 
global data "C:\Users\wb562318\OneDrive - WBG\Documents\POV-SAR\SL\PA\Analysis\Data"
global lfs  $data/LFS
global hies $data/HIES
global output "C:\Users\wb562318\OneDrive - WBG\Documents\POV-SAR\SL\PA\Analysis\Out"

//Clean lfs 2023 
//import delimited "$lfs2019/2019Annual-Outfile-with-Computer.csv", clear 
use "$lfs/RAW/LFS_2023" , clear 

* year = Year
* note: the variable already exists in harmonized form
confirm var ïyear

* int_year = interview year
g year = ïyear

* int_month = interview month
g int_month = month

* hhid = Household identifier
* NOTE: hhid = DDPPPUUH
*	district		: district (2 digits)
*	psu				: primary sampling unit (3 digits)
*	hunit			: housing unit number (2 digits)
*	hhold			: HH number (1 digit)
foreach var in district psu hunit {
	tostring `var', g(`var'_str)
	replace `var'_str = "0" + `var'_str if length(`var'_str)<2
}
replace psu_str = "0" + psu_str if length(psu_str)<3
g hhid = district_str + psu_str + hunit_str + string(hhold)

* pid = Personal identifier
rename serno pid

* confirm unique identifiers: hhid + pid
isid hhid pid

* weight = Household weight
g weight = annual_factor

* relationharm = Relationship to head of household harmonized across all regions
recode rship (1=1) (2=2) (3=3) (4=4) (5=5) (6/7=7) (9=6) (*=.), g(relationship)

* relationcs = Original relationship to head of household
label define rship 1 "Head of Household" 2 "Wife / Husband" 3 "Son / Daughter" 4 "Parents" 5 "Other Relative" 6 "Domestic Servant" 7 "Boarder" 9 "Other"
label values rship rship
decode rship, g(relationcs)

* household member. All excluding household workers
*gen hhmember=(relationharm!=7)
gen hhmember=1 //in HIES, we use all including boarders

* hsize = Household size, not including household workers
bys hhid: egen hhsize = total(hhmember)

* strata = Strata
confirm var psu_str

* psu = PSU
confirm var psu

* spdef = Spatial deflator (if one is used)
g spdef = .

* subnatid1 = Subnational ID - highest level
g province = floor(district/10)
label define province 1 "Western Province" 2 "Central Province" 3 "Southern Province" 4 "Northern Province" 5 "Eastern Province" 6 "North Western Province" 7 "North Central Province" 8 "Uva Province" 9 "Sabaragamuwa Province"
label values province province
decode province, g(subnatid1)
replace subnatid1 = string(province) + " - " + subnatid1

* subnatid2 = Subnational ID - second highest level
label define district 11 "Colombo" 12 "Gampaha" 13 "Kalutara" 21 "Kandy" 22 "Matale" 23 "Nuwara Eliya" 31 "Galle" 32 "Matara" 33 "Hambantota" 41 "Jaffna" 42 "Mannar" 43 "Vavuniya" 44 "Mullaitivu" 45 "Kilinochchi" 51 "Batticaloa" 52 "Ampara" 53 "Trincomalee" 61 "Kurunegala" 62 "Puttalam" 71 "Anuradhapura" 72 "Polonnaruwa" 81 "Badulla" 82 "Moneragala" 91 "Ratnapura" 92 "Kegalle"
label values district district
decode district, g(subnatid2)
replace subnatid2 = string(district) + " - " + subnatid2

//replace subnatid2 = string(district_str) + " - " + subnatid2

* subnatid3 = Subnational ID - third highest level
g subnatid3 = ""

********************************************************************************
	//Demographics 
********************************************************************************
*District dummies
tab district, gen(dist_)

tab sector, gen(sector_)

* urban = Urban (1) or rural (0)
recode sector (2/3=0) (1=1) (*=.), g(urban)
gen rural=sector==2 

* language = Language
g		language = "Sinhala" if sin==1
replace	language = language + ", " + "Tamil" if ~missing(language) & tamil==1
replace	language = "Tamil" if missing(language) & tamil==1
replace	language = language + ", " + "English" if ~missing(language) & eng==1
replace	language = "English" if missing(language) & eng==1


*Ethnicity
label define ethn 1"Sinhala" 2"Tamil" 3"Indian Tamil" 4"Moor/Muslim" 5"Malay" 6"Burgher" 9"Other"
label values eth ethn 

gen sinhala=(eth==1)

*religion 
gen buddhist=rel==1 

* age = Age of individual (continuous)
confirm var age

* male = Sex of household member (male=1)
recode sex (1=1) (2=0) (*=.), g(male)

* marital = Marital status
recode marital (1=2) (2=1) (3=5) (4/5=4) (*=.)
gen married=marital ==1 


*Female headed household
gen femaleHHH=(rship==1 & male==0)
bysort hhid: egen female_hhh=max(femaleHHH)

********************************************************************************
	//Dependency Ratios  
********************************************************************************
bys hhid: egen age_avg=mean(age)

gen dep=age<15 | age>=65
gen child=age<15
gen old= age>=65

gen adults = age>=15
gen labor = age>=15 & age<65 

bysort hhid: egen num_deps=total (dep)
bysort hhid: egen num_kids=total (child)
bysort hhid: egen num_old=total (old)
bysort hhid: egen num_labor=total (labor) , missing 
bysort hhid: egen num_adults=total (adults) , missing 

gen dep_ratio=num_deps/num_labor
gen share_dep=num_deps/hhsize
gen share_kids=num_kids/hhsize

mdesc dep_ratio 
tab dep_ratio if num_deps==num_labor 
tab dep_ratio 

gen sh_adults = num_adults / hhsize 

********************************************************************************
//Education
********************************************************************************

* educat7 = Highest level of education completed (7 categories)
* note: adapted from LKA_2016_HIES code for SARMD
destring edu, replace 
tab edu
recode edu (19 = 1) (0/5 = 2) (6 = 3) (7/10 = 4) (11/14 = 5) (17=6) (15/16=7) if age>=5, g(educat7)
note educat7: For LKA_2019_LFS, to be consistent with LKA_2016_HIES we categorized "Special Education learning / learnt" as educat7 = 6 "Higher than secondary but not university".

* educat5 = Highest level of education completed (5 categories)
recode educat7 (0=0) (1=1) (2=2) (3/4=3) (5=4) (6/7=5), g(educat5)
note educat5: For LKA_2019_LFS, to be consistent with LKA_2016_HIES we categorized "Special Education learning / learnt" as educat5 = 5 "some tertiary/post-secondary".

tab educat5 

gen noedu=educat5==1
gen atleast_sec=(educat7==5 | educat7==7)
gen schoolage_noschool=(age>=5 & age<=16 & cuedu==5)


bysort hhid: egen have_atleast_secedu=max(atleast_sec)
bysort hhid: egen have_schoolage_noschl=max(schoolage_noschool)

//currently in edu 
tab cuedu 
gen curr_school = cuedu ==1 
tab curr_school cuedu 
bys hhid: egen has_in_school = max(curr_school)
gen sh_in_school = has_in_school / hhsize 


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
	//assert `var'!=. // Froze because of problem with 2 hhid's without hhhead
	drop aux_`var'
	}
	
	tab relationship if hhid=="11043071" | hhid=="82001091"
	
	g aux_edu_sh_5plus_none 	= educat5==1 if (age>=5 & age<.)
	g aux_edu_sh_1564_none 		= educat5==1 if (age>=15 & age<65)
	g aux_edu_yr_25plus_avg 	= educat5 if age>=25 & age<.
		
	loc tomean edu_sh_5plus_none edu_sh_1564_none edu_yr_25plus_avg
	foreach var of loc tomean{
	bys hhid: egen `var'=mean(aux_`var')
	drop aux_`var'
	}
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
********************************************************************************
//Disability
********************************************************************************

* eye_dsablty = Difficulty seeing
recode p15 (1=4) (2=3) (3=2) (4=1) (*=.), g(eye_dsablty)

* hear_dsablty = Difficulty hearing
recode p16 (1=4) (2=3) (3=2) (4=1) (*=.), g(hear_dsablty)

* walk_dsablty = Difficulty walking or climbing steps - not compatible with HIES
*recode p17 (1=4) (2=3) (3=2) (4=1) (*=.), g(walk_dsablty)

* conc_dsord = Difficulty remembering or concentrating
recode p18 (1=4) (2=3) (3=2) (4=1) (*=.), g(conc_dsord)

* slfcre_dsablty = Difficulty with self-care
recode p19 (1=4) (2=3) (3=2) (4=1) (*=.), g(slfcre_dsablty)

* comm_dsablty = Difficulty communicating
recode p20 (1=4) (2=3) (3=2) (4=1) (*=.), g(comm_dsablty)


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


********************************************************************************
//Digital device ownership
********************************************************************************

*Ownership of a cell phone, computer (individual)
destring c1_*, replace
recode c1_* (.=0)

egen cellphone_i = rowmin(c1_4a c1_5a)
recode cellphone_i (2=0)

egen computer_i = rowmin(c1_1a c1_2a c1_3a)
recode computer_i (2=0)

bysort hhid: egen cellphone_hh=total(cellphone_i)
bysort hhid: egen computer_hh=total(computer_i)

gen cellphone=(cellphone_hh>0)
gen computer=(computer_hh>0)

********************************************************************************
// EMPLOYMENT STATUS  
********************************************************************************

destring q*, replace 

* minlaborage - Labor module application age (7-day ref period)
g minlaborage = 15

* lstatus - Labor status (7-day ref period)
g		lstatus = 1 if q2==1 | q4==1
replace	lstatus = 2 if mi(lstatus) & ((q47==1 & q48==1) | q47==3)
replace	lstatus = 3 if mi(lstatus) & q2==2

gen lstat_active=lstatus==1

* empstat - Employment status, primary job (7-day ref period)
recode q9 (1=2) (4=3) (2=4) (3=5) if lstatus==1, g(empstat)
replace empstat=1 if empstat==2 & (q14==1 | q14==2)
label define empstat 1"Public Employee" 2 "Private sector employee" 3 "Family Worker" 4 "Employer" 5 "Self-employed" 
label values empstat empstat 
tab empstat, gen(empstat_)
tab empstat 

tab q16
gen formal = q16==1  if lstatus==1 
recode q16 (1=0) (2/3=1) (4=0) , gen (informal) 
tab formal q16
tab informal q16 
tabstat informal 
  
// Will use employment status to better refine income definitions and match LFS definition selfemployed+employer
gen public 				= empstat ==1 
gen private 			= empstat ==2

tab empstat private 

gen employee				=empstat<=2 
tab employee empstat 
tab employee public 
tab public formal

gen ownaccount_employer		=empstat>=4 
gen familyworker			=empstat==3 
gen employer				=empstat==4 
gen self_employed           =empstat==5

tab employee empstat 
tab ownaccoun empstat 
tab familyworker empstat 

foreach var in public private employee ownaccount familyworker employer self_employed{
	gen `var'_hhh = `var' if relationship==1 
}


********************************************************************************
*Industry
********************************************************************************

gen industry=q8
tostring industry, replace
replace industry="0"+industry if strlen(industry)==4

gen main_industry=substr(industry, 1, 2)
destring main_industry, replace

gen broad_industry=1	 if main_industry<3
replace broad_industry=2 if main_industry>=5 & main_industry<40
replace broad_industry=3 if main_industry>=41 & main_industry<=43
replace broad_industry=4 if broad_industry==. & main_industry!=.

label define broad_ind 1 "Agri" 2 "Manufacturing (excl construction)" 3 "Construction" 4 "Services"
label values broad_industry broad_ind

tab broad_industry, gen(broad_ind_)
tab broad_industry if male==0


********************************************************************************
*Occupation
********************************************************************************

gen occupation=q7
tostring occupation, replace
replace occupation="0"+occupation if strlen(occupation)==3

gen main_occupation=substr(occupation,1,1)
destring main_occupation, replace

gen skill_level=3 		if main_occupation>=1 & main_occupation<=3
replace skill_level=2 	if main_occupation>=4 & main_occupation<=8
replace skill_level=1 	if main_occupation==9
replace skill_level=0 	if main_occupation==0

label define skill_level 0 "Armed forces" 1 "Skill level 1" 2 "Skill level 2" 3 "Skill level 3"
label values skill_level skill_level

tab skill_level, gen(skill_)

********************************************************************************
*Income from paid employment - excluding in-kind payments?
********************************************************************************
//HARMONIZE USING SARLAB METHODOLOGY 
// Primary and Second Wages/Allowances/Bonus 
destring q45_a_1 q45_a_2 q45_a_3 q45_b_3 q45_b_4 q45_c_1 q46_a_1 q46_a_2 q46_a_3 q46_b_3 q46_b_4 q46_c_1, replace

//Check for zeros: 
su q45_a_1 q45_a_2 q45_a_3 , d
su q45_b_3 q45_b_4 ,d 
su q45_c_1 , d 
su q46_a_1 q46_a_2 q46_a_3, d  
su q46_b_3 q46_b_4 , d 
su q46_c_1 , d 

********************************************************************************
//Only wages: SARLAB HARMONIZATION  
//Wage_nc_week - Wage payment adjusted, primary/secondary job, excl. bonuses
********************************************************************************
//check daily earners 
gen daily1 =  q45_b_1 * q45_b_2 
gen daily2 = q45_b_3

sum daily* , d // big difference: use daily pay*number days worked for accuracy. 

//primary occupation
sum q45* ,d  

g		wage_nc = q45_a_1 			if lstatus==1 & ~mi(q45_a_1)
replace	wage_nc = q45_b_1 * q45_b_2 if lstatus==1 & ~mi(q45_b_1)
//replace wage_nc = q45_c_1 			if lstatus==1 & ~mi(q45_c_1)

//secondary occupation 
su q46_* ,d 
g		wage_nc_2 = q46_a_1 			if lstatus==1 & ~mi(q46_a_1)
replace	wage_nc_2 = q46_b_1 * q46_b_2 	if lstatus==1 & ~mi(q46_b_1)
//replace	wage_nc_2 = q46_c_1 			if lstatus==1 & ~mi(q46_c_1)

gen 	wages_primary 		= wage_nc 
egen 	ind_wages			= rowtotal(wage_nc wage_nc_2) , missing  

mdesc ind_wages if lstatus==1  
sum wages_primary ind_wages, d 


********************************************************************************
//SARLAB HARMONIZATION: see Do/Help/LKA_2019_LFS_v01_M_v01_A_SARLD_ALL 
*total wage, primary job (7-day ref period): Wages+Allowances (overtime and in-kind)
********************************************************************************

egen	wage_total_mse_month = rowtotal(q45_a_1 q45_a_2 q45_a_3), missing
g		wage_total_we_wages_month = q45_b_1 * q45_b_2
g		wage_total_we_inkind_month = q45_b_4
egen	wage_total_month = rowtotal(wage_total_mse_month wage_total_we_wages_month wage_total_we_inkind_month /*q45_c_1*/), mis
g		wage_total = wage_total_month if lstatus==1


* wage_total_2 - Annualized total wage, secondary job (7-day ref period)
egen	wage_total_2_mse_month = rowtotal(q46_a_1 q46_a_2 q46_a_3), missing
g		wage_total_2_we_wages_month = q46_b_1 * q46_b_2
g		wage_total_2_we_inkind_month = q46_b_4
egen	wage_total_2_month = rowtotal(wage_total_2_mse_month wage_total_2_we_wages_month wage_total_2_we_inkind_month /*q46_c_1*/), mis
g		wage_total_2 = wage_total_2_month  if lstatus==1

egen 	inc_paidemp_mon = rowtotal (wage_total wage_total_2) , missing 
sum 	ind_wages inc_paidemp_mon, d 

//Check components : sample size 
sum *_mse_month  , d // only 52 from secondary wages 
sum *_wages_month, d // only 292 from daily wages second emp
sum *_inkind_month, d //only 292 from in kind secondary, but sufficient mass from primary. Excluding in-kind makes a big difference in zeros at the bottom of the distribution. 
 

//check how different b1*b2 is from b3 
 
********************************************************************************
*Income from self-employment - not sure if this is net of input costs
********************************************************************************
gen 	inc_selfemp_primary = q45_c_1 
egen 	inc_selfemp_mon= rowtotal (q45_c_1 q46_c_1) , missing 

********************************************************************************
*Total Labor Income 
********************************************************************************

egen labor_income_total = rowtotal (inc_paidemp_mon inc_selfemp_mon) 	, missing

egen labor_income_nc 			= rowtotal (ind_wages inc_selfemp_mon) 	, missing  //non-contributory 
egen labor_income_primary_nc 	= rowtotal (wages_primary inc_selfemp_primary) 	, missing  //non-contributory 


* wage_nc_week - Wage payment adjusted to 1 week, primary job, excl. bonuses, etc. (7-day ref period)
g		wage_nc_week = q45_a_1 if lstatus==1 & ~mi(q45_a_1)
replace	wage_nc_week = q45_b_1 * q45_b_2 if lstatus==1 & ~mi(q45_b_1)
replace	wage_nc_week = q45_c_1 if lstatus==1 & ~mi(q45_c_1)

gen hourly_wage= wage_nc_week / q20
replace hourly_wage=. if lstatus!=1
replace hourly_wage=. if lstatus==1 & wage_nc_week==0
label var hourly_wage "Hourly wage"

gen lnyl = ln(hourly_wage) 
/*two kdensity lnyl if employee==1 & informal==1  & private==1  || kdensity lnyl if employee==1 & public==1 ///
|| kdensity lnyl if employee==1 & formal==1 & private==1 , legend(label(1 "Informal Employee") label(2 "Formal, Public") label(3 "Formal, Private")) ytitle("Log Hourly Wage") scheme(stcolor)
graph export "$output/s2s/loghourlywage2023.png", as(png) name("Graph") replace 
*/
gen has_wages    		= ind_wages>0 & ind_wages!=. 
gen has_emp_inc  		= inc_paidemp_mon>0 & inc_paidemp_mon!=.
gen has_selfemp_inc 	= inc_selfemp_mon>0 & inc_selfemp_mon!=. 

corr has_wages employee


********************************************************************************
* HOUSEHOLD AGGREGATES 
********************************************************************************

bysort 	hhid: egen hh_inc_paidemp	=	total    (inc_paidemp_mon) 	, missing 
bysort 	hhid: egen hh_wages			=	total (ind_wages)  	, missing 
bysort 	hhid: egen hh_wages_primary	=	total (wages_primary)  	, missing 
bysort  hhid: egen hh_inc_selfemp_primary = 	total (inc_selfemp_primary)    , missing 

bysort 	hhid: egen hh_inc_selfemp	=	total    	(inc_selfemp_mon) 	, missing 

bys 	hhid: egen hh_mon_inc 	= total   	 (labor_income_total) , missing 
bys 	hhid: egen hh_mon_inc_nc 	= total 		(labor_income_nc)  , missing 
bys 	hhid: egen hh_mon_inc_primary_nc = total (labor_income_primary_nc)  , missing 

********************************************************************************
//All in per-capita terms 
********************************************************************************

gen hh_paidemp_pc = hh_inc_paidemp / 	hhsize 
gen hh_wages_pc	  = hh_wages       /	hhsize 
gen hh_wages_primary_pc = hh_wages_primary / hhsize 

gen hh_selfemp_pc = hh_inc_selfemp / 	hhsize 
gen hh_selfemp_primary_pc = hh_inc_selfemp_primary /hhsize 

gen	hh_inc_pc	 = hh_mon_inc/hhsize 
gen	hh_inc_nc_pc = hh_mon_inc_nc/hhsize 
gen	hh_inc_primary_nc_pc = hh_mon_inc_primary_nc/hhsize 

//Shares of labor income from primary occupation pc : check

gen sh_wages_pc 	= hh_wages_primary_pc /  hh_inc_primary_nc_pc
gen sh_selfemp_pc   = hh_selfemp_primary_pc / hh_inc_primary_nc_pc

gen have_wages    		= hh_wages>0 & hh_wages!=. 
gen have_emp_inc  		= hh_inc_pc>0 & hh_inc_pc!=.
gen have_selfemp_inc 	= hh_inc_selfemp>0 & hh_inc_selfemp!=. 

// Household head 
gen wages_hhh 			= wage_total if rship==1 
gen wages_primary_hhh 	= wages_primary if rship==1 
gen incself_hhh = inc_selfemp_mon if rship==1 


sum hh_inc_pc 		if hh_inc_pc		!=0 [aw=weight]
sum hh_inc_paidemp 	if hh_inc_paidemp	!=0 [aw=weight]
sum hh_inc_selfemp 	if hh_inc_selfemp	!=0 [aw=weight]

//Per-capita values 
sum hh_inc_pc 		if hh_inc_pc	!=0 [aw=weight]
sum hh_paidemp_pc 	if hh_paidemp_pc!=0 [aw=weight]
sum hh_selfemp_pc 	if hh_selfemp_pc!=0 [aw=weight]


********************************************************************************
*Temporal and Spatial Deflation  
********************************************************************************
tab year 
tab month

merge m:1 year month using "$data/NCPI_series" , keepusing(cpi_base2013 avg_2019 avg_2023) 
keep if _merge==3 
drop _merge 

merge m:1 district using "$data/HIES/RAW/spatial_priceindex.dta",  nogen
bys year: egen avg_cpi = mean(cpi_base2013)
tab avg_cpi avg_2023 
tab avg_2019

********************************************************************************
* Income in real terms 
********************************************************************************

gen rpcinc1  = (hh_inc_primary_nc_pc*avg_2019)/cpi_base2013
gen rpcwage1 = (hh_wages_primary_pc*avg_2019)/cpi_base2013
gen rpcself1 = (hh_selfemp_primary_pc*avg_2019)/cpi_base2013

gen rpcinc_tot  = (hh_inc_pc*avg_2019)/cpi_base2013
gen rpcwage_tot = (hh_paidemp_pc*avg_2019)/cpi_base2013
gen rpcself_tot = (hh_selfemp_pc*avg_2019)/cpi_base2013

/*
gen rpcinc2  = (hh_inc_sec_nc_pc*avg_cpi)/cpi_base2013
gen rpcwage2 = (hh_wages_sec_pc*avg_cpi)/cpi_base2013
gen rpcself2 = (hh_selfemp_sec_pc*avg_cpi)/cpi_base2013
*/
//spatial 
foreach var in rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot /*rpcinc2 rpcwage2 rpcself2*/ {
	replace `var' = `var'*lpindex1
}

tabstat  hh_inc_primary_nc_pc rpcinc* cpi_base2013, by(month)
mdesc rpc* 
sum rpc*

//Shares for Shapley 
gen has_rpcwage_tot= rpcwage_tot>0  & rpcwage_tot!=.
gen has_rpcself_tot = rpcself_tot>0  & rpcself_tot!=. 

bys hhid : egen num_wages =total(has_rpcwage_tot) , missing
bys hhid : egen num_self =total(has_rpcself_tot) , missing

********************************************************************************
*Income by sector
********************************************************************************

bysort hhid: egen hh_agri_inc = total (labor_income_total) if broad_ind_1==1 , missing 
bysort hhid: egen hh_inc_agri = mean(hh_agri_inc)  
drop hh_agri_inc

bysort hhid: egen hh_ind_inc=total(labor_income_total) if broad_ind_2==1 | broad_ind_3==1 , missing 
bysort hhid: egen hh_inc_ind=mean(hh_ind_inc) 
drop hh_ind_inc

bysort hhid: egen hh_serv_inc=total(labor_income_total) if broad_ind_4==1
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

bysort hhid: egen num_agri_emp= total (broad_ind_1) , missing 
bysort hhid: egen num_indexcons_emp= total (broad_ind_2) , missing 
bysort hhid: egen num_cons_emp= total (broad_ind_3) , missing 
egen num_ind_emp=rowtotal(num_indexcons_emp num_cons_emp) , missing 
bysort hhid: egen num_serv_emp= total (broad_ind_4) , missing 

egen most_emp=rowmax(num_agri_emp num_ind_emp num_serv_emp)

gen hh_main_agri=(most_emp==num_agri_emp)
gen hh_main_ind=(most_emp==num_ind_emp)
gen hh_main_serv=(most_emp==num_serv_emp)

replace hh_main_agri=0 if most_emp==. | most_emp==0
replace hh_main_ind=0 if most_emp==. | most_emp==0
replace hh_main_serv=0 if most_emp==. | most_emp==0

bysort 	hhid: egen num_ecactive=total(lstat_active)
gen 	hh_lfpr	=	num_ecactive/(hhsize-num_kids)

gen share_employed = num_ecactive / hhsize 
gen sh_wrkng_adults = num_ecactive/num_adults 

bysort hhid: egen have_skilled_worker=max(skill_4)
bysort hhid: egen have_semiskilled_worker=max(skill_3)

bysort hhid: egen num_skilled_worker=total(skill_4), missing
bysort hhid: egen num_semiskilled_worker=total(skill_3) , missing

gen sh_skilled_worker 		= num_skilled_worker 	/ num_ecactive
gen sh_semiskilled_worker 	= num_semiskilled_worker / num_ecactive

sum sh_skilled_worker sh_semiskilled_worker

//Employment Status 
mdesc empstat 
bysort hhid: egen num_public_emp  		=total (empstat_1) , missing  
bysort hhid: egen num_pvt_emp     		=total (empstat_2)  , missing 
bysort hhid: egen num_family_worker 	=total (empstat_3) , missing  
bysort hhid: egen num_employer			=total (empstat_4) , missing 
bysort hhid: egen num_self_emp			=total (empstat_5) , missing 

bysort hhid: egen have_public_emp=max(empstat_1)
bysort hhid: egen have_pvt_emp=max(empstat_2)
bysort hhid: egen have_family_worker=max(empstat_3)
bysort hhid: egen have_employer=max(empstat_4)
bysort hhid: egen have_self_emp=max(empstat_5)

gen sh_employee =  (num_public_emp+num_pvt_emp) / num_ecactive
gen sh_selfempl =  num_self_emp / num_ecactive
gen sh_ecactive =  num_ecactive / hhsize 


********************************************************************************
// FIREWOOD/WATER CONSUMPTION 
********************************************************************************
tab q6_a
tab q6_b_1
tab q6_b_2
tab q6_b_3

recode q6_a (1=1) (2=0) , gen(ownconsumption)
gen collected_wood=0
replace collected_wood = 1 if q6_b_1>0 & q6_a==1

gen collected_water=0
replace collected_water = 1 if q6_b_2>0 & q6_a==1
 
gen homerepairs=0 
replace homerepairs = 1 if q6_b_3>0 & q6_a==1 
tab collected_wood q6_b_1
tab collected_wood 
tab collected_water 
tab homerepairs
corr collected_wood collected_water 

bys hhid: egen collects_water = max(collected_water)
bys hhid: egen collects_firewood = max(collected_wood)
bys hhid: egen does_homerepair = max(homerepairs)

*******************************************************************************
// Additional variables to improve predictions
*******************************************************************************
//computer literacy module 
destring c* , replace 
//activities by computer 
su c2_* c5-c7
recode c2_a (2=0) , gen (aware_activities)

tab c2_b_1
tab c2_b_2
tab c2_b_3
gen 	aware_for_edu 	= 	0 if c2_a!=.
replace aware_for_edu	=	1 if c2_b_1==1 

gen 	aware_for_work	=	0 if c2_a!=.
replace aware_for_work 	=	1 if c2_b_1==2| c2_b_2==2

recode c3_a (2=0) , gen (can_activities)

tab c3_b_1
tab c3_b_2
tab c3_b_3
gen can_for_edu 	= 0 if c3_a!=.
replace can_for_edu	=1 if c3_b_1==1 

gen can_for_work	=0 if c3_a!=.
replace can_for_work =1 if c3_b_1==2| c3_b_2==2


recode c5   (2=0) , gen (can_use_phone)
recode c6  (2=0)  , gen (emailed_12m)
recode c7  (2=0)  , gen (used_internet_12m)

tab c8_1
tab c8_2
tab c8_3
gen 	used_pc = 0 
replace used_pc =1 if c8_1==1 
gen 	used_phone  = 0 
replace used_phone =1 if c8_1==2 | c8_2==2 

tab c9_1
tab c9_2
tab c9_3
gen 	internet_office = 0 
replace internet_office =1 if c9_1 ==1 

gen 	internet_home =0 
replace internet_home=1 if c9_1==2 | c9_2==2 

//at household level 
foreach var in aware_activities aware_for_edu aware_for_work can_activities can_for_edu can_for_work can_use_phone emailed_12m used_internet_12m used_phone internet_office internet_home {
	
	bys hhid: 	egen hh_`var' =  max(`var')
	
	label var `var'  "At least one in HH: `var'"
}

*******************************************************************************
// Summary stats relevant variables 
*******************************************************************************

tab district 
tab eth
tab rel
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

mean dist_* sector_*  sinhala buddhist male age married hhsize noedu atleast_sec schoolage_noschool no_* cellphone computer [aw=weight]

mean lstat_active empstat_* broad_ind_* skill_*  [aw=weight]

mean inc_paidemp_mon [aw=weight] if inc_paidemp_mon!=0
mean inc_selfemp_mon [aw=weight] if inc_selfemp_mon!=0
//mean monthly_income [aw=weight] if monthly_income!=0

*keep hhid pid rship weight dist* sector urban sector_* hhsize district eth sin age sex male rel buddhist educat5 educat7 noedu atleast_sec schoolage_noschool marstat married empstat* lstat_active inc* rpccons hhexppm cellphone computer eye_dsablty hear_dsablty conc_dsord slfcre_dsablty comm_dsablty broad_industry skill_level broad_ind_* skill_* no_* major_* inc_emp_excl_bonus

gen data="LFS2023"

keep if rship==1

//recode have_public_emp have_pvt_emp have_family_worker have_employer have_self_emp have_skilled_worker have_semiskilled_worker (.=0)

*Household level variables
mean dist_* sector_* female_hh sinhala buddhist age married hhsize cellphone computer have_atleast_secedu have_schoolage_noschl share_dep share_kids num_kids have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp hh_lfpr have_emp_inc hh_inc_pc [aw=weight]

mean hh_inc_pc if hh_inc_pc!=0 [aw=weight]
mean hh_inc_paidemp if hh_inc_paidemp!=0 [aw=weight]
mean hh_inc_selfemp if hh_inc_selfemp!=0 [aw=weight]

gen popwt=hhsize*weight
svyset [pw=popwt] , psu(psu)
svy: total popwt 

foreach var in age sinhala buddhist married{
	rename `var' `var'_hhh 
}

//Percentiles of the income distribution by type of income 
pctile ptile_selfemp 		= hh_selfemp_primary_pc [pw=weight] , nq(100)
pctile ptile_wages 			= hh_wages_primary_pc [pw=weight] , nq(100)
pctile ptile_hh_inc_prim_nc = hh_inc_primary_nc_pc [pw=weight], nq(100)


keep hhid psu weight popwt sector province district subnatid2 dist_* sector* urban rural age_avg hhsize share_dep num_dep num_kids num_old share_kids dep_ratio cellphone computer have_atleast_secedu have_schoolage_noschl have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv  have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp have_emp_inc data hh_lfpr sex_ratio *mem* edu* 								///
 hh_wages have_* wages_hhh *_pc ptile* 														/// 
 labor_income* collects_* sh_in_school have_schoolage_noschl has_in_school has_*_disab *hhh ///
 sh_selfempl sh_employee sh_ecactive 														///
 hh_aware* hh_can* hh_use* hh_internet* hh_emailed* rpc* *_tot ///
 num_ecactive sh_adults sh_wrkng_adults num_adults num_wages num_self 
save "$data/lfs2023_clean", replace 
