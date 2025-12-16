***********************************************************************
*	Clean 2016 LFS
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

*********************************************************************************
//Clean lfs 2016 
*********************************************************************************
import delimited "$lfs/RAW/2016LFSAnnual_OutFile_With_Computer.csv", clear 

quietly destring p10-p14 q2-q5 q7-q10 q11-q14 q17 q21 q24-q27 q33 q39 q47 q48 q51 q44 q50 q45a1-q45c1 q46a1-q46c1 q58 q62 q63a2 q63a5, replace

	foreach v of varlist month sector district huno hhno{
		tostring `v', gen(`v'_str) format(%02.0f)
	}
	tostring psu, gen(psu_str) format(%03.0f)
	tostring hhserno, gen(ser_str) format(%03.0f)
	egen hhid=concat(month_str sector_str district_str psu_str huno_str hhno_str ser_str)
	label var hhid "Household id"

	tostring p1, gen(str_pid) format(%02.0f)
	egen pid=concat(hhid str_pid), punct("-")
	label var pid "Individual ID"

	gen weight=annual_factor 
	label var weight "Household sampling weight"

	rename psu psu_orig
	egen psu=concat(month_str sector_str district_str psu_str)
	label var psu "Primary sampling units"

	gen ssu=hhid
	label var ssu "Secondary sampling units"


	egen strata=concat(sector_str district_str)
	label var strata "Strata"

/*%%=============================================================================================
	3: Geography
================================================================================================*/

	gen subnatid1_num=district
	recode subnatid1_num (11/13=1) (21/23=2) (31/33=3) (41/45=4) (51/53=5) (61/62=6) (71/72=7) (81/82=8) (91/92=9)
	gen subnatid1=""
	replace subnatid1="1 - Western" if subnatid1_num==1
	replace subnatid1="2 - Central" if subnatid1_num==2
	replace subnatid1="3 - Southern" if subnatid1_num==3
	replace subnatid1="4 - Northern Area" if subnatid1_num==4
	replace subnatid1="5 - Eastern" if subnatid1_num==5
	replace subnatid1="6 - North-western" if subnatid1_num==6
	replace subnatid1="7 - North-central" if subnatid1_num==7
	replace subnatid1="8 - Uva" if subnatid1_num==8
	replace subnatid1="9 - Sabaragamuwa" if subnatid1_num==9
	label var subnatid1 "Subnational ID at First Administrative Level"

	gen subnatid2_num=district
	label de lblsubnatid2 11 "11-Colombo" 12 "12-Gampaha" 13 "13-Kalutara" 21 "21-Kandy" 22 "22-Matale" 23 "23-Nuwara Eliya" 31 "31-Galle" 32 "32-Matara" 33 "33-Hambantota" 41 "41-Jaffna" 42 "42-Kilinochchi" 43 "43-Mannar" 44 "44-Vavuniya" 45 "45-Mullaituvu" 51 "51-Batticaloa" 52 "52-Ampara" 53 "53-Trincomalee" 61 "61-Kurunegala" 62 "62-Puttalam" 71 "71-Anradhapura" 72 "72-Polonnaruwa" 81 "81-Badulla" 82 "82-Moneragala" 91 "91-Ratnapura" 92 "92-Kegalle"
	label values subnatid2_num lblsubnatid2
	decode (subnatid2_num), gen(subnatid2)
	label var subnatid2 "Subnational ID at Second Administrative Level"
	drop subnatid2_num

	gen subnatid3=""
	label var subnatid3 "Subnational ID at Third Administrative Level"

/*%%=============================================================================================
	4: Demography
================================================================================================*/
	gen byear=.
	replace byear=p5y+2000 if inrange(p5y,0,15)
	replace byear=p5y+1900 if inrange(p5y,16,99)
	gsort hhid byear
	bys hhid: gen newp1=_n
	bys hhid: egen hhsize=max(newp1)
	label var hhsize "Household size"

	gen age=.
	replace age=year-byear
	//replace age=98 if age>98 & age!=.
	label var age "Individual age"

	gen male=p4 if inrange(p4,1,2)
	recode male 2=0
	label var male "Sex - Ind is male"
	la de lblmale 1 "Male" 0 "Female"
	label values male lblmale

	gen byte relationharm=p3
	recode relationharm (7/9=6)
	label var relationharm "Relationship to the head of household - Harmonized"
	la de lblrelationharm  1 "Head of household" 2 "Spouse" 3 "Children" 4 "Parents" 5 "Other relatives" 6 "Other and non-relatives"
	label values relationharm lblrelationharm

	gen relationcs=p3
	label var relationcs "Relationship to the head of household - Country original"

	gen byte marital=p9 if inrange(p9,1,5)
	recode marital (1=2) (2=1) (3=5) (5=4)
	label var marital "Marital status"
	la de lblmarital 1 "Married" 2 "Never Married" 3 "Living together" 4 "Divorced/Separated" 5 "Widowed"
	label values marital lblmarital

********************************************************************************
// Code from 2019 // 
********************************************************************************
*District dummies
tab district, gen(dist_)
tab sector, gen(sector_)

* urban = Urban (1) or rural (0)
recode sector (2/3=0) (1=1) (*=.), g(urban)
gen rural=sector==2 

su p12 - p14 
* language = Language
g		language = "Sinhala" if p12==1
replace	language = language + ", " + "Tamil" if ~missing(language) & p13==1
replace	language = "Tamil" if missing(language) & p13==1
replace	language = language + ", " + "English" if ~missing(language) & p14==1
replace	language = "English" if missing(language) & p14==1


*Ethnicity
label define ethn 1"Sinhala" 2"Tamil" 3"Indian Tamil" 4"Moor/Muslim" 5"Malay" 6"Burgher" 9"Other"
label values p7 ethn 
tab p7
gen sinhala=(p7==1)

*religion 
gen buddhist=p8==1 

* age = Age of individual (continuous)
confirm var age

*married 
gen married=marital ==1 


*Female headed household
gen femaleHHH=(relationharm==1 & male==0)
bysort hhid: egen female_hhh=max(femaleHHH)

********************************************************************************
	//Dependency Ratios  
********************************************************************************
bys hhid: egen age_avg=mean(age)

gen dep=age<15 | age>=65
gen child=age<15
gen old= age>=65

gen labor = age>=15 & age<65 

bysort hhid: egen num_deps=total (dep)
bysort hhid: egen num_kids=total (child)
bysort hhid: egen num_old=total (old)
bysort hhid: egen num_labor=total (labor) , missing 

gen dep_ratio=num_deps/num_labor
gen share_dep=num_deps/hhsize
gen share_kids=num_kids/hhsize

mdesc dep_ratio 
tab dep_ratio if num_deps==num_labor 
tab dep_ratio 

********************************************************************************
//Education
********************************************************************************
	gen byte ed_mod_age=5
	label var ed_mod_age "Education module application age"
	
	gen school=.
	replace school=0 if p11==5
	replace school=1 if inrange(p11,1,4)
	replace school=. if age<ed_mod_age & age!=.
	label var school "Attending school"
	la de lblschool 0 "No" 1 "Yes"
	label values school lblschool
	gen byte literacy=.
	replace literacy=1 if p12==1|p13==1|p14==1
	replace literacy=0 if p12!=1&p13!=1&p14!=1
	replace literacy=. if age<10 &!mi(literacy)
	label var literacy "Individual can read & write"
	la de lblliteracy 0 "No" 1 "Yes"
	label values literacy lblliteracy
	
	gen byte educy=p10
	replace educy=p10 if inrange(p10,0,13)
	replace educy=16 if p10==14
	replace educy=18 if p10==15
	replace educy=19 if p10==16
	replace educy=0 if p10==19
	replace educy=. if age<ed_mod_age
	replace educy=. if educy>age & !mi(educy) & !mi(age)
	label var educy "Years of education"
*</_educy_>


*<_educat7_>
	gen byte educat7=.
	replace educat7=1 if p10==19
	replace educat7=2 if inrange(p10,0,4)
	replace educat7=3 if p10==5
	replace educat7=4 if inrange(p10,6,10)
	replace educat7=5 if inrange(p10,11,13)
	replace educat7=6 if p10==14
	replace educat7=7 if inlist(p10,15,16)
	replace educat7=. if age<ed_mod_age
	label var educat7 "Level of education 1"
	la de lbleducat7 1 "No education" 2 "Primary incomplete" 3 "Primary complete" 4 "Secondary incomplete" 5 "Secondary complete" 6 "Higher than secondary but not university" 7 "University incomplete or complete"
	label values educat7 lbleducat7
*</_educat7_>


*<_educat5_>
	gen byte educat5=educat7
	recode educat5 (4=3) (5=4) (6 7=5)
	replace educat5=. if age<ed_mod_age	
	label var educat5 "Level of education 2"
	la de lbleducat5 1 "No education" 2 "Primary incomplete"  3 "Primary complete but secondary incomplete" 4 "Secondary complete" 5 "Some tertiary/post-secondary"
	label values educat5 lbleducat5 
*</_educat5_>

**********************************************
// Imputation variables 
**********************************************

	gen noedu=educat5==1
	gen atleast_sec=(educat7==5 | educat7==6)
	gen schoolage_noschool=(age>=5 & age<=16 & school==0)


	bysort hhid: egen have_atleast_secedu=max(atleast_sec)
	bysort hhid: egen have_schoolage_noschl=max(schoolage_noschool)

//currently in edu 
tab school 
gen curr_school = school ==1 
tab curr_school school 
bys hhid: egen has_in_school = max(curr_school)
gen sh_in_school = has_in_school / hhsize 


*Sex structure 
		g aux_fem 	= p4==2
		g aux_male	= p4==1
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

	g aux_edu_hhh_none		= educat5==1 if relationharm==1
	g aux_edu_hhh_prim		= educat5==2 if relationharm==1
	g aux_edu_hhh_secincomp	= educat5==3 if relationharm==1
	g aux_edu_hhh_sec		= educat5==4 if relationharm==1
	g aux_edu_hhh_high 		= educat5==5 if relationharm==1
	g aux_edu_yr_hhh = 		educat5 if relationharm==1
	g aux_edu_yr_max = 		educat5
	
	loc tomax edu_hhh_none edu_hhh_prim edu_hhh_secincomp edu_hhh_sec edu_hhh_high edu_yr_hhh edu_yr_max
	foreach var of loc tomax {
	bys hhid: egen `var'=max(aux_`var')
	//assert `var'!=. // Froze because of problem with 2 hhid's without hhhead
	drop aux_`var'
	}

	tab hhid if relationharm==. 
	
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
//Disability: No disability module in 2016 :(
********************************************************************************
/*
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
*/

********************************************************************************
//Digital device ownership
********************************************************************************

*Ownership of a computer (individual): No cell phone in 2016
destring qc*, replace
recode qc01_* (.=0)

//egen cellphone_i = rowmin(c1_4a c1_5a)
//recode cellphone_i (2=0)

egen computer_i = rowmin(qc01_1 qc01_2)
recode computer_i (2=0)

//bysort hhid: egen cellphone_hh=total(cellphone_i)
bysort hhid: egen computer_hh=total(computer_i)

//gen cellphone=(cellphone_hh>0)
gen computer=(computer_hh>0)

********************************************************************************
// EMPLOYMENT STATUS  
********************************************************************************

destring q*, replace 

* minlaborage - Labor module application age (7-day ref period)
g minlaborage = 15

	gen byte lstatus=.
	replace lstatus=1 if !mi(q7)
	replace lstatus=2 if (q48==1&q51==1)|q47==3
	replace lstatus=3 if lstatus==. 
	replace lstatus=. if age<minlaborage
	label var lstatus "Labor status"
	la de lbllstatus 1 "Employed" 2 "Unemployed" 3 "Non-LF"
	label values lstatus lbllstatus
	
	gen lstat_active=lstatus==1

* empstat - Employment status, primary job (7-day ref period)

tab q9 

//no public vs private in 2016
tab q9
recode q9 (1=1) (3=2) (4=3) (2=4) if lstatus==1, g(empstat)
label define empstat 1"Employee" 2 "Self-employed"  3 "Family Worker" 4 "Employer" 
label values empstat empstat 
tab empstat, gen(empstat_)

tab q14
recode q14 (1/2=1) (3=0), gen(public_sector)
recode q14 (1/2=0) (3=1), gen(private_sector)

tab public_sector q14 
gen public_emp=0 if empstat!=. 
replace public_emp =1 if empstat==1 & public_sector==1

gen private_emp=0 if empstat!=. 
replace private_emp =1 if empstat==1 & private_sector==1

gen employee				=empstat==1
gen self_employed          	=empstat==2
gen familyworker			=empstat==3 
gen employer  				=empstat==4

tab employee empstat 
tab self_employed empstat 
tab employer empstat 
tab familyworker empstat 

foreach var in public_emp private_emp employee self_employed employer  familyworker{
	gen `var'_hhh = `var' if relationharm==1 
}


********************************************************************************
*Industry
********************************************************************************

gen industry=q8
tostring industry, replace
replace industry="0"+industry if strlen(industry)==4

gen main_industry=substr(industry, 1, 2)
destring main_industry, replace
tab main_industry

gen broad_industry=1	 if main_industry<=3 //include fisheries in agriculture 
replace broad_industry=2 if main_industry>=5 & main_industry<40
replace broad_industry=3 if main_industry>=41 & main_industry<=43
replace broad_industry=4 if broad_industry==. & main_industry!=.

label define broad_ind 1 "Agri" 2 "Manufacturing (excl construction)" 3 "Construction" 4 "Services"
label values broad_industry broad_ind

tab broad_industry, gen(broad_ind_)

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
destring q45* q46* , replace

//Check for zeros: 
su q45a* , d
su q45b* ,d 
su q45c* , d 
su q46a*, d  
su q46b* , d 
su q46c* , d 

********************************************************************************
//Only wages: SARLAB HARMONIZATION  
//Wage_nc_week - Wage payment adjusted, primary/secondary job, excl. bonuses
********************************************************************************
//check daily earners 
gen daily1 =  q45b1 * q45b2 
gen daily2 = q45b3

sum daily* , d // more similar than in 2019 

//primary occupation
sum q45* ,d  

g		wage_nc = q45a1 			if lstatus==1 & ~mi(q45a1)
replace	wage_nc = q45b1 * q45b2 if lstatus==1 & ~mi(q45b1)
//replace wage_nc = q45c1 			if lstatus==1 & ~mi(q45c1)

//secondary occupation 
su q46* ,d 
g		wage_nc_2 = q46a1 			if lstatus==1 & ~mi(q46a1)
replace	wage_nc_2 = q46b1 * q46b2 	if lstatus==1 & ~mi(q46b1)
//replace	wage_nc_2 = q46c1 			if lstatus==1 & ~mi(q46c1)

gen 	wages_primary 		= wage_nc 
egen 	ind_wages			= rowtotal(wage_nc wage_nc_2) , missing  

mdesc ind_wages if lstatus==1  
sum wages_primary ind_wages, d 


********************************************************************************
//SARLAB HARMONIZATION: see Do/Help/LKA_2019_LFS_v01_M_v01_A_SARLD_ALL 
*total wage, primary job (7-day ref period): Wages+Allowances (overtime and in-kind)
********************************************************************************

egen	wage_total_mse_month = rowtotal(q45a1 q45a2 q45a3), missing
g		wage_total_we_wages_month = q45b1 * q45b2
g		wage_total_we_inkind_month = q45b4
egen	wage_total_month = rowtotal(wage_total_mse_month wage_total_we_wages_month wage_total_we_inkind_month /*q45c1*/), mis
g		wage_total = wage_total_month if lstatus==1


* wage_total_2 - Annualized total wage, secondary job (7-day ref period)
egen	wage_total_2_mse_month = rowtotal(q46a1 q46a2 q46a3), missing
g		wage_total_2_we_wages_month = q46b1 * q46b2
g		wage_total_2_we_inkind_month = q46b4
egen	wage_total_2_month = rowtotal(wage_total_2_mse_month wage_total_2_we_wages_month wage_total_2_we_inkind_month /*q46c1*/), mis
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
gen 	inc_selfemp_primary = q45c1 
egen 	inc_selfemp_mon= rowtotal (q45c1 q46c1) , missing 

********************************************************************************
*Has secondary employment 
********************************************************************************
gen has_secondjob= 0 if lstatus==1 
replace has_secondjob=1 if q46c1>0 & q46c1!=. | wage_total_2>0 & wage_total_2!=. 
tab has_secondjob 

********************************************************************************
*Total Labor Income 
********************************************************************************

egen labor_income_total = rowtotal (inc_paidemp_mon inc_selfemp_mon) 	, missing

egen labor_income_nc 			= rowtotal (ind_wages inc_selfemp_mon) 	, missing  //non-contributory 
egen labor_income_primary_nc 	= rowtotal (wages_primary inc_selfemp_primary) 	, missing  //non-contributory 

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

bys 	hhid: egen hh_mon_inc 		= total   	 (labor_income_total) , missing 
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
gen wages_hhh 			= wage_total 	if relationharm==1 
gen wages_primary_hhh 	= wages_primary if relationharm==1 
gen incself_hhh = inc_selfemp_mon 		if relationharm==1 
gen has_secondjob_hhh = has_secondjob if relationharm==1 

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

merge m:1 year month using "$data/NCPI_series", keepusing(cpi_base2013 avg_2016 avg_2019 ) 
keep if _merge==3 
drop _merge 

merge m:1 district using "$data/HIES/RAW/spatial_priceindex.dta",  nogen
bys year: egen avg_cpi = mean(cpi_base2013)
tab avg_cpi avg_2016
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
foreach var in rpcinc1 rpcwage1 rpcself1 rpcinc_tot rpcwage_tot rpcself_tot /*rpcinc2 rpcwage2 rpcself2 */{
	replace `var' = `var'*lpindex1
}

tabstat  hh_inc_primary_nc_pc rpcinc* cpi_base2013, by(month)
mdesc rpc* 
sum rpc*

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

bysort hhid: egen num_ecactive=total(lstat_active)
gen hh_lfpr=num_ecactive/(hhsize-num_kids)

bysort hhid: egen have_skilled_worker=max(skill_4)
bysort hhid: egen have_semiskilled_worker=max(skill_3)

//Employment Status 
mdesc empstat 
bysort hhid: egen num_public_emp  		=total (public_emp) , missing  
bysort hhid: egen num_pvt_emp     		=total (private_emp)  , missing 
bysort hhid: egen num_family_worker 	=total (familyworker) , missing  
bysort hhid: egen num_employer			=total (employer) , missing 
bysort hhid: egen num_self_emp			=total (self_employed) , missing 

bysort hhid: egen have_public_emp	=max(public_emp)
bysort hhid: egen have_pvt_emp		=max(private_emp)
bysort hhid: egen have_family_worker=max(familyworker)
bysort hhid: egen have_employer		=max(employer)
bysort hhid: egen have_self_emp		=max(self_employed)

gen sh_employee =  (num_public_emp+num_pvt_emp) / num_ecactive
gen sh_selfempl =  num_self_emp / num_ecactive
gen sh_ecactive = num_ecactive / hhsize 

bys hhid : egen num_secondjob = total (has_secondjob), missing 
gen share_secondjob = num_secondjob / num_ecactive

********************************************************************************
// FIREWOOD/WATER CONSUMPTION 
********************************************************************************
tab q6a
su q6bhh1 q6bhh2 q6bhh3 

recode q6a (1=1) (2=0) , gen(ownconsumption)
gen collected_wood=0
replace collected_wood = 1 if q6bhh1>0 & ownconsumption==1

gen collected_water=0
replace collected_water = 1 if q6bhh2>0 & ownconsumption==1
 
gen homerepairs=0 
replace homerepairs = 1 if q6bhh3>0 & ownconsumption==1
 
tab collected_wood q6bhh1
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
destring qc* , replace 

//activities by computer 
su 		qc04* 
recode 	qc04 (2=0) , gen (aware_activities)

tab qc04a1
tab qc04a2
tab qc04a3

gen 	aware_for_edu 	= 	0 if qc04!=.
replace aware_for_edu	=	1 if qc04a1==1 

gen 	aware_for_work	=	0 if qc04!=.
replace aware_for_work 	=	1 if qc04a1==2| qc04a2==2

recode qc05 (2=0) , gen (can_activities)

tab qc05a1
tab qc05a2
tab qc05a3
gen can_for_edu 	= 0 if qc05!=.
replace can_for_edu	=1 if qc05a1==1 

gen can_for_work	=0 if qc05!=.
replace can_for_work =1 if qc05a1==2| qc05a2==2


recode qc07  (2=0) , gen (can_use_phone)
recode qc08  (2=0)  , gen (emailed_12m)
recode qc09  (2=0)  , gen (used_internet_12m)

tab qc10a1
tab qc10a2
tab qc10a3
tab qc10a4

gen 	used_pc = 0 
replace used_pc =1 if qc10a1==1 
gen 	used_phone  = 0 
replace used_phone =1 if qc10a1==2 | qc10a2==2 

tab qc11a1  
tab qc11a2
tab qc11a3
gen 	internet_office = 0 
replace internet_office =1 if qc11a1 ==1 

gen 	internet_home =0 
replace internet_home=1 if qc11a2==2 | qc11a1==2 

//at household level 
foreach var in aware_activities aware_for_edu aware_for_work can_activities can_for_edu can_for_work can_use_phone emailed_12m used_internet_12m used_phone internet_office internet_home {
	
	bys hhid: 	egen hh_`var' =  max(`var')
	
	label var `var'  "At least one in HH: `var'"
}

*******************************************************************************
// Summary stats relevant variables 
*******************************************************************************
gen data="LFS2016"

keep if relationharm==1

//recode have_public_emp have_pvt_emp have_family_worker have_employer have_self_emp have_skilled_worker have_semiskilled_worker (.=0)

*Household level variables
//mean dist_* sector_* female_hh sinhala buddhist age married hhsize  computer have_atleast_secedu have_schoolage_noschl share_dep share_kids num_kids have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp hh_lfpr have_emp_inc hh_inc_pc [aw=weight]

mean hh_inc_pc if hh_inc_pc!=0 [aw=weight]
mean hh_inc_paidemp if hh_inc_paidemp!=0 [aw=weight]
mean hh_inc_selfemp if hh_inc_selfemp!=0 [aw=weight]

gen popwt		=hhsize*weight
svyset 			[pw=popwt] , psu(psu)
svy: 			total popwt 

foreach var in age sinhala buddhist married{
	rename `var' `var'_hhh 
}

//Percentiles of the income distribution by type of income 
pctile ptile_selfemp 		= hh_selfemp_primary_pc [pw=weight] , nq(100)
pctile ptile_wages 			= hh_wages_primary_pc [pw=weight] , nq(100)
pctile ptile_hh_inc_prim_nc = hh_inc_primary_nc_pc [pw=weight], nq(100)


keep hhid psu weight popwt sector district dist_* sector* urban rural age_avg hhsize share_dep num_dep num_kids num_old share_kids dep_ratio computer have_atleast_secedu have_schoolage_noschl have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv  have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp have_emp_inc data hh_lfpr sex_ratio *mem* edu* hh_wages have_* wages_hhh *_pc ptile* labor_income* collects_* sh_in_school have_schoolage_noschl has_in_school *hhh sh_selfempl sh_employee sh_ecactive hh_aware* hh_can* hh_use* hh_internet* hh_emailed* rpc* has_secondjob_hhh share_secondjob

save "$data/lfs2016_clean", replace 
