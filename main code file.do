clear all 
set more off

***load the ACS data 
cd "E:\DATA"
use acsdata.dta, clear

***use only a subset of the data
*time window: 2005 2006 2007 2008 | 2009 | 2010 2011 2012 2013
*age: [18,65)
drop if year<2005
drop if year>2013
drop if age<18
drop if age>=65

***adjust wages for inflation, using 1999 as base year
gen incw99 = incwage * cpi99
label variable incw99 "Wage and salary income in 1999 dollars"

***create year dummy
gen tau=1 if year >= 2009
replace tau=0 if tau==.
label variable tau "indicator of year 2009 and afterwards"

***create industry dummy based on occupations (the OCC variable)
*correction
gen isstem=2 if occ==0 | occ==. /*missing values*/
replace isstem=1 if (occ==110 | occ==300 | occ==360 | (occ>=1000 & occ<= 1240) | (occ>=1310 & occ<= 1965) | occ==4930) /*STEM OCC*/
replace isstem=0 if isstem==. /*Non-STEM OCC*/
replace isstem=. if isstem==2 /*bring back the missing values*/

label variable isstem "indicator of a STEM occupation"
label values isstem isstem_lbl

***create age dummies (two cutoffs)
gen leq25=1 if age<=25
replace leq25=0 if leq25==.
label variable leq25 "indicator of age less than or equal to 25"

gen leq30=1 if age<=30
replace leq30=0 if leq30==.
label variable leq30 "indicator of age less than or equal to 30"

label define leq25l 1 "young" 0 "old"
label define leq30l 1 "young" 0 "old"
label values leq25 leq25l
label values leq30 leq30l

***number the four groups based on the industry dummy and the age dummy
gen group25=0 if isstem==0 & leq25==0
replace group25=1 if isstem==0 & leq25==1
replace group25=2 if isstem==1 & leq25==0
replace group25=3 if isstem==1 & leq25==1
replace group25=4 if group25==.
label variable group25 "indicator of the four groups based on the 25 age cutoff"

gen group30=0 if isstem==0 & leq30==0
replace group30=1 if isstem==0 & leq30==1
replace group30=2 if isstem==1 & leq30==0
replace group30=3 if isstem==1 & leq30==1
replace group30=4 if group30==.
label variable group30 "indicator of the four groups based on the 30 age cutoff"

/*label define grouprule 0 "Non-STEM, Old" 1 "Non-STEM, Young" 2 "STEM, Old" 3 "STEM, Young" 4 "N/A"*/
label values group25 grouprule
label values group30 grouprule

*define a dummy for having higher education
gen highereduc=1 if educ>=7
replace highereduc=0 if highereduc==.
label variable highereduc "indicator of having at least one year of postsecondary education"
label define highereduc_lbl 1 "postsecondary educ" 0 "high school or less"
label values highereduc highereduc_lbl

*percentage of people with postsecondary educ by STEM and Non-STEM
tab isstem highereduc [fw=perwt], missing row


*regression
*log transform wages (Y)
generate ln_incw99 = log(incw99)
label variable ln_incw99 "ln transformation of incw99"

**categorical data (covariates and groups)
*sex
gen female= (sex==2)
gen male= (sex==1)
label variable male "indicator of male"
label variable female "indicator of female"

*employment status
tab empstat,generate(emp) /*no N/A */
rename (emp1 emp2 emp3) (employed unemployed notinlf)

*marital status
gen married=(marst==1 | marst==2) /*married (spouse present and absent) vs others (including separated, divorced, widowed, never married)*/
label variable married "indicator of being married"

*race
gen white=(race==1)
gen asian=(race==4 | race==5 | race==6)
gen raceother=(race==2 | race==3| race==7|race==8|race==9)

label define racecat_lbl 1 "White" 2 "Asian" 3 "raceother"

gen racecat=1 if race==1
replace racecat=2 if race==4 | race==5 | race==6
replace racecat=3 if race==2 | race==3| race==7|race==8|race==9

label values racecat racecat_lbl
label variable racecat "race groups: 3"

*ind
label define indcat_lbl 0 "N/A" 1 "Agriculture, Forestry, Fishing and Hunting" 2 "Mining" 3 "Construction" 4 "Manufacturing" 5 "Wholesale Trade" 6 "Retail Trade" 7 "Transportation and Warehousing" 8 "Utilities" 9 "Information and Communications" 10 "Finance, Insurance Real Estate, and Rental and Leasing" 11 "Professional, Scientific, Management, Administrative, and Waste Management Services" 12 "Educational, Health and Social Services" 13 "Arts, Entertainment, Recreation, Accommodations, and Food Services" 14 "Other Services (Except Public Administration)" 15 "Public Administration" 16 "Armed Forces" 17 "Unemployed"

gen indcat=0 if ind==0
replace indcat=1 if ind>=170 & ind<=290
replace indcat=2 if ind>=370 & ind<=490
replace indcat=3 if ind==770
replace indcat=4 if ind>=1070 & ind<=3990
replace indcat=5 if ind>=4070 & ind<=4590
replace indcat=6 if ind>=4670 & ind<=5790
replace indcat=7 if ind>=6070 & ind<=6390
replace indcat=8 if ind>=570 & ind<=690
replace indcat=9 if ind>=6470 & ind<=6780
replace indcat=10 if ind>=6870 & ind<=7190
replace indcat=11 if ind>=7270 & ind<=7790
replace indcat=12 if ind>=7860 & ind<=8470
replace indcat=13 if ind>=8560 & ind<=8690
replace indcat=14 if ind>=8770 & ind<=9290
replace indcat=15 if ind>=9370 & ind<=9590
replace indcat=16 if ind>=9670 & ind<=9870
replace indcat=17 if ind==9920

label values indcat indcat_lbl
label variable indcat "industry categories: 17"

*define occ categories:
gen occcat=0 if occ==0 | occ==.
*management
replace occcat=1 if occ>=10 & occ<=430 & occ!= 425
*Business
replace occcat=2 if (occ>=500 & occ<=740) | (occ==425)
*Financial
replace occcat=3 if (occ>=800 & occ<=950)
*CS and Math
replace occcat=4 if (occ>=1000 & occ<=1240)
*Architechture and Engineering
replace occcat=5 if (occ>=1300 & occ<=1560)
*Life, Physical, and Social Science
replace occcat=6 if (occ>=1600 & occ<=1965)
*Community and Social Services
replace occcat=7 if (occ>=2000 & occ<=2060)
*Legal Occupation
replace occcat=8 if (occ>=2100 & occ<=2145)
*Education
replace occcat=9 if (occ>=2200 & occ<=2550)
*Arts and media
replace occcat=10 if (occ>=2600 & occ<=2920)
*Healthcare
replace occcat=11 if (occ>=3000 & occ<=3540)
*Healthcare support
replace occcat=12 if (occ>=3600 & occ<=3655)
*Protective Services
replace occcat=13 if (occ>=3700 & occ<=3955)
*Food Prep
replace occcat=14 if (occ>=4000 & occ<=4150)
*Building and Cleaning
replace occcat=15 if (occ>=4200 & occ<=4250)
*Personal care and Service
replace occcat=16 if (occ>=4300 & occ<=4650)
*Sales
replace occcat=17 if (occ>=4700 & occ<=4965)
*Office
replace occcat=18 if (occ>=5000 & occ<=5940)
*Farming, fishing and forestry
replace occcat=19 if (occ>=6000 & occ<=6130)
*Construction
replace occcat=20 if (occ>=6200 & occ<=6765)
*Extraction
replace occcat=21 if (occ>=6800 & occ<=6940)
*Installation
replace occcat=22 if (occ>=7000 & occ<=7630)
*Production
replace occcat=23 if (occ>=7700 & occ<=8965)
*Transportation
replace occcat=24 if (occ>=9000 & occ<=9750)
*Military
replace occcat=25 if (occ>=9800 & occ<=9920)

label define occ_lbl 0 "N/A" 1 "Management, Business, Science, and Arts Occupations" 2 "Business Operations Specialists" 3 "Financial Specialists" 4 "Computer and Mathematical Occupations" 5 "Architecture and Engineering Occupations" 6 "Life, Physical, and Social Science Occupations" 7 "Community and Social Services Occupations" 8 "Legal Occupations" 9 "Education, Training, and Library Occupations" 10 "Arts, Design, Entertainment, Sports, and Media Occupations" 11 "Healthcare Practitioners and Technical Occupations" 12 "Healthcare Support Occupations" 13 "Protective Service Occupations" 14 "Food Preparation and Serving Occupations" 15 "Building and Grounds Cleaning and Maintenance Occupations" 16 "Personal Care and Service Occupations" 17 "Sales and Related Occupations" 18 "Office and Administrative Support Occupations" 19 "Farming, Fishing, and Forestry Occupations" 20 "Construction and Extraction Occupations" 21 "Extraction Workers" 22 "Installation, Maintenance, and Repair Workers" 23 "Production Occupations" 24"Transportation and Material Moving Occupations" 25 "Military Specific Occupations"
label values occcat occ_lbl

*citizenship 
/*asuume that citizen==0 indicates the individual is a US citizen*/
gen uscitizen=(citizen==0)
label variable uscitizen "indicator of being a US citizen

*time-trend
egen isstem2_year=concat(isstem year), p(-)
*cluster
egen indcat_year_st=concat(indcat year statefip), p(-)

preserve
*keep only full time workers
drop if employed==0
drop if uhrswork<30

*base
reg ln_incw99 tau##isstem##leq25 [pw=perwt], r
estimates store base

xi: reg ln_incw99 tau##isstem##leq25 i.indcat i.statefip i.year [pw=perwt], r
estimates store addfixed

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year [pw=perwt], r
estimates store addcontrols

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt], r
estimates store addstemtd

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt], robust cluster(indcat_year_st)
estimates store addcluster

esttab base addfixed addcontrols addstemtd addcluster using "table1round1", rtf replace se label mtitles("base" "fixed effect" "controls" "stem-specific time-trend" "cluster-robust se") addnotes("I: cluster=(indcat, year)" "II: cluster=(indcat, year, statefip)" "Robust standard errors.") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

*by group
*unclustered
xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==1 [pw=perwt], r
estimates store male

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==2 [pw=perwt], r
estimates store female

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==1 [pw=perwt], r
estimates store citizen

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==0 [pw=perwt], r
estimates store foreigner
*clustered
xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==1 [pw=perwt], robust cluster(indcat_year_st)
estimates store male_cluster

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==2 [pw=perwt], robust cluster(indcat_year_st)
estimates store female_cluster

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==1 [pw=perwt], robust cluster(indcat_year_st)
estimates store citizen_cluster

xi: reg ln_incw99 tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==0 [pw=perwt],robust cluster(indcat_year_st)
estimates store foreigner_cluster

esttab male female citizen foreigner male_cluster female_cluster citizen_cluster foreigner_cluster using "table2round1", rtf replace se label mtitles("male" " female" "citizen" "foreigner" "male" "female" "citizen" "foreigner") addnotes("The first four columns have unclustered se" "The last four columns have cluster-robust se") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

esttab male_cluster female_cluster citizen_cluster foreigner_cluster using "table2round2", rtf replace se label mtitles("male" "female" "citizen" "foreigner") addnotes("Cluster-Robust Standard Error") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)

restore
preserve

*uhrswork
reg uhrswork tau##isstem##leq25 [pw=perwt], r
estimates store base

xi: reg uhrswork tau##isstem##leq25 i.indcat i.statefip i.year [pw=perwt], r
estimates store addfixed

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year [pw=perwt], r
estimates store addcontrols

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt], r
estimates store addstemtd

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt], robust cluster(indcat_year_st)
estimates store addcluster

esttab base addfixed addcontrols addstemtd addcluster using "table3round1", rtf replace se label mtitles("base" "fixed effect" "controls" "stem-specific time-trend" "cluster-robust se") addnotes("I: cluster=(indcat, year)" "II: cluster=(indcat, year, statefip)" "Robust standard errors.") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)


*by group
*unclustered
xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==1 [pw=perwt], r
estimates store male

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==2 [pw=perwt], r
estimates store female

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==1 [pw=perwt], r
estimates store citizen

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==0 [pw=perwt], r
estimates store foreigner
*clustered
xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==1 [pw=perwt], robust cluster(indcat_year_st)
estimates store male_cluster

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if sex==2 [pw=perwt], robust cluster(indcat_year_st)
estimates store female_cluster

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==1 [pw=perwt], robust cluster(indcat_year_st)
estimates store citizen_cluster

xi: reg uhrswork tau##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if uscitizen==0 [pw=perwt],robust cluster(indcat_year_st)
estimates store foreigner_cluster

esttab male female citizen foreigner male_cluster female_cluster citizen_cluster foreigner_cluster using "table4round1", rtf replace se label mtitles("male" " female" "citizen" "foreigner" "male" "female" "citizen" "foreigner") addnotes("The first four columns have unclustered se" "The last four columns have cluster-robust se") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)



******Event Study
preserve
*keep only full time workers
drop if employed==0
drop if uhrswork<30
/*timetrend = year-2004
2005 2006 2007 2008 2009 2010 2011 2012 2013
  1    2    3    4    5    6    7    8    9 */
  
xi: reg ln_incw99 year##isstem##leq25 highereduc i.racecat married i.indcat i.statefip [pw=perwt],robust cluster(indcat_year_st)
estimates store event_wages
  
xi: reg uhrswork year##isstem##leq25 highereduc i.racecat married i.indcat i.statefip [pw=perwt],robust cluster(indcat_year_st)
estimates store event_uhrs
  
xi: reg ln_incw99 year##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt],robust cluster(indcat_year_st)

xi: reg uhrswork i.year##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.isstem2_year [pw=perwt],robust cluster(indcat_year_st)
estimates store event_clusted
******


*Falsification Test
preserve
drop if employed==0
drop if uhrswork<30

gen tau06=(year>=2006)
xi: reg ln_incw99 tau06##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if year<= 2007 [pw=perwt],robust cluster(indcat_year_st)
estimates store event_06

gen tau07=(year>=2007)
xi: reg ln_incw99 tau07##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if year<=2008 [pw=perwt],robust cluster(indcat_year_st)
estimates store event_07

gen tau11=(year>=2011)
xi: reg ln_incw99 tau11##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if year>= 2010 & year<= 2012 [pw=perwt],robust cluster(indcat_year_st)
estimates store event_11

gen tau12=(year>=2012)
xi: reg ln_incw99 tau12##isstem##leq25 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year if year>= 2010 & year<= 2013 [pw=perwt],robust cluster(indcat_year_st)
estimates store event_12

esttab event_06 event_07 event_11 event_12 using "falsification_test_new", rtf replace se label mtitles("2006" "2007" "2011" "2012") addnotes("cluster-robust standard error in the parentheses") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)


*Plot
* 25, incw99
restore 
preserve
drop if employed==0
drop if uhrswork<30

bysort year isstem leq25: asgen meaninc25 = incw99, w(perwt)
collapse (mean) meaninc25, by(year isstem leq25)
egen max_age=max(meaninc25), by(year isstem)
egen min_age=min(meaninc25), by(year isstem)
bysort year isstem: gen delbyage=max_age-min_age
egen max_stem=max(meaninc25), by(year leq25)
egen min_stem=min(meaninc25), by(year leq25) 
bysort year leq25: gen delbystem=max_stem-min_stem

summarize delbyage
twoway (connected delbyage year if isstem ==1) (connected delbyage year if isstem==0), ytitle("difference in the group mean by age") legend(label(1 "STEM") label(2 "Non-STEM")) title("difference in the group mean of wages by age") yscale(range(10000 45000)) ylabel(minmax) xline(2009, xcolor(olive) lpattern(dash))

summarize delbystem
twoway (connected delbystem year if leq30==1) (connected delbystem year if leq30==0), ytitle("difference in the group mean by STEM") legend(label(1 "Young") label(2 "Old")) title("difference in the group mean of wages by STEM") yscale(range(6000 30000)) ylabel(minmax) xline(2009, lcolor(olive)  lpattern(dash))

*uhrswork
restore 
preserve
bysort year isstem leq25: asgen meanuhrs25 = uhrswork, w(perwt)
collapse (mean) meanuhrs25, by(year isstem leq25)
egen max_age=max(meanuhrs25), by(year isstem)
egen min_age=min(meanuhrs25), by(year isstem)
bysort year isstem: gen delbyage=max_age-min_age
egen max_stem=max(meanuhrs25), by(year leq25)
egen min_stem=min(meanuhrs25), by(year leq25) 
bysort year leq25: gen delbystem=max_age-min_age

summarize delbyage
*change range
twoway (connected delbyage year if isstem ==1) (connected delbyage year if isstem==0), ytitle("difference in the group mean by age") legend(label(1 "STEM") label(2 "Non-STEM")) title("difference in the group mean of usual hours worked per week by age") yscale(range(0, 20)) ylabel(minmax)

summarize delbystem 
*change range
twoway (connected delbystem year if leq25==1) (connected delbystem year if leq25==0), ytitle("difference in the group mean by STEM") legend(label(1 "Young") label(2 "Old")) title("difference in the group mean of usual hours worked per week by STEM") yscale(range(0, 20)) ylabel(minmax)

* 30 age cutoffs
*ln_incw99
restore 
preserve
drop if employed==0
drop if uhrswork<30

bysort year isstem leq30: asgen meaninc30 = incw99, w(perwt)
collapse (mean) meaninc30, by(year isstem leq30)
egen max_age=max(meaninc30), by(year isstem)
egen min_age=min(meaninc30), by(year isstem)
bysort year isstem: gen delbyage=max_age-min_age
egen max_stem=max(meaninc30), by(year leq30)
egen min_stem=min(meaninc30), by(year leq30) 
bysort year leq30: gen delbystem=max_stem-min_stem

summarize delbyage
twoway (connected delbyage year if isstem ==1) (connected delbyage year if isstem==0), ytitle("difference in the group mean by age") legend(label(1 "STEM") label(2 "Non-STEM")) title("difference in the group mean of wages by age") yscale(range(15000 28000)) ylabel(minmax) xline(2009, xcolor(olive) lpattern(dash))

summarize delbystem
twoway (connected delbystem year if leq30==1) (connected delbystem year if leq30==0), ytitle("difference in the group mean by STEM") legend(label(1 "Young") label(2 "Old")) title("difference in the group mean of wages by STEM") yscale(range(10000 30000)) ylabel(minmax) xline(2009, lcolor(olive)  lpattern(dash))

*usual hours worked
restore 
preserve
bysort year isstem leq30: asgen meanuhrs30 = uhrswork, w(perwt)
collapse (mean) meanuhrs30, by(year isstem leq30)
egen max_age=max(meanuhrs30), by(year isstem)
egen min_age=min(meanuhrs30), by(year isstem)
bysort year isstem: gen delbyage=max_age-min_age
egen max_stem=max(meanuhrs30), by(year leq30)
egen min_stem=min(meanuhrs30), by(year leq30) 
bysort year leq30: gen delbystem=max_stem-min_stem
 
summarize delbyage
twoway (connected delbyage year if isstem ==1) (connected delbyage year if isstem==0), ytitle("difference in the group mean by age") legend(label(1 "STEM") label(2 "Non-STEM")) title("difference in the group mean of wages by age") yscale(range(15000 28000)) ylabel(minmax) xline(2009, lcolor(olive) lpattern(dash))
summarize delbystem
twoway (connected delbystem year if leq30==1) (connected delbystem year if leq30==0), ytitle("difference in the group mean by STEM") legend(label(1 "Young") label(2 "Old")) title("difference in the group mean of wages by STEM") yscale(range(20 50)) ylabel(minmax) xline(2009, lcolor(olive)  lpattern(dash))

*summary statistics
outreg2 using data.doc, replace sum(log) keep(statefip female white asian raceother uscitizen highereduc employed unemployed notinlf indcat uhrswork incw99 leq25 tau isstem occcat) eqkeep(N mean)
tab 

bysort year isstem leq25: asgen fre=highereduc, w(perwt)
collapse (mean) fre, by(year isstem leq25)
drop if isstem==.
twoway (connected fre year if isstem==1 & leq==1) (connected fre year if isstem==1 & leq==0) (connected fre year if isstem==0 & leq==1)(connected fre year if isstem==0 & leq==0), ytitle("the proportion of hihgly-educated people") legend(label(1 "STEM, Young") label(2 "STEM, Old") label(3 "Non-STEM, Young") label(4 "Non-STEM, Old")) title("the proportion of hihgly-educated population by groups") ylabel(minmax)

*age 30 cutoffs
preserve
drop if employed==0
drop if uhrswork<30
*base
reg ln_incw99 tau##isstem##leq30 [pw=perwt], r
estimates store base

xi: reg ln_incw99 tau##isstem##leq30 i.indcat i.statefip i.year [pw=perwt], r
estimates store addfixed

xi: reg ln_incw99 tau##isstem##leq30 highereduc i.racecat married i.indcat i.statefip i.year [pw=perwt], r
estimates store addcontrols

xi: reg ln_incw99 tau##isstem##leq30 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt], r
estimates store addstemtd

xi: reg ln_incw99 tau##isstem##leq30 highereduc i.racecat married i.indcat i.statefip i.year i.isstem2_year [pw=perwt], robust cluster(indcat_year_st)
estimates store addcluster

esttab base addfixed addcontrols addstemtd addcluster using "robust30", rtf replace se label mtitles("base" "fixed effect" "controls" "stem-specific time-trend" "cluster-robust se") addnotes("I: cluster=(indcat, year)" "II: cluster=(indcat, year, statefip)" "Robust standard errors.") stats(N r2 F, labels("Observations" "R-squared" "F-statistic")) star(+ 0.10 * 0.05 ** 0.01 *** 0.001)
