*Project: location and isolation
*Author: Siyun Peng
*Date: 2024/7/5

*trtchild in atusresp //minutes with any child <18

cd "C:\Users\peng_admin\OneDrive - Indiana University\ATUS"

********************************************************************************

**# 1 LOADING ACTIVITY DATA AND MERGING WHO DATA

********************************************************************************

use "atusact_0322.dta", clear

*Home 
recode tewhere (1=1) (2=2) (3/99=3) (else=.),gen(place)
lab define place 1 "Home" 2 "Work" 3 "Other"
lab values place place

*Merging with WHO file
merge 1:m tucaseid tuactivity_n using "atuswho_0322.dta"
*100 percent of cases merged so I can safely drop the _merge variable
drop _merge

*Consolidating 'who' variable into categories
recode tuwho_code (-1 -2 -3=.) (18 19=1) (else=0),gen(alone)
label define alone 0 "Not alone" 1 "Alone" 
lab values alone alone




********************************************************************************

**# 2 Identify total, alone, and leisure time by location

********************************************************************************




duplicates drop tucaseid tuactivity_n,force //only count one activity regardless of multiple people

*total awake time
recode trcodep (10101/10299=0) (10401 10499=0) (else=1),gen(awake) 

gen time = tuactdur24/60 if awake==1
egen t_time = total(time), by(tucaseid)
lab var t_time "Total awake time"

gen time_home = tuactdur24/60 if awake==1 & place==1 
egen h_time = total(time_home), by(tucaseid)
lab var h_time "Time awake at home"

gen time_work = tuactdur24/60 if awake==1 & place==2 
egen w_time = total(time_work), by(tucaseid)
lab var w_time "Time awake at work"

gen time_othr = tuactdur24/60 if awake==1 & place==3
egen o_time = total(time_othr), by(tucaseid)
lab var o_time "Time awake at other place"

*alone time
gen alone_total = tuactdur24/60 if alone==1
egen t_alone = total(alone_total), by(tucaseid)
lab var t_alone "Total alone time"

gen alone_home = tuactdur24/60 if place==1 & alone==1
egen h_alone = total(alone_home), by(tucaseid)
lab var h_alone "Alone time at home"

gen alone_work = tuactdur24/60 if place==2 & alone==1
egen w_alone = total(alone_work), by(tucaseid)
lab var w_alone "Alone time at work"

gen alone_othr = tuactdur24/60 if place==3 & alone==1
egen o_alone = total(alone_othr), by(tucaseid)
lab var o_alone "Alone time at other place"

*social time
gen social_total = tuactdur24/60 if alone==0
egen t_social = total(social_total), by(tucaseid)
lab var t_social "Total social time"

gen social_home = tuactdur24/60 if place==1 & alone==0
egen h_social = total(social_home), by(tucaseid)
lab var h_social "Social time at home"

gen social_work = tuactdur24/60 if place==2 & alone==0
egen w_social = total(social_work), by(tucaseid)
lab var w_social "Social time at work"

gen social_othr = tuactdur24/60 if place==3 & alone==0
egen o_social = total(social_othr), by(tucaseid)
lab var o_social "Social time at other place"


/*Create total leisure social hours:
11 Eating and Drinking
12 Socializing, Relaxing, and Leisure
13 Sports, Exercise, & Recreation 
*/

*leisure time 
recode trcodep (050201 050202 050203=1) (110101/119999=1) (120101/129999=1) (130101/139999=1) (else=0),gen(leisure) 

gen tleisure = tuactdur24/60 if leisure==1 & alone==0
egen t_leisure = total(tleisure), by(tucaseid) 
lab var t_leisure "Total leisure social time"

gen hleisure = tuactdur24/60 if leisure==1 & alone==0 & place==1
egen h_leisure = total(hleisure), by(tucaseid) 
lab var h_leisure "Leisure social time at home"

gen wleisure = tuactdur24/60 if leisure==1 & alone==0 & place==2
egen w_leisure = total(wleisure), by(tucaseid) 
lab var w_leisure "Leisure social time at work"

gen oleisure = tuactdur24/60 if leisure==1 & alone==0 & place==3
egen o_leisure = total(oleisure), by(tucaseid) 
lab var o_leisure "Leisure social time at other place"

*Create total vitual hours:
recode trcodep (160101/169999=1) (else=0),gen(virtual)

gen tvirtual = tuactdur24/60 if virtual==1 
egen t_virtual = total(tvirtual), by(tucaseid) //total virtual time with anyone
lab var t_virtual "Total virtual social time"

gen hvirtual = tuactdur24/60 if virtual==1 & place==1
egen h_virtual = total(hvirtual), by(tucaseid) 
lab var h_virtual "Virtual social time at home"

gen wvirtual = tuactdur24/60 if virtual==1 & place==2
egen w_virtual = total(wvirtual), by(tucaseid) 
lab var w_virtual "Virtual social time at work"

gen ovirtual = tuactdur24/60 if virtual==1 & place==3
egen o_virtual = total(ovirtual), by(tucaseid) 
lab var o_virtual "Virtual social time at other place"




********************************************************************************

**# 3 MERGING DATA (LONG FORMAT) WITH RESPODENT SUMMARY FILE

********************************************************************************





*Dropping all duplicate cases leaves us with a single line for each R
duplicates drop tucaseid, force
keep tucaseid *_time *_social *_alone *_leisure *_virtual alone place

merge 1:1 tucaseid using "atussum_0322.dta"
drop _merge

*Merging with replicating weights file
merge 1:1 tucaseid using "atusrepwgt_0322.dta"
drop _merge

*Merging with Respondent file
merge 1:1 tucaseid  using "atusresp_0322.dta"
drop _merge

*Merging with Roster file again to get living measure
merge 1:m tucaseid using "atusrost_0322.dta",keepusing(terrp)
drop _merge
*create living arrangement variable using roster file 
bysort tucaseid: egen hou_num=count(tulineno)
lab var hou_num "# people in the household"
recode terrp (-2 -3 18 19=.) (22 23 27 40=1) (20 21=2) (24/26=3) (28/30=4) ,gen(living0)
lab define living0 1 "Living with children" 2 "Living with partner" 3 "Living with family" 4 "Living with non-kin",replace
lab values living0 living0
bysort tucaseid: egen living_min=min(living0)
bysort tucaseid: gen living=0 if hou_num==1 //=alone if only self in the household
bysort tucaseid: replace living=1 if living_min==1 //=children if a child is in the household
bysort tucaseid: replace living=2 if living_min==2 //=partner if partner in the household and no child
bysort tucaseid: replace living=3 if living_min==3 //=family if family in the household and no partner
bysort tucaseid: replace living=4 if living_min==4 //=non-kin if only non-kin in the household
drop living_min living0
lab define living 0 "Living alone" 1 "Living with children" 2 "Living with partner" 3 "Living with family" 4 "Living with non-kin",replace
lab values living living
duplicates drop tucaseid,force //keep first occurence

*merge with current population surveys
merge 1:1 tucaseid tulineno using  "atuscps_0322.dta" //tulineno indentify members of househouse
*Not all cases match because only a subset of CPS completed the ATUS. Since this is a study of the ATUS, we can safely drop all non-ATUS CPS respondents (_merge==2)
drop if _merge==2
drop _merge

********************************************************************************

**# 4 SAVING DATA (LONG FORMAT)

********************************************************************************

keep if inrange(tuyear,2010,2019) //drop years after COVID-19 since it changes people's social patterns, drop years before 2010 because ATUS did not collect WHO during working before 2010 
save "C:\Users\peng_admin\OneDrive - Indiana University\ATUS\Location and social isolation\ATUS_10_19.dta", replace




