*Project: location and isolation predict health
*Author: Siyun Peng
*Date: 2024/7/5




***************************************************************
**# 1 data clean
***************************************************************

  

use "C:\Users\peng_admin\OneDrive - Indiana University\ATUS\Location and social isolation\ATUS_10_19.dta",clear //home
cd "C:\Users\peng_admin\OneDrive - Indiana University\ATUS\Location and social isolation\Results"

rename living living5
recode living5 (0=1) (1/4=0),gen(living)
lab var living "Living alone" 
lab define living_alone 0 "Not alone" 1 "Living alone"
lab values living living_alone

lab var teage "Age"
drop if teage<18 //drop people<18 

recode trohhchild (1=1) (2=0),gen(hhkid)
lab var hhkid "Own children<18 in the household"
lab de hhkid 0 "No" 1 "Yes"
lab val hhkid hhkid

recode tesex (1=0) (2=1),gen(women)
lab var women "Women"
lab de women 0 "Men" 1 "Women"
lab val women women

recode ptdtrace (4=3) (3 5/max=4),gen(race)
lab var race "Race"
lab de race 1 "White" 2 "Black" 3 "Asian" 4 "Other"
lab val race race

recode telfs (1=0) (2/5=1),gen(nowork)
recode telfs (1 2=0) (3/5=1),gen(nowork2)
lab var nowork "Not working"
lab var nowork2 "Not working"
lab define nowork 0 "Currently working" 1 "Not working"
lab values nowork nowork
lab values nowork2 nowork

recode peeduca (31/38=1) (39=2) (40/42=3) (43/46=4),gen(edu)
lab def edu 1 "Less than HS" 2 "HS or GED" 3 "Some college/technical" 4 "College"
lab val edu edu
lab var edu "Education"

recode edu (1/3=0) (4=1),gen(college)
lab var college "College"

recode pemaritl (1 2=1) (3/6=0),gen(partner)
replace partner=1 if living==1
lab def partner 1 "Partnered" 0 "No partner" 
lab val partner partner
lab var partner "Partner status"

recode pedis* (1=1) (2=0)
egen faq=rowtotal(pedis*),mi
recode faq (1/max=1)
lab var faq "Functional activities limitation"

lab var tuyear "Year"

drop if missing(living)
		
		
 

	
		
***************************************************************
**# 2 regression with weights
***************************************************************



*apply weights
svyset [pw=tufnwgtp], sdrweight(tufnwgtp???) vce(sdr) mse //sdrweight: replicating weights fix variance


*descriptive table (Table 1)
desctable women teage i.race i.edu faq nowork i.living *_time *_social *_alone *_leisure *_virtual, filename("descriptives") stats(svymean svysemean min max) listwise



/*Awake, alone and leisure time (Figure 1, 2)*/


estimates clear
foreach x of varlist *_time *_social *_alone *_leisure {
    svy: reg `x' c.teage##c.teage living women i.race i.edu faq nowork i.tudiaryday i.trholiday i.tuyear
	estimates store `x'
margins, saving(`x', replace) 
marginsplot, ylab(0 (1) 8) recast(bar) tit("") ytit("`: var label `x''",size(medlarge)) xtit("") xlab(,angle(45) labsize(medlarge)) legend(off) plotopts(mlabel(_margin) mlabf(%12.1f) mlabp(9) mlabcolor(red) lcolor(red) fcolor(red%50*0.5)) ciopt(color(red)) saving(`x',replace) 
}

combomarginsplot "t_time" "h_time" "w_time" "o_time", labels("Total" "Home" "Work" "Other") recast(bar) title("") xtitle("") ytitle("Time") plotopts(mlabel(_margin) mlabf(%12.1f) mlabp(12) mlabcolor(red) lcolor(red) fcolor(red%50*0.5)) ciopt(color(red)) savefile(time,replace)

combomarginsplot "t_social" "h_social" "w_social" "o_social", labels("Total" "Home" "Work" "Other") recast(bar) title("") xtitle("") ytitle("Social time") plotopts(mlabel(_margin) mlabf(%12.1f) mlabp(12) mlabcolor(red) lcolor(red) fcolor(red%50*0.5)) ciopt(color(red))  savefile(social,replace)

combomarginsplot "t_alone" "h_alone" "w_alone" "o_alone", labels("Total" "Home" "Work" "Other") recast(bar) title("") xtitle("") ytitle("Alone time") plotopts(mlabel(_margin) mlabf(%12.1f) mlabp(12) mlabcolor(red) lcolor(red) fcolor(red%50*0.5)) ciopt(color(red))  savefile(alone,replace)

combomarginsplot "t_leisure" "h_leisure" "w_leisure" "o_leisure", labels("Total" "Home" "Work" "Other") recast(bar) title("") xtitle("") ytitle("Leisure time") plotopts(mlabel(_margin) mlabf(%12.1f) mlabp(12) mlabcolor(red) lcolor(red) fcolor(red%50*0.5)) ciopt(color(red)) savefile(leisure,replace)

combomarginsplot "time" "social" "alone" "leisure", label("Awake time" "Social time" "Alone time" "Leisure social time") title("") xtitle("") ytitle("Hours/day") plotopts(mlabel(_margin) mlabf(%12.1f) mlabp(12)) 
graph export "means.tif",replace

coefplot (t_time, aseq(Total) \ h_time, aseq(Home) ///
         \ w_time, aseq(Work) \ o_time, aseq(Other)) ///
		 , bylabel("Awake time") || ///
		 (t_social, aseq(Total) \ h_social, aseq(Home) ///
         \ w_social, aseq(Work) \ o_social, aseq(Other)) ///
		 , bylabel("Social time") || ///
		 (t_alone, aseq(Total) \ h_alone, aseq(Home) ///
         \ w_alone, aseq(Work) \ o_alone, aseq(Other)) ///
		 , bylabel("Alone time") || ///
		 (t_leisure, aseq(Total) \ h_leisure, aseq(Home) ///
         \ w_leisure, aseq(Work) \ o_leisure, aseq(Other)) ///
		 , bylabel("Leisure social time") ///
		 keep(living nowork) xline(0) xtitle("Hours/day", size(small)) legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) ytit("") coeflabels(nowork="Not working") byopts(compact rows(1)) //ylabel(,labsize(med))
graph export "living.tif",replace

coefplot (t_time, aseq(Total) \ h_time, aseq(Home) ///
         \ w_time, aseq(Work) \ o_time, aseq(Other)) ///
		 , bylabel("Awake time") || ///
		 (t_social, aseq(Total) \ h_social, aseq(Home) ///
         \ w_social, aseq(Work) \ o_social, aseq(Other)) ///
		 , bylabel("Social time") || ///
		 (t_alone, aseq(Total) \ h_alone, aseq(Home) ///
         \ w_alone, aseq(Work) \ o_alone, aseq(Other)) ///
		 , bylabel("Alone time") || ///
		 (t_leisure, aseq(Total) \ h_leisure, aseq(Home) ///
         \ w_leisure, aseq(Work) \ o_leisure, aseq(Other)) ///
		 , bylabel("Leisure social time") ///
		 keep(women 2.race 3.race 4.race 2.edu 3.edu 4.edu faq) xline(0) xtitle("Hours/day", size(small)) legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(tiny) ytit("") coeflabels(faq="Activities limitations" 3.edu="Some college") byopts(compact rows(1)) //ylabel(,labsize(med))
graph export "demo.tif",replace

esttab * using "means.csv",label replace b(%5.2f) se(%5.2f) nogap r2 compress nonum noomitted noconstant


***************************************************************
**# 3 Sensitivity analysis
***************************************************************




*employed-absent count as employed (FIgure S1)
estimates clear
foreach x of varlist *_time *_social *_alone *_leisure {
    svy: reg `x' c.teage##c.teage living women i.race i.edu faq nowork2 i.tudiaryday i.trholiday i.tuyear
	estimates store `x'1
	svy: reg `x' c.teage##c.teage living women i.race i.edu faq nowork i.tudiaryday i.trholiday i.tuyear
	estimates store `x'2
}
coefplot (t_time*, aseq(Total) \ h_time*, aseq(Home) ///
         \ w_time*, aseq(Work) \ o_time*, aseq(Other)) ///
		 , bylabel("Awake time") || ///
		 (t_social*, aseq(Total) \ h_social*, aseq(Home) ///
         \ w_social*, aseq(Work) \ o_social*, aseq(Other)) ///
		 , bylabel("Social time") || ///
		 (t_alone*, aseq(Total) \ h_alone*, aseq(Home) ///
         \ w_alone*, aseq(Work) \ o_alone*, aseq(Other)) ///
		 , bylabel("Alone time") || ///
		 (t_leisure*, aseq(Total) \ h_leisure*, aseq(Home) ///
         \ w_leisure*, aseq(Work) \ o_leisure*, aseq(Other)) ///
		 , bylabel("Leisure social time") ///
		 keep(nowork2 nowork) xline(0) xtitle("Hours/day", size(small)) legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) ytit("") coeflabels(nowork2="Absent as employed" nowork="Absent as unemployed") byopts(compact rows(1)) //ylabel(,labsize(med))
graph export "work2.tif",replace

esttab *1 using "work2.csv",label replace b(%5.2f) se(%5.2f) nogap r2 compress nonum noomitted noconstant


*stratified by 65+ to differentiate retirement vs. unemployment (FIgure S2)
estimates clear
clonevar nowork_c=nowork
foreach x of varlist *_time *_social *_alone *_leisure {
    svy: reg `x' c.teage##c.teage living women i.race i.edu faq nowork i.tudiaryday i.trholiday i.tuyear if teage>64
	estimates store `x'o
    svy: reg `x' c.teage##c.teage living women i.race i.edu faq nowork_c i.tudiaryday i.trholiday i.tuyear if teage<65
	estimates store `x'y
}
coefplot (t_time*, aseq(Total) \ h_time*, aseq(Home) ///
         \ w_time*, aseq(Work) \ o_time*, aseq(Other)) ///
		 , bylabel("Awake time") || ///
		 (t_social*, aseq(Total) \ h_social*, aseq(Home) ///
         \ w_social*, aseq(Work) \ o_social*, aseq(Other)) ///
		 , bylabel("Social time") || ///
		 (t_alone*, aseq(Total) \ h_alone*, aseq(Home) ///
         \ w_alone*, aseq(Work) \ o_alone*, aseq(Other)) ///
		 , bylabel("Alone time") || ///
		 (t_leisure*, aseq(Total) \ h_leisure*, aseq(Home) ///
         \ w_leisure*, aseq(Work) \ o_leisure*, aseq(Other)) ///
		 , bylabel("Leisure social time") ///
		 keep(nowork nowork_c) xline(0) xtitle("Hours/day", size(small)) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) ytit("") coeflabels(nowork="65+" nowork_c="<65") byopts(compact rows(1)) //ylabel(,labsize(med))
graph export "work65.tif",replace

esttab *o using "work65.csv",label replace b(%5.2f) se(%5.2f) nogap r2 compress nonum noomitted noconstant



*Detail living arrangement (Figure S3)
estimates clear
foreach x of varlist *_time *_social *_alone *_leisure {
    svy: reg `x' c.teage##c.teage i.living5 women i.race i.edu faq nowork i.tudiaryday i.trholiday i.tuyear
	estimates store `x'
}
coefplot (t_time, aseq(Total) \ h_time, aseq(Home) ///
         \ w_time, aseq(Work) \ o_time, aseq(Other)) ///
		 , bylabel("Awake time") || ///
		 (t_social, aseq(Total) \ h_social, aseq(Home) ///
         \ w_social, aseq(Work) \ o_social, aseq(Other)) ///
		 , bylabel("Social time") || ///
		 (t_alone, aseq(Total) \ h_alone, aseq(Home) ///
         \ w_alone, aseq(Work) \ o_alone, aseq(Other)) ///
		 , bylabel("Alone time") || ///
		 (t_leisure, aseq(Total) \ h_leisure, aseq(Home) ///
         \ w_leisure, aseq(Work) \ o_leisure, aseq(Other)) ///
		 , bylabel("Leisure social time") ///
		 keep(1.living5 2.living5 3.living5 4.living5) xline(0) xtitle("Hours/day", size(small)) legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(vsmall) ytit("") byopts(compact rows(1)) //ylabel(,labsize(med))
graph export "living5.tif",replace

esttab * using "living5.csv",label replace b(%5.2f) se(%5.2f) nogap r2 compress nonum noomitted noconstant


*Virtual contact time with family and friend  (Figure S4)
estimates clear
foreach x of varlist *_virtual {
	svy: reg `x' c.teage##c.teage living women i.race i.edu faq nowork i.tudiaryday i.trholiday i.tuyear
	estimates store `x'
}
coefplot (t_virtual, aseq(Total) \ h_virtual, aseq(Home) ///
         \ w_virtual, aseq(Work) \ o_virtual, aseq(Other)), keep(living nowork) xline(0) xtitle("Hours/day", size(small)) legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) ytit("") title("Virtual social time") byopts(compact rows(1)) 
graph export "virtual.tif", replace

esttab * using "virtual.csv",label replace b(%5.2f) se(%5.2f) nogap r2 compress nonum noomitted noconstant

*count model  (No convegence: indicator of bad model fit)
estimates clear
foreach x of varlist *_time *_social *_alone *_leisure {
    svy: nbreg `x' c.teage##c.teage living women i.race i.edu faq nowork i.tudiaryday i.trholiday i.tuyear
	estimates store `x'
}
coefplot (t_time, aseq(Total) \ h_time, aseq(Home) ///
         \ w_time, aseq(Work) \ o_time, aseq(Other)) ///
		 , bylabel("Awake time") || ///
		 (t_social, aseq(Total) \ h_social, aseq(Home) ///
         \ w_social, aseq(Work) \ o_social, aseq(Other)) ///
		 , bylabel("Social time") || ///
		 (t_alone, aseq(Total) \ h_alone, aseq(Home) ///
         \ w_alone, aseq(Work) \ o_alone, aseq(Other)) ///
		 , bylabel("Alone time") || ///
		 (t_leisure, aseq(Total) \ h_leisure, aseq(Home) ///
         \ w_leisure, aseq(Work) \ o_leisure, aseq(Other)) ///
		 , bylabel("Leisure social time") ///
		 keep(living nowork) xline(0) xtitle("Hours/day", size(small)) legend(off) mlabel mlabformat(%9.2f) mlabposition(12) mlabsize(small) ytit("") coeflabels(nowork="Not working") byopts(compact rows(1)) //ylabel(,labsize(med))
graph export "count.tif",replace

esttab * using "count.csv",label replace b(%5.2f) se(%5.2f) nogap r2 compress nonum noomitted noconstant

*plot means by living arrangement
foreach x of varlist h_time h_alone {
    svy: reg `x' c.teage##c.teage i.living i.nowork women i.race i.edu faq i.tudiaryday i.trholiday i.tuyear
    margins i.living, saving(`x', replace) 
}
combomarginsplot "h_time" "h_alone", labels("Awake time at home" "Alone time at home" ) recast(bar) title("") xtitle("") ytitle("Hours/day") file1opts(mlabel(_margin) mlabf(%12.1f) mlabp(12) mlabcolor(red) lcolor(red) fcolor(red%50*0.5)) fileci1opt(color(red)) file2opts(mlabel(_margin) mlabf(%12.1f) mlabp(12) mlabcolor(blue) lcolor(blue) fcolor(blue%50*0.5)) fileci2opt(color(blue))
graph export "alone.tif",replace


