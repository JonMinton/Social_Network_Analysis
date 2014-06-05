

cd "J:\_My Documents\Dropbox\GSPC\_20 GSPC Bulletin No.47 Mar 14\Data"
use "GUdata2014q1_UoGprocessed_a_I.dta" , clear
sum sellingp tom askingp
gen D = lower(descript)
***
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos.dta" , replace
***

use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos.dta" , clear
*
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Programs"
do "WordLists.do"
*
keep id D Type_I_n 	Type_II_n 	Type_III_n 	Type_IV_n 	Type_All_n   Type_CORE_n
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
***
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , replace
***

*The above gives rise to six new variables:   Type_I_n 	Type_II_n 	Type_III_n 	Type_IV_n 	Type_All_n   Type_CORE_n




*=======================================================
*Run the PATHOS program to create incidence variables:
*=======================================================
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Programs"
do "PATHOS.do"
*
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
*
*
*---------------------------
*Pathos Type I: Originality
*---------------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
quietly {
PATHOS Type_I_n, npathos(36) occurences(5) description(D)
}
sort id
save "AQMENpathos_1a_temp_i.dta" , replace 
*
*
*---------------------------
*Pathos Type II: Ambience
*-------------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
quietly {
PATHOS Type_II_n , npathos(24) occurences(5) description(D)
}
sort id
save "AQMENpathos_1a_temp_ii.dta" , replace 
*
*
*---------------------------
*Type III: Prestige
*---------------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
quietly {
PATHOS Type_III_n, npathos(31) occurences(5) description(D)
}
sort id
save "AQMENpathos_1a_temp_iii.dta" , replace 
*
*
*---------------------------
*Type IV: Excitement
*---------------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
quietly {
PATHOS Type_IV_n, npathos(90) occurences(5) description(D)
}
sort id
save "AQMENpathos_1a_temp_iv.dta" , replace 
*
*
*---------------------------
*Pathos_All
*---------------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
quietly {
PATHOS Type_All_n, npathos(181) occurences(5) description(D)
}
sort id
save "AQMENpathos_1a_temp_all.dta" , replace 
*
*
*---------------------------
*Pathos_CORE
*---------------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_1a.dta" , clear
quietly {
PATHOS Type_CORE_n, npathos(28) occurences(5) description(D)
}
sort id
save "AQMENpathos_1a_temp_core.dta" , replace 




*=======================================================
*Merge the Temporary files created above
*=======================================================
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos.dta" , clear

sort id
merge id using "AQMENpathos_1a_temp_i.dta", keep(Type_I_n_all) _merge(_merge_Type_i)
sort id
merge id using "AQMENpathos_1a_temp_ii.dta", keep(Type_II_n_all) _merge(_merge_Type_ii)
sort id
merge id using "AQMENpathos_1a_temp_iii.dta", keep(Type_III_n_all) _merge(_merge_Type_iii)
sort id
merge id using "AQMENpathos_1a_temp_iv.dta", keep(Type_IV_n_all) _merge(_merge_Type_iv)
sort id
merge id using "AQMENpathos_1a_temp_all.dta", keep(Type_All_n_all) _merge(_merge_Type_all)
sort id
merge id using "AQMENpathos_1a_temp_core.dta", keep(Type_CORE_n_all) _merge(_merge_Type_core)
*
save  "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2.dta"  , replace
*
tab _merge_Type_i  
tab _merge_Type_ii 
tab _merge_Type_iii 
tab _merge_Type_iv 
tab _merge_Type_all
tab _merge_Type_core
*
drop  _merge_Type_i  _merge_Type_ii _merge_Type_iii _merge_Type_iv _merge_Type_all

*=======================================================
* Length of description
*=======================================================
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2.dta" , clear

gen dscrptn_charcount = length(D)
drop if dscrptn_charcount == 0

gen dscrptn_wordcount = wordcount(D)
drop if dscrptn_wordcount == 0
tab dscrptn_wordcount
hist dscrptn_wordcount, discrete

sum dscrptn_charcount dscrptn_wordcount
*
*

*---------------------------
*Descriptives: Number, n, of Pathos words in each Description:
*---------------------------
sum Type_I_n_all 
hist Type_I_n_all 

sum Type_II_n_all 
hist Type_II_n_all 

sum Type_III_n_all 
hist Type_III_n_all 

sum Type_IV_n_all 
hist Type_IV_n_all 

sum Type_All_n_all
hist Type_All_n_all

sum Type_CORE_n_all
hist Type_CORE_n_all


*---------------------------
*Descriptives: Proportion, p, of Pathos words in each Description:
*---------------------------
gen Type_I_p_all = Type_I_n_all / dscrptn_wordcount
sum Type_I_p_all 
hist Type_I_p_all 

gen Type_II_p_all = Type_II_n_all / dscrptn_wordcount
sum Type_II_p_all 
hist Type_II_p_all 

gen Type_III_p_all = Type_III_n_all / dscrptn_wordcount
sum Type_III_p_all 
hist Type_III_p_all 

gen Type_IV_p_all = Type_IV_n_all / dscrptn_wordcount
sum Type_IV_p_all 
hist Type_IV_p_all 

gen Type_All_p_all = Type_All_n_all / dscrptn_wordcount
sum Type_All_p_all
hist Type_All_p_all

gen Type_CORE_p_all = Type_CORE_n_all / dscrptn_wordcount
sum Type_CORE_p_all
hist Type_CORE_p_all


*========================================
*Activation Date based time variables:
*========================================
*QUARTER DUMMIES (WITHOUT YEAR IDENTIFIER):
gen actv_qrt = quarter(activati)
label variable actv_qrt "Quarter (i.e. 1, 2, 3, or 4) in which the property was sold"
tab actv_qrt
*
*MONTH DUMMIES:
gen actv_mnth = month(activati)
label variable actv_mnth "Month (i.e. 1, 2,...,12) in which the property was sold"
tab actv_mnth
*
*YEAR DUMMIES:
gen actv_year = year(activati)
label variable actv_year "Year (i.e. 1999, 2000,...,2004) in which the property was sold"
tab actv_year
*
egen actv_quart = concat(actv_year actv_qrt) , punct("q")
label variable actv_quart "String Version of Activation Quarter: 1999q1, ...,  2004q4 etc"
*
gen actv_quarter = 4*(actv_year - 1999 ) + actv_qrt
label variable actv_quarter "Activation Quarter Number: 1999q1 =1; 2004q4 =24"
*
tab actv_mnth, gen(actv_month_)
tab actv_qrt, gen(actv_qrt_)
*========================================
gen cbd_glas_km = cbd_glas / 1000
*========================================
*
gen postcode = pcode
sort postcode 
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2.dta" , replace
*

*=============================
*Attach Datazones and intermed geogs
*=============================
cd "J:\_My Documents\DATA\Data Zones"
use "latestpcinfowithlinkpc.dta"
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
sort postcode
save "latestpcinfowithlinkpc.dta"
*
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2.dta" , clear
merge postcode using "latestpcinfowithlinkpc.dta" , _merge(_merge_Datazones)
tab _merge_Datazones
keep if _merge_Datazones == 3
drop _merge_Datazones
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta", replace

sum tom

*=============================
*Datazone averages for Each Year
*=============================
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta" , clear
gen activ_year = year(activati)
tab activ_year
keep activ_year Type_I_p_all-Type_CORE_p_all datazone
keep if (activ_year == 2000) | (activ_year == 2001) | (activ_year == 2002) 
collapse  Type_I_p_all-Type_CORE_p_all  (count) activ_year , by(datazone)
rename activ_year n
hist n
keep if n > 30
sum Type_I_p_all-Type_CORE_p_all

*** only 15 datazones with enough obs ***

*=============================
*Intermediate geog averages for Each Year
*=============================
*--------------------
*2000,2001, 2002:
*--------------------
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta" , clear
gen activ_year = year(activati)
tab activ_year
keep activ_year Type_I_p_all-Type_CORE_p_all intermed
keep if (activ_year == 2000) | (activ_year == 2001) | (activ_year == 2002) 
collapse  Type_I_p_all-Type_CORE_p_all  (count) activ_year , by(intermed)
rename activ_year n_CY2001
rename Type_I_p_all 	Type_I_p_all_CY2001
rename Type_II_p_all	Type_II_p_all_CY2001
rename  Type_III_p_all 	Type_III_p_all_CY2001
rename Type_IV_p_all 	Type_IV_p_all_CY2001
rename Type_All_p_all 	Type_All_p_all_CY2001
rename Type_CORE_p_all	Type_CORE_p_all_CY2001
hist n_CY2001
sum Type_I_p_all-Type_CORE_p_all
sum Type_I_p_all-Type_CORE_p_all if n > 30
sort intermed
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2aINTERMED_2000TO2002.dta", replace

*--------------------
*2010, 2011, 2012
*--------------------
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta" , clear
gen activ_year = year(activati)
tab activ_year
keep activ_year Type_I_p_all-Type_CORE_p_all intermed
keep if (activ_year == 2010) | (activ_year == 2011) | (activ_year == 2012)
collapse  Type_I_p_all-Type_CORE_p_all  (count) activ_year , by(intermed)
rename activ_year n_CY2011
rename Type_I_p_all 	Type_I_p_all_CY2011
rename Type_II_p_all	Type_II_p_all_CY2011
rename  Type_III_p_all 	Type_III_p_all_CY2011
rename Type_IV_p_all 	Type_IV_p_all_CY2011
rename Type_All_p_all 	Type_All_p_all_CY2011
rename Type_CORE_p_all	Type_CORE_p_all_CY2011
hist n_CY2011
sum Type_I_p_all-Type_CORE_p_all
sum Type_I_p_all-Type_CORE_p_all if n > 30
sort intermed
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2aINTERMED_2010TO2012.dta", replace

outsheet using "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data\average pathos_2000to2002__and__2010to2012.csv", comma replace


*--------------------
*Merge
*--------------------
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2aINTERMED_2000TO2002.dta", clear
merge intermed , using "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2aINTERMED_2010TO2012.dta"
tab _merge
keep if _merge == 3
sum Type_All_p_all_CY2001 Type_All_p_all_CY2011
sum Type_All_p_all_CY2001 Type_All_p_all_CY2011 if (n_CY2001 > 30) & (n_CY2011 > 30)

*=============================
*Repeat Sales
*=============================
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2a.dta" , clear
tab sld_year , sum(Type_All_p_all)
tabstat Type_I_p_all-Type_CORE_p_all, by(sld_year) stat(median)

gen pcodelower = lower(pcode)
gen streetlower = lower(street)

egen x = concat (pcodelower streetlower), punct(_)
sort x
capture drop repeatsales  

cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Programs"
do "Repeatsales.do"  // this program creates the "repeatsales" variable which counts the total number of times a property has sold.

tab repeatsales

cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2b.dta" , replace
  
*========================
*Remove Dodgy Repeat sales
*========================
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2b.dta" , clear
keep if repeatsales >0
sort repeatsales x activati
gen date_active = activati
gen date_sold = solddate
format date_sold date_active %td
drop if (activati == activati [_n+1]) & (x == x [_n+1]) 

*Re-run the Repeat sales program:
capture drop repeatsales  
sort x
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Programs"
do "Repeatsales.do"  // this program creates the "repeatsales" variable which counts the total number of times a property has sold.
tab repeatsales
keep if repeatsales >0
sort repeatsales x activati
drop if (activati == activati [_n+1]) & (x == x [_n+1]) 

***Save:
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2c.dta" , replace
***


*========================
*Select years: i.e. one year either way around two Census dates
*========================
*For Census 2001 Choose: 2000, 2001 and 2002
*For Census 2011 Choose: 2010, 2011 and 2012
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2c.dta" , clear
*
gen activ_year = year(activati)
tab activ_year
tabstat Type_I_p_all-Type_CORE_p_all, by(activ_year) 
tabstat askingp, by(activ_year) 
keep if (activ_year == 2000) | (activ_year == 2001) | (activ_year == 2002) | (activ_year == 2010) | (activ_year == 2011) | (activ_year == 2012)
*
*Census year dummies:
gen Census2001 = 0
replace Census2001 = 1 if (activ_year == 2000) | (activ_year == 2001) | (activ_year == 2002)
gen Census2011 = 0
replace Census2011 = 1 if (activ_year == 2010) | (activ_year == 2011) | (activ_year == 2012)
tab Census2001 
tab Census2011 
*
*Re-run the Repeat sales program:
capture drop repeatsales  
sort x
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Programs"
do "Repeatsales.do"  // this program creates the "repeatsales" variable which counts the total number of times a property has sold.
tab repeatsales
keep if repeatsales >0
tab repeatsales
sort repeatsales x activati
drop if (activati == activati [_n+1]) & (x == x [_n+1]) 
*
***Save:
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
save "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2d.dta" , replace
***


*========================
*Now keep only if first sale in Census Window #1 and second sale in Census Window #2:
*========================
cd "J:\_My Documents\Dropbox\AQMEN_II\USIRP\PHASE 2\PH2_Language of Selling\Data"
use "GUdata2014q1_UoGprocessed_a_I__AQMENpathos_2d.dta" , clear
*
sort repeatsales x activati
gen MATCH = 1 if (Census2001 == Census2011 [_n+1]) & (x == x [_n+1])
replace MATCH = 2 if (Census2001 == Census2011 [_n-1]) & (x == x [_n-1])
tab MATCH
browse

keep if (MATCH == 1) | (MATCH == 2)
tab MATCH












capture drop repeatsales  
gen repeatsales = 0 

sort x

| (x == x [_n-1]) 


replace repeatsales = 1 if (x == x [_n+1]) | (x == x [_n-1]) 

 
sort x
replace repeatsales = 2 if (x == x [_n+2]) ///
  | (x == x [_n-1] & x == x [_n+1]) ///
  | (x == x [_n-2])

sort x
replace repeatsales = 3 if (x == x [_n+3]) ///   a a a a
  | (x == x [_n-1] & x == x [_n+2]) ///
  | (x == x [_n-2] & x == x [_n+1]) ///
  |                        (x == x [_n-3])


sort x
replace repeatsales = 4 if (x == x [_n+4]) ///   a a a a a
  | (x == x [_n-1] & x == x [_n+3]) ///
  | (x == x [_n-2] & x == x [_n+2]) ///
  | (x == x [_n-3] & x == x [_n+1]) ///
  |                        (x == x [_n-4])


sort x
replace repeatsales = 5 if (x == x [_n+5]) ///   a a a a a a
  | (x == x [_n-1] & x == x [_n+4]) ///
  | (x == x [_n-2] & x == x [_n+3]) ///
  | (x == x [_n-3] & x == x [_n+2]) ///
  | (x == x [_n-4] & x == x [_n+1]) ///
  |                        (x == x [_n-5])

sort x
replace repeatsales = 6 if (x == x [_n+6]) ///   a a a a a a a
  | (x == x [_n-1] & x == x [_n+5]) ///
  | (x == x [_n-2] & x == x [_n+4]) ///
  | (x == x [_n-3] & x == x [_n+3]) ///
  | (x == x [_n-4] & x == x [_n+2]) ///
  | (x == x [_n-5] & x == x [_n+1]) ///
  |                        (x == x [_n-6])

sort x
replace repeatsales = 7 if (x == x [_n+7]) ///   a a a a a a a a
  | (x == x [_n-1] & x == x [_n+6]) ///
  | (x == x [_n-2] & x == x [_n+5]) ///
  | (x == x [_n-3] & x == x [_n+4]) ///
  | (x == x [_n-4] & x == x [_n+3]) ///
  | (x == x [_n-5] & x == x [_n+2]) ///
  | (x == x [_n-6] & x == x [_n+1]) ///
  |                        (x == x [_n-7])

sort x
replace repeatsales = 8 if (x == x [_n+8]) ///   
  | (x == x [_n-1] & x == x [_n+7]) ///
  | (x == x [_n-2] & x == x [_n+6]) ///
  | (x == x [_n-3] & x == x [_n+5]) ///
  | (x == x [_n-4] & x == x [_n+4]) ///
  | (x == x [_n-5] & x == x [_n+3]) ///
  | (x == x [_n-6] & x == x [_n+2]) ///
  | (x == x [_n-7] & x == x [_n+1]) ///
  |                        (x == x [_n-8])

sort x
replace repeatsales = 9 if (x == x [_n+9]) ///   
  | (x == x [_n-1] & x == x [_n+8]) ///
  | (x == x [_n-2] & x == x [_n+7]) ///
  | (x == x [_n-3] & x == x [_n+6]) ///
  | (x == x [_n-4] & x == x [_n+5]) ///
  | (x == x [_n-5] & x == x [_n+4]) ///
  | (x == x [_n-6] & x == x [_n+3]) ///
  | (x == x [_n-7] & x == x [_n+2]) ///
  | (x == x [_n-8] & x == x [_n+1]) ///
  |                        (x == x [_n-9])


sort x
replace repeatsales = 10 if (x == x [_n+10]) ///   
  | (x == x [_n-1] & x == x [_n+9]) ///
  | (x == x [_n-2] & x == x [_n+8]) ///
  | (x == x [_n-3] & x == x [_n+7]) ///
  | (x == x [_n-4] & x == x [_n+6]) ///
  | (x == x [_n-5] & x == x [_n+5]) ///
  | (x == x [_n-6] & x == x [_n+4]) ///
  | (x == x [_n-7] & x == x [_n+3]) ///
  | (x == x [_n-8] & x == x [_n+2]) ///
  | (x == x [_n-9] & x == x [_n+1]) ///
  |                        (x == x [_n-10])



sort x
replace repeatsales = 11 if (x == x [_n+11]) ///   
  | (x == x [_n-1] & x == x [_n+10]) ///
  | (x == x [_n-2] & x == x [_n+9]) ///
  | (x == x [_n-3] & x == x [_n+8]) ///
  | (x == x [_n-4] & x == x [_n+7]) ///
  | (x == x [_n-5] & x == x [_n+6]) ///
  | (x == x [_n-6] & x == x [_n+5]) ///
  | (x == x [_n-7] & x == x [_n+4]) ///
  | (x == x [_n-8] & x == x [_n+3]) ///
  | (x == x [_n-9] & x == x [_n+2]) ///
  | (x == x [_n-10] & x == x [_n+1]) ///
  |                        (x == x [_n-11])  ///




sort x
replace repeatsales = 12 if (x == x [_n+12]) ///   
  | (x == x [_n-1] & x == x [_n+11]) ///
  | (x == x [_n-2] & x == x [_n+10]) ///
  | (x == x [_n-3] & x == x [_n+9]) ///
  | (x == x [_n-4] & x == x [_n+8]) ///
  | (x == x [_n-5] & x == x [_n+7]) ///
  | (x == x [_n-6] & x == x [_n+6]) ///
  | (x == x [_n-7] & x == x [_n+5]) ///
  | (x == x [_n-8] & x == x [_n+4]) ///
  | (x == x [_n-9] & x == x [_n+3]) ///
  | (x == x [_n-10] & x == x [_n+2]) ///
  | (x == x [_n-11] & x == x [_n+1]) ///
  |                        (x == x [_n-12])


sort x
replace repeatsales = 13 if (x == x [_n+13]) ///   
  | (x == x [_n-1] & x == x [_n+12]) ///
  | (x == x [_n-2] & x == x [_n+11]) ///
  | (x == x [_n-3] & x == x [_n+10]) ///
  | (x == x [_n-4] & x == x [_n+9]) ///
  | (x == x [_n-5] & x == x [_n+8]) ///
  | (x == x [_n-6] & x == x [_n+7]) ///
  | (x == x [_n-7] & x == x [_n+6]) ///
  | (x == x [_n-8] & x == x [_n+5]) ///
  | (x == x [_n-9] & x == x [_n+4]) ///
  | (x == x [_n-10] & x == x [_n+3]) ///
  | (x == x [_n-11] & x == x [_n+2]) ///
  | (x == x [_n-12] & x == x [_n+1]) ///
  |                        (x == x [_n-13])

sort x
replace repeatsales = 14 if (x == x [_n+14]) ///   
  | (x == x [_n-1] & x == x [_n+13]) ///
  | (x == x [_n-2] & x == x [_n+12]) ///
  | (x == x [_n-3] & x == x [_n+11]) ///
  | (x == x [_n-4] & x == x [_n+10]) ///
  | (x == x [_n-5] & x == x [_n+9]) ///
  | (x == x [_n-6] & x == x [_n+8]) ///
  | (x == x [_n-7] & x == x [_n+7]) ///
  | (x == x [_n-8] & x == x [_n+6]) ///
  | (x == x [_n-9] & x == x [_n+5]) ///
  | (x == x [_n-10] & x == x [_n+4]) ///
  | (x == x [_n-11] & x == x [_n+3]) ///
  | (x == x [_n-12] & x == x [_n+2]) ///
  | (x == x [_n-13] & x == x [_n+1]) ///
  |                        (x == x [_n-14])


sort x
replace repeatsales = 15 if (x == x [_n+15]) ///   
  | (x == x [_n-1] & x == x [_n+14]) ///
  | (x == x [_n-2] & x == x [_n+13]) ///
  | (x == x [_n-3] & x == x [_n+12]) ///
  | (x == x [_n-4] & x == x [_n+11]) ///
  | (x == x [_n-5] & x == x [_n+10]) ///
  | (x == x [_n-6] & x == x [_n+9]) ///
  | (x == x [_n-7] & x == x [_n+8]) ///
  | (x == x [_n-8] & x == x [_n+7]) ///
  | (x == x [_n-9] & x == x [_n+6]) ///
  | (x == x [_n-10] & x == x [_n+5]) ///
  | (x == x [_n-11] & x == x [_n+4]) ///
  | (x == x [_n-12] & x == x [_n+3]) ///
  | (x == x [_n-13] & x == x [_n+2]) ///
  | (x == x [_n-14] & x == x [_n+1]) ///
  |                        (x == x [_n-15])

tab repeatsales


tab activ_year
tab repeatsales
keep if repeatsales >0

capture drop repeatsale_sequence 
gen repeatsale_sequence = "NONE"
sort x activati
*one repeat sale:
	replace repeatsale_sequence = "1st"  if (repeatsales == 1) & (x == x [_n+1])  
	replace repeatsale_sequence = "2nd"  if (repeatsales == 1) & (x == x [_n-1])  
*two repeat sales:
	replace repeatsale_sequence = "1st"  if (repeatsales == 2) & (x == x [_n+2])  
	replace repeatsale_sequence = "2nd"  if (repeatsales == 2) & ((x == x [_n-1]) & (x == x [_n+1]))   
	replace repeatsale_sequence = "3rd"  if (repeatsales == 2) & (x == x [_n-2])  
*three repeat sales:
	replace repeatsale_sequence = "1st"  if (repeatsales == 3) & (x == x [_n+3])  
	replace repeatsale_sequence = "2nd"  if (repeatsales == 3) & ((x == x [_n-1]) & (x == x [_n+2]))   
	replace repeatsale_sequence = "3rd"  if (repeatsales == 3) & ((x == x [_n+1]) & (x == x [_n-2]))   
	replace repeatsale_sequence = "4th"  if (repeatsales == 3) & (x == x [_n-3])  

	
sort repeatsales x activati
capture drop askingp_1st_sale  
capture drop RSaskingp_1st_sale 
capture drop RSaskingp_2nd_sale
sort repeatsales x activati
gen RSaskingp_1st_sale = askingpr if (repeatsale_sequence == "1st") 
gen RSaskingp_2nd_sale = askingpr[1] if (repeatsale_sequence == "1st") 

& (repeatsales >= 2) 

	
	replace repeatsales = 1 if (x == x [_n+1]) | (x == x [_n-1]) 

sort x activati
replace repeatsales = 2 if (x == x [_n+2]) ///
  | (x == x [_n-1] & x == x [_n+1]) ///
  | (x == x [_n-2])
