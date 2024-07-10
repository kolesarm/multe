clear matrix
clear mata
clear
set maxvar 32000
set more off

use "Data/ecls.dta"

** from ecls.do replication file **

*************************************
*** Recode Covariates
***************************************
**** Rename some variables
rename X1SESQ5 sesquint
rename X1CHRACE race
rename X1INTVID interviewerid_9months
rename X2INTVID interviewerid_2years
rename X1ASAGE agem_9months
rename X2ASAGE agem_2years
rename BCMOMAGE momage

*** Cap days premature at 77
gen days_premature = X1CHPREM
replace days_premature = 77 if (X1CHPREM > 77 & X1CHPREM != .)

*** Cap number of siblings at 6
gen num_siblings = X1NUMSIB
replace num_siblings = 6 if X1NUMSIB >= 6

*** redefine parent configuration
gen parent_configuration = 0
replace parent_configuration = 1 if (X1HPARNT == 1)
replace parent_configuration = 2 if (X1HPARNT == 4 | X1HPARNT == 5)
replace parent_configuration = 3 if (X1HPARNT == 2 | X1HPARNT == 3 | X1HPARNT == 10)
replace parent_configuration = 4 if (X1HPARNT == 6 | X1HPARNT == 7 | X1HPARNT == 8 | X1HPARNT == 9)

****create our race variable (Dropping observations that do not have valid race)
drop if race < 0
generate rr_white = (race == 1)
generate rr_black = (race == 2)
generate rr_hispanic = (race == 3 | race == 4)
generate rr_asian = (race == 5)
generate rr_other = (race == 6 | race == 7 | race == 8)

**** Create birthweight variables
gen birthweight = .
replace birthweight = 1 if (BCBRTHWT < 1500 & BCBRTHWT >= 0)
replace birthweight = 2 if (BCBRTHWT >= 1500 & BCBRTHWT < 2500)
replace birthweight = 3 if (BCBRTHWT >= 2500 & BCBRTHWT < 3500)
replace birthweight = 4 if (BCBRTHWT >= 3500 & BCBRTHWT != .)

*code gender
gen female = (X1CHSEX == 2)


** Mom's age
gen Mmomage = 0
replace Mmomage = 1 if momage < 0 | momage==.
replace momage = 0 if Mmomage == 1
for num 1/5: gen momage_X = momage^X

** Generate Child Age Dummies
gen age_9months = agem_9months
replace age_9months = 8 if age_9months <= 8
replace age_9months = 16 if age_9months >= 16
forvalues i = 8/15{
  replace age_9months = `i' if (agem_9months > `i' & agem_9months < (`i' + 1))
}

gen age_2years = agem_2years
replace age_2years = 23 if age_2years <= 23
replace age_2years = 26 if age_2years >= 26
forvalues i = 23/25{
  replace age_2years = `i' if (agem_2years > `i' & agem_2years < (`i' + 1))
}

******************************
** Scores
******************************
* Missing Values for Test Scores
foreach k in  X1NCATTP X1RMTLS X1MTL_A X1MTL_B X1MTL_C X1MTL_D X1MTL_E X1MTL_F X1MTL_G X1MTL_H X1MTL_I X1MTL_J X1RMTRS X2MTLSCL X2MTL_A X2MTL_B X2MTL_C X2MTL_D X2MTL_E X2MTL_F X2MTL_G X2MTL_H X2MTL_I X2MTL_J X2MTRSCL{
        replace `k' = . if (`k' == -9 | `k' == -99)
}

rename X1NCATTP parentscore
rename X1RMTLS iq_9months
rename X1RMTRS motorcomp_9months
rename X1MTL_A explore_9months
rename X1MTL_B explorepurp_9months
rename X1MTL_C babble_9months
rename X1MTL_D solve_9months
rename X1MTL_E name_objects_9months
rename X1MTL_F receptive_vocab_9months
rename X1MTL_G expressive_vocab_9months
rename X1MTL_H listening_comp_9months
rename X1MTL_I matching_9months
rename X1MTL_J counting_9months
rename X2MTLSCL iq_2years
rename X2MTRSCL motorcomp_2years
rename X2MTL_A explore_2years
rename X2MTL_B explorepurp_2years
rename X2MTL_C babble_2years
rename X2MTL_D solve_2years
rename X2MTL_E name_objects_2years
rename X2MTL_F receptive_vocab_2years
rename X2MTL_G expressive_vocab_2years
rename X2MTL_H listening_comp_2years
rename X2MTL_I matching_2years
rename X2MTL_J counting_2years


* Keep only the observations that we are going to use
keep if iq_2years != . & interviewerid_2years != . & W2C0 != .


foreach k of varlist X1HHREGN sesquint  parent_configuration birthweight X1MBRTST {
        tab(`k'), gen(c`k')
}

foreach x of varlist iq_9months {
        egen std_`x' = std(`x')
        ***this is not properly weighted

        reg std_`x' [pw=W1C0]
        generate holdmean`x'=_coef[_c] if std_`x'!=.
        replace std_`x' = std_`x' - holdmean`x'
        reg std_`x' [pw=W1C0]
        gen holdse`x'=_se[_c] if std_`x'!=.
        egen numobs`x'=count(std_`x') if std_`x'!=.
        gen holdsdratio`x'=holdse`x'*(numobs`x'^.5)
        replace std_`x'=std_`x'/holdsdratio`x'
}
foreach x of varlist iq_2years {
        egen std_`x'=std(`x')
        ***this is not properly weighted

        reg std_`x' [pw=W2C0]
        generate holdmean`x'=_coef[_c] if std_`x'!=.
        replace std_`x'=std_`x'-holdmean`x'
        reg std_`x' [pw=W2C0]
        generate holdse`x'=_se[_c] if std_`x'!=.
        egen numobs`x'=count(std_`x') if std_`x'!=.
        gen holdsdratio`x'=holdse`x'*(numobs`x'^.5)
        replace std_`x'=std_`x'/holdsdratio`x'
}

gen Mparentscore=0
replace Mparentscore = 1 if parentscore == .
replace parentscore = 0 if Mparentscore == 1
for num 1/5: gen parentscore_X = parentscore^X

foreach x of varlist momage_3 momage_4 momage_5 parentscore_3 parentscore_4 parentscore_5 {
        replace `x' = `x' / 100000
}
xi: areg iq_9months rr_black rr_hispanic rr_asian rr_other $base_9months $prenatal [pw=W1C0], absorb(interviewerid_9months)
gen use=e(sample)
drop if use == 0


** More sensible names and recoding **


rename X1MBRTST multiple_birth
recode multiple_birth (-9=999)
label define mb 1 "Singleton" 2 "Twin" 3 "Higher order" 999 "Not ascertained"
label values multiple_birth mb
rename parentscore parent_score
rename sesquint SES_quintile
rename X1HHREGN region
rename interviewerid_2years interviewer_ID_24
rename interviewerid_9months interviewer_ID_9
rename momage mom_age
rename num_siblings siblings

recode days_premature (-9=999)
label define na 999 "Not ascertained"
label values days_premature na

rename parent_configuration family_structure

label define fs 1 "Both biological parents present" 2 "One biological parent present" 3 "One biological and one other parent present" 4 "Other"
label values family_structure fs

label define bw 1 "<1500g" 2 "1500--2499g" 3 "2500-3499g" 4 ">3500g"
label values birthweight bw
rename Mmomage mom_age_NA
rename age_9months age_9
rename age_2years age_24
rename std_iq_9months std_iq_9
rename std_iq_2years std_iq_24
rename Mparentscore parent_score_NA

drop race
gen race = 0*rr_white+1*rr_black+2*rr_hispanic+3*rr_asian+4*rr_other
label define race 0 "White" 1 "Black" 2 "Hispanic" 3 "Asian" 4 "Other"
label values race race

keep W1C0 W2C0 multiple_birth parent_score SES_quintile region interviewer_ID_24 interviewer_ID_9 mom_age days_premature siblings family_structure birthweight female mom_age_NA age_9 age_24 std_iq_9 std_iq_24 parent_score_NA race
compress
label drop _1789F
label values parent_score
label values interviewer_ID_24
keep if birthweight ~= .
save "mental.dta", replace
