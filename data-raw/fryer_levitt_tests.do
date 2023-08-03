log using fryer_levitt_tests.log, replace
use mental.dta, clear

global base_9months "i.age_9 female"
global base_2years "i.age_24 female"
global prenatal "i.birthweight i.days_premature i.multiple_birth"
global home "i.siblings i.family_structure i.region c.mom_age##c.mom_age##c.mom_age##c.mom_age##c.mom_age mom_age_NA c.parent_score##c.parent_score##c.parent_score##c.parent_score##c.parent_score parent_score_NA"

qui: eststo r1: reg std_iq_24 i.race $base_2years [pw=W2C0], robust
qui: mlogit race $base_2years [pw=W2C0], robust
disp e(chi2)
disp e(df_m)
disp e(p)
qui: eststo a1: reg std_iq_24 i.race $base_2years [pw=W2C0] if (1.race | 0.race), robust
qui: eststo a2: reg std_iq_24 i.race $base_2years [pw=W2C0] if (2.race | 0.race), robust
qui: eststo a3: reg std_iq_24 i.race $base_2years [pw=W2C0] if (3.race | 0.race), robust
qui: eststo a4: reg std_iq_24 i.race $base_2years [pw=W2C0] if (4.race | 0.race), robust

qui: eststo r2: reg std_iq_24 i.race $base_2years, robust
qui: mlogit race $base_2years, robust
disp e(chi2)
disp e(df_m)
disp e(p)

* ATE
tabulate age_24, generate(age_24a)
drop age_24a4
foreach v of varlist age_24a* female  {
    qui: summarize `v'
    generate `v'd = `v' - r(mean)
}
foreach v of varlist age_24a* female  {
    qui: summarize `v' [w=W2C0]
    generate `v'w = `v' - r(mean)
}

qui: eststo r3: reg std_iq_24 i.race##c.femaled i.race##c.age_24a*d, robust
qui: eststo r4: reg std_iq_24 i.race##c.femalew i.race##c.age_24a*w [pw=W2C0], robust

qui: eststo b1: reg std_iq_24 i.race $base_2years if (1.race | 0.race), robust
qui: eststo b2: reg std_iq_24 i.race $base_2years if (2.race | 0.race), robust
qui: eststo b3: reg std_iq_24 i.race $base_2years if (3.race | 0.race), robust
qui: eststo b4: reg std_iq_24 i.race $base_2years if (4.race | 0.race), robust

esttab r1 r2 r3 r4, b(%13.11f) se(%13.11f) wide keep(*.race)
esttab a1 a2 a3 a4, b(%13.11f) se(%13.11f) wide keep(*.race)
esttab b1 b2 b3 b4, b(%13.11f) se(%13.11f) wide keep(*.race)

qui: eststo r5: reg std_iq_24 i.race i.days_premature if (days_premature~=56), cluster(interviewer_ID_24)
qui: mlogit race i.days_premature if (days_premature~=56), cluster(interviewer_ID_24)
disp e(chi2)
disp e(df_m)
disp e(p)
esttab r5, b(%13.11f) se(%13.11f) wide keep(*.race)
* TODO: ATE OAT

* qui: eststo r4: areg std_iq_24 i.race $base_2years i.SES_quintile $home $prenatal [pw=W2C0] , absorb(interviewer_ID_24)

log close
