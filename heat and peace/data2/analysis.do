cd "/Users/ramfishman/Dropbox/heat and peace (1)"
use "peace_and_attackes.dta"



gen week = _n
tsset week

gen date = date(survey_date,"DMY")
gen month=month(date)
gen year=year(date)

gen orig_q3 = q3
replace q3=. if q3<1.5
gen orig_q2 = q2
replace q2=. if q2<1.5

eststo clear
foreach l of numlist 0(1)12 {
eststo L`l': qui reg q3 L`l'.total_attacks 
eststo L`l'_i: qui reg q3 L`l'.total_attacks intifada
local lags `lags' L`l'.total_attacks
local lags_m   `lags_m'   L`l'
local lags_m_i `lags_m_i' L`l'_i
eststo all: qui reg q3 `lags'
eststo all_i: qui reg q3 `lags' intifada
}
coefplot (`lags_m', label(Separately)) (`lags_m_i', label(Separately Intifada)) (all, label(Together)) (all_i, label(Together Intifada)), keep(*total_attacks*)  xline(0)

foreach v of varlist total_attacks FO_total_attacks {
gen m_`v' = `v' + L1.`v' + L2.`v' + L3.`v' 
}

gen attacks=.
label var m_total_attacks "Number of Attacks"
label var m_FO_total_attacks "Number of Attacks"
label var attacks "Number of Attacks"

replace intifada=0 if intifada==.

eststo clear
foreach q in 1 2 3 {
replace attacks = m_total_attacks
eststo q`q'_bt  : reg q`q' attacks 
eststo q`q'_bt_i: reg q`q' attacks intifada
replace attacks = m_FO_total_attacks
eststo q`q'_fo  : reg q`q' attacks 
eststo q`q'_fo_i: reg q`q' attacks intifada
}

esttab, p(2) b(2) mtitles
coefplot (q1_bt, label(Betselem)) (q1_bt_i, label("Betselem, Control for Intifada")) (q1_fo, label(MOFA)) (q1_fo_i, label("MOFA, Control for Intifada")), title(q1) || ///
         (q2_bt, label(Betselem)) (q2_bt_i, label("Betselem, Control for Intifada")) (q2_fo, label(MOFA)) (q2_fo_i, label("MOFA, Control for Intifada")), title(qdfdf2) || ///
		 (q3_bt, label(Betselem)) (q3_bt_i, label("Betselem, Control for Intifada")) (q3_fo, label(MOFA)) (q3_fo_i, label("MOFA, Control for Intifada")), title(q3) || ///
	 , keep(attacks) xline(0) levels(90) 
