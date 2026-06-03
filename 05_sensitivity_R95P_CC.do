cd "C:\Users\saral\Desktop\SALURBAL-CLIMATE\SALURBAL-C\MS278\Model_results\City_center\R95PCC"

import excel "C:\Users\saral\Desktop\SALURBAL-CLIMATE\SALURBAL-C\MS278\Data\data_prec_final.xlsx", sheet("Sheet1") firstrow

histogram R95PCC
graph export "hist.jpg", as(jpg) name("Graph") quality(90)

twoway (scatter R95PCC YEAR_dec, mcolor(%20) msymbol(o)) (lowess R95P YEAR_dec, lcolor(blue)) (lfit R95PCC YEAR_dec,lpattern(dash) lcolor(red)), legend(order(1 "Pontos" 2 "Loess" 3 "Linear")) ytitle("R95P") xtitle("YEAR_dec")
graph export "R95PCC_scatter.png", replace width(2000)

****************************************************
* 1. Null model
****************************************************
mixed R95PCC || SALID1:, vce(robust)
estat recovariance
estat icc

outreg2 using R95PCC_null_model, replace word dec(2) ci

*******************************************************
* 2. Add year as fixed effect - linear
*******************************************************
* linear
mixed R95PCC c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
predict linear_model, xb
est store linear_model

****************************************************
* 3. Model plus time - linear - get random slope
****************************************************
mixed R95PCC c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
estat icc

predict resid_linear, residuals
twoway (scatter resid_linear YEAR_dec) (lowess resid_linear YEAR_dec), title("Residuals from Linear Model")

* obs vs fitted
predict yhat, fitted
*twoway (scatter R95PCC YEAR_dec, mcolor(gs10) msymbol(o)) (line yhat YEAR_dec, sort lcolor(black) lwidth(medthick)), legend(order(1 "Observed" 2 "Fitted")) ytitle("R95PCC (%)") xtitle("Year (decades)")
*graph export "R95PCC_time_only_model.png", replace width(2000)

* Plots obs vs pred for each city
predict yhat_fixed, xb
predict re_intercept re_slope, reffects
gen yhat_city = _b[_cons] + re_intercept + (_b[YEAR_dec] + re_slope) * YEAR_dec
corr yhat_fixed yhat_city

set scheme s1color

egen city_group = cut(SALID1), group(12)
levelsof city_group, local(groups)

sort SALID1 YEAR_dec

foreach g of local groups {

    preserve
    keep if city_group==`g'

    sort SALID1 YEAR_dec

    twoway ///
    (line R95PCC YEAR_dec, sort lcolor(black) lwidth(thin)) ///
   (line yhat_city YEAR_dec, sort lcolor(red) lwidth(medthick)), ///
   by(SALID1, cols(4) compact note("")) ///
    legend(order(1 "Observed" 2 "Predicted")) ///
    name(graph`g', replace)

    graph export "R95PCC_obs_vs_pred_group_`g'.png", replace width(2000)

    restore
}

* Extract random effects aleatórios
predict double re1 re2, reffects

* check order
describe re1 re2

* Rename
rename re1 slope_re
rename re2 intercept_re

* slope for each city
scalar b_fixed = _b[YEAR_dec]
gen slope_total = b_fixed + slope_re

* line for each city
preserve
keep SALID1 slope_re intercept_re slope_total
bysort SALID1: keep if _n == 1

* Export
export excel using "R95PCC_city_random_slopes.xlsx", replace firstrow(variables)
export delimited using "R95PCC_city_random_slopes.csv", replace

restore

* check
estat recovariance

****************************************************
* 4. Univariate mixed models - year linear
****************************************************
preserve

    tempfile results
    tempname memhold
    postfile `memhold' str20 outcome str30 variable estimate min95 max95 p ///
        using `results', replace

    local vars elevation_z slope_z coastal total_pop_z pop_density_guf_z pop_over65_z GDP_z NDVI_z education_z

    foreach v of local vars {
        mixed R95PCC `v' c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
        
        // Captura os resultados diretamente
        matrix b = e(b)
        matrix V = e(V)
        
        local estimate = b[1, 1]
        local se = sqrt(V[1, 1])
        local min95 = `estimate' - 1.96 * `se'
        local max95 = `estimate' + 1.96 * `se'
        
        // Usa distribuição normal para valor-p (aproximação comum para mixed)
        local z = abs(`estimate'/`se')
        local p = 2 * (1 - normal(`z'))
        
        post `memhold' ("R95PCC") ("`v'") (`estimate') (`min95') (`max95') (`p')
    }

    postclose `memhold'
    
    use `results', clear
    export delimited using "R95PCC_univariate.csv", replace

restore

* 4.1. Climate zones- no interaction
* Tropical as reference
encode CLZ, gen(CLZ_num)
label list CLZ_num

mixed R95PCC ib4.CLZ_num c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

preserve

    mixed R95PCC ib4.CLZ_num c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

    parmest, norestore level(95)
    export delimited using "R95PCC_CLZ.csv", replace

restore

****************************************************
* 5. Climate zones - interaction term
****************************************************
mixed R95PCC ib4.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

testparm i.CLZ_num#c.YEAR_dec

preserve

mixed R95PCC ib4.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

parmest, norestore level(95)
export delimited using "R95PCC_CLZ_interaction.csv", replace

restore

****************************************************
* 6. Hybrid mixed-effects models with time slope
****************************************************
preserve

tempfile results
tempname memhold

postfile `memhold' ///
    str20 outcome ///
    str30 variable_type ///
    str30 variable ///
    estimate min95 max95 p ///
    slope slope_min95 slope_max95 slope_p ///
    using `results', replace

* htbrid variables
local hybrids total_pop pop_density_guf pop_over65 GDP NDVI education

foreach var in `hybrids' {

    mixed R95PCC ///
        `var'_wht_z ///
        `var'_btw_z ///
        c.YEAR_dec ///
        || SALID1: c.YEAR_dec, vce(robust)

    matrix b = e(b)
    matrix V = e(V)

    local cnames : colnames b

    local wht_pos   : list posof "`var'_wht_z" in cnames
    local btw_pos   : list posof "`var'_btw_z" in cnames
    local year_pos  : list posof "YEAR_dec"     in cnames

    
    * Slope (YEAR_dec)
    local slope    = b[1,`year_pos']
    local slope_se = sqrt(V[`year_pos',`year_pos'])

    local slope_min95 = `slope' - 1.96*`slope_se'
    local slope_max95 = `slope' + 1.96*`slope_se'

    local z_slope = abs(`slope'/`slope_se')
    local slope_p = 2*(1 - normal(`z_slope'))

    * Within
    local estimate = b[1,`wht_pos']
    local se       = sqrt(V[`wht_pos',`wht_pos'])

    local min95 = `estimate' - 1.96*`se'
    local max95 = `estimate' + 1.96*`se'

    local z = abs(`estimate'/`se')
    local p = 2*(1 - normal(`z'))

    post `memhold' ///
        ("R95PCC") ("within") ("`var'") ///
        (`estimate') (`min95') (`max95') (`p') ///
        (`slope') (`slope_min95') (`slope_max95') (`slope_p')

    * Between
    local estimate = b[1,`btw_pos']
    local se       = sqrt(V[`btw_pos',`btw_pos'])

    local min95 = `estimate' - 1.96*`se'
    local max95 = `estimate' + 1.96*`se'

    local z = abs(`estimate'/`se')
    local p = 2*(1 - normal(`z'))

    post `memhold' ///
        ("R95PCC") ("between") ("`var'") ///
        (`estimate') (`min95') (`max95') (`p') ///
        (`slope') (`slope_min95') (`slope_max95') (`slope_p')
}

postclose `memhold'

use `results', clear
export delimited using "R95PCC_hybrid_models_with_slope.csv", replace

restore