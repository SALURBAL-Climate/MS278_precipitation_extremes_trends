cd "C:\Users\saral\Desktop\SALURBAL-CLIMATE\SALURBAL-C\MS278\Model_results\City_center\Rx1dayCC"

import excel "C:\Users\saral\Desktop\SALURBAL-CLIMATE\SALURBAL-C\MS278\Data\data_prec_final.xlsx", sheet("Sheet1") firstrow

histogram Rx1dayCC
graph export "hist.jpg", as(jpg) name("Graph") quality(90)

twoway (scatter Rx1dayCC YEAR_dec, mcolor(%20) msymbol(o)) (lowess Rx1dayCC YEAR_dec, lcolor(blue)) (lfit Rx1dayCC YEAR_dec,lpattern(dash) lcolor(red)), legend(order(1 "Pontos" 2 "Loess" 3 "Linear")) ytitle("Rx1dayCC") xtitle("YEAR_dec")
graph export "Rx1dayCC_scatter.png", replace width(2000)

****************************************************
* 1. Null model
****************************************************
mixed Rx1dayCC || SALID1:, vce(robust)
estat recovariance
estat icc

outreg2 using Rx1dayCC_null_model, replace word dec(2) ci

*******************************************************
* 2. Add year as fixed effect - linear and non- linear
*******************************************************
* 3.1. linear
mixed Rx1dayCC c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
predict linear_model, xb
est store linear_model

* 3.2. quad
gen YEAR_dec2 = YEAR_dec^2

mixed Rx1dayCC c.YEAR_dec YEAR_dec2 || SALID1: c.YEAR_dec, vce(robust)
predict quad_model, xb
est store quad_model

* 3.3. cubic
gen YEAR_dec3 = YEAR_dec^3

mixed Rx1dayCC c.YEAR_dec YEAR_dec2 YEAR_dec3 || SALID1: c.YEAR_dec, vce(robust)
predict cubic_model, xb
est store cubic_model

*3.4. cub - splines
mkspline year_sp = YEAR_dec, nknots(3) cubic

mixed Rx1dayCC year_sp* || SALID1: c.YEAR_dec, vce(robust)
predict spline_model, xb
est store spline_model

estimates stats linear_model quad_model cubic_model spline_model

* 3.5. Compare models and plot 
twoway (lowess Rx1dayCC YEAR_dec, mcolor(gs13) msymbol(o) msize(vsmall)) (line linear_model YEAR_dec, sort lwidth(medthick)) (line quad_model YEAR_dec, sort lwidth(medthick)) (line cubic_model YEAR_dec, sort lwidth(medthick)) (line spline_model YEAR_dec, sort lwidth(thick)), legend(order(1 "Observed (Lowess)" 2 "Linear" 3 "Quadratic" 4 "Cubic" 5 "Spline")) xtitle("YEAR_dec") ytitle("Rx1dayCC")
graph export "Rx1dayCC_linear_non_linear.png", replace width(2000)

twoway (scatter Rx1dayCC YEAR_dec, mcolor(gs13) msymbol(o) msize(vsmall)) (line quad_model YEAR_dec, sort lwidth(medthick)) (line spline_model YEAR_dec, sort lwidth(thick)) (line linear_model YEAR_dec, sort lwidth(medthick)), legend(order(2 "Quad" 3 "Spline" 4 "Linear")) xtitle("YEAR_dec") ytitle("Rx1dayCC")
graph export "Rx1dayCC_linear_non_linear_wrong.png", replace width(2000)

****************************************************
* 3. Model plus time - linear - get random slope
****************************************************
mixed Rx1dayCC c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
estat icc

predict resid_linear, residuals
twoway (scatter resid_linear YEAR_dec) (lowess resid_linear YEAR_dec), title("Residuals from Linear Model")

* obs vs fitted
predict yhat, fitted
*twoway (scatter Rx1dayCC YEAR_dec, mcolor(gs10) msymbol(o)) (line yhat YEAR_dec, sort lcolor(black) lwidth(medthick)), legend(order(1 "Observed" 2 "Fitted")) ytitle("Rx1dayCC (%)") xtitle("Year (decades)")
*graph export "Rx1dayCC_time_only_model.png", replace width(2000)

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
    (line Rx1dayCC YEAR_dec, sort lcolor(black) lwidth(thin)) ///
   (line yhat_city YEAR_dec, sort lcolor(red) lwidth(medthick)), ///
   by(SALID1, cols(4) compact note("")) ///
    legend(order(1 "Observed" 2 "Predicted")) ///
    name(graph`g', replace)

    graph export "Rx1dayCC_obs_vs_pred_group_`g'.png", replace width(2000)

    restore
}

* Histogram random intercept and slope
predict u0 u1, reffects
histogram u1, normal title("Random intercepts")
graph export "Rx1dayCC_randon_inter_hist.png", replace width(2000)
histogram u0, normal title("Random slopes (time)")
graph export "Rx1dayCC_randon_slope_hist.png", replace width(2000)

*QQ plots
qnorm u1, title("Q-Q plot: random intercepts")
graph export "Rx1dayCC_qq_inter.png", replace width(2000)
qnorm u0, title("Q-Q plot: random slopes")
graph export "Rx1dayCC_qq_slope.png", replace width(2000)

predict ehat, resid
qnorm ehat, title("Q-Q plot: residuals")
graph export "Rx1dayCC_qq_residuals.png", replace width(2000)

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
export excel using "Rx1dayCC_city_random_slopes.xlsx", replace firstrow(variables)
export delimited using "Rx1dayCC_city_random_slopes.csv", replace

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
        mixed Rx1dayCC `v' c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
        
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
        
        post `memhold' ("Rx1dayCC") ("`v'") (`estimate') (`min95') (`max95') (`p')
    }

    postclose `memhold'
    
    use `results', clear
    export delimited using "Rx1dayCC_univariate.csv", replace

restore

* 4.1. Climate zones- no interaction
* Tropical as reference
encode CLZ, gen(CLZ_num)
label list CLZ_num

mixed Rx1dayCC ib4.CLZ_num c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

preserve

    mixed Rx1dayCC ib4.CLZ_num c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

    parmest, norestore level(95)
    export delimited using "Rx1dayCC_CLZ.csv", replace

restore

****************************************************
* 5. Climate zones - interaction term
****************************************************
mixed Rx1dayCC ib4.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

testparm i.CLZ_num#c.YEAR_dec

preserve

mixed Rx1dayCC ib4.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

parmest, norestore level(95)
export delimited using "Rx1dayCC_CLZ_interaction.csv", replace

restore

* 5.1. control for elevaton slope and coastal
mixed Rx1dayCC elevation_z coastal slope_z ib4.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

testparm i.CLZ_num#c.YEAR_dec

preserve

mixed Rx1dayCC elevation_z coastal slope_z ib4.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

parmest, norestore level(95)

export delimited using "Rx1dayCC_CLZ_interaction_controlled.csv", replace

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
local hybrids pop_density_guf pop_over65 GDP NDVI education

foreach var in `hybrids' {

    mixed Rx1dayCC ///
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
        ("Rx1dayCC") ("within") ("`var'") ///
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
        ("Rx1dayCC") ("between") ("`var'") ///
        (`estimate') (`min95') (`max95') (`p') ///
        (`slope') (`slope_min95') (`slope_max95') (`slope_p')
}

postclose `memhold'

use `results', clear
export delimited using "Rx1dayCC_hybrid_models_with_slope.csv", replace

restore

* 6.1. control for elevation coastal and slope
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

local hybrids total_pop pop_density_guf pop_over65 GDP NDVI education

foreach var in `hybrids' {

    mixed Rx1dayCC ///
        `var'_wht_z ///
        `var'_btw_z ///
        elevation_z coastal slope_z c.YEAR_dec ///
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
        ("Rx1dayCC") ("within") ("`var'") ///
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
        ("Rx1dayCC") ("between") ("`var'") ///
        (`estimate') (`min95') (`max95') (`p') ///
        (`slope') (`slope_min95') (`slope_max95') (`slope_p')
}

postclose `memhold'

use `results', clear
export delimited using "Rx1dayCC_hybrid_models_with_slope_controlled.csv", replace

restore