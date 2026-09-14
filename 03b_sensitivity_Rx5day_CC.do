cd "C:\Users\saral\Desktop\SALURBAL-CLIMATE\SALURBAL-C\MS278"
pwd

global root "."

capture mkdir "${root}/Model_results/City_center/Rx5dayCC"

import excel "Data/data_prec_final_wht_polar.xlsx", sheet("Sheet1") firstrow

****************************************************
* 1. Null model
****************************************************
mixed Rx5dayCC || SALID1:, vce(robust)

estat recovariance
matrix C = r(Cov2)
scalar var_between = C[1,1]

* var
scalar var_within = exp(_b[lnsig_e:_cons])^2

* ICC
scalar ICC = var_between / (var_between + var_within)

scalar ICC_pct = ICC * 100

preserve

clear
set obs 1

gen str10 Index = "Rx5dayCC"

gen double Between = var_between

gen double Within = var_within

gen double ICC_pct = ICC_pct

format Between %15.6f
format Within  %15.6f
format ICC_pct %15.4f

list, noobs clean

* Export
export excel using "Model_results/City_center/Rx5dayCC/Rx5dayCC_null_variance.xlsx", replace firstrow(variables)

restore

*******************************************************
* 2. Add year as fixed effect - linear
*******************************************************
* linear
mixed Rx5dayCC c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
predict linear_model, xb
est store linear_model

****************************************************
* 3. Model plus time - linear - get random slope
****************************************************
mixed Rx5dayCC c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
estat icc

* 3.1 Overall time trend
preserve

parmest, norestore level(95)

keep if parm == "YEAR_dec"

keep parm estimate min95 max95 p

export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_overall_trend.csv", replace

restore

* 3.2 Extract random effects and export city-specific slopes
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
export excel using "Model_results/City_center/Rx5dayCC/Rx5dayCC_city_random_slopes.xlsx", replace firstrow(variables)
export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_city_random_slopes.csv", replace

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
        mixed Rx5dayCC `v' c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)
        
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
        
        post `memhold' ("Rx5dayCC") ("`v'") (`estimate') (`min95') (`max95') (`p')
    }

    postclose `memhold'
    
    use `results', clear
    export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_univariate.csv", replace

restore

* 4.1. Climate zones- no interaction
* Tropical as reference
encode CLZ, gen(CLZ_num)
label list CLZ_num

preserve

    mixed Rx5dayCC ib3.CLZ_num c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

    parmest, norestore level(95)
    export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ.csv", replace

restore

****************************************************
* 5. Climate zones - interaction term
****************************************************
mixed Rx5dayCC ib3.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

preserve

parmest, norestore level(95)

*export delimited using "Rx5dayCC_CLZ_interaction_coefficients.csv", replace

restore

* 5.1. Global test of interaction
mixed Rx5dayCC ib3.CLZ_num##c.YEAR_dec || SALID1: c.YEAR_dec, vce(robust)

testparm i.CLZ_num#c.YEAR_dec

* Store results from the joint test
local chi2 = r(chi2)
local df   = r(df)
local p    = r(p)

preserve

clear
set obs 1

gen chi2 = `chi2'
gen df   = `df'
gen p    = `p'

format chi2 %9.3f
format df %9.0f
format p %9.4f

export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_interaction_test.csv", replace

restore

* Trends
tempname memhold
postfile `memhold' ///
str15 Climate_zone ///
double(Base_coef Base_LCI Base_UCI Base_p ///
Trend Trend_LCI Trend_UCI Trend_p) ///
using CLZ_results_temp.dta, replace

* Tropical (reference)
lincom YEAR_dec

local trend      = r(estimate)
local trend_lci  = r(lb)
local trend_uci  = r(ub)
local trend_p    = r(p)

post `memhold' ///
("Tropical") ///
(0) (.) (.) (.) ///
(`trend') (`trend_lci') (`trend_uci') (`trend_p')

* Arid
lincom 1.CLZ_num

local base      = r(estimate)
local base_lci  = r(lb)
local base_uci  = r(ub)
local base_p    = r(p)

lincom YEAR_dec + 1.CLZ_num#c.YEAR_dec

local trend      = r(estimate)
local trend_lci  = r(lb)
local trend_uci  = r(ub)
local trend_p    = r(p)

post `memhold' ///
("Arid") ///
(`base') (`base_lci') (`base_uci') (`base_p') ///
(`trend') (`trend_lci') (`trend_uci') (`trend_p')

* Temperate
lincom 2.CLZ_num

local base      = r(estimate)
local base_lci  = r(lb)
local base_uci  = r(ub)
local base_p    = r(p)

lincom YEAR_dec + 2.CLZ_num#c.YEAR_dec

local trend      = r(estimate)
local trend_lci  = r(lb)
local trend_uci  = r(ub)
local trend_p    = r(p)

post `memhold' ///
("Temperate") ///
(`base') (`base_lci') (`base_uci') (`base_p') ///
(`trend') (`trend_lci') (`trend_uci') (`trend_p')

postclose `memhold'

preserve

use CLZ_results_temp.dta, clear

format Base_coef Base_LCI Base_UCI %9.2f
format Trend Trend_LCI Trend_UCI %9.2f

format Base_p Trend_p %8.4f

order Climate_zone ///
      Base_coef Base_LCI Base_UCI Base_p ///
      Trend Trend_LCI Trend_UCI Trend_p

list, clean

export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_CLZ_results.csv", replace

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

    mixed Rx5dayCC ///
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
        ("Rx5dayCC") ("within") ("`var'") ///
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
        ("Rx5dayCC") ("between") ("`var'") ///
        (`estimate') (`min95') (`max95') (`p') ///
        (`slope') (`slope_min95') (`slope_max95') (`slope_p')
}

postclose `memhold'

use `results', clear
export delimited using "Model_results/City_center/Rx5dayCC/Rx5dayCC_hybrid_models_with_slope.csv", replace

restore