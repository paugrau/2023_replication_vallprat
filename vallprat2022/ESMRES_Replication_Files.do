*************************************************************
*************************************************************
* Economic Shocks, Mobilization, and Regional Elite Splits  *
* 							***							    *
*             Comparative Political Studies                 *
*************************************************************
*************************************************************
ssc install grstyle
ssc install interflex
net from http://www.stata.com
net cd users
net cd vwiggins
net install grc1leg

cls
clear all

capture cd "C:\Users\Usuari\OneDrive - Istituto Universitario Europeo\Courses\2nd Term\Challenges to Democracy\Replication-SH2023\Week 9\vallprat2022" // Define the folder

set more off

*Graph format
set scheme s2mono, permanently //plotplain
grstyle init
grstyle set plain, horizontal grid noextend // overall format
grstyle set compact // reduce size
grstyle color ci_area gs12%30 // CI transparent 
grstyle symbolsize p small

graph set window fontface "Times New Roman"
graph set eps fontface "Times New Roman"


use "C:\Users\Usuari\OneDrive - Istituto Universitario Europeo\Courses\2nd Term\Challenges to Democracy\Replication-SH2023\Week 9\vallprat2022\CPS_ESMRES_Dataset-Panel.dta"

keep if Year>1900

** DESCRIPTIVES **

* Table 1: Summary Statistics

eststo clear
estpost sum Lliga ColShock logManre  /// SocisIACSI1904
	logPop literacy foreigners1887 logind gini Peasants1918 dum_Landowners MagnitudeDist ByElection Manre_dist //
	
	*Nice Summary Statistics table 
	esttab, cells("count(label(N)) mean(label(Mean) fmt(2)) sd(label(SD) fmt(2)) min(label(Min.) fmt(2)) max (label(Max.) fmt(2))") ///
		nomtitles nonumber noobs label title("District-level Summary Statistics")
		
** MAIN ANALYSES **
		
* Table 2: Baseline Results

eststo clear

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners
local time c.timetrend

qui eststo: logit Lliga c.ColShock logManre `control' , vce(cluster DistId)
qui eststo: logit Lliga c.ColShock logManre c.timetrend `control' , vce(cluster DistId)
qui eststo: logit Lliga c.ColShock logManre c.timetrend ib8.ProvId `control' , vce(cluster DistId)
qui eststo: logit Lliga c.ColShock##c.logManre c.timetrend ib8.ProvId `control', vce(cluster DistId) // c.dum_Landowners

	* Nice Table 2
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
		star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
		"Controls = `control'" "Time Trend = timetrend" "Prov FE = *.ProvId", labels("Yes" "No"))  ///
		order (ColShock logManre c.ColShock#c.logManre) ///
		interaction(" × ") wrap 
		

* Figure 4: Main Effects
local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners

qui logit Lliga c.ColShock logManre i.timetrend ib8.ProvId `control', vce(cluster DistId)
	
	* Graph Main effects of the main Independent variables
	margins, at(ColShock=(0(.5)18))
	marginsplot, addplot(hist ColShock, ylabel(0(.2)1) xlabel(0(2)18) color(gs11%20) yaxis(2) yscale(off alt lcolor() axis(2))) recast(line) plot1opts(lpattern(shortdash)) recastci(rarea) ciopts(lcolor(gs12) color(%30)) legend(off) title("") ytitle("Expected Probability of Lliga Candidacy") name(CS, replace)
	margins, at(logManre=(0(.1)4))
	marginsplot, addplot(hist logManre, ylabel(0(.2)1) xlabel(0(.5)4) color(gs11%20) yaxis(2) yscale(/*off*/ alt lcolor() axis(2)) ytitle(,orientation(rvertical) axis(2))) recast(line) plot1opts(lpattern(shortdash)) recastci(rarea) ciopts(lcolor(gs12) color(%30)) legend(off) title("") ytitle("") yscale(off) name(Manresa, replace) 
	
	* Nice Figure 4
	graph combine CS Manresa, ycommon r(1)
	
	
* Figure 5: Heterogeneous Effects
local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners

quietly logit Lliga c.ColShock##c.logManre c.timetrend ib8.ProvId `control', vce(cluster DistId) 

margins, dydx(ColShock) at(logManre=(0(.5)4)) level(95)

	* Nice Figure 5
	marginsplot, addplot(hist logManre, xlabel(0(.5)4) color(gs11%20) yaxis(2) yscale(off alt lcolor() axis(2))) recast(line) plot1opts(lpattern(shortdash)) recastci(rarea) ciopts(lcolor(gs12) color(%30)) yline(0, lp(solid) lc(black)) legend(off) title("") ytitle("Colonial Shock Marginal Effects") name(CS_Id, replace)
		
	
** MECHANISMS **

* Figure 6: Alternative Measures of Exposure to the Colonial Shock

gen texap =.
	label var texap "Industry Exposure"

eststo clear
estimates clear
local varap ColShock Per_Pop_T_cotton looms1900_per Per_Textile telwool_per othertex

	foreach x in `varap'{
	replace texap=.
	replace texap=`x'
		
		local control MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1
		local geo i.BCN i.ProvId
		local mob logManre

	eststo: logit Lliga c.texap `mob' i.Year `geo' `control', vce(cluster DistId)
	estimates store `x'_est1
	eststo: logit Lliga c.texap##c.logManre i.Year `geo' `control', vce(cluster DistId) 
	estimates store `x'_est2
	}
	
	* Nice Figure 6
	coefplot ColShock_est2 Per_Pop_T_cotton_est2 looms1900_per_est2 Per_Textile_est2 telwool_per_est2 othertex_est2, keep(c.texap#c.logManre) levels(95 90) xline(0) legend(off) asequation swapnames eqrename(^ColShock_est2$ = "Colonial Shock" ^Per_Pop_T_cotton_est2$ = "% Cotton Workers 1861" ^looms1900_per_est2$ = "% Cotton Looms 1900" ^Per_Textile_est2$ = "% Cotton Taxes 1910s" ^telwool_per_est2$ = "% Wool 1861" ^othertex_est2$ = "% Other Textile 1910s", regex) xtitle("Marginal Effects") title("Industrialization Exposure × (log) Identity Mobilization") name(ColShock_coefplot, replace) note("Bold: 90% CI | Thin: 95% CI", pos(5) size(small))


* Figure 7: Time Periods
gen period =.
	replace period=1 if Year<1908
	replace period=2 if Year>1907 & Year<1914
	replace period=3 if Year>1913 & Year<1918
	replace period=4 if Year>1917

eststo clear

local control c.dum_Landowners MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist i.Year if ElectionType==1

eststo: logit Lliga c.ColShock##c.logManre ib8.ProvId `control`x'', vce(cluster DistId)

estimates store ColShockManresa_All


forval x=1/4{
local control`x' c.dum_Landowners MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist i.Year if ElectionType==1 & period==`x' 
eststo: logit Lliga c.ColShock##c.logManre ib8.ProvId `control`x'', vce(cluster DistId)
estimates store ColShockManresa_`x'
}
	

	coefplot ColShockManresa_All ColShockManresa_1 ColShockManresa_2 ColShockManresa_3 ColShockManresa_4, keep(c.ColShock#c.logManre) levels(95 90) yline(0) legend(size(*1.3) pos(6) r(1)  order(3 "All periods" 6 "1901-1907" 9 "1908-1913" 12 "1914-1917" 15 "1918-1923" /*1 "95% CI" 2 "90% CI"*/)) xscale(off) coeflabels(c.ColShock#c._Manresa = " ") vertical ytitle("Marginal Effects") title("Colonial Shock × (log) Identity Mobilization") note("Bold: 90% CI | Thin: 95% CI", pos(5) size(small))
	

** ROBUSTNESS **

* Figure 8: Colonial Shock vs. Pre-1898 Outcomes
preserve

* Keep a single observation per constituency
collapse TurnoDist1898 logManre ColShock, by(Dist ElectionType)

grstyle color ci_area gs12%30	
twoway scatter TurnoDist1898 ColShock if ElectionType==1 || scatter TurnoDist1898 ColShock if ElectionType==2 ||  lfitci TurnoDist1898 ColShock, name(Turno1, replace) title("Pacific Turn Compliance") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1))

twoway scatter logManre ColShock if ElectionType==1 || scatter logManre ColShock if ElectionType==2 || lfitci logManre ColShock , name(Manresa_CS, replace) title("(log) Identity Mobilization") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1)) 

grc1leg Turno1 Manresa_CS, r(1)

restore


******************************
*** SUPPLEMENTARY MATERIAL ***
******************************

** APPENDIX C: MECHANISMS **

* Alternative Exposure to Colonial Shock Indicators
eststo clear
estimates clear
local varap ColShock Per_Pop_T_cotton looms1900_per Per_Textile telwool_per othertex

	foreach x in `varap'{
	replace texap=.
	replace texap=`x'
		
		local control MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1
		local geo i.BCN i.ProvId
		local mob logManre

	eststo: logit Lliga c.texap `mob' i.Year `geo' `control', vce(cluster DistId)
	estimates store `x'_est1
	eststo: logit Lliga c.texap##c.logManre i.Year `geo' `control', vce(cluster DistId) 
	estimates store `x'_est2
	}
	
	label var texap "Industry Exposure"
	
	* Table C.1
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( /// 
			"Controls = MagnitudeDist logPop ByElection literacy foreigners1887 dum_Landowners " ///
			"Year FE = *.Year" ///
			"Geography FE = *.ProvId *.BCN", labels("Yes" "No")) ///  
			order (texap logManre c.texap#c.logManre dum_Landowners) ///
			interaction(" × ") wrap ///
			mgroups("Colonial Shock" "Cotton Workers 1861" "Cotton Looms 1861" "Cotton Taxes 1910s" "Wool 1861" "Other Textile", ///
                         pattern(1 0 1 0 1 0 1 0 1 0 1 0))
						 
* Type of Elections

eststo clear
local control1 dum_Landowners MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop logind Manre_dist if ElectionType==1
local control2 dum_Landowners MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop logind  Manre_dist if ElectionType==2

eststo: logit Lliga c.ColShock logManre c.timetrend ib8.ProvId `control1', vce(cluster DistId)
eststo: logit Lliga c.ColShock##c.logManre c.dum_Landowners c.timetrend ib8.ProvId `control1', vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre c.timetrend ib8.ProvId `control2', vce(cluster DistId)
eststo: logit Lliga c.ColShock##c.logManre c.dum_Landowners c.timetrend ib8.ProvId `control2', vce(cluster DistId)

	* Table C.2
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 logind dum_Landowners" ///
			"Prov FE = *.ProvId" "Time Trend = timetrend", labels("Yes" "No"))  ///
			order (ColShock logManre c.ColShock#c.logManre) ///
			interaction(" × ") wrap ///
			mgroups("Legislative Districts" "Provincial Districts", pattern(1 0 1 0  ))


* Time Periods
eststo clear

local control c.dum_Landowners MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist i.Year if ElectionType==1
eststo: logit Lliga c.ColShock##c.logManre ib8.ProvId `control`x'', vce(cluster DistId)
estimates store ColShockManresa_All

forval x=1/4{
local control`x' c.dum_Landowners MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist i.Year if ElectionType==1 & period==`x' 
eststo: logit Lliga c.ColShock##c.logManre ib8.ProvId `control`x'', vce(cluster DistId)
estimates store ColShockManresa_`x'
}


	* Table C.3
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 dum_Landowners" ///
			"Prov FE = *.ProvId" "Year FE = *.Year", labels("Yes" "No"))  ///
			interaction(" × ") wrap nonumber ///
			mgroups("All" "1901--07" "1909--13" "1914--17" "1918--23", pattern(1 1 1 1 ))


** APPENDIX D: PRE-TREATMENT DIFFERENCES **
preserve

clear all
* Reload full data
use "C:\Users\Usuari\OneDrive - Istituto Universitario Europeo\Courses\2nd Term\Challenges to Democracy\Replication-SH2023\Week 9\vallprat2022\CPS_ESMRES_Dataset-Panel.dta"

*  Generate indicator for 20th century
gen segle20 = 1 
replace segle20 = 0 if Year<1899

* Keep a single observation per constituency (1 before Colonial Shock and 1 after colonial shock)
collapse TurnoutDist ColShock TurnoDist1898 perNoDyn1898 logind logManre BCN, by(Dist ElectionType segle20)
label var ColShock "Colonial Shock"
label var logManre "(log) Identity Mobilization"

	* Colonial Shock vs. Pacific Turn <1898
	reg TurnoDist1898 ColShock if segle20==1
		loc r2 : di %9.2f e(r2)*100   //Get the R-square with two decimal multiplied by 100
		loc t =  _b[ColShock]/_se[ColShock]  // get the t-statistic
		loc p : di %9.3f 2*ttail(e(df_r),abs(`t'))   //Calculate p' value

		* Nice Figure D.1
		twoway scatter TurnoDist1898 ColShock if ElectionType==1 & segle20==1|| scatter TurnoDist1898 ColShock if ElectionType==2 & segle20==1 ||  lfitci TurnoDist1898 ColShock, name(Turno1, replace) ytitle("Pacific Turn Compliance") title("1876-1898") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1)) note("R-square (%): `r2'""p value: `p'", ring(0) pos(2))

	* Colonial Shock vs. Non-Monarchist Candidates
	reg perNoDyn1898 ColShock if segle20==1
		loc r2 : di %9.2f e(r2)*100   //Get the R-square with two decimal multiplied by 100
		loc t =  _b[ColShock]/_se[ColShock]  // get the t-statistic
		loc p : di %9.3f 2*ttail(e(df_r),abs(`t'))   //Calculate p' value

		* Nice Figure D.2
		twoway scatter perNoDyn1898 ColShock if ElectionType==1 & segle20==1 || scatter perNoDyn1898 ColShock if ElectionType==2 & segle20==1 || lfitci perNoDyn1898 ColShock , name(perNoDyn1, replace) ytitle("% Non-Monarchist") title("1876-1898") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1)) note("R-square (%): `r2'""p value: `p'", ring(0) pos(2))
		
		
	* Colonial Shock vs. Turnout
	reg TurnoutDist ColShock if segle20==0
		loc r2 : di %9.2f e(r2)*100   //Get the R-square with two decimal multiplied by 100
		loc t =  _b[ColShock]/_se[ColShock]  // get the t-statistic
		loc p : di %9.3f 2*ttail(e(df_r),abs(`t'))   //Calculate p' value

		* Nice Figure D.3
		twoway scatter TurnoutDist ColShock if segle20==0 & ElectionType==1, mcolor(black) || scatter TurnoutDist ColShock if segle20==0 & ElectionType==2, mcolor(gray) || lfitci TurnoutDist ColShock if segle20==0 , name(PartS19, replace) ytitle("Turnout") title("1890-1898") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1)) note("R-square (%): `r2'" "p value: `p'", ring(0) pos(2))
		
		
		
	* Identity Mobilization vs. Pacific Turn <1898
	reg TurnoDist1898 logManre if segle20==1
		loc r2 : di %9.2f e(r2)*100   //Get the R-square with two decimal multiplied by 100
		loc t =  _b[logManre]/_se[logManre]  // get the t-statistic
		loc p : di %9.3f 2*ttail(e(df_r),abs(`t'))   //Calculate p' value

		* Nice Figure D.4
		twoway scatter TurnoDist1898 logManre if ElectionType==1 & segle20==1 || scatter TurnoDist1898 logManre if ElectionType==2 & segle20==1 ||  lfitci TurnoDist1898 logManre, name(Turno_Manresa1, replace) ytitle("Pacific Turn Compliance") title("1876-1898") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1)) note("R-square (%): `r2'""p value: `p'", ring(0) pos(2))

		
	* Identity Mobilization 1892 vs. Colonial Shock 1898
	reg logManre ColShock if segle20==1
		loc r2 : di %9.2f e(r2)*100   //Get the R-square with two decimal multiplied by 100
		loc t =  _b[ColShock]/_se[ColShock]  // get the t-statistic
		loc p : di %9.3f 2*ttail(e(df_r),abs(`t'))   //Calculate p' value

		* Nice Figure D.5
		twoway scatter logManre ColShock if ElectionType==1 & segle20==1 || scatter logManre ColShock if ElectionType==2 & segle20==1 || lfitci logManre ColShock , name(Manresa_CS, replace) ytitle("(log) Identity Mobilization") legend(order (1 "Legislative" 2 "Provincial" 3 "95% CI") r(1)) note("R-square (%): `r2'""p value: `p'", ring(0) pos(2))
	

restore


** APPENDIX E: ROBUSTNESS CHECKS **

** Bivariate association **
eststo clear

eststo: logit Lliga c.ColShock , robust
eststo: logit Lliga c.ColShock ib8.ProvId , robust
eststo: logit Lliga c.ColShock c.timetrend , robust
eststo: logit Lliga c.ColShock c.timetrend ib8.ProvId , robust

	* Nice Table E.1
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) noconstant indicate( ///
		"Prov FE = *.ProvId" "Time Trend = timetrend", labels("Yes" "No")) 



** Validity Heterogeneous Effects **
* ssc install interflex // if necessary

* Figure E.1.a
interflex Lliga ColShock logManre MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners timetrend, fe(ProvId) vce(cluster) cluster(DistId) ylab("Lliga Candidacy") dlab("Colonial Shock") xlab ("(log) Identity Mobilization") nbins(2) 

* Figure E.1.b	
interflex Lliga ColShock logManre MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners timetrend, fe(ProvId) vce(cluster) cluster(DistId) ylab("Lliga Candidacy") dlab("Colonial Shock") xlab ("(log) Identity Mobilization") nbins(3) 
	
* Figure E.1.c
interflex Lliga ColShock logManre MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners timetrend, fe(ProvId) vce(cluster) cluster(DistId) ylab("Lliga Candidacy") dlab("Colonial Shock") xlab ("(log) Identity Mobilization") nbins(4) 

	

** Robustness: Excluding City of BCN **

eststo clear

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners

eststo clear
eststo: logit Lliga c.ColShock logManre `control' if BCN==0, vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre c.timetrend `control' if BCN==0, vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre c.timetrend ib8.ProvId `control' if BCN==0, vce(cluster DistId)
eststo: logit Lliga c.ColShock##c.logManre c.timetrend ib8.ProvId `control' if BCN==0, vce(cluster DistId) 
	
	* Nice Table E.2
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop dum_Landowners Peasants1918 logind" ///
			"Time Trend = timetrend" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock logManre c.ColShock#c.logManre) ///
			interaction(" × ") wrap 

** Robustness: Excluding Constituencies in Tarragona Province **

eststo clear

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind if ProvId!=43 // 

eststo clear
eststo: logit Lliga c.ColShock logManre `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre i.Year `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre i.Year ib8.ProvId `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock##c.logManre i.Year ib8.ProvId `control', vce(cluster DistId) 
	
	* Nice Table E.3
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"\midrule Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 logind" ///
			"Year FE = *.Year" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock logManre c.ColShock#c.logManre) ///
			interaction(" × ") wrap


			
** Robustness: Alternative Identity Mobilization Measurement **
eststo clear

label var _Manresa "# Manresa Delegates"

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners if BCN==0 // The Barcelona Capital has extreme values
local mob _Manresa 

eststo clear
eststo: logit Lliga c.ColShock _Manresa `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock _Manresa i.Year `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock _Manresa i.Year ib8.ProvId `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock##c._Manresa i.Year ib8.ProvId `control', vce(cluster DistId) // c.dum_Landowners
	
	* Nice Table E.4
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 logind dum_Landowners" ///
			"Year FE = *.Year" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock _Manresa c.ColShock#c._Manresa) ///
			interaction(" × ") wrap
		

* b) Manresa percent municipalities with delegates
eststo clear

label var _ManresaPercent "% Localities Delegates"

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners
local mob _ManresaPercent 

eststo clear
eststo: logit Lliga c.ColShock _ManresaPercent `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock _ManresaPercent i.Year `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock _ManresaPercent i.Year ib8.ProvId `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock##c._ManresaPercent i.Year ib8.ProvId `control', vce(cluster DistId) 
	
	* Nice Table E.5
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 logind dum_Landowners" ///
			"Year FE = *.Year" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock _ManresaPercent c.ColShock#c._ManresaPercent) ///
			interaction(" × ") wrap
			
			
* c) Manresa delegates per ten thousand inhabitants
eststo clear

gen Manresa10k = _Manresa/Pop*10000

label var Manresa10k "Delegates/10k People"

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners

eststo clear
eststo: logit Lliga c.ColShock Manresa10k `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock Manresa10k i.Year `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock Manresa10k i.Year ib8.ProvId `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock##c.Manresa10k i.Year ib8.ProvId `control', vce(cluster DistId)

	* Nice Table E.6
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 logind dum_Landowners" ///
			"Year FE = *.Year" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock Manresa10k c.ColShock#c.Manresa10k) ///
			interaction(" × ") wrap 


** Alternative Colonial Shock Measure **

* Generate Variable of exporting Sectors
gen exports =  Per_Textile + Per_Wood + Per_Paper // cotton + wood/cork + paper [Mostly in Barcelona & Girona]
gen nonexports = Per_Ceramics + Per_Metal + Per_Chem + Per_Hemp_Linen + Per_Wool + Per_Silk // Ceramics+Metal+Chemistry+hemp+wool+silk

* Export vs. Non-Export Sectors
	* Figure E.2.a
	interflex Lliga logManre exports nonexports MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga Candidates") dlab("(log) Id. Mobilization") xlab("% Export Oriented Sectors") //note("Sectors: Cotton, Wood/Cork, Paper")

	* Figure E.2.b
	interflex Lliga logManre nonexports exports MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga Candidates") dlab("(log) Id. Mobilization") xlab("% Non-Export Oriented Sectors")
	
* Generate Variable exports in relative terms
gen expnonexp = exports/(exports+nonexports)*100
gen nonexpexp = nonexports/(exports+nonexports)*100
	* Figure E.3.a
	interflex Lliga logManre expnonexp nonexports MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga") dlab("(log) Identity Mobilization") xlab("Exports / (Exports+Non-Exports) × 100")
		
	* Figure E.3.b
	interflex Lliga logManre nonexpexp nonexports MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga") dlab("(log) Identity Mobilization") xlab("Non-Exports / (Exports+Non-Exports) × 100")

	
* Specific Export Sectors
	* Figure E.4.a: Cotton
	interflex Lliga logManre Per_Textile MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga Candidates") dlab("(log) Id. Mobilization") xlab("% Cotton Taxes")

	* Figure E.4.b: Cork
	interflex Lliga logManre Per_Wood Per_Textile MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga Candidates") dlab("(log) Id. Mobilization") xlab("% Cork Taxes")


* Specific Non-Export Sectors
	* Figure E.5.a: Wool
	interflex Lliga logManre Per_Wool Per_Textile MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga Candidates") dlab("(log) Id. Mobilization") xlab("% Wool Taxes")
		
	* Figure E.5.b: Metallurgy
	interflex Lliga logManre Per_Metal Per_Textile MagnitudeDist literacy ByElection foreigners1887 logPop dum_Landowners if ElectionType==1, fe(Year BCN ProvId) vce(cluster) cluster(DistId) n(3) ylab("Lliga Candidates") dlab("(log) Id. Mobilization") xlab("% Metallurgical Taxes")


** Year Fixed-Effects **
eststo clear

local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind 

eststo clear
eststo: logit Lliga c.ColShock logManre `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre i.Year `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock logManre i.Year ib8.ProvId `control' , vce(cluster DistId)
eststo: logit Lliga c.ColShock##c.logManre i.Year ib8.ProvId `control', vce(cluster DistId) // c.dum_Landowners
	
	* Nice Table E.7
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop Peasants1918 logind" ///
			"Year FE = *.Year" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock logManre c.ColShock#c.logManre) ///
			interaction(" × ") wrap 


** Colonial Shock Over Time **
* Lliga Number of Cumulative Candidacies
sort ElectionType Dist Year
bysort ElectionType Dist: gen id =_n

gen LligaCum = Lliga if id==1
	replace LligaCum = Lliga+LligaCum[_n-1] if id>1
	order LligaCum, a(Lliga)
	label var LligaCum "Lliga Cumulative"
	
eststo clear
local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logind LligaCum Manre_dist logPop if BCN==0

logit Lliga c.ColShock##c.timetrend c.dum_Landowners logManre ib8.ProvId `control', vce(cluster DistId)
margins, at(timetrend=(1(2)23)) dydx(ColShock)

	* Nice Figure E.6
	marginsplot, recast(line) plot1opts(lpattern(shortdash)) recastci(rarea) ciopts(lcolor(gs12) color(%30)) yline(0, lp(solid) lc(black)) ///
	addplot(hist timetrend, discrete yaxis(2) yscale(alt lcolor() axis(2)) ylabel(0(.05).5, axis(2)) color(gs11%20) xlabel(1(1)23)) legend(off) ytitle("Average Marginal Effects on the Probability" "of Lliga Candidate Running (95%)") title("")
	
	
** Strategic Decision **
gen LligaWin = Lliga
	replace LligaWin = 0 if Party!=10 // Party is the party of the winning candidate

eststo clear
local control MagnitudeDist literacy ByElection foreigners1887 Peasants1918 logPop Manre_dist logind dum_Landowners if BCN==0

eststo clear
eststo: logit LligaWin c.ColShock logManre `control', vce(cluster DistId)
eststo: logit LligaWin c.ColShock logManre c.timetrend `control', vce(cluster DistId)
eststo: logit LligaWin c.ColShock logManre c.timetrend ib8.ProvId `control', vce(cluster DistId)
eststo: logit LligaWin c.ColShock##c.logManre c.timetrend ib8.ProvId `control', vce(cluster DistId) // c.dum_Landowners
	
	esttab, b(2) se(2) pr2(2) label nomtitles eqlabels(none) ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( ///
			"Controls = Manre_dist logPop MagnitudeDist ByElection foreigners1887 literacy logPop dum_Landowners Peasants1918 logind" ///
			"Time Trend = timetrend" "Prov FE = *.ProvId" , labels("Yes" "No"))  ///
			order (ColShock logManre c.ColShock#c.logManre) ///
			interaction(" × ") wrap


** Alternative Dependent Variable: single observation per constituency **

* Use Cross-Section Dataset
clear all
use "C:\Users\Usuari\OneDrive - Istituto Universitario Europeo\Courses\2nd Term\Challenges to Democracy\Replication-SH2023\Week 9\vallprat2022\CPS_ESMRES_Dataset-Cross-Section.dta"

* Cross-Section Analysis: Colonial Shock
eststo clear
local control logPop Manre_dist ib8.ProvId
local mob c._Manresa 

eststo: reg Lliga c.ColShock, robust
eststo: reg Lliga c.ColShock c.IdMob `control' , robust
eststo: reg Lliga c.ColShock##c.IdMob `control', robust
eststo: reg Lliga c.ColShock##c.IdMob `control' if ElectionType==1, robust
eststo: reg Lliga c.ColShock##c.IdMob `control' if ElectionType==2, robust

	* Nice Table E.9
	esttab, b(2) se(2) ar2(2) label nomtitles ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate( "Controls = logPop Manre_dist" /// 
			"Province FE = *.ProvId", labels("Yes" "No"))  ///
			order (ColShock IdMob c.ColShock#c.IdMob) ///
			interaction(" × ") wrap ///
			mgroups("All Districts" "Legislative" "Provincial", ///
								 pattern(1 0 0 1 1)) 


* Correlation Cotton Looms 1861 & 1931 (Figure E.7)
twoway lfitci cotton_looms1861 cotton_looms1931 if ElectionType==1 & Constituency!="Barcelona" || scatter cotton_looms1861 cotton_looms1931 if ElectionType==1 & Constituency!="Barcelona", mlabel(Constituency) xlabel(0(2000)12000) ytitle("Cotton looms 1861") xtitle("Cotton looms 1931") m(i) mlabp(0) legend(off)



* Alternative Pre-1900 industrialization indicators
eststo clear 
local control logPop Manre_dist c.dum_Landowners ib8.ProvId i.BCN 

eststo: reg Lliga c.WCotton, robust
eststo: reg Lliga c.WCotton c.IdMob `control' i.BCN, robust
eststo: reg Lliga c.cotton_tel_per, robust
eststo: reg Lliga c.cotton_tel_per c.IdMob `control' i.BCN, robust
eststo: reg Lliga c.WoolFactories, robust 
eststo: reg Lliga c.WoolFactories c.IdMob `control' i.BCN, robust
eststo: reg Lliga c.looms1900_per, robust
eststo: reg Lliga c.looms1900_per c.IdMob `control' i.BCN, robust
eststo: reg Lliga c.spindles1900_per, robust
eststo: reg Lliga c.spindles1900_per c.IdMob `control' i.BCN, robust

	* Nice Table E.10			
	esttab, b(2) se(2) ar2(2) label nomtitles ///
			star(* 0.10 ** 0.05 *** 0.01) nobase indicate("Identity = IdMob" /// 
			"Controls = logPop Manre_dist dum_Landowners" /// 
			"Province FE = *.ProvId" "Barcelona dummy = *.BCN", labels("Yes" "No"))  ///
			order (WCotton cotton_tel_per WoolFactories looms1900_per spindles1900_per) ///
			interaction(" × ") wrap ///
			mgroups("1861" "1900", pattern(1 0 0 0 0 0 1 0 0 0))


		