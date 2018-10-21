// Set the project directory folder where data and do files are saved
global projdir 	= 	"D:/Dropbox/@Dissertation/Survey/Code"

// Code variables and create analytic sample
cd 		"$projdir"
do		"$projdir/MoneyinFamilies_Measures.do"

// Missing data
tab 	organ, m		/* Drop people who didn't answer the primary question of interest 	*/
drop if organ==.		/* 31 observations dropped */

*******************************************************************
// Minimal Detectable Effect
// Orga
	//Marst
power twomeans .29, 	power(0.8) n(3986) sd(.45) //shared
power twomeans .23, 	power(0.8) n(3986) sd(.42) //sep
power twomeans .48, 	power(0.8) n(3986) sd(.50) //both
	//Parent
power twomeans .36, 	power(0.8) n(3986) sd(.48) //shared
power twomeans .20, 	power(0.8) n(3986) sd(.40) //sep
power twomeans .44, 	power(0.8) n(3986) sd(.50) //both
	//Rel. Earning
power twomeans .40, 	power(0.8) n(3986) sd(.49) //shared
power twomeans .17, 	power(0.8) n(3986) sd(.37) //sep
power twomeans .43, 	power(0.8) n(3986) sd(.50) //both
	

// Distributuion
	//Marst
power twomeans .21, 	power(0.8) n(1784) sd(.12) //His
power twomeans .23, 	power(0.8) n(1784) sd(.14) //Her
power twomeans .56, 	power(0.8) n(1784) sd(.18) //Joint
	//Parent
power twomeans .21, 	power(0.8) n(1784) sd(.13) //His
power twomeans .23, 	power(0.8) n(1784) sd(.13) //Her
power twomeans .56, 	power(0.8) n(1784) sd(.19) //Joint
	//Rel. Earning
power twomeans .28, 	power(0.8) n(1784) sd(.15) //His
power twomeans .13, 	power(0.8) n(1784) sd(.06) //Her
power twomeans .59, 	power(0.8) n(1784) sd(.19) //Joint
*******************************************************************

********************************************************************************************
// Table 2 -- Demographic Characteristics (unweighted)
********************************************************************************************
tabstat		female		rmar		rcohab		nevmar		altmar		child				///
			whitedum	lths		hsdum		bach		employ		incdum		age		///
			, stat(mean sd) col(stat)


********************************************************************************************
// Table 3 -- Statistical Analysis of Perceptions of Money Organization
********************************************************************************************
// Be careful here because the relative earning coefficients come out in the opposite order 
// as presented in the tables.

// Marital status, duration, parental status, relative earnings
// Use robust option to match outcome of svy output. Can't use svy and get BIC, hausman, or lr
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.mar i.dur i.par i.relinc  $ivars, robust baseoutcome(1)

esttab  m1 using FamilyIncome--table3.csv, cells(b(star fmt(2)) se(par fmt(2))) ///
		varlabels(_cons Constant) legend label stats(N) stardetach replace
estat 		ic
mlogtest, 	combine
*mlogtest, 	hausman			/* Can't be run with survey weights or the robust option */
*mlogtest, 	hausman base	/* Can't be run with survey weights or the robust option */
mlogtest, 	wald
*mlogtest, 	lr				/* Can't be run with survey weights or the robust option */

// Figure for presentation (not in paper)
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.mar i.dur i.par i.relinc  $ivars, robust baseoutcome(1)

margins i.mar									, predict(outcome(1)) 	/* Shared 	*/
margins i.mar									, predict(outcome(3))	/* Both	  	*/
margins i.mar									, predict(outcome(2))	/* Separate	*/

margins i.dur									, predict(outcome(1)) 	/* Shared 	*/
margins i.dur									, predict(outcome(3))	/* Both	  	*/
margins i.dur									, predict(outcome(2))	/* Separate	*/


margins i.par									, predict(outcome(1)) 	/* Shared 	*/
margins i.par									, predict(outcome(3))	/* Both	  	*/
margins i.par									, predict(outcome(2))	/* Separate	*/

********************************************************************************************
// Table 4 -- Marital Status Interactions
********************************************************************************************
// Be careful here because the relative earning coefficients come out in the opposite order 
// as presented in the tables.

// Marital status * Duration
********************************************************************************************
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.mardur 		i.par i.relinc  $ivars, robust baseoutcome(1)

esttab  m1 using FamilyIncome--table4a.csv, cells(b(star fmt(2)) se(par fmt(2))) ///
		varlabels(_cons Constant) legend label stats(N) stardetach replace
estat 		ic
mlogtest, 	combine
*mlogtest, 	hausman			/* Can't be run with survey weights or the robust option */
*mlogtest, 	hausman base	/* Can't be run with survey weights or the robust option */
mlogtest, 	wald
*mlogtest, 	lr				/* Can't be run with survey weights or the robust option */

// change reference group for comparisons
			mlogit 		organize 	ib(2).mardur 	i.par i.relinc  $ivars, robust baseoutcome(1) /* Cohab parents */
			mlogit 		organize 	ib(3).mardur 	i.par i.relinc  $ivars, robust baseoutcome(1) /* Married nonparents */


// Figure 2
mlogit 		organize 	i.mardur 	i.par i.relinc  $ivars, robust baseoutcome(1)

* SHARED
// use "coeflegend post" option to see what Stata calls each variable
margins 	i.mardur									, predict(outcome(1)) /* coeflegend post */
			test (1bn.mardur=2.mardur)
			test (1bn.mardur=3.mardur)
			test (1bn.mardur=4.mardur)
			test (2.mardur=3.mardur)
			test (2.mardur=4.mardur)
			test (3.mardur=4.mardur)
			
			nlcom _b[1bn.mardur]
			nlcom _b[2.mardur]
			nlcom _b[3.mardur]
			nlcom _b[4.mardur]
			

*BOTH
margins 	i.mardur									, predict(outcome(3)) /* coeflegend post */
			test (1bn.mardur=2.mardur)
			test (1bn.mardur=3.mardur)
			test (1bn.mardur=4.mardur)
			test (2.mardur=3.mardur)
			test (2.mardur=4.mardur)
			test (3.mardur=4.mardur)
			
			nlcom _b[1bn.mardur]
			nlcom _b[2.mardur]
			nlcom _b[3.mardur]
			nlcom _b[4.mardur]


* SEPARATE
margins 	i.mardur									, predict(outcome(2)) /* coeflegend post */
			test (1bn.mardur=2.mardur)
			test (1bn.mardur=3.mardur)
			test (1bn.mardur=4.mardur)
			test (2.mardur=3.mardur)
			test (2.mardur=4.mardur)
			test (3.mardur=4.mardur)
			
			nlcom _b[1bn.mardur]
			nlcom _b[2.mardur]
			nlcom _b[3.mardur]
			nlcom _b[4.mardur]

// Marital status * Parental Status
********************************************************************************************
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.marpar i.dur i.relinc  $ivars, robust baseoutcome(1)

esttab  m1 using FamilyIncome--table4b.csv, cells(b(star fmt(2)) se(par fmt(2))) ///
		varlabels(_cons Constant) legend label stats(N) stardetach replace
estat 		ic
mlogtest, 	combine
*mlogtest, 	hausman			/* Can't be run with survey weights or the robust option */
*mlogtest, 	hausman base	/* Can't be run with survey weights or the robust option */
mlogtest, 	wald
*mlogtest, 	lr				/* Can't be run with survey weights or the robust option */

// change reference group for comparisons
			mlogit 		organize 	ib(2).marpar i.dur i.relinc  $ivars, robust baseoutcome(1) /* Cohab parents */
			mlogit 		organize 	ib(3).marpar i.dur i.relinc  $ivars, robust baseoutcome(1) /* Married nonparents */

********************************************************************************************


********************************************************************************************
// Figure 3
mlogit 		organize 	i.marpar 	i.dur i.relinc  $ivars, robust baseoutcome(1)

* SHARED
margins 	i.marpar									, predict(outcome(1)) coeflegend post
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)
			
			nlcom _b[1bn.marpar]
			nlcom _b[2.marpar]
			nlcom _b[3.marpar]
			nlcom _b[4.marpar]
			

*BOTH
margins 	i.marpar									, predict(outcome(3)) coeflegend post
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)
			
			nlcom _b[1bn.marpar]
			nlcom _b[2.marpar]
			nlcom _b[3.marpar]
			nlcom _b[4.marpar]


* SEPARATE
margins 	i.marpar									, predict(outcome(2)) coeflegend post
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)
			
			nlcom _b[1bn.marpar]
			nlcom _b[2.marpar]
			nlcom _b[3.marpar]
			nlcom _b[4.marpar]


********************************************************************************************
// Table 5 -- Statistical Analysis of Perceptions of Degree of Partial-Pooling
********************************************************************************************

// Be careful here because the relative earning coefficients come out in the opposite order 
// as presented in the tables.

global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	regress		jointper 	i.relinc	mar par			dur	$ivars
estimates 	store 		jointper
eststo m2: 	regress		hiscent 	i.relinc	mar par			dur	$ivars
estimates 	store 		hiscent4a
eststo m3: 	regress		hercent 	i.relinc	mar par			dur	$ivars
estimates 	store 		hercent4a

esttab  m1 m2 m3 using FamilyIncome--table5.csv, cells(b(star fmt(2)) se(par fmt(2))) ///
		varlabels(_cons Constant) legend label stats(N) stardetach replace
		
/// Combine regression outputs
suest 		 hiscent4a hercent4a

/* Test significance between his and her % by Mar/Par 		
	Also see cnk estimates									*/
test 		[hiscent4a_mean]cp	=[hercent4a_mean]cp					/* Cohab/Parents 		*/
test 		[hiscent4a_mean]mnk	=[hercent4a_mean]mnk				/* Married/Non parents 	*/
test 		[hiscent4a_mean]mp	=[hercent4a_mean]mp					/* Married/Parents		*/

/* Test significance between his and her percentages 		*/
test 		[hiscent4a_mean]1.relinc	=[hercent4a_mean]3.relinc	/* Male breadwinner/His vs Female Breadwinner/Her		*/

/* Need the cnk estimates for MAR/Par Comparisons			*/
	svy:		regress		hiscent 	cnk mnk mp			$ivars	
	estimates 	store 		hiscent4b
	svy:		regress		hercent 	cnk mnk mp			$ivars
	estimates 	store 		hercent4b

	/// Combine regression outputs
	suest		 hiscent4b hercent4b
	test 		[hiscent4b_mean]cnk	=[hercent4b_mean]cnk				/*Cohab/Non Parents		*/

// Change reference group for Table marpar comparisons
eststo m1: 	regress		jointper 	cnk mnk mp	i.relinc	dur	$ivars
eststo m1: 	regress		jointper 	cp  cnk mp	i.relinc	dur	$ivars

eststo m2: 	regress		hiscent 	cnk mnk mp	i.relinc	dur	$ivars
eststo m2: 	regress		hiscent 	cp  cnk mp	i.relinc	dur	$ivars

eststo m3: 	regress		hercent 	cnk mnk mp	i.relinc	dur	$ivars
eststo m3: 	regress		hercent 	cp  cnk mp	i.relinc	dur	$ivars
********************************************************************************************


********************************************************************************************
//Figure 4
// Use the Bonferroni adjustment means take alpha level ( p = .05) and divide it by number of 
// pairwise comparisons (3) == .05/3 == .016. Thus, p value needs to be below .016.

global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"

regress		jointper 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc
			test (1bn.relinc=3.relinc)		/*MB to EE*/
			test (2.relinc=3.relinc)		/*FB to EE*/
			test (1bn.relinc=2.relinc)		/*MB to FB*/
			
			nlcom _b[1bn.relinc]
			nlcom _b[2.relinc]
			nlcom _b[3.relinc]

			
regress		hiscent 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc
			test (1bn.relinc=3.relinc)		/*MB to EE*/
			test (2.relinc=3.relinc)		/*FB to EE*/
			test (1bn.relinc=2.relinc)		/*MB to FB*/

			nlcom _b[1bn.relinc]
			nlcom _b[2.relinc]
			nlcom _b[3.relinc]

			
regress		hercent 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc
			test (1bn.relinc=3.relinc)		/*MB to EE*/
			test (2.relinc=3.relinc)		/*FB to EE*/
			test (1bn.relinc=2.relinc)		/*MB to FB*/
			
			nlcom _b[1bn.relinc]
			nlcom _b[2.relinc]
			nlcom _b[3.relinc]

//Appendix Figure
/*
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"

regress		jointper 	i.marpar	i.relinc	dur	$ivars
margins 	i.marpar
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)

			nlcom _b[1bn.marpar]
			nlcom _b[2.marpar]
			nlcom _b[3.marpar]
			nlcom _b[4.marpar]

			
regress		hiscent 	i.marpar	i.relinc	dur	$ivars
margins 	i.marpar
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)

			nlcom _b[1bn.marpar]
			nlcom _b[2.marpar]
			nlcom _b[3.marpar]
			nlcom _b[4.marpar]

			
regress		hercent 	i.marpar	i.relinc	dur	$ivars
margins 	i.marpar
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)

			nlcom _b[1bn.marpar]
			nlcom _b[2.marpar]
			nlcom _b[3.marpar]
			nlcom _b[4.marpar]
*/
********************************************************************************************
// Paper Notes
********************************************************************************************
// Number of respondents per condition
tab marpar relinc

// Do gendered relative earnings predict the type of distribution?
	// No
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
mlogit 		organize 	i.relinc	$ivars	, baseoutcome(1)

// Interaction effect of marpar and relitive earnings on Organizational Strategy
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
mlogit 		organize 	i.marpar##i.relinc		$ivars	,  baseoutcome(1)
margins		marpar#relinc, predict(outcome(3)) coeflegend post

// Interaction effect of marpar and relitive earnings on Organizational Strategy
regress		hercent 	i.marpar##i.relinc	dur	$ivars
margins 	marpar#relinc,											coeflegend post

regress		hiscent 	i.marpar##i.relinc	dur	$ivars
margins 	marpar#relinc,											coeflegend post

********************************************************************************************
// Robustness check of the partial-pooling conclusions
********************************************************************************************
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"


regress		alljoinper 	i.marpar	i.relinc	dur	$ivars
margins 	i.marpar,											coeflegend post

regress		allhis 		i.marpar	i.relinc	dur	$ivars
margins 	i.marpar,											coeflegend post

regress		allher 		i.marpar	i.relinc	dur	$ivars
margins 	i.marpar,											coeflegend post


regress		alljoinper 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc,											coeflegend post

regress		allhis	 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc,											coeflegend post

regress		allher	 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc,											coeflegend post


/// Interactions
global 		ivars " rcohab nevmar altmar child whitedum lths bach employ incdum age"
regress		hercent 	i.marpar	i.relinc##i.female	dur	$ivars
regress		hiscent 	i.marpar	i.relinc##i.female	dur	$ivars


********************************************************************************************
// Appendix Table A -- Dependent Variables by Vignette Condition (unweighted)
********************************************************************************************
tabstat		shared			separate		both										///
			hiscent			hercent			jointper									///
			, by(mar)		stat(mean sd)	col(stat)

tabstat		shared			separate		both										///
			hiscent			hercent			jointper									///
			, by(parent)	stat(mean sd)	col(stat)

tabstat		shared			separate		both										///
			hiscent			hercent			jointper									///
			, by(relinc)	stat(mean sd)	col(stat)

			
mean shared, over(mar)
lincom 		[shared]_subpop_1 	- [shared]_subpop_2
mean separate, over(mar)
lincom 		[separate]_subpop_1 - [separate]_subpop_2
mean both, over(mar)
lincom 		[both]_subpop_1 	- [both]_subpop_2
mean hiscent, over(mar)
lincom 		[hiscent]_subpop_1 	- [hiscent]_subpop_2
mean hercent, over(mar)
lincom 		[hercent]_subpop_1 	- [hercent]_subpop_2
mean jointper, over(mar)
lincom 		[jointper]_subpop_1 - [jointper]_subpop_2

mean shared, over(par)
lincom 		[shared]_subpop_1 	- [shared]_subpop_2
mean separate, over(par)
lincom 		[separate]_subpop_1 - [separate]_subpop_2
mean both, over(par)
lincom 		[both]_subpop_1 	- [both]_subpop_2
mean hiscent, over(par)
lincom 		[hiscent]_subpop_1 	- [hiscent]_subpop_2
mean hercent, over(par)
lincom 		[hercent]_subpop_1 	- [hercent]_subpop_2
mean jointper, over(par)
lincom 		[jointper]_subpop_1 - [jointper]_subpop_2


mean shared, over(femearn)
lincom 		[shared]0 	- [shared]1
mean separate, over(femearn)
lincom 		[separate]0 - [separate]1
mean both, over(femearn)
lincom 		[both]0 	- [both]1
mean hiscent, over(femearn)
lincom 		[hiscent]0 	- [hiscent]1
mean hercent, over(femearn)
lincom 		[hercent]0 	- [hercent]1
mean jointper, over(femearn)
lincom 		[jointper]0 - [jointper]1

mean shared, over(equalearn)
lincom 		[shared]0 	- [shared]1
mean separate, over(equalearn)
lincom 		[separate]0 - [separate]1
mean both, over(equalearn)
lincom 		[both]0 	- [both]1
mean hiscent, over(equalearn)
lincom 		[hiscent]0 	- [hiscent]1
mean hercent, over(equalearn)
lincom 		[hercent]0 	- [hercent]1
mean jointper, over(equalearn)
lincom 		[jointper]0 - [jointper]1

mean shared, over(femequal)
lincom 		[shared]0 	- [shared]1
mean separate, over(femequal)
lincom 		[separate]0 - [separate]1
mean both, over(femequal)
lincom 		[both]0 	- [both]1
mean hiscent, over(femequal)
lincom 		[hiscent]0 	- [hiscent]1
mean hercent, over(femequal)
lincom 		[hercent]0 	- [hercent]1
mean jointper, over(femequal)
lincom 		[jointper]0 - [jointper]1
