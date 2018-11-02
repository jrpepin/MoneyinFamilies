// Set the project directory folder where data and do files are saved
global projdir 	= 	"D:/Dropbox/@Dissertation/Survey/Code/MoneyinFamilies" /* Home */
* global projdir 	= 	"\\prc-cs-f9dkb42\jpepin$\MoneyinFamilies" /* Campus */

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
// Table 3 -- Statistical Analysis of Marital Status, Parental Status, & Relationship Duration
********************************************************************************************
// Install net install st0085_2.pkg if not already installed

// Use robust option to match outcome of svy output. Can't use svy and get BIC, hausman, or lr
// AKA Appendix Table A
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.mar i.par i.dur i.relinc  $ivars, robust baseoutcome(1)

esttab  m1 using FamilyIncome--table3.csv, cells(b(star fmt(2)) se(par fmt(2))) ///
		varlabels(_cons Constant) legend label stats(N) stardetach replace
estat 		ic
mlogtest, 	combine
*mlogtest, 	hausman			/* Can't be run with survey weights or the robust option */
*mlogtest, 	hausman base	/* Can't be run with survey weights or the robust option */
mlogtest, 	wald
*mlogtest, 	lr				/* Can't be run with survey weights or the robust option */

// This code doesn't run all the way through. Must run m1 after each test
********************************************************************************************

global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.mar i.par i.dur i.relinc  $ivars, robust baseoutcome(1)


//Marital status
margins i.mar									, predict(outcome(1)) 	/* Shared 	*/
margins i.mar									, predict(outcome(1)) 	 coeflegend post
				test (_b[1bn.mar]=_b[2.mar])
margins i.mar									, predict(outcome(2))	/* Both	  	*/ 
margins i.mar									, predict(outcome(2))	coeflegend post
				test (_b[1bn.mar]=_b[2.mar])
margins i.mar									, predict(outcome(3))	/* Separate	*/
margins i.mar									, predict(outcome(3))	coeflegend post
				test (_b[1bn.mar]=_b[2.mar])
//Parental status
margins i.par									, predict(outcome(1)) 	/* Shared 	*/
margins i.par									, predict(outcome(1)) 	coeflegend post
				test (_b[1bn.par]=_b[2.par])
margins i.par									, predict(outcome(2))	/* Both	  	*/
margins i.par									, predict(outcome(2))	coeflegend post
				test (_b[1bn.par]=_b[2.par])
margins i.par									, predict(outcome(3))	/* Separate	*/
margins i.par									, predict(outcome(3))	coeflegend post
				test (_b[1bn.par]=_b[2.par])
//Relationshp duration
margins i.dur									, predict(outcome(1)) 	/* Shared 	*/
margins i.dur									, predict(outcome(1)) 	coeflegend post
				test (_b[1bn.dur]=_b[2.dur])
margins i.dur									, predict(outcome(2))	/* Both	  	*/
margins i.dur									, predict(outcome(2))	coeflegend post
				test (_b[1bn.dur]=_b[2.dur])
margins i.dur									, predict(outcome(3))	/* Separate	*/
margins i.dur									, predict(outcome(3))	coeflegend post
				test (_b[1bn.dur]=_b[2.dur])
*/

********************************************************************************************
// Table 4 -- Marital Status Interactions
********************************************************************************************
			
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
// Figure 2
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"

* SHARED
mlogit 		organize 	i.marpar 	i.dur i.relinc  $ivars, robust baseoutcome(1)
margins 	i.marpar									, predict(outcome(1)) /* Get CI */
margins 	i.marpar									, predict(outcome(1)) coeflegend post
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)	

*BOTH
mlogit 		organize 	i.marpar 	i.dur i.relinc  $ivars, robust baseoutcome(1)
margins 	i.marpar									, predict(outcome(3)) /* Get CI */
margins 	i.marpar									, predict(outcome(3)) coeflegend post
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)

* SEPARATE
mlogit 		organize 	i.marpar 	i.dur i.relinc  $ivars, robust baseoutcome(1)
margins 	i.marpar									, predict(outcome(2)) /* Get CI */
margins 	i.marpar									, predict(outcome(2)) coeflegend post
			test (1bn.marpar=2.marpar)
			test (1bn.marpar=3.marpar)
			test (1bn.marpar=4.marpar)
			test (2.marpar=3.marpar)
			test (2.marpar=4.marpar)
			test (3.marpar=4.marpar)

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


// Figure 3
mlogit 		organize 	i.mardur 	i.par i.relinc  $ivars, robust baseoutcome(1)

* SHARED
// use "coeflegend post" option to see what Stata calls each variable
mlogit 		organize 	i.mardur 	i.par i.relinc  $ivars, robust baseoutcome(1)
margins 	i.mardur									, predict(outcome(1)) /* get CI */
margins 	i.mardur									, predict(outcome(1)) coeflegend post
			test (1bn.mardur=2.mardur)
			test (1bn.mardur=3.mardur)
			test (1bn.mardur=4.mardur)
			test (2.mardur=3.mardur)
			test (2.mardur=4.mardur)
			test (3.mardur=4.mardur)
			
*BOTH
mlogit 		organize 	i.mardur 	i.par i.relinc  $ivars, robust baseoutcome(1)
margins 	i.mardur									, predict(outcome(3)) /* get CI */
margins 	i.mardur									, predict(outcome(3)) coeflegend post
			test (1bn.mardur=2.mardur)
			test (1bn.mardur=3.mardur)
			test (1bn.mardur=4.mardur)
			test (2.mardur=3.mardur)
			test (2.mardur=4.mardur)
			test (3.mardur=4.mardur)

* SEPARATE
mlogit 		organize 	i.mardur 	i.par i.relinc  $ivars, robust baseoutcome(1)
margins 	i.mardur									, predict(outcome(2)) /* get CI */
margins 	i.mardur									, predict(outcome(2)) coeflegend post
			test (1bn.mardur=2.mardur)
			test (1bn.mardur=3.mardur)
			test (1bn.mardur=4.mardur)
			test (2.mardur=3.mardur)
			test (2.mardur=4.mardur)
			test (3.mardur=4.mardur)

			
********************************************************************************************
// Table 5 -- Statistical Analysis of Perceptions of Degree of Partial-Pooling
********************************************************************************************
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
			
regress		hiscent 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc
			test (1bn.relinc=3.relinc)		/*MB to EE*/
			test (2.relinc=3.relinc)		/*FB to EE*/
			test (1bn.relinc=2.relinc)		/*MB to FB*/
			
regress		hercent 	i.marpar	i.relinc	dur	$ivars
margins 	i.relinc
			test (1bn.relinc=3.relinc)		/*MB to EE*/
			test (2.relinc=3.relinc)		/*FB to EE*/
			test (1bn.relinc=2.relinc)		/*MB to FB*/

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

// Table 3 comparisons within categories
/*
global 		ivars "female rcohab nevmar altmar child whitedum lths bach employ incdum age"
eststo m1: 	mlogit 		organize 	i.mar i.par i.dur i.relinc  $ivars, robust baseoutcome(1)

// comparisons within marital status
	predict pcohab1 if mar==1, outcome(1)
	predict pcohab2 if mar==1, outcome(2)
	predict pcohab3 if mar==1, outcome(3)
	ttest pcohab1 == pcohab2
	ttest pcohab1 == pcohab3
	ttest pcohab2 == pcohab3
	
	predict pmar1 if mar==2, outcome(1)
	predict pmar2 if mar==2, outcome(2)
	predict pmar3 if mar==2, outcome(3)
	ttest pmar1 == pmar2
	ttest pmar1 == pmar3
	ttest pmar2 == pmar3
	
// comparisons within parental status
	predict nokid1 if par==1, outcome(1)
	predict nokid2 if par==1, outcome(2)
	predict nokid3 if par==1, outcome(3)
	ttest nokid1 == nokid2
	ttest nokid1 == nokid3
	ttest nokid2 == nokid3
	
	predict kid1 if par==2, outcome(1)
	predict kid2 if par==2, outcome(2)
	predict kid3 if par==2, outcome(3)
	ttest kid1 == kid2
	ttest kid1 == kid3
	ttest kid2 == kid3
	
// comparisons within relationship duration
	predict three1 if dur==1, outcome(1)
	predict three2 if dur==1, outcome(2)
	predict three3 if dur==1, outcome(3)
	ttest three1 == three2
	ttest three1 == three3
	ttest three2 == three3
	
	predict sev1 if dur==2, outcome(1)
	predict sev2 if dur==2, outcome(2)
	predict sev3 if dur==2, outcome(3)
	ttest sev1 == sev2
	ttest sev1 == sev3
	ttest sev2 == sev3
 */

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

