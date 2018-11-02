// Data for this analysis can be found at http://www.tessexperiments.org/data/pepin791.html

// Change the project directory and project data to your local settings.
global projdir 	= 	"D:/Dropbox/@Dissertation/Survey/Code"
* global projdir 	= 	"\\prc-cs-f9dkb42\jpepin$\MoneyinFamilies" /* Campus */

// Replace text in quotes with name (and location) of data file
global projdata =	"D:/Dropbox/Data/TESS/TESS3_217_Pepin_Client.dta"
* global projdata =	"C:\Users\jpepin\Dropbox\Data\TESS\TESS3_217_Pepin_Client.dta" /* Campus */
use		"$projdata", clear

********************************************************************************************
// Demographics
********************************************************************************************

rename 		ppage 		age
recode 		ppgender 	(1=0 "Male") (2=1 "Female"), gen (female)
rename		ppeducat	educ
recode		ppwork		(1/2=1 "Full-time") (3/4=2 "Unemployed") (5=3 "Retired")	(6/7=4 "Not in labor force"), gen (work)
recode		ppmarit		(1=1 "Married") (6=2 "Cohabit") (5=3 "Never married") (3=4 "Div/Sep") (4=4) (2=5 "Widowed"), gen (relate)
rename		ppincimp	income
//// Change income in JMF Tables!!
recode		income		(1/12=0 "< than $60,000") (12/19=1 "> than $60,000"), gen (incdum)
recode		ppethm		(1=1 "White") (2=2 "Black") (4=3 "Hispanic") (3=4 "Other") (5=4), gen(race)
recode		REL2		(1=1 "weekplus") (2=2 "Weekly") (3=3 "Monthly") (4=4 "Yearly") (5=4) (6=5 "Never") (-1=6 "Unknown"), gen (religfreq)
recode		REL1		(1=1 "Catholic") (2=2 "Evangelical or Portestant Christian") (13=3 "None") (else=4 "Other Religion"), gen(religion)
rename		PPREG4		region

gen			altmar=0
replace		altmar=1 if ppmarit==2 | ppmarit==3 | ppmarit==4

gen			child=0
replace		child=1 if PPT01>=1 | PPT25>=1 | PPT612>=1 | PPT1317>=1

gen			hsdum=0
replace		hsdum=1 if educ==2 | educ==3

gen			whitedum=0
replace		whitedum=1 if race==1

gen			employ=0
replace		employ=1 if work==1

tab educ, 		gen(e1)
tab work, 		gen(w1)
tab	relate,		gen(r1)
tab race,		gen(b1)
tab religfreq,	gen(c1)
tab religion,	gen(f1)
tab	region,		gen(d1)

rename			e11		lths
rename			e12		hs
rename			e13		somecol
rename			e14		bach
rename			w11		fulltime
rename			w12		unemp
rename			w13		retired
rename			w14		nilf
rename			r11		rmar
rename			r12		rcohab
rename			r13		nevmar
rename			r14		divsep
rename			r15		widow
rename			b11		white
rename			b12		black
rename			b13		hisp 
rename			b14		other
rename			c11		weekplus
rename			c12		weekly
rename			c13		monthly
rename			c14		yearly
rename			c15		never
rename			c16		unknrel
rename			f11		catholic
rename			f12		christian
rename			f13		none
rename			f14		otherrel
rename			d11		northeast
rename			d12		midwest
rename			d13		south
rename			d14		west

********************************************************************************************
// Vignette Conditions
********************************************************************************************
// Install http://fmwww.bc.edu/repec/bocode/v/vreverse.hlp if not already installed

*Married and Parent Condition
vreverse DOV_RELSTAT, gen(mar)
rename 		DOV_PARENTST 	parent

egen 		marpar = group(mar parent)
label 		define marparlbl 1 "Cohabit/Nonparent" 2 "Cohabit/Parent" 3 "Married/Nonparent" 4 "Married/Parent"
label		values marpar marparlbl

tab 		marpar, gen(mp)
rename 		mp1 cnk
rename 		mp2 cp
rename 		mp3 mnk
rename 		mp4 mp

*Breadwinner Condition
cap drop	relinc
gen			relinc=.
replace		relinc=1 if DOV_EARNING==1
replace		relinc=2 if DOV_EARNING==3
replace		relinc=3 if DOV_EARNING==2

label define relinclbl 1 "Male Breadwinner" 2 "Equal Earners" 3 "Female Breadwinner"
label values relinc relinclbl


	// breadwinner dums
	cap drop	femearn
	gen			femearn=.
	replace		femearn=0 if relinc==1
	replace		femearn=1 if relinc==2
	
	cap drop	equalearn
	gen			equalearn=.
	replace		equalearn=0 if relinc==1
	replace		equalearn=1 if relinc==3
	
	cap drop	femequal
	gen			femequal=.
	replace		femequal=0 if relinc==2
	replace		femequal=1 if relinc==3


*Relation Duration
rename 		DOV_RELDUR		dur

*Married and Duration Condition
cap drop 	mardur
egen		mardur	= group(mar dur)
label		define mardurlbl 1 "Cohab/3 years" 2 "Cohab/7 years" 3 "Married/3 years" 4 "Married/7 years"
label		values mardur mardurlbl

********************************************************************************************
// Dependent Variables
********************************************************************************************
cap drop	organize
gen			organize=.
replace		organize=1 if B01==1
replace		organize=2 if B01==3
replace		organize=3 if B01==2

label define 	organizelbl 		1 "Shared" 2 "Both" 3 "Separate" 
label values 	organize 			organizelbl

tab organize, gen(o1)
rename	o11 shared
rename	o12 both
rename	o13 separate

rename	B02_Shared 		herjoint
rename	B03_Shared		hisjoint
rename	B02_Individual	herindv
rename	B03_Individual	hisindv

replace	herjoint=. 	if	herjoint==-1
replace	hisjoint=. 	if	hisjoint==-1
replace	herindv=. 	if	herindv==-1
replace	hisindv=. 	if	hisindv==-1


edit 	CaseID DOV_B02_MaxValue herjoint herindv DOV_B03_MaxValue hisjoint hisindv ///
		if (herindv==. | hisindv==. ) & orga==3

drop 	if CaseID==902 |  CaseID==1962

		
replace herjoint=0 	if 	herjoint==. & orga==3
replace	hisindv=0 	if	hisindv==.	& orga==3

count


cap drop	jointtot
gen			jointtot=.
replace		jointtot= herjoint + hisjoint

edit 	CaseID DOV_B02_MaxValue herjoint herindv DOV_B03_MaxValue hisjoint hisindv ///
		if (jointtot==. | herindv==. | hisindv==. ) & orga==3
		
drop if CaseID==1013

count

gen	jointall=jointtot
replace jointall=4000 if jointtot==.

// Percentages
cap drop	herperj
gen			herperj=.
replace		herperj=herjoint/1200 if relinc==1 & orga==3
replace		herperj=herjoint/2800 if relinc==2 & orga==3
replace		herperj=herjoint/2000 if relinc==3 & orga==3

cap drop	herperi
gen			herperi=.
replace		herperi=herindv/1200 if relinc==1 & orga==3
replace		herperi=herindv/2800 if relinc==2 & orga==3
replace		herperi=herindv/2000 if relinc==3 & orga==3

cap drop	hisperj
gen			hisperj=.
replace		hisperj=hisjoint/2800 if relinc==1 & orga==3
replace		hisperj=hisjoint/1200 if relinc==2 & orga==3
replace		hisperj=hisjoint/2000 if relinc==3 & orga==3

cap drop	hisperi
gen			hisperi=.
replace		hisperi=hisindv/2800 if relinc==1 & orga==3
replace		hisperi=hisindv/1200 if relinc==2 & orga==3
replace		hisperi=hisindv/2000 if relinc==3 & orga==3

cap drop	jointper
gen			jointper=.
replace		jointper=(herjoint + hisjoint)/4000 	if orga==3

cap drop	alljoinper
gen			alljoinper=jointper
replace		alljoinper=1 if orga==1
replace		alljoinper=0 if orga==2

cap drop	hiscent
gen			hiscent=.
replace		hiscent=hisindv/4000 if orga==3

cap drop	allhis
gen			allhis=hiscent
replace		allhis=0 				if orga==1
replace		allhis=.5				if orga==2 & relinc==3
replace		allhis=.7				if orga==2 & relinc==1
replace		allhis=.3				if orga==2 & relinc==2



cap drop	hercent
gen			hercent=.
replace		hercent=herindv/4000 	if orga==3

cap drop	allher
gen			allher=hercent
replace		allher=0 				if orga==1
replace		allher=.5				if orga==2 & relinc==3
replace		allher=.7				if orga==2 & relinc==2
replace		allher=.3				if orga==2 & relinc==1
