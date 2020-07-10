/*=============================================================================
					DESCRIPTION OF PROGRAM
					------------------------

DESCRIPTION:	Compile "attributes" for each interview. These "attributes"
				are indicators or counts that are used in either reports
				or the reject/review/approve decision--or both.

DEPENDENCIES:	createAttribute.do

INPUTS:			

OUTPUTS:		

SIDE EFFECTS:	

AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
=============================================================================*/

/*=============================================================================
LOAD DATA FRAME AND HELPER FUNCTIONS
=============================================================================*/

/*-----------------------------------------------------------------------------
Initialise attributes data set
-----------------------------------------------------------------------------*/

clear
capture erase "`attributesPath'"
gen interview__id = ""
gen interview__key = ""
gen attribName = ""
gen attribVal = .
gen attribVars = ""
order interview__id interview__key attribName attribVal attribVars
save "`attributesPath'", replace

/*-----------------------------------------------------------------------------
Load helper functions
-----------------------------------------------------------------------------*/

include "`progDir'/helper/createAttribute.do"

/*=============================================================================
IDENTIFY CASES TO REVIEW
=============================================================================*/

use "`constructedDir'/casesToReview.dta", clear
tempfile casesToReview
save "`casesToReview'"

/*=============================================================================
CREATE ATTRIBUTES
=============================================================================*/

/*-----------------------------------------------------------------------------
Call attempts
-----------------------------------------------------------------------------*/

use "`rawDir'/attempts.dta", clear

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
number call atempts made
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

createAttribute using "`attributesPath'", ///
	countWhere(!mi(attempts__id) & !mi(number_called)) ///
	byGroup(interview__id interview__key) ///
	attribName(num_attempts) ///
	attribVars(attempts__id)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
number of numbers tried
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

use "`rawDir'/attempts.dta", clear

drop if mi(number_called)
duplicates drop interview__id interview__key number_called, force

createAttribute using "`attributesPath'", ///
	countWhere(!mi(number_called)) ///
	byGroup(interview__id interview__key) ///
	attribName(num_numbers_tried) ///
	attribVars(number_called)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
member ever answered
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

use "`rawDir'/attempts.dta", clear

createAttribute using "`attributesPath'", ///
	anyWhere(`talk_var' == 1) ///
	byGroup(interview__id interview__key) ///
	attribName(member_ever_answered) ///
	attribVars(`talk_var')

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
numbers attempts where unreachable
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

createAttribute using "`attributesPath'", ///
	countWhere( ///
		inlist(`answered_var', ///
			.a, /// no answer provided
			2, /// no answer
			3, /// invalid
			4 /// unreachable
		) | /// 
		( ///
			`answered_var' == 1 & /// answered
			`talk_var' == 2  /// but not member
			& inlist(get_hh_contact, 1, 2) /// and do not know household or won't visit them
		) ///
		) ///
	byGroup(interview__id interview__key) ///
	attribName(num_attempts_noreach) ///
	attribVars(`answered_var'|`talk_var'|get_hh_contact)

/*-----------------------------------------------------------------------------
Numbers
-----------------------------------------------------------------------------*/

use "`rawDir'/numeros.dta", clear

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
number of numbers
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

createAttribute using "`attributesPath'", ///
	countWhere(!mi(numbers__id)) ///
	byGroup(interview__id interview__key) ///
	attribName(num_numbers_provided) ///
	attribVars(number_list)

* TODO: don't count preferred number in issues
* so numbers attempted could be >= numbers provided if subtract preferred number that may or may not have been used

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
number added
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

gen new_number = (preload_number == "##N/A##")

createAttribute using "`attributesPath'", ///
	anyWhere(new_number == 1) ///
	byGroup(interview__id interview__key) ///
	attribName(number_added) ///
	attribVars(number_list)

/*-----------------------------------------------------------------------------
Household
-----------------------------------------------------------------------------*/

use "`rawDir'/`hholds'", clear

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
interview result
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

local suffixes "complete partial language_unknown no_answer invalid_number unreachable dont_know_hh wont_get_hh"
local values "1 2 4 5 6 7 8 9"

local num_pairs: word count `suffixes'

forvalues i = 1/`num_pairs' {

	local suffix: word `i' of `suffixes'
	local value: word `i' of `values'

	createAttribute using "`attributesPath'", ///
		genAttrib(`interview_result' == `value') ///
		attribName(result_`suffix') ///
		attribVars(`interview_result')

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
possible to continue
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

createAttribute using "`attributesPath'", ///
	genAttrib(`interview_continue' == 1) ///
	attribName(can_continue) ///
	attribVars(`interview_continue')

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
employment
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* 
* respondent works in hhold enterprise
createAttribute using "`attributesPath'", ///
	genAttrib(s06q05 == 2) /// TODO: see whether agree to include 1, although creates LOTS of cases
	attribName(work_biz_mem) ///
	attribVars(s06q05)

* hhold works in family enterprise
createAttribute using "`attributesPath'", ///
	genAttrib(s06q10 == 1) ///
	attribName(work_biz_hh) ///
	attribVars(s06q10)

* respondent works in family ag
createAttribute using "`attributesPath'", ///
	genAttrib(s06q05 == 3) ///
	attribName(work_ag_mem) ///
	attribVars(s06q05)

* hhold works in ag
createAttribute using "`attributesPath'", ///
	genAttrib(s06q14 == 1) ///
	attribName(work_ag_hh) ///
	attribVars(s06q14) 
*/
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
income
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
/* 
* hhold enterprise is income source
createAttribute using "`attributesPath'", ///
	genAttrib(s08q01__2 == 1) ///
	attribName(income_biz) ///
	attribVars(s08q01)

* family farm is income source
createAttribute using "`attributesPath'", ///
	genAttrib(s08q01__1 == 1) ///
	attribName(income_ag) ///
	attribVars(s08q01)
*/
/*-----------------------------------------------------------------------------
Member
-----------------------------------------------------------------------------*/

use "`rawDir'/membres.dta", clear

* number of household heads
createAttribute using "`attributesPath'", ///
	countWhere(s02q07 == 1) ///
	byGroup(interview__id interview__key) ///
	attribName(num_heads) ///
	attribVars(s02q07)

* total number of original members
createAttribute using "`attributesPath'", ///
	countWhere(!mi(preload_pid)) ///
	byGroup(interview__id interview__key) ///
	attribName(num_preloaded_members) ///
	attribVars(preload_pid)

* current number of original members
createAttribute using "`attributesPath'", ///
	countWhere(!mi(preload_pid) & s02q03 == 1) ///
	byGroup(interview__id interview__key) ///
	attribName(num_still_members) ///
	attribVars(s02q03)


