
/*=============================================================================
					DESCRIPTION OF PROGRAM
					------------------------

DESCRIPTION:	Create "issues" for each interview. Issues are of three types:
				those that warrant rejection, those that are comments (to be
				posted to rejected interviews), and those that are SuSo
				validation errors. Issues are used in the reject/review/approve
				decision.

DEPENDENCIES:	createSimpleIssue.do, createComplexIssue.do

INPUTS:			

OUTPUTS:		

SIDE EFFECTS:	

AUTHOR: 		Arthur Shaw, jshaw@worldbank.org
=============================================================================*/

/*=============================================================================
LOAD DATA FRAME AND HELPER FUNCTIONS
=============================================================================*/

/*-----------------------------------------------------------------------------
Initialise issues data frame
-----------------------------------------------------------------------------*/

clear
capture erase "`issuesPath'"
gen interview__id = ""
gen interview__key = ""
gen issueType = .
label define types 1 "Critical error" 2 "Comment" 3 "SuSo validation error" 4 "Needs review"
label values issueType types
gen issueDesc = ""
gen issueComment = ""
gen issueLoc = ""
gen issueVars = ""
save "`issuesPath'", replace


/*-----------------------------------------------------------------------------
Load helper functions
-----------------------------------------------------------------------------*/

include "`progDir'/helper/createSimpleIssue.do"

include "`progDir'/helper/createComplexIssue.do"

/*=============================================================================
CRITICAL ISSUES
=============================================================================*/

/*-----------------------------------------------------------------------------
Interview result
-----------------------------------------------------------------------------*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
attempts in conflict with result
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

local suffixes "no_answer invalid_number unreachable"

#delim;

local descs `"
"no answer"
"number invalid"
"phone off/unreachable"
"';

#delim cr

local nitems: word count `suffixes'

forvalues i = 1/`nitems' {

	local suffix: word `i' of `suffixes'
	local desc: word `i' of `descs'

	local resultProbComm = "ERROR: the interview result conflicts with the call attempt details. " + ///
		"The result is `desc', but in the call attempts, " + ///
		"one successfully reached someone."	

	local descTxt = "result " + "`desc' " + "but able to reach"

	createComplexIssue , ///
		attributesFile(`attributesPath') ///
		issuesFile(`issuesPath') ///
		whichAttributes(result_`suffix' num_attempts num_attempts_noreach) ///
		issueCondit(result_`suffix' == 1 & (num_attempts > num_attempts_noreach)) ///
		issueType(1) ///
		issueDesc("`descTxt'") ///
		issueComm("`resultProbComm'")

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
possible to continue with unused numbers or for interviewer-provided other reasons
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

forvalues i = 1/`nitems' {

	local suffix: word `i' of `suffixes'
	local desc: word `i' of `descs'

	local continueComm = "ERROR: the interview result is `desc', " + ///
		"but there are numbers that have not been tried or it is otherwise " + ///
		"possible de continue the interview according to the interview result. " + ///
		"Please try to reach the household by all means possibles."

	local descTxt = "Result " + "`desc' " + "but can continue"

	createComplexIssue , ///
		attributesFile(`attributesPath') ///
		issuesFile(`issuesPath') ///
		whichAttributes(result_`suffix' num_numbers_provided num_numbers_tried can_continue) ///
		issueCondit( ///
			result_`suffix' == 1 & ///
			((num_numbers_provided > num_numbers_tried) | ///
			(can_continue == 1)) ///
		) ///
		issueType(1) ///
		issueDesc("`descTxt'") ///
		issueComm("`continueComm'")

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
partially complete but possible to complete
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

local continueComm2 = "ERROR: the interview result is partially completed, " + ///
	"but that section also says that it may be possible to continue the interview . " + ///
	"Please try to complete the interview or correct the interview result."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(result_partial num_numbers_provided num_numbers_tried can_continue) ///
	issueCondit(result_partial == 1 & can_continue == 1) ///
	issueType(1) ///
	issueDesc("Partially complete interview, but possible to complete") ///
	issueComm("`continueComm2'")

/*-----------------------------------------------------------------------------
Income-generating activities
-----------------------------------------------------------------------------*/
/* 
* respondent works in NFE, but no hhold NFE reported
local bizComm = "ERROR: Le répondant travaille dans une entreprise familiale, " + ///
	"mais aucune entreprise familiale n'est déclarée. " + ///
	"Veuillez vérifier l'activité du répondant et l'activité des membres du ménage dans la section 5."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(work_biz_mem work_biz_hh result_complete) ///
	issueCondit(result_complete == 1 & work_biz_mem == 1 & work_biz_hh == 0) ///
	issueType(1) ///
	issueDesc("Répondant travaille dans une entreprise, mais aucune entreprise déclarée") ///
	issueComm("`bizComm'")

* respondent works in family ag, but no family ag reported
local agComm = "ERROR: Le répondant travaille dans une l'agriculture ou l'élevage familial, " + ///
	"mais activité agricule n'est déclarée. " + ///
	"Veuillez vérifier l'activité du répondant et l'activité des membres du ménage dans la section 5."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(work_ag_mem work_ag_hh result_complete) ///
	issueCondit(result_complete == 1 & work_ag_mem == 1 & work_ag_hh == 0) ///
	issueType(1) ///
	issueDesc("Répondant travaille dans l'agriculture, mais aucune activité agricole déclarée") ///
	issueComm("`agComm'")

* family ag is income source, but no family ag reported
local agComm = "ERROR: Le ménage tire une partie de son revenu de l'agriculture familiale, " + ///
	"mais aucune activité agricole n'est déclarée. " + ///
	"Veuillez vérifier les sources de revenu dans la section 8 " + ///
	"et les activités dans la section 5."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(income_ag work_ag_hh result_complete) ///
	issueCondit(result_complete == 1 & work_ag_hh == 1 & income_ag == 0) ///
	issueType(1) ///
	issueDesc("Ménage subsiste de l'activité agricole, mais aucune activité agricole déclarée") ///
	issueComm("`agComm'")

* NFE is income source, but no NFE reported
local bizComm = "ERROR: Le ménage exploite une entreprise familiale depuis le début de la crise " + ///
	"mais ne déclare pas cette entreprise comme source de revenu. " + ///
	"Veuillez vérifier les activités dans la section 5" + ///
	"et les sources de revenu dans la section 8."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(income_biz work_biz_hh result_complete) ///
	issueCondit(result_complete == 1 & work_biz_hh == 1 & income_biz == 0) ///
	issueType(1) ///
	issueDesc("Ménage subsiste de l'activité commerciale, mais aucune entreprise déclarée") ///
	issueComm("`bizComm'") 
*/
/*-----------------------------------------------------------------------------
Members
-----------------------------------------------------------------------------*/

* more than 1 head
local moreHeadComm = "ERROR: There is more than 1 household head. " + ///
	"Check the relationship of all members marked as head."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(num_heads result_complete) ///
	issueCondit(result_complete == 1 & num_heads > 1) ///
	issueType(1) ///
	issueDesc("More than 1 head") ///
	issueComm("`moreHeadComm'")

* no head
local noHeadComm = "ERROR: NO household head. " + ///
	"Check the relationship of all members."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(num_heads result_complete) ///
	issueCondit(result_complete == 1 & num_heads == 0) ///
	issueType(1) ///
	issueDesc("No head") ///
	issueComm("noHeadComm")

* no original members
local noStillMember = "ERROR: No members were part of the original household. " + ///
	"Confirm that the current household does not contain any original members " + ///
	"from LFS."

createComplexIssue , ///
	attributesFile(`attributesPath') ///
	issuesFile(`issuesPath') ///
	whichAttributes(num_still_members result_complete) ///
	issueCondit(result_complete == 1 & num_still_members == 0) ///
	issueType(1) ///
	issueDesc("No original member") ///
	issueComm("noStillMember")
