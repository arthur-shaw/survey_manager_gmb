
/*=============================================================================
Process data
=============================================================================*/

/*-----------------------------------------------------------------------------
Identify cases to be reviewed for rejection/approval
-----------------------------------------------------------------------------*/

include "`progDir'/identifyCasesToReview.do"

/*-----------------------------------------------------------------------------
Compile interview attributes
-----------------------------------------------------------------------------*/

include "`progDir'/compileAttributes.do"

/*-----------------------------------------------------------------------------
Compile interview issues
-----------------------------------------------------------------------------*/

include "`progDir'/compileIssues.do"
