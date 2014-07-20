/*****************************************************************

 NAME:
	ticktock

 FUNCTION:
	timing routine

 SYNTAX:
	float= tick();

 ON INPUT:

 ON OUTPUT:
	tick:	time in seconds		float

 CALLS:
	(clock) getrusage

 COMMENTS:

 LIBRARIES:

 REFERENCES:

 VERSION(S):
	1. original version		j. behrens	6/93
	2. timing with getrusage	j.behrens	6/93

*****************************************************************/

#include <sys/time.h>
#include <sys/resource.h>

struct rusage rusage;

float tick_()
{
	float tmp;

/* take the time from routine getrusage */
	getrusage(RUSAGE_SELF,&rusage);
	tmp= (float)(rusage.ru_utime.tv_sec)+ (float)(rusage.ru_utime.tv_usec) * 1.0e-06;
	return (tmp);

} /* end of ticktock */
