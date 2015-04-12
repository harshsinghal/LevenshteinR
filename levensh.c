

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <Rdefines.h>

SEXP levensh(SEXP srvec,SEXP trvec)

{

	// Input two vectors of words/strings
	
    PROTECT(srvec = AS_CHARACTER(srvec));
	PROTECT(trvec = AS_CHARACTER(trvec));
	
	
	
	// Length of the vectors 
	int srlen = length(srvec);
	int trlen = length(trvec);
	
	
	// Create the resultvector to be returned back to R.
	SEXP resultvector = R_NilValue;
	PROTECT(resultvector=allocVector(INTSXP,srlen*trlen));

	int resultlistctr = 0;
	
	// Integers to iterate over the vector entities
	int srveci;
	int trveci;

	for(srveci = 0; srveci < srlen; srveci++)  // Loop over each of the source words
	{
	
		for(trveci = 0; trveci < trlen;trveci++) // Loop over each of the target words
		{
			
				
				  char *sr;  // String pointer to each entity in the char vector of source words
				  char *tr;  // String pointer to each entity in the char vector of target words
			
				// Extract the source word and the target word
				SEXP srword = R_NilValue;
				SEXP trword = R_NilValue;

				srword = STRING_ELT(srvec,srveci);
				trword = STRING_ELT(trvec,trveci);


				sr = R_alloc(strlen(CHAR(STRING_ELT(srvec, srveci))), sizeof(char));
				tr = R_alloc(strlen(CHAR(STRING_ELT(trvec, trveci))), sizeof(char));

				 strcpy(sr, CHAR(STRING_ELT(srvec, srveci)));
				 strcpy(tr, CHAR(STRING_ELT(trvec, trveci)));
				 
				// Get length of each word

				int srwordlen = strlen(sr);
				int trwordlen = strlen(tr);

				int srmatrows = srwordlen + 1;
				int trmatcols = trwordlen + 1;
				
				
				// Create matrix for levenshtein distance computation
				SEXP u = R_NilValue;	
				PROTECT(u = allocMatrix(INTSXP, srmatrows, trmatcols)); 
					
				// First column values
				int i;
				for (i = 0; i < srmatrows; i++) {
				INTEGER(u)[i] = i;
				}

				// First row values
				for (i = 1; i < trmatcols; i++) {
				INTEGER(u)[i*srmatrows] = i;
				}


				// Iterate over the sr word and the tr word.
				int srl,trl,cost;
				
				int aboveval, leftval, diagaboveval, minval;

				for(srl=0;srl < srwordlen;srl++)
				{
					for(trl=0;trl < trwordlen;trl++)
						{
					
						//if(strcmp(*sr,*tr) == 0)
						if(*(sr + srl) == *(tr + trl))
								 cost = 0;
						else
								 cost = 1;

						
						aboveval = INTEGER(u)[((srl+1) + ((trl+1)*srmatrows))-1] + 1;
						leftval = INTEGER(u)[(srl+1) + ((trl)*srmatrows)] + 1;
						diagaboveval = INTEGER(u)[(srl) + ((trl)*srmatrows)] + cost;
						
						if(aboveval >= leftval)
								minval = leftval;
						else 
								minval = aboveval;
						
						if(minval >= diagaboveval)
								minval = diagaboveval;
						
						INTEGER(u)[(srl+1) + ((trl+1)*srmatrows)] = minval;
		
						}
   				}


		INTEGER(resultvector)[resultlistctr] = INTEGER(u)[(trmatcols*srmatrows)-1];
		UNPROTECT(1);
		
		resultlistctr++;

		} // Loop on each word in the target vector

	} // Loop on each word in the source vector


    UNPROTECT(3);	
	return resultvector;
}
