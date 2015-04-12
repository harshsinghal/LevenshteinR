# ****** CONVENIENCE FUNCTION TO CREATE  A VECTOR OF WORDS *************# 

MakeWordsVector<- function(veclen,wordlenmin,wordlenmax)
{
	wordlist <- NULL
	for(i in 1:veclen)
	{
		wordeach <- paste(letters[sample(1:26,sample(floor(runif(1,wordlenmin,wordlenmax)),1,replace=TRUE),replace=TRUE)],collapse="")
		wordlist <- c(wordlist,wordeach)
	}
	
	return(as.character(wordlist))
}
	
# ************** X-X-X-X ************************************



 
#************** LEVENSHTEIN FUNCTION WRAPPER   **************

#******************** FUNCTION DEFINITION *******************

# FUNCTION PARAMETERS ARE:
# 1) wordvec1 : A character vector of strings/words 
# 2) wordvec2 : A character vector of strings/words 
# 3) columanr : A logical (TRUE/FALSE). If TRUE a columnar represenation is returned.
#										If FALSE a matrix representation is returned.
#
# 4) cutoff: A numeric value representing levenshteing distance
# 5) minmax: Either "min" or "max". If "min", only those pairs of words that have Levenshtein
#									distance greater than cutoff will be returned. 
#									If "max" only those pairs of words that have Levenshtein
#									distance less than the cutoff will be reutrned.
#
# NOTE:	The cutoff and minmax parameter are relevant when columnar is TRUE
# NOTE: wordvec1 and wordvec2 need not be of the same lengths

Levenshtein <- function(wordvec1,wordvec2,columnar,cutoff,minmax){
	
	
	### Provide a summary of Vector lengths and other parameters ###
	cat("\nLevenshtein Distance Computation\n")
	cat(paste("Number of Words in Word Vector 1 = ",length(wordvec1),sep=""))
	cat(paste("\n","Number of Words in Word Vector 2 = ",length(wordvec2),sep=""))
	
	
	
	cat("\nComputing....")
	
	
	#	Call the dll 
	dyn.load("levensh.dll")
	
	# Run the dll
	# timeobj object contains the time taken information
	# resultobj is an integer vector length being length(wordvec1)*length(wordvec2)
	timeobj<-system.time(resultobj <- .Call("levensh",as.character(wordvec1),as.character(wordvec2)))
	
	# Unload the dll
	dyn.unload("levensh.dll")
	
	# Print time taken information
	
	cat("\n\nTime Taken for Computation is \n")
	print(timeobj)
	
	
	#**************** COLUMNAR REPRESENTATION *********************#
	if(columnar == TRUE)
	{
		dfwords<- expand.grid(wordvec2,wordvec1)
		dfindices <- expand.grid(1:length(wordvec2),1:length(wordvec1))
		
		columnardf <- data.frame(wordvec1.words=dfwords[,2],wordvec1.indices=dfindices[,2],
				wordvec2.words=dfwords[,1],wordvec2.indices=dfindices[,1],levenshtein.dist=resultobj,stringsAsFactors=FALSE)
		
		if(tolower(minmax)== "min")
			{
				cat(paste("\nReturned: Columnar Representation for those Word pairs with Minimum Levenshtein Distance = ",cutoff,"\n",sep="")) 	
				columnardf <- columnardf[columnardf[,5] >= cutoff,]	
					if(nrow(columnardf)!=0)
						{
							columnardf$minmax <- "min"
						}
					else
						stop("***All Levenshtein distances are below the cutoff value. No object returned.***")
			}
		
		else
			{
				cat(paste("\nReturned: Columnar Representation for those Word pairs with Maximum Levenshtein Distance = ",cutoff,"\n",sep="")) 	
				columnardf <- columnardf[columnardf[,5] <= cutoff,]	
				
				if(nrow(columnardf)!=0)
				{
					columnardf$minmax <- "max"
				}
				else
					stop("***All Levenshtein distances are above the cutoff value. No object returned.***")
				
				
			}
		
		columnardf[,1] <- as.character(columnardf[,1])
		columnardf[,3] <- as.character(columnardf[,3])
		cat("\n ** Structure of Returned object ** \n")
		str(columnardf)
		
		return(columnardf)
	}
	

	
	#**************** MATRIX REPRESENTATION *********************#
	if(columnar == FALSE)
		{
			cat(paste("\nReturned: Matrix Representation of Levenshtein Distances","\n",sep=""))
			columnmatrix <- matrix(resultobj,nrow=length(wordvec1),ncol=length(wordvec2),byrow=TRUE)
		
			row.names(columnmatrix) <- wordvec1
			colnames(columnmatrix) <- wordvec2
			
			cat("\n** Structure of Returned object ** \n")
			str(columnmatrix)
			
			return(columnmatrix)
		}
}

#************************** X-X-X-X ************************



#************* USE CASE EXAMPLE ************** 
# Create two word vectors

wordvec1 <- MakeWordsVector(100,10,20)  # A vector of 500 words, wordlengths varying from 10 to 20
wordvec2 <- MakeWordsVector(2000,10,15) # A vector of 800 words, wordlngths varying from 100 to 150

# Call the Levenshtein wrapper function
resultobjret<-Levenshtein(wordvec1,wordvec2,columnar=TRUE,cutoff=4,minmax="min")

