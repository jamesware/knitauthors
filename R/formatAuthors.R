#' format authors

#' @param authorTable Input data. A data.frame with columns entitled "Name" and "Affiliation", and upto 5 optional columns labelled "info1" .. "info5". An example is included in the package (exampleInput).  Expects 1 row for each unique author / affiliation pair (i.e. 2 rows for an author with 2 affiliations).  Rows should be ordered to reflect intended authorship order.
#' @param infoSymbol[n] the superscript symbol chosen to represent each supplementary designation.  Note that some symbols may require additional escape characters for correct interpretation by markdown.
#' @param infoText[n] description of supplementary designation (e.g. Corresponding author)
#'   
#' @return Returns markdown-formatted text
#' @export formatAuthors
#' @import dplyr
#'   
#' @examples
#' # display internal example input format
#' exampleInput
#' 
#' # number author affiliations
#' formatAuthors(exampleInput)
#' 
#' 
#' 
formatAuthors <- function(authorTable,
                          infoSymbol1="\\*",
                          infoSymbol2="†",
                          infoSymbol3="§",
                          infoSymbol4="‡",
                          infoSymbol5="#",
                          infoText1="These authors contributed equally to this work",
                          infoText2="These authors contributed equally to this work",
                          infoText3="Corresponding author",
                          infoText4="Did not contribute much",
                          infoText5="Can't read or write"
){
  # authorTable <- exampleInput #for line-by-line testing
  authors <- unique(authorTable$Name)
  affiliations <- unique(authorTable$Affiliation)
  
  Name_output <- NULL
  
  for (i in authors) {
    # i <- authors[1]  # for testing line by line
    test <- dplyr::filter(authorTable, Name == i)
    test <- unlist(dplyr::select_(test,"Affiliation"))
    matched_affiliations <- sapply(affiliations,
                                   function(x) {
                                     if (x %in% test) {1} else {0}
                                   }
    )
    matched_affiliations <- which(matched_affiliations == TRUE)
    affiliation_numbers <- paste(matched_affiliations, collapse=",")
    
    # Add additional designations (info1 .. info5)
    for (j in 1:5){
      if(ncol(authorTable) >= j+2
         & 
           sum(authorTable[authorTable$Name==i,j+2],na.rm=T)>0
      ){
        affiliation_numbers <-  paste(affiliation_numbers,
                                      c(infoSymbol1,infoSymbol2,infoSymbol3,infoSymbol4,infoSymbol5)[j],
                                      sep=",")
      }
    }
    
    
    Name_output_add <- paste(i, "^", affiliation_numbers, "^", sep="")
    Name_output <- append(Name_output, Name_output_add)
  }
  
  affiliations_list <- NULL
  n <- 1
  for (i in affiliations) {
    affiliations_add <- paste("^", n, "^", i, sep="")
    affiliations_list <- append(affiliations_list, affiliations_add)
    n <- n + 1
  }
  
  # Concatenate output
  cat(Name_output, sep=",")
  cat("\n\n")
  cat(affiliations_list, sep="  \n")
  cat("\n")
  
  # Other designations
  if(ncol(authorTable) > 2){
    for (j in seq(1,ncol(authorTable)-2)){
      cat("^",c(infoSymbol1,infoSymbol2,infoSymbol3,infoSymbol4,infoSymbol5)[j],"^",
          c(infoText1,infoText2,infoText3,infoText4,infoText5)[j],
          "  \n",sep="")
    }
  }
  
}