#' format authors

#' @param authorTable the input data
#'   
#' @return Returns markdown-formatted text
#' @export formatAuthors
#' @import dplyr
#'   
#' @examples
#' authorTable <- read.delim("data-raw/authors.tab", sep="\t",)
#' knitauthors::formatAuthors(authorTable)
#' 
#' 
#' 
formatAuthors <- function(authorTable,
                          firstSymbol="\\*",
                          lastSymbol="†",
                          corrSymbol="\\§",
                          coFirstText="These authors contributed equally to this work",
                          coLastText="These authors contributed equally to this work",
                          corrAuthorText="Corresponding author"
){
  # other useful symbols: ‡, #
  
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
    
    # add coFirst designation
    if("coFirst" %in% names(authorTable)){
      if(sum(authorTable$coFirst[authorTable$Name==i],na.rm=T)>0){
        affiliation_numbers <-  paste(affiliation_numbers, firstSymbol, sep=",")
      }
    }
    
    # add coLast designation
    if("coLast" %in% names(authorTable)){
      if(sum(authorTable$coLast[authorTable$Name==i],na.rm=T)>0){
        affiliation_numbers <-  paste(affiliation_numbers, lastSymbol, sep=",")
      }
    }
    
    # add corresponding author designation
    if("corresponding" %in% names(authorTable)){
      if(sum(authorTable$corresponding[authorTable$Name==i],na.rm=T)>0){
        affiliation_numbers <-  paste(affiliation_numbers, corrSymbol, sep=",")
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
  if("coFirst" %in% names(authorTable)){
    cat(firstSymbol,coFirstText,"  \n",sep="")
  }
  if("coLast" %in% names(authorTable)){
    cat(lastSymbol,coLastText,"  \n",sep="")
  }
  if("corresponding" %in% names(authorTable)){
    cat(corrSymbol,corrAuthorText,"  \n",sep="")
  }
}