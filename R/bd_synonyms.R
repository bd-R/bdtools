#'
#' @title Function to get synonyms of the species.
#'
#' @name bd_synonym
#' @param name (string) scientfic name of the species.
#' @return returns a data.frame which contains synonyms and other related taxonomic info.
#' @example 
#' df<-bd_synonym("Chironomus riparius") #Enter the data base from the choices, data base supported are 'itis','col','eol','tropicos','nbn','worms'
#' 
#'
#'
#'

# ----------Function Start---------- 

bd_synonym<-function(name=NULL){
  
  db1<- readline(prompt="Enter The data base from where you want synonym, supported data base are 'itis','col','eol','tropicos','nbn','worms' ")
  if(db1=="itis"|| db1=="col" || db1=="eol" || db1=="tropicos" || db1=="worms" || db1=="nbn"){
    
    id<-taxize::get_ids(names=name, db=db1)
    attr<-lapply(id, attributes)
    if((attr[[1]]$match)=="found"){
      syn<-taxize::synonyms(id,db=db1)
      return(as.data.frame(syn[[1]][[1]]))
      
      
    }else{
      
      stop("There is no match for synonym or incorrect scientific name")
      
    }
    
  }else{
    
    stop("The entered data base is incorrect, please select from 'itis','col','eol','tropicos','nbn','worms' ")
  }
  
  
}

# ----------Function End---------- 