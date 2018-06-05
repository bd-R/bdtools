#'
#' @title Function to get synonyms of the species.
#'
#' @name bd_synonym
#' @param name (string) scientfic name of the species.
#' @return returns a data.frame which contains synonyms and other related taxonomic info.
#' @example 
#' df<-bd_synonym("Chironomus riparius") #Enter the data base from the choices, data base supported are 'itis','col','eol','tropicos','nbn'
#' 
#' df<-bd_synonym("Balaenoptera musculus") #Marine species example "worms data base"
#' 
#' df<-bd_synonym("Gadus morhua") #With marine i.e. worms database this will return synonym and with other databases it will return error
#' 
#' df<-bd_synonym("Aquila chrysaetos") #"nbn" data base example
#'
#' df<-bd_synonym("Puma concolor") #"itis", "col" database example

# ----------Function Start---------- 

bd_synonym<-function(name=NULL){
  
  db1<- readline(prompt="Enter The data base from where you want synonym, 
                 below are the choices (for marine species use worms as the data base)
                 1) 'itis'
                 2) 'col'
                 3) 'tropicos'
                 4) 'nbn'
                 5) 'worms' ")
  if(db1=="itis"|| db1=="col" || db1=="tropicos" || db1=="nbn"){
    
    id<-taxize::get_ids(names=name, db=db1)
    attr<-lapply(id, attributes)
    if((attr[[1]]$match)=="found"){
      syn<-taxize::synonyms(id,db=db1)
      df<-as.data.frame(syn[[1]][[1]])
      if(nrow(df)==0){
        
        stop("No match for synonym with the data base, please change the data base selected")
        
      }else{
        
        return(df)
      }
      
    }else{
      
      stop("There is no match for synonym or incorrect scientific name")
      
    }
    
  }else if(db1=="worms"){
    
    syn<-synonyms(name,db="worms")
    df<-as.data.frame(syn[[1]])
    if(nrow(df)==0){
      
      stop("There is no match for synonym with worms data base")
      
    }else{
      
      return(df)
      
    }
    
  }else{
    
    stop("The entered data base is incorrect, please select from 'itis','col','tropicos','nbn','worms' ")
  }
  
  
}

# ----------Function End---------- 