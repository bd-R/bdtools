#'
#'
#' @title Function for taxonomic validation
#'
#' @name bd_taxonomic_validation
#' @param df (data.frame) contains a data frame with species data
#' @return (data.frame) returns a data frame containing filtered data based on user choice
#' @example 
#' key <- name_backbone(name='Puma concolor')$speciesKey
#' dat <- occ_search(taxonKey=key, return='data', limit=1200)
#' df<-bd_taxonomic_validation(dat) #Enter the taxonomic level and value, kingdom-Animalia 
#' 
#' key <- name_backbone(name='Trochilidae')$speciesKey
#' dat <- occ_search(taxonKey=key, return='data', limit=1200)
#' df<-bd_taxonomic_validation(dat) #Enter taxonomic level and value, order-Carnivora
#' 
#' 
#' key <- name_backbone(name='Delphinids')$speciesKey
#' dat <- occ_search(taxonKey=key, return='data', limit=1200)
#' df<-bd_taxonomic_validation(dat) #Enter taxonomic level and value, phylum-chordata
#' 


#---------- Function Start----------


bd_taxonomic_validation<-function(df=NULL){
  
  choice<-c("kingdom","phylum","class","order","family","genus")
  index<-menu(choice,title = "Choose the taxonomic level for validating the data")
  tax_lvl<- choice[index]
  
  tax_ch<-readline(prompt="Enter the value of taxonomic level chosen above")
  
  if(tax_lvl %in% names(df)){
    
    df1<-apply(df,2,tolower)
    out<-as.integer(df1[,tax_lvl]==tolower(tax_ch))
    out[is.na(out)]<-0
    per<-(sum(out)/nrow(dat))*100
    mis<-as.double(nrow(df)-sum(out))
    
    cat("The number of records with mismatch with given taxonomic level and value are:-",mis,"\n")
    cat("The percentage of matched records with given taxonomic level and value are :- ",per,"\n")
    
    choice1<-c("Yes","No")
    index1<-menu(choice1,title="Do you want to filter the data based on the taxonomic level chosen ?")
    if(index1==1){
      df2<-df
      df2<-cbind(df2,data.frame(out=out))
      df2<-subset(df2,out==1)
      return(df2[,1:(ncol(df2)-1)])
    }else{
      return(df)
    }
    
  }else{
    
    warning("The entered taxonomic level is not present in data")
    return(df)
    
  }
  
}

#----------Function End----------
