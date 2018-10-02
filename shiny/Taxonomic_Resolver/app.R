library(shiny)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(plotly)

suppressMessages(library(rgbif))
suppressMessages(library(sp))
suppressMessages(library(spatialEco))
suppressMessages(library(ggmap))
suppressMessages(library(plyr))
suppressMessages(library(htmlTable))
suppressMessages(library(bdvis))
suppressMessages(library(plotly))
suppressMessages(library(pracma))
suppressMessages(library(xtable))
suppressMessages(library(htmlTable))
suppressMessages(library(taxize))


# UI ----

ui <-dashboardPage(
  dashboardHeader(
    title = "Taxonomic_Resolver",
    titleWidth = 350),
  dashboardSidebar(
    width=350,
    
    sidebarMenu(
      menuItem("Data_upload", tabName = "data_upload", icon = icon("dashboard")),
      menuItem("Online_data", tabName ="Online_data", icon = icon("th")
               #menuSubItem("CountryCode", tabName = "CountryCode"),
               #menuSubItem("ClassID", tabName = "ClassID"), 
               #menuSubItem("NumberOfObs", tabName = "NumberOfObs") 
      ),
      menuItem("Taxonomic_Resolver",tabName = "taxonomic_resolver", icon = icon("th")),
      menuItem("Download_Data",tabName = "download_data", icon = icon("th"))
      
      
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    
    tabItems(
      
      tabItem(
        tabName = "data_upload",
        sidebarLayout(
          
          # Sidebar panel for inputs ----
          sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
          ),
          
          # Main panel for displaying outputs ----
          mainPanel(
            
            # Output: Data file ----
            tableOutput("contents")
            
          )
          
        )
      ),
      
      # Second tab content
      
      
      tabItem(tabName = "Online_data",
              
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                  
                  textInput("countryCode",label = h3("CountryCode"), value = "AU",  placeholder = "Enter the country code for example AU for Australia"),
                  
                  tags$hr(),
                  
                  numericInput("classID",label =h3("ClassID"),  value = 359, min = 10, max = 1000),
                  
                  tags$hr(),
                  
                  numericInput("obs",label = h3("NumberOfObs"), value = 5000, min = 100, max = 10000)
                  
                ),
                
                mainPanel = (
                  
                  tableOutput("data_online")
                  
                )
                
              )
      ),
      
      # Third Tab
      
      tabItem(tabName = "taxonomic_resolver",
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(  
                  selectInput("clusterNum2", label = h3("Taxonomic Resolver "),
                              choices = list("YES"="YES", "NO"="NO"),
                              selected = "YES"),
                  
                  selectInput("clusterNum",label = "Select Taxonomic Rank if you want to resolve data", 
                              choices = list( "KINGDOM" = "KINGDOM", "PHYLUM" = "PHYLUM", "CLASS" = "CLASS", "ORDER" = "ORDER","FAMILY" = "FAMILY","GENUS"="GENUS","SPECIES"="SPECIES","SUBSPECIES"="SUBSPECIES"), 
                              selected = "GENUS")
                ),
                
                mainPanel = (
                  
                  
                  tabsetPanel(type = "tabs",
                              tabPanel("Data table",tableOutput("plot1")),
                              tabPanel("Map", plotOutput("plot2"))
                              
                  )
                  
                )
              )
              
      ),
      
      # Fourth Tab
      
      tabItem(tabName = "download_data",
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(  
                  
                  # selectInput("clusterNum3", label = h3("Do you want to filter the data based on taxonomic rank "),
                  #             choices = list("YES"="YES", "NO"="NO"),
                  #             selected = "YES"),
                  # 
                  # selectInput("clusterNum4",label = "Choose a Taxonomic Rank for data filtering and download", 
                  #             choices = list( "KINGDOM" = "KINGDOM", "PHYLUM" = "PHYLUM", "CLASS" = "CLASS", "ORDER" = "ORDER","FAMILY" = "FAMILY","GENUS"="GENUS","SPECIES"="SPECIES","SUBSPECIES"="SUBSPECIES"), 
                  #             selected = "GENUS"),
                  
                  # selectInput("dataset", "Choose a dataset for downloading",
                  #             choices = list("FILTERED"="FILTERED", "ACTUAL"="ACTUAL"),
                  #             selected = "FILTERED"),
                  
                  
                  selectInput("dataset", "Choose a dataset:",
                              choices = c("Resolved_data", "Filtered_data", "Actual_data")),
                  
                  # Button
                  
                  
                  
                  downloadButton("downloadData1", "Download data")
                  
                  
                ),
                mainPanel("Data Download Hub")
                
                
              )
              
      )
      
      
      
      
    )
  )
) 



# Server logic ----
server <- function(input, output) {
  
  
  #colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])
  
  options(shiny.maxRequestSize=30*1024^2)
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  selectCountry<-reactive({
    
    num<-reactive(input$countryCode)
    return(num())
    
  })
  
  selectClassID<-reactive({
    
    num<-reactive(input$classID)
    return(num())
    
  })
  
  selectObservations<-reactive({
    
    num<-reactive(input$obs)
    return(num())
    
  })
  
  
  output$data_online<- renderTable({
    
    
    df1<-occ_data(
      country = selectCountry(),     # Country code for australia
      classKey= selectClassID(),      # Class code for mammalia
      limit=selectObservations(),
      hasCoordinate = T
      
      
    )
    
    dat<-df1$data
    #exclude_missing(dat, .Inf = F)
    #is.na(dat) <- sapply(dat, is.infinite)
    return(dat[,1:5])
    
  })
  
  
  
  
  Data_Online <- reactive({
    
    set.seed(123)
    df1<-occ_data(
      country = selectCountry(),     # Country code for australia
      classKey= selectClassID(),      # Class code for mammalia
      limit=selectObservations(),
      hasCoordinate = T
      
      
    )
    dat<-df1$data
    return(dat)
    
    
  })
  
  
  DATA<-reactive({
    
    infile<-input$file1
    if (is.null(infile)){
      return(NULL)      
    }
    
    req(input$file1)
    
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote,
                   stringsAsFactors = FALSE
    )
    
    
    
    df<-setNames(df, tolower(names(df)))
    
    df<-subset(df, select = c(name,kingdom,genus,phylum,family,order,class,eventdate,month,year,decimallatitude,decimallongitude,taxonrank))
    return(df)
    
  })
  
  
  selectedActualData <- reactive({
    
    X<-DATA()
    if(is.null(X)){
      X<-Data_Online()
    }
    
    return(X)
    
  })
  
  
  selectedData <- reactive({
    
    #X<-subset(X,select=c(eventDate,month,year))
    X<-DATA()
    if(is.null(X)){
      X<-Data_Online()
    }
    X<-setNames(X, tolower(names(X)))
    #X<-data.frame(X)
    #col_num1<-which( colnames(X)=="taxonRank")
    num <- reactive((input$clusterNum))
    data_taxon<-subset(X,X$taxonrank ==num())
    return(data_taxon)
    
  })
  
  selectedColumn <- reactive({
    
    #X<-subset(X,select=c(eventDate,month,year))
    X<-DATA()
    if(is.null(X)){
      X<-Data_Online()
    }
    X<-setNames(X, tolower(names(X)))
    num <- reactive((input$clusterNum))
    #num1<-num()
    #col_num
    col_num<-which( colnames(X)==(tolower(num())))
    return(col_num)
    
  })
  
  
  bd_taxonomic_resolver<-function(df,tax_rank){
    
    
    if(nrow(df)==0){
      return(0)
    }else{
      df_subset<-subset(df,taxonrank==tax_rank)
      df_na<-subset(df,is.na(taxonrank))
      
      if(nrow(df_na)!=0){
        count1<-nrow(df_na)
        count2<-0
        for (i in 1:nrow(df_na)){
          if(!is.na( df_na$name[i])){
            
            #using only two data bases 'itis', 'ncbi' to keep the function simple
            tax_hierarchy_itis <- suppressMessages(
              as.data.frame(taxize::classification(taxize::get_uid(df_na$name[i]), db = "itis")[[1]]))
            tax_hierarchy_ncbi <- suppressMessages(
              as.data.frame(taxize::classification(taxize::get_uid(df_na$name[i]), db = "ncbi")[[1]]))
          }
          
          if(suppressMessages(!is.na(tax_hierarchy_ncbi[[1]]))){
            
            df_na$taxonrank[i]<-tax_hierarchy_ncbi$rank[nrow(tax_hierarchy_ncbi)]
            
          }else if(suppressMessages(!is.na(tax_hierarchy_itis[[1]]))){
            
            df_na$taxonrank[i]<-tax_hierarchy_itis$rank[nrow(tax_hierarchy_itis)]
            
          }else{
            count2=count2+1
            
          }
          
        }
        #Now combine both the data frames to form the final data frame
        
        df_subset<-df_subset[!is.na(df_subset$taxonrank),]
        df_na<-subset(df_na,taxonrank=tax_rank)
        #df_final<-rbind(df_subset,df_na)
        
        #cat("The number of records with missing taxon rank were",count1,"\n")
        #cat("The number of records which are resolved:",count2,"\n")
        if(nrow(df_na)==0){
          return(1)
        }else{
          return(df_na)
        }
      }else{
        
        return(1)
      }
      
    }
    
    
  }
  
  
  data_download_resolver<-reactive({
    
    X=selectedData()
    
    if(input$clusterNum2=="YES"){
      tax_name<-input$clusterNum
      dataFrame<-bd_taxonomic_resolver(X,tax_name)
      if(dataFrame==0){
        #renderText({warning("The data frame is empty")})
        return(NULL)
        
      }else if(dataFrame==1){
        
        df_subset<-subset(X,taxonrank==tax_name)
        return(df_subset)
      }else{
        return(dataFrame)
        
      }
      
    }else{
      
      return(X)
    }
    
  })
  
  
  output$plot1<-renderTable({
    X=selectedData()
    
    if(input$clusterNum2=="YES"){
      tax_name<-input$clusterNum
      dataFrame<-bd_taxonomic_resolver(X,tax_name)
      if(dataFrame==0){
        #renderText({warning("The data frame is empty")})
        return(NULL)
        
      }else if(dataFrame==1){
        
        df_subset<-subset(X,taxonrank==tax_name)
        return(head(df_subset))
      }else{
        return(head(dataFrame))
        
      }
      
    }else{
      
      return(head(X))
    }
    
  })
  
  
  output$plot2<-renderPlot({
    X=selectedData()
    if(input$clusterNum2=="YES"){
      
      tax_name<-input$clusterNum
      dataFrame<-bd_taxonomic_resolver(X,tax_name)
      if(dataFrame==0){
        renderText({warning("The data frame is empty")})
        NULL
        
      }else if(dataFrame==1){
        df_subset<-subset(X,taxonrank==tax_name)
        if(nrow(df_subset)==0){
          print(NULL)
        }else{
          mapgilbert <- get_map(location = c(lon = mean(df_subset$decimallongitude), lat =
                                               mean(df_subset$decimallatitude)), zoom = 4,maptype = "terrain", scale = 2)
          
          map_1<-ggmap(mapgilbert) +
            geom_point(data = df_subset, aes(x = decimallongitude, y = decimallatitude, alpha = 0.5, size =2,     colour="Red" ),shape=20)
          print(map_1)
        }
        
      }else{
        df_subset<-subset(X,taxonrank==tax_name)
        if(nrow(df_subset)==0){
          print(NULL)
        }else{
          df_subset<-df_subset[!is.na(df_subset$taxonrank),]
          mapgilbert <- get_map(location = c(lon = mean(df_subset$decimallongitude), lat =
                                               mean(df_subset$decimallatitude)), zoom = 4,maptype = "terrain", scale = 2)
          
          map_1<-ggmap(mapgilbert) +
            geom_point(data = df_subset, aes(x = decimallongitude, y = decimallatitude, alpha = 0.5, size =2,     colour="Red" ),shape=20)+geom_point(data=dataFrame,aes(x = decimallongitude, y = decimallatitude,     alpha = 0.5, size =4,colour="Blue" ),shape=20)
          print(map_1)
        }
        
      }
      
    }else{
      tax_name<-input$clusterNum
      df_subset<-subset(X,taxonrank==tax_name)
      if(nrow(df_subset)==0){
        print(NULL)
      }else{
        mapgilbert <- get_map(location = c(lon = mean(df_subset$decimallongitude), lat =
                                             mean(df_subset$decimallatitude)), zoom = 4,maptype = "terrain", scale = 2)
        
        map_1<-ggmap(mapgilbert) +
          geom_point(data = df_subset, aes(x = decimallongitude, y = decimallatitude, alpha = 0.5, size =2,     colour="Red" ),shape=20)
        print(map_1)
      }
    }
    
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Resolved_data" = as.matrix(data_download_resolver()),
           "Filtered_data" = as.matrix(selectedActualData()),
           "Actual_data" = as.matrix(selectedData()))
  })
  
  output$downloadData1 <- downloadHandler(
    
           
        filename = function() {
          paste(input$dataset, ".csv", sep = "")
        },
        
        content = function(file) {
          
          write.csv(datasetInput(), file, row.names = FALSE)
        }
        
      
  )
  
  
  # output$downloadData2 <- downloadHandler(
  #   
  #   filename= function(){
  #     paste("bd_data_filtered", ".csv", sep= "")  
  #     
  #   }
  #   
  #   content = function(file){
  #     #X=selectedData()
  #     write.csv(selectedData(), file, row.names = FALSE)
  #     
  #   }
  #   
  # )
  # 
  # output$downloadData3 <- downloadHandler(
  #   
  #   filename= function(){
  #     paste("bd_data_actual", ".csv", sep= "")  
  #     
  #   }
  #   
  #   content = function(file){
  #     #X=selectedActualData()
  #     write.csv(selectedActualData(), file, row.names = FALSE)
  #     
  #   }
  #   
  # )
  
  # output$plot3<-renderDataTable({
  #   
  # })
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)






# Taxonomic Resolver
# =====================================
#   
#   Inputs {.sidebar}
# -----------------------------------------------------------------------
#   
#   Do you want to resolve the missing data?
#   ```{r}
# selectInput("clusterNum2", label = h3("Taxonomic Resolver "),
#             choices = list("YES"="YES", "NO"="NO"),
#             selected = "YES")
# 
# ```
# 
# 
# 
# If you want to resolve the data, select the taxonomic level for the resolution?
#   ```{r}
# selectInput("clusterNum3", label = h3("Taxonomic Resolver Level"),
#             choices = list( "KINGDOM" = "KINGDOM", "PHYLUM" = "PHYLUM", "CLASS" = "CLASS", "ORDER" = "ORDER","FAMILY" = "FAMILY","GENUS"="GENUS","SPECIES"="SPECIES","SUBSPECIES"="SUBSPECIES"),
#             selected = "SPECIES")
# 
# ```
# 
# 
# ```{r}
# 
# selectedTaxonomy<-reactive({
#   
#   num <- reactive((input$clusterNum2))
#   if(num()=="YES"){
#     return(1)
#   }else{
#     return(0)
#   }
# })
# 
# ```
# 
# 
# ```{r}
# selectedColumn1 <- reactive({
#   
#   #X<-subset(X,select=c(eventDate,month,year))
#   num <- reactive((input$clusterNum3))
#   #num1<-num()
#   #col_num
#   col_num<-num()
#   return(col_num)
#   
# })
# 
# 
# 
# ```
# 
# 
# ```{r}
# 
# bd_taxonomic_resolver<-function(df,tax_rank){
#   
#   
#   if(nrow(df)==0){
#     return(0)
#   }else{
#     df_subset<-subset(df,taxonRank==tax_rank)
#     df_na<-subset(df,is.na(taxonRank))
#     
#     if(nrow(df_na)!=0){
#       count1<-nrow(df_na)
#       count2<-0
#       for (i in 1:nrow(df_na)){
#         if(!is.na( df_na$name[i])){
#           
#           #using only two data bases 'itis', 'ncbi' to keep the function simple
#           tax_hierarchy_itis <- suppressMessages(
#             as.data.frame(taxize::classification(taxize::get_uid(df_na$name[i]), db = "itis")[[1]]))
#           tax_hierarchy_ncbi <- suppressMessages(
#             as.data.frame(taxize::classification(taxize::get_uid(df_na$name[i]), db = "ncbi")[[1]]))
#         }
#         
#         if(suppressMessages(!is.na(tax_hierarchy_ncbi[[1]]))){
#           
#           df_na$taxonRank[i]<-tax_hierarchy_ncbi$rank[nrow(tax_hierarchy_ncbi)]
#           
#         }else if(suppressMessages(!is.na(tax_hierarchy_itis[[1]]))){
#           
#           df_na$taxonRank[i]<-tax_hierarchy_itis$rank[nrow(tax_hierarchy_itis)]
#           
#         }else{
#           count2=count2+1
#           
#         }
#         
#       }
#       #Now combine both the data frames to form the final data frame
#       
#       df_subset<-df_subset[!is.na(df_subset$taxonRank),]
#       df_na<-subset(df_na,taxonRank=tax_rank)
#       #df_final<-rbind(df_subset,df_na)
#       
#       #cat("The number of records with missing taxon rank were",count1,"\n")
#       #cat("The number of records which are resolved:",count2,"\n")
#       if(nrow(df_na)==0){
#         return(1)
#       }else{
#         return(df_na)
#       }
#     }else{
#       
#       return(1)
#     }
#     
#   }
#   
#   
# }
# 
# 
# 
# ```
# 
# 
# 
# 
# 
# Column {.tabset}
# -----------------------------------------------------------------------
#   
#   ###DataDisplay
#   
#   ```{r}
# renderDataTable({
#   if(selectedTaxonomy()==1){
#     
#     tax_name<-selectedColumn1()
#     dataFrame<-bd_taxonomic_resolver(X,tax_name)
#     if(dataFrame==0){
#       renderText({warning("The data frame is empty")})
#       NULL
#       
#     }else if(dataFrame==1){
#       
#       df_subset<-subset(X,taxonRank==tax_name)
#       df_subset
#     }else{
#       dataFrame
#       
#     }
#     
#   }else{
#     
#     X
#   }
#   
#   
#   
# })
# 
# ```
# 
# 
# ###MapPlot
# ```{r}
# 
# renderPlot({
#   if(selectedTaxonomy()==1){
#     
#     tax_name<-selectedColumn1()
#     dataFrame<-bd_taxonomic_resolver(X,tax_name)
#     if(dataFrame==0){
#       renderText({warning("The data frame is empty")})
#       NULL
#       
#     }else if(dataFrame==1){
#       df_subset<-subset(X,taxonRank==tax_name)
#       if(nrow(df_subset)==0){
#         print(NULL)
#       }else{
#         mapgilbert <- get_map(location = c(lon = mean(df_subset$decimalLongitude), lat =
#                                              mean(df_subset$decimalLatitude)), zoom = 4,maptype = "terrain", scale = 2)
#         
#         map_1<-ggmap(mapgilbert) +
#           geom_point(data = df_subset, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =2,     colour="Red" ),shape=20)
#         print(map_1)
#       }
#       
#     }else{
#       df_subset<-subset(X,taxonRank==tax_name)
#       if(nrow(df_subset)==0){
#         print(NULL)
#       }else{
#         df_subset<-df_subset[!is.na(df_subset$taxonRank),]
#         mapgilbert <- get_map(location = c(lon = mean(df_subset$decimalLongitude), lat =
#                                              mean(df_subset$decimalLatitude)), zoom = 4,maptype = "terrain", scale = 2)
#         
#         map_1<-ggmap(mapgilbert) +
#           geom_point(data = df_subset, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =2,     colour="Red" ),shape=20)+geom_point(data=dataFrame,aes(x = decimalLongitude, y = decimalLatitude,     alpha = 0.5, size =4,colour="Blue" ),shape=20)
#         print(map_1)
#       }
#       
#     }
#     
#   }else{
#     tax_name<-selectedColumn1()
#     df_subset<-subset(X,taxonRank==tax_name)
#     if(nrow(df_subset)==0){
#       print(NULL)
#     }else{
#       mapgilbert <- get_map(location = c(lon = mean(df_subset$decimalLongitude), lat =
#                                            mean(df_subset$decimalLatitude)), zoom = 4,maptype = "terrain", scale = 2)
#       
#       map_1<-ggmap(mapgilbert) +
#         geom_point(data = df_subset, aes(x = decimalLongitude, y = decimalLatitude, alpha = 0.5, size =2,     colour="Red" ),shape=20)
#       print(map_1)
#     }
#   }
#   
# })
# ```
