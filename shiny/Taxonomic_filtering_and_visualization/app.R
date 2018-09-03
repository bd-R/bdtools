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
    title = "Taxonomic_filtering_and_visualization",
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
      menuItem("Taxonomic_Visualization",tabName = "taxonomic_visualization", icon = icon("th"))
      
      
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
                  
                  numericInput("obs",label = h3("NumberOfObs"), value = 500, min = 100, max = 10000)
                  
                ),
                
                mainPanel = (
                  
                  tableOutput("data_online")
                  
                )
                
              )
      ),
      
      tabItem(tabName = "taxonomic_visualization",
              sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(  
                  selectInput("clusterNum",label = "Taxonomic_Rank", 
                              choices = list( "KINGDOM" = "KINGDOM", "PHYLUM" = "PHYLUM", "CLASS" = "CLASS", "ORDER" = "ORDER","FAMILY" = "FAMILY","GENUS"="GENUS","SPECIES"="SPECIES","SUBSPECIES"="SUBSPECIES"), 
                              selected = "GENUS")
                ),
                
                mainPanel = (
                  
                  
                  tabsetPanel(type = "tabs",
                              tabPanel("Bar-Plot", plotOutput("plot1")),
                              tabPanel("Pie-Chart", plotlyOutput("plot2")),
                              tabPanel("Chronohorogram", tableOutput("plot3")),
                              tabPanel("Tempolar", tableOutput("plot4")),
                              tabPanel("TaxoTree", tableOutput("plot5"))
                  )
                  
                )
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
    
    df<-subset(df, select = c(name,kingdom,genus,phylum,family,order,class,eventdate,month,year,taxonrank))
    return(df)
    
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
  
  
  
  output$plot1 <- renderPlot({
    #X1<-selectedData()
    
    X<-DATA()
    if(is.null(X)){
      X<-Data_Online()
    }
    X<-setNames(X, tolower(names(X)))
    #col_num1<-which( colnames(X)=="taxonRank")
    ggplot(X, aes(X$taxonrank)) +geom_bar(fill = "green")+ labs(y="Count", x ="Taxonomic Level")
    #c_2<-ddply(selectedData(),~year,summarise,frequency=length((year)))
    #ggplot(c_2, aes(x=year,y=frequency)) + geom_bar(stat="identity")
    
    
    
    
  })
  
  output$plot2 <- renderPlotly({
    X2<-selectedData()
    var<-selectedColumn()
    #X3<-X2[!is.na(X2$var),]
    if(var>0){
      dat<-X2[,var]
      Z<-count(X2[,var])
      plot_ly(Z, labels = Z[,1],values=~freq, type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              hoverinfo = 'text')
    }else{
      NULL
    }
  })
  
  output$plot3 <- renderPlot({
    data_bdvis<-selectedData()
    data_bdvis<-subset(data_bdvis,select=c(eventdate,month,year))
    if(nrow(data_bdvis)==0){
      NULL
    }else{
      names(data_bdvis)[names(data_bdvis) == "eventdate"] <- "Date_collected"
      chronohorogram(data_bdvis)
    }
    
    
  })
  
  
  output$plot4 <- renderPlot({
    data_tempolar<-selectedData()
    data_tempolar<-subset(data_tempolar,select=c(eventdate,month,year))
    if(nrow(data_tempolar)==0){
      NULL
    }else{
      names(data_tempolar)[names(data_tempolar) == "eventdate"] <- "Date_collected"
      tempolar(data_tempolar,timescale = "m",plottype = "r")
    }
    
  })
  
  
  output$plot5 <- renderPlot({
    data_taxotree<-selectedData()
    data_taxotree<-subset(data_taxotree,select=c(family,genus))
    data_taxotree_na<-na.omit(data_taxotree)
    if(nrow(data_taxotree_na)==0 || ncol(data_taxotree_na)<2){
      NULL
    }else{
      names(data_taxotree)[names(data_taxotree) == "family"] <- "Family"
      names(data_taxotree)[names(data_taxotree) == "genus"] <- "Genus"
      taxotree(data_taxotree)
    }
    
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)
