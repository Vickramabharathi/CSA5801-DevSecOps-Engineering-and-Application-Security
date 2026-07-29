library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("SupplyShield: Bayesian Software Supply Chain Risk Dataset Generator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("rows",
                   "Number of Records",
                   value = 100,
                   min = 10,
                   max = 10000),
      
      actionButton("generate","Generate Dataset"),
      
      br(),
      br(),
      
      downloadButton("downloadData","Download CSV")
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Dataset",
                 DTOutput("table")),
        
        tabPanel("Risk Distribution",
                 plotOutput("plot1")),
        
        tabPanel("Attack Type",
                 plotOutput("plot2"))
        
      )
      
    )
    
  )
  
)

server <- function(input, output){
  
  dataset <- eventReactive(input$generate,{
    
    n <- input$rows
    
    Supplier <- sample(
      paste("Supplier",1:50),
      n,
      replace=TRUE)
    
    Dependency_Type <- sample(
      
      c("Open Source",
        "Commercial",
        "Internal"),
      
      n,
      replace=TRUE,
      
      prob=c(0.6,0.25,0.15)
      
    )
    
    Attack_Type <- sample(
      
      c("Dependency Confusion",
        "Typosquatting",
        "Malicious Package",
        "Compromised Repository",
        "Backdoor Injection"),
      
      n,
      
      replace=TRUE
      
    )
    
    Vulnerability_Score <- round(runif(n,1,10),1)
    
    Package_Trust_Score <- round(runif(n,40,100),1)
    
    Patch_Delay_Days <- sample(0:90,n,replace=TRUE)
    
    Build_Failure_Rate <- round(runif(n,0,30),2)
    
    Developer_Awareness <- sample(
      
      c("Low","Medium","High"),
      
      n,
      
      replace=TRUE,
      
      prob=c(0.3,0.4,0.3)
      
    )
    
    CI_CD_Security <- sample(
      
      c("Weak","Moderate","Strong"),
      
      n,
      
      replace=TRUE,
      
      prob=c(0.25,0.45,0.30)
      
    )
    
    #####################################################
    # Bayesian Probability
    #####################################################
    
    Prior <- 0.30
    
    Likelihood <- Vulnerability_Score/10
    
    Evidence <- 0.50
    
    Posterior <- (Likelihood*Prior)/Evidence
    
    Posterior <- round(pmin(Posterior,1),3)
    
    Risk_Level <- ifelse(
      
      Posterior<0.30,
      
      "Low",
      
      ifelse(
        
        Posterior<0.60,
        
        "Medium",
        
        "High"
        
      )
      
    )
    
    data.frame(
      
      Supplier,
      
      Dependency_Type,
      
      Attack_Type,
      
      Vulnerability_Score,
      
      Package_Trust_Score,
      
      Patch_Delay_Days,
      
      Build_Failure_Rate,
      
      Developer_Awareness,
      
      CI_CD_Security,
      
      Posterior,
      
      Risk_Level
      
    )
    
  })
  
  output$table <- renderDT({
    
    datatable(dataset())
    
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(dataset(),
           
           aes(Risk_Level))+
      
      geom_bar()
    
  })
  
  output$plot2 <- renderPlot({
    
    ggplot(dataset(),
           
           aes(Attack_Type))+
      
      geom_bar()+
      
      coord_flip()
    
  })
  
  output$downloadData <- downloadHandler(
    
    filename=function(){
      
      "SupplyShield_Dataset.csv"
      
    },
    
    content=function(file){
      
      write.csv(dataset(),file,row.names=FALSE)
      
    }
    
  )
  
}

shinyApp(ui,server)