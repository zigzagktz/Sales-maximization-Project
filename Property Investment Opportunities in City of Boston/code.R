#install.packages("ggplot2")
#install.packages("shiny")


library(plotly)
library(ggplot2)
library(shiny)
library(lubridate)
install.packages("ggedit")
library(ggedit)
install.packages("gghighlight")
library(gghighlight)


#global

# mini global for  barplot
myData <- read.csv(file.choose())
topfour <- c("\tPrudential","Back Bay","West Fens|Kenmore Square","Downtown")
myData$Location <- as.character(myData$Location)
sub <- subset(myData, myData$Location %in% topfour)
colnames(sub) <- c("Categories","Location","Median")
sub$Location[sub$Location=="West Fens|Kenmore Square"] <- "Kenmore Square"
sub$Location[sub$Location=="\tPrudential"] <- "Prudential"
sub$Median <- sub$Median/1000000









ui <- fluidPage(# Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs
    
    
    
    sidebarPanel(
      width=3,
      
      #creating button for histogram
      
      
      
      #creating slider for line
      

      
      
      conditionalPanel(condition = "input.tab != 'aa'", fileInput(
        "file1",
        "Choose dataset",
        multiple = TRUE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"))
        ),
      conditionalPanel(condition = "input.tab != 'aa'",sliderInput("x","Year Range",min=1800,max=2015,value=1950,step = .5) ),
      conditionalPanel(condition = "input.tab != 'bb'",  radioButtons( "m" , label="Select Category", choices = list("Residential"=1,"Commercial"=2), selected = 1 )
)
      
    ),
    
    
    
    # Output
    mainPanel(
      tabsetPanel( id="tab",
      tabPanel("Famous Places", plotOutput("phonePlot",width = "700px",height = "400px"),value='aa'),
      tabPanel("Trend In Kenmore Square", plotOutput("plot",width = "700px",height = "400px"),value='bb')
      
      
      )
    )
  )
     
) 
      

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  
  output$phonePlot <- renderPlot({
    {
      
      # Fill in the spot we created for a plot
      output$phonePlot <- renderPlot({
        
        # Render a barplot
        
        ifelse(input$m==1,r <- subset(sub,Categories=="Residential"),r <- subset(sub,Categories=="Commercial"))
        
        library(ggplot2)
        

        ggplot(r,aes(Location, Median,fill=Location)) + geom_bar(stat="identity",colour="Green") +
        
          ylim(0,max(sub$Median[sub$Categories== c("Commercial")])) +
          ylab("Median Assessment value (Million)")+
          xlab("Locations") +
          labs(x="Locations",y="Median Assessment value (Million)")+
          ggtitle("Most Famous Locations in Boston")+
          theme(plot.title = element_text(hjust = 0.5))+
          theme_light()+
          theme(panel.grid = element_blank())+
          theme(axis.text.x = element_text(size=11,color = "Black")) + 
          gghighlight(r$Median==r$Median[r$Location=="Kenmore Square"]) +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold")) 
        
        
      })
    }
    
    
    
    
  })
  
  
  
  
  output$plot <- renderPlot({
    
    
     ifelse(input$file1$name=="Kenmore Year Remodel.csv",name<- "Kenmore Year Remodel",name <- "Kenmore Year Built")
    ifelse(input$file1$name=="Kenmore Year Remodel.csv", threshold<- 15, threshold<- 75 )
    
  
    
     req(input$file1)
    df1 <- read.csv(input$file1$datapath)
    
    colnames(df1) <- c("categories","location","year.remod","median")
    
    a <- df1$year.remod[df1$year.remod > input$x[1]]
    b <- (df1$median[df1$year.remod  > input$x[1]]) / (1000000)
    c <- df1$categories[df1$year.remod > input$x[1]]
    
    
    
    a <- as.Date(as.character(a),format = "%Y")
    a <- year(a)
    da <- cbind(a,b)
    da <- as.data.frame(da)
    da["categories"] <- c
    da$c<- as.character(da$c)
    


     ggplot(da,aes((a),b,colour= categories)) + 
      geom_line(aes(),size=1) +
       
       
       ylim(0,110)+
       
      theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
    
    scale_x_continuous(limits = c(min(da$a), max(da$a))) +
      xlab(name) +
      ylab("Median Assessment Value (Million)") +
      ggtitle("Assessment Value in Kenmore Square ") +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position = c(0.95, 0.95),legend.justification = c("right", "top") ) +
      geom_point()+
      annotate(c("point"), x = da$a[da$b==max(b)],y=max(b), colour = "#CC0000",alpha=0.5,size=5)+
       annotate(c("point"), x = da$a[da$b==max(da$b[da$categories=="Residential"])],y=max(da$b[da$categories=="Residential"]), colour = "#CC0000",alpha=0.8,size=5) 
       
    
  
                             
   
  })
}

shinyApp(server = server, ui = ui)


