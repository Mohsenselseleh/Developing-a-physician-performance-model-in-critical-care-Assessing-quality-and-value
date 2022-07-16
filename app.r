library(shiny)
library("shinydashboard")
library(tidyverse)



Docs = read.csv("df_doctors_v20220321 - df_doctors_v20220321.csv")

Patients = read.csv("df_patients_v20220321 - df_patients_v20220321.csv")

## mrging both doc and patient data
Merged = merge.data.frame(Docs,Patients, by = "DocID") %>% as.data.frame()


## Gender Docs

DocGender = Docs %>%
  group_by(M6) %>%
  summarise(Count = n())%>%
  arrange(desc(Count))

## Gender Patients
PatientsGender = Patients %>%
  group_by(P7) %>%
  summarise(Count = n())%>%
  arrange(desc(Count))


SOFA = read.csv("df_traj_v20220321 - df_traj_v20220321.csv")
# names(SOFA)



ui <- dashboardPage(
  dashboardHeader(title = "Dash"),
  dashboardSidebar(
    sidebarMenu(
      h3("Settings"), ## title for settings 
      menuItem("Gender Graph",  ## name of the side tab
               radioButtons(
                 "Gender",
                 "Select Data",
                 choices = c("Doctors","Patients","Both"),
                 selected = "Both",
                 inline = FALSE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               )
      ), menuItem("DocID & Physician Training",  ## name of the side tab
               radioButtons(
                 "DocID",
                 "",
                 choices = c("DocID","Physician Training"),
                 selected = "Physician Training",
                 inline = FALSE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               )
      ),menuItem("APACHE-II",  ## name of the side tab
               radioButtons(
                 "DocID1",
                 "",
                 choices = c("DocID","Physician Training"),
                 selected = "Physician Training",
                 inline = FALSE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               )
      ),menuItem("Sofa (Patients)",  ## name of the side tab
               radioButtons(
                 "DocID2",
                 "",
                 choices = c("DocID","Physician Training"),
                 selected = "Physician Training",
                 inline = FALSE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               )
      ),
      menuItem("ICU Stay Plot", 
               radioButtons(
                 "ICU",
                 "Select Parameter",
                 choices = c("Primary Diagnosis","Discharge Status"),
                 # selected = "Both",
                 inline = FALSE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               ),
               sliderInput('bin', 'bin size', min = 1, max = 100, value = 25 ),
               sliderInput('alpha', 'Alpha', min = .1, max = 1, value = .5 )
      ),menuItem("Sofa Plot", 
               sliderInput('bin1', 'bin size', min = 1, max = 100, value = 25 ),
               sliderInput('alpha1', 'Alpha', min = .1, max = 1, value = .5 )
      ),
      menuItem("Table",
               radioButtons(
                 "Table",
                 "Select Table",
                 choices = c("Patients per Doctor","Patients per Primary Diagnosis","DocID & Physician Training"),
                 # selected = "Both",
                 inline = FALSE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
               )
      )
    )
    
  ),
  dashboardBody(
    
    
    fluidPage(
      tabsetPanel( ## tabs in the main body
        tabPanel("Gender Plot", ## tab name
                 box(status = "warning", solidHeader = FALSE, ## using box, makes it look good
                     collapsible = TRUE,width = 12,
                     plotOutput("GenderPlot") ## plot output
                 )
        ),
        tabPanel("DocID & Physician Training",
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     plotOutput("DocID")
                 )
                 
        ),tabPanel("APACHE-II",
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     plotOutput("DocID1")
                 )
                 
        ),tabPanel("Sofa (Patients)",
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     plotOutput("DocID2")
                 )
                 
        ),
        tabPanel("ICU Stay Plot",
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     plotOutput("BoxPlot")
                 )
                 
        ), tabPanel("Sofa Score",
                    box(status = "warning", solidHeader = FALSE,
                        collapsible = TRUE,width = 12,
                        plotOutput("Sofa")
                    )
                    
        ),
        tabPanel("Table", 
                 box(status = "warning", solidHeader = FALSE,
                     collapsible = TRUE,width = 12,
                     dataTableOutput("Table") ## table output
                 )
        )
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  
  output$BoxPlot = renderPlot({
    req(input$ICU) ## waits until it gets the value from the inputr handle
    req(input$alpha) ## waits until it gets the value from the inputr handle
    req(input$bin) ## waits until it gets the value from the inputr handle
    b = NULL
    if(input$ICU == "Discharge Status"){
    
      b =  ggplot(Patients, aes(x=P9,fill = P8, color = P8)) +
        geom_histogram( alpha=input$alpha, position="identity",bins=input$bin)+
        xlab("ICU length of stay")+
        ylab("Discharge Status")  +
        ggtitle("")+
        theme_classic()  +
        guides(fill = guide_legend(title="Discharge Status"), 
               color = FALSE, 
               shape = FALSE) 
      
      
    }
    if(input$ICU == "Primary Diagnosis"){

      b =  ggplot(Patients, aes(x=P9,fill = P10, color = P10)) +
        geom_histogram( alpha=input$alpha, position="identity",bins=input$bin)+
        xlab("ICU length of stay")+
        ylab("Primary Diagnosis")+
        ggtitle("")+
        theme_classic()  +
        guides(fill = guide_legend(title="Primary Diagnosis"), 
               color = FALSE, 
               shape = FALSE)
      
    }
    b
    
  })  
  
  output$DocID = renderPlot({

    req(input$DocID) ## waits until it gets the value from the inputr handle
    b = NULL
    M7P8DA = Merged %>%
      group_by( DocID,M7,P8) %>%
      summarise(PatientsCount = n())
    names(M7P8DA)

    
    if(input$DocID == "DocID"){
      b =   ggplot(M7P8DA, aes(fill=P8, x=DocID, y= PatientsCount)) +
        geom_bar(position="dodge", stat="identity") +
        ylab("Number of Patients")+
        xlab("") +
        theme_classic()+
        theme(axis.text.x=element_text(angle=45, hjust=1))+ 
        guides(fill=guide_legend(title="Alive Status"))

    }
    if(input$DocID == "Physician Training"){

      b =    ggplot(M7P8DA, aes(fill=P8, x=M7, y= PatientsCount)) +
        geom_bar(position="dodge", stat="identity") +
        ylab("Number of Patients")+
        xlab("") +
        theme_classic()+
        theme(axis.text.x=element_text(angle=45, hjust=1))+ 
        guides(fill=guide_legend(title="Alive Status"))
      
    }
    b
    
  })
  
  output$DocID1 = renderPlot({

    req(input$DocID1) ## waits until it gets the value from the inputr handle
    b = NULL
    M7P5DA = Merged %>%
      group_by( DocID,M7,P5) %>%
      summarise(PatientsCount = n())
    names(M7P5DA)
    
    
    if(input$DocID1 == "DocID"){
    
      b =   ggplot(M7P5DA, aes(x=DocID, y=P5, color=DocID, fill = DocID, alpha = 0.6)) +
        geom_boxplot()+
        ylab("APACHE-II")+
        xlab("") +
        theme_classic()+
        theme(axis.text.x=element_text(angle=45, hjust=1))+ 
        theme(legend.position="none")
      
      
      
    }
    if(input$DocID1 == "Physician Training"){

      b =        ggplot(M7P5DA, aes(x=M7, y=P5, color=M7, fill = M7, alpha = 0.6)) +
        geom_boxplot()+
        ylab("APACHE-II")+
        xlab("") +
        theme_classic()+
        theme(axis.text.x=element_text(angle=45, hjust=1))+ 
        theme(legend.position="none")
      
    }
    b
    
  })
  
  output$DocID2 = renderPlot({

    req(input$DocID2) ## waits until it gets the value from the inputr handle
    b = NULL
    M7P6DA = Merged %>%
      group_by( DocID,M7,P6) %>%
      summarise(PatientsCount = n())
    names(M7P6DA)
    
    
    if(input$DocID2 == "DocID"){
      b =   ggplot(M7P6DA, aes(x=DocID, y=P6, color=DocID, fill = DocID, alpha = 0.6)) +
        geom_boxplot()+
        ylab("Sofa (Patients)")+
        xlab("") +
        theme_classic()+
        theme(axis.text.x=element_text(angle=45, hjust=1))+ 
        theme(legend.position="none")
      
    }
    if(input$DocID2 == "Physician Training"){

      b = ggplot(M7P6DA, aes(x=M7, y=P6, color=M7, fill = M7, alpha = 0.6)) +
        geom_boxplot()+
        ylab("Sofa (Patients)")+
        xlab("") +
        theme_classic()+
        theme(axis.text.x=element_text(angle=45, hjust=1))+ 
        theme(legend.position="none")
      
    }
    b
    
  })
  
  output$Sofa = renderPlot({
    req(input$alpha1) ## waits until it gets the value from the inputr handle
    req(input$bin1) ## waits until it gets the value from the inputr handle
    b = NULL
    
    b =  ggplot(SOFA, aes(x=SOFA)) +
      geom_histogram( alpha=input$alpha1, position="identity",bins=input$bin1)+
      xlab("SOFA Score")+
      # ylab("Discharge Status")  +
      ggtitle("")+
      theme_classic() 
    b
    
  })
  
  output$GenderPlot = renderPlot({
    req(input$Gender) ## waits until it gets the value from the inputr handle
    
    g = NULL
    
    if(input$Gender == "Both"){
      MergedGender = Merged %>%
        group_by(M6,P7) %>%
        summarise(Count = n())%>%
        arrange(desc(Count))
      
      g = ggplot(MergedGender, aes(fill=P7, x=M6, y= Count)) +
        geom_bar(position="dodge", stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        ylab("Number of Patients")+
        xlab("Doctors' Gender") +
        ggtitle("Gender-wise distribution of Patients and Doctors")+
        geom_text(data=data.frame(MergedGender), 
                  aes(M6, Count, group=P7, label=MergedGender$Count), 
                  position = position_dodge(width=0.9),vjust = 1, 
                  size=4)+
        theme_classic()+ 
        guides(fill=guide_legend(title="Patients' Gender"))
      
    }
    if(input$Gender == "Doctors"){
      g = ggplot(DocGender, aes(fill=M6, x=M6, y= Count)) +
        geom_bar(position="dodge", stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        ylab("Number of Doctors")+
        xlab("Doctors' Gender") +
        ggtitle("Gender-wise distribution of Doctors")+
        geom_text(data=data.frame(DocGender), 
                  aes(M6, Count,group=M6, label=DocGender$Count), 
                  position = position_dodge(width=0.9),vjust = 1, 
                  size=4)+
        theme_classic()+ 
        guides(fill=guide_legend(title="Gender"))
      
    } 
    if(input$Gender == "Patients"){
      g =  ggplot(PatientsGender, aes(fill=P7, x=P7, y= Count)) +
        geom_bar(position="dodge", stat="identity") +
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        ylab("Number of Patients")+
        xlab("Patients' Gender") +
        ggtitle("Gender-wise distribution of Patients")+
        geom_text(data=data.frame(PatientsGender), 
                  aes(P7, Count,group=P7, label=PatientsGender$Count), 
                  position = position_dodge(width=0.9),vjust = 1, 
                  size=4)+
        theme_classic()+ 
        guides(fill=guide_legend(title="Gender"))
      
    }
    g
    
    
  })
  
  output$Table = renderDataTable({
    req(input$Table)
    t = NULL
    if(input$Table == "Patients per Doctor"){
      t = Merged %>%
        group_by(DocID,P8) %>%
        summarise(Count = n())%>%
        mutate(Percent = prop.table(Count))%>%
        mutate(Percent = round(Percent,2)*100)
      
      names(t)[2] = "Alive Status"
    }
    
    if(input$Table == "Patients per Primary Diagnosis"){
      t = PatientsPerDomain = Merged %>%
        group_by(P10,P8) %>%
        summarise(Count = n())%>%
        mutate(Percent = prop.table(Count))%>%
        mutate(Percent = round(Percent,2)*100)
        
      names(t)[2] = "Alive Status"
    }  
    
    if(input$Table == "DocID & Physician Training"){
      t = Merged %>%
        group_by(DocID,M7,P8) %>%
        summarise(Count = n())%>%
        mutate(Percent = prop.table(Count))%>%
        mutate(Percent = round(Percent,2)*100)
      
      names(t)[2] = "Physician Training"
      names(t)[3] = "Alive Status"
    }
    
    t
  })
  
}

shinyApp(ui, server)

