# Session 06 - 2021-05-22
# Shiny-App: Titanic-Orakel

# Bibliotheken importieren
library(shiny)
library(shinythemes)
library(e1071)
library(rpart.plot)
library(tidyverse)

# Modelle laden
model_svm <- readRDS("titanic_svm.rds")
model_dt <- readRDS("titanic_dt.rds")

# User-Interface

# Tag List für Footer
ui <- tagList(
  
  # Navbar Page
  navbarPage(title = "Titanic-Orakel",               # App-Name
             header = HTML("<meta charset = 'utf-8'"), # UTF-8
             theme = shinytheme("cerulean"),         # maritimes Theme
             fluid = TRUE,                           # fluidPage
             
             # 1. Tab mit Schiff-Icon
             tabPanel("Prognose", icon = icon("ship"),
                      
                      # Layout der Sidebar
                      sidebarPanel(
                        
                        # Überschrift für Parameter
                        tags$label(h3("Dateneingabe", style = "margin-bottom: 20px;")),
                        
                        # Features eingeben
                        sliderInput("pclass", "Passagierklasse:",
                                    min = 1, max = 3,
                                    value = 2),
                        
                        radioButtons("sex", inline = TRUE, "Geschlecht:",
                                     c("Frau" = 1,
                                       "Mann" = 0)),
                        
                        numericInput("age", "Alter:",
                                     value = "0"),
                        
                        radioButtons("relatives", inline = TRUE, "Mit Familie reisend?",
                                     c("Ja" = 1,
                                       "Nein" = 0)),
                        
                        selectInput("embarked", "Einstiegshafen:",
                                    c("Cherbourg" = 0,
                                      "Southampton" = 1,
                                      "Queenstown" = 2)),
                        
                        # Action-Button löst observeEvent() aus
                        actionButton("action", label = "Wird die Person überleben?", class = "btn btn-primary"),
                        
                        # Margin für Sidebar
                        style = "margin-top: 15px;"  
                        
                      ), # Ende Sidebar Panel
                      
                      # Layout des Main Panels
                      mainPanel(
                        
                        # Überschrift für Prognose
                        tags$label(h3("Wer überlebt die Titanic?", style = "margin-bottom: 20px;")),
                        
                        # Erklärtext
                        HTML("
                              <p>Füllen Sie das Formular aus, um herauszufinden, ob die fragliche Person das Unglück übersteht oder nicht.</p>
                              <p>Der Wert <i>p(überlebt)</i> gibt die Überlebenswahrscheinlichkeit an, während der Wert <i>p(verstorben)</i> die Wahrscheinlichkeit ausdrückt, dass die Person verstirbt.</p>
                              <p><strong>Beispiel:</strong> Ein <i>p(überlebt)</i>-Wert von 0.43 sagt aus, dass die Wahrscheinlichkeit des Überlebens bei 43% liegt.</p>
                                 "),
                        
                        # Output: Anzeige der Prognose
                        tags$div(
                          tableOutput("value1"),
                          style = "margin-top: 20px;"
                        )
                        
                      ) # Ende Main Panel
                      
             ), # Ende 1. Tab
             
             # 2. Tab mit Info-Icon
             tabPanel("FAQ", icon = icon("info-circle"),
                      
                      # Layout des Main Panels
                      mainPanel(
                        
                        # Überschrift
                        tags$label(h3("Häufig gestellte Fragen", style = "margin-bottom: 20px;")),
                        
                        # Fragen und Antworten
                        HTML("
                              <h4>Wie funktioniert das Orakel?</h4>
                              <p style = 'margin-bottom: 25px;'>
                                Hinter dem Orakel verbirgt sich der Klassifikations-Algorithmus <i>Support Vector Machines</i> (SVM).
                                Anhand eines Trainingssets mit echten Passagierdaten der Titanic hat die Maschine gelernt, gemäß welcher Parameter eine Person das Unglück überlebt oder nicht.
                                Dieses Modell stellt die Grundlage für die Prognosen dar.
                              </p>
                              
                              <h4>Welche Faktoren beeinflussen das Ergebnis?</h4>
                              <p style = 'margin-bottom: 20px;'>
                                Sofern das Formular ausgefüllt wurde, erscheint an dieser Stelle ein Entscheidungsbaum, der Aufschluss darüber gibt, welche Parameter besonders großen Einfluss auf das Ergebnis haben.
                              </p>
                            "),
                        
                        # Output: Anzeige des Decision Trees
                        plotOutput("value2")
                        
                      ) # Ende Main Panel
                      
             ) # Ende 2. Tab
             
  ), # Ende Navbar Page
  
  # Footer
  tags$footer("© 2021. All Rights Reserved.", style = "
              text-align: center;
              width: 100%;
              height: 50px;
              margin-top: 25px;
              font-size: 10px;
              color: #555555;
              padding: 18px;
              background-color: #F5F5F5;")
  
) # Ende Tag List

# Server-Logik
server <- function(input, output, session) {
  
  # observeEvent reagiert auf Action-Button
  observeEvent(input$action, {
    
    # User-Input Variablen zuweisen
    pclass <- as.numeric(input$pclass)
    sex <- as.numeric(input$sex)
    age <- input$age
    relatives <- as.numeric(input$relatives)
    embarked <- as.numeric(input$embarked)
    
    # Input in Data Frame speichern
    data <- data.frame(pclass, sex, age, relatives, embarked)
    
    # Prognosen auf Basis des SVM-Modells anstellen
    result <- predict(model_svm, data, probability = TRUE)
    my_result <- data.frame(attr(result, "probabilities"))
    
    # Output-Tabelle generieren, wobei Spalten umbenannt werden
    output$value1 <- renderTable(my_result %>%
                                   rename("p(überlebt)" = X1, "p(verstorben)" = X0))
    
    # Decision Tree generieren
    output$value2 <- renderPlot({
      rpart.plot(model_dt, roundint = FALSE)
    })
    
  }) # Ende observeEvent
  
} # Ende Server-Logik

# User-Interface und Server-Logik zusammenbauen
shinyApp(ui = ui, server = server)