#
# Solvate / Hydrate Stoichiometry calculator
# Author: r.y.penchev@gmail.com
# Version 2.0
# New to Version 2.0: Direct solution for stoichiometry
# Features: 1) TGA and DVS 2) Table output 3) Cell match to target 4) Single user solvent addition
#           5) Direct solution for stoichiometry
#
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(DT)

# list of solvents
solvents <- c("water","acetone","acetonitrile","dmso","ethanol","ethyl acetate","ethylene glycol","glycerol","heptane",
              "hexane","mek","methanol","mibk","mtbe","octane","propanol","thf","toluene","xylene","2-MeTHF")
# list of solvents Mw
solvents_Mw <- c(18.015, 58.08, 41.05, 78.13, 46.07, 88.11, 62.07, 92.09, 100.2, 86.18, 72.11, 32.04, 100.16, 88.15, 114.23, 
                 60.1, 72.11, 92.14, 106.16, 86.13)
# list of stoichiometris to calculate weight loss for
stoichiometry <- c(0.25, 0.3333, 0.5, 0.6666, 0.75, 1, 1.25, 1.3333, 1.5, 1.6666, 1.75, 2, 2.5, 3, 3.5, 4, 5, 6, 7)


# HEADER
header <- dashboardHeader(title = "Solvate Stoichiometry 2.0", titleWidth = 310, disable = FALSE)
# SIDEBAR
sidebar <- dashboardSidebar(disable = TRUE)
# BODY
body <- dashboardBody(
  fluidRow(
    box(width = 2, title = "Input", status = "primary",solidHeader = TRUE, collapsible = FALSE,
      awesomeRadio("rb1", label = h4("Select measurement type:"),
                     choices = list("TGA" = 1, "DVS" = 2), 
                     selected = 1, status = "danger"), 
      
      radioGroupButtons(inputId = "rb2",
                        label = h4("Solve for:"), choices = c("Stoichiometry" = 1,
                                                     "Weight Chng %" = 2), selected = 1, individual = FALSE,
                        checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                      style = "color: steelblue"),
                                         no = tags$i(class = "fa fa-circle-o",
                                                     style = "color: steelblue"))),
      
      
      numericInput(inputId = "api_mw", label = h4("Enter API Mw:"), value = NULL),
      numericInput(inputId = "loss", label = h4("Enter weight change [%]:"), value = NULL),
      
      conditionalPanel(condition = "input.rb2 == 2",
        sliderInput("deviation", label = h4("Set search range (+/- of weight change):"), min = 0.1, max = 1, value = 0.5, step = 0.1)
      ),
      
      pickerInput(inputId = "solvent_choice", label = h4("Choose solvent(s):"), choices = solvents, multiple = TRUE),
      textInput(inputId = "user_solvent_mw", label = h4("... or enter solvent Mw:"), value = NULL),
      actionButton(inputId = "calc", label = "Calculate")
    ),
    
      box(width = 10, title = "Output", status = "primary",solidHeader = TRUE, collapsible = FALSE,
          footer=HTML("Author: r.y.penchev@gmail.com"),
          DT::dataTableOutput("table"),
          br(),
          textOutput("text1"),
          uiOutput('eq')
      )
    
  )
) # end of BODY

# UI
ui <- dashboardPage(header, sidebar, body, skin = "green")

# SERVER
server <- function(input, output) {
  
  ################################### USER DEFINED FUNCTIONS ####################################
  # function calculating the TGA weight loss percentage
  tga_mass_loss <- function(stoichiometry, Mw_sol) {
    ((stoichiometry*Mw_sol)/((stoichiometry*Mw_sol)+(1*input$api_mw)))*100
  }
  # function calculating the DVS weight loss percentage
  dvs_mass_loss <- function(stoichiometry, Mw_sol) {
    ((stoichiometry*Mw_sol)/input$api_mw)*100
  }
  # function calculating the TGA stoichiometry
  tga_sto <- function(Mw_sol) {
    (input$loss*input$api_mw)/(Mw_sol*(100-input$loss))
  }
  # function calculating the DVS stoichiometry
  dvs_sto <- function(Mw_sol) {
    (input$loss*input$api_mw)/(100*Mw_sol)
  }
  ################################### END OF USER FUNCTIONS #####################################
  
  ################################### DEFINITIONS OF reactiveValues #############################
  # list of solvents
  values_solvents <- reactiveValues(solvents = c("water","acetone","acetonitrile","dmso","ethanol","ethyl acetate","ethylene glycol","glycerol","heptane",
                                                 "hexane","mek","methanol","mibk","mtbe","octane","propanol","thf","toluene","xylene","2-MeTHF"))
  # list of solvents Mw
  values_solvents_Mw <- reactiveValues(solvents_Mw =  c(18.015, 58.08, 41.05, 78.13, 46.07, 88.11, 62.07, 92.09, 100.2, 86.18, 72.11, 32.04, 100.16, 88.15, 114.23, 
                                                        60.1, 72.11, 92.14, 106.16, 86.13))
  # list of stoichiometries
  values_stoichiometry <- reactiveValues(stoichiometry = c(0.25, 0.3333, 0.5, 0.6666, 0.75, 1, 1.25, 1.3333, 1.5, 1.6666, 1.75, 2, 2.5, 3, 3.5, 4, 5, 6, 7)) 
   
  ############################ END OF DEFINITIONS OF reactiveValues #############################
  
  
  observeEvent(input$calc, {
    # use 'match' function to find the position of the user selected solvents in the 'solvents' list
    # if the user types Mw in the text box with inputId = "user_solvent_mw", it gets added to the selection at number eqial to: length(values_solvents$solvents)+1
    if (input$user_solvent_mw == "") {
      user_solvents_position_in_list <- match(input$solvent_choice, values_solvents$solvents)
    } else {
      # add another solvent position
      user_solvents_position_in_list <- match(input$solvent_choice, values_solvents$solvents)
      user_solvents_position_in_list <- c(user_solvents_position_in_list, length(values_solvents$solvents)+1)
      # add new solvent name ("user solvent") to the list of solvents 
      values_solvents$solvents <- c(values_solvents$solvents, "user solvent")
      # add the new Mw to the list of Mw's
      values_solvents_Mw$solvents_Mw <- c(values_solvents_Mw$solvents_Mw, as.numeric(input$user_solvent_mw))
    }
    
    # logic selecting the type of calculation
    if (input$rb1==1 & input$rb2==1) {
      # calculate the matrix(!) of TGA stoichiometry for the selected solvents
      mass_loss_data <- t(as.matrix(tga_sto(values_solvents_Mw$solvents_Mw[user_solvents_position_in_list])))
    } else if (input$rb1==1 & input$rb2==2) {
      # calculate the matrix(!) of TGA weight change for each combination of stoichiometry and solvent
      mass_loss_data <- outer(values_stoichiometry$stoichiometry, values_solvents_Mw$solvents_Mw[user_solvents_position_in_list], tga_mass_loss)  
    } else if (input$rb1==2 & input$rb2==1) {
      # calculate the matrix(!) of DVS stoichiometry for the selected solvents
      mass_loss_data <- t(as.matrix(dvs_sto(values_solvents_Mw$solvents_Mw[user_solvents_position_in_list]))) 
    } else {
      # calculate the matrix(!) of DVS weight change for each combination of stoichiometry and solvent
      mass_loss_data <- outer(values_stoichiometry$stoichiometry, values_solvents_Mw$solvents_Mw[user_solvents_position_in_list], dvs_mass_loss)  
    }
    
    # add the solvent names to the matrix
    colnames(mass_loss_data) <- values_solvents$solvents[user_solvents_position_in_list]
    
    # convert matrix to data frame
    mass_loss_data.df <- as.data.frame(mass_loss_data)
    
    if (input$rb2==2) { # this "if" part dials with the waight loss matrix!
      
      # add the stoichiometry vector to the data frame in last position, move it at the begining and round the numbers
      mass_loss_data.df <- cbind(mass_loss_data.df, stoichiometry)   # the 'stoichiometry' here is not the reactiveValues object!!!
      mass_loss_data.df <- mass_loss_data.df %>% select(stoichiometry, everything()) %>% 
      mutate_if(is.numeric, round, digits = 2)
    
      ############################### DT Table CELL COLOR PREP WORK  ########################################
      # define the top and bottom boundary of the range around the mass change, to be used for setting the cell color
      boundary_top <- input$loss + input$deviation
      boundary_bottom <- input$loss - input$deviation
    
      # determine the data frame values within this range. this returns logical data frame
      # with TRUE for values within the range. line below omits the Stoichiometry column
      # from the search
      mass_loss_data.df_remove1st <- mass_loss_data.df[, -1]
      hits_df <- (mass_loss_data.df_remove1st < boundary_top & mass_loss_data.df_remove1st > boundary_bottom)
    
      # gets the actual values that are within the range and determines their number
      hits_values <- mass_loss_data.df_remove1st[hits_df]
      number_of_hits_values <- length(hits_values)
      ########################### END OF CELL COLOR PREP WORK ################################################
    
      if (number_of_hits_values >0) {
        output$table <- DT::renderDataTable(
          datatable(mass_loss_data.df, filter = 'top', class="cell-border stripe", 
                    rownames = FALSE,
                    extensions = c('Buttons'),
                    options = list(dom = "Blfrt", # removded 'ip' from the dom string
                                   buttons = list("copy", list(extend = "collection",buttons = c("csv", "excel", "pdf"), text = "Save")), # end of buttons custamization
                                   pageLength=19, lengthChange = FALSE, autoWidth = TRUE, 
                                   searchHighlight = TRUE) # end of options
          ) %>% # end of datatable
            # set cell color based on value but omitting the Stoichiometry column 
            formatStyle(names(mass_loss_data.df),  color = 'blue', backgroundColor = styleEqual(hits_values, rep('yellow',number_of_hits_values))) %>%
            formatStyle('stoichiometry',  color = 'black', backgroundColor = '', fontWeight = 'bold')
        ) # end of DT render
      } else {
        output$table <- DT::renderDataTable(
          datatable(mass_loss_data.df, filter = 'top', class="cell-border stripe", 
                    rownames = FALSE,
                    extensions = c('Buttons'),
                    options = list(dom = "Blfrt", # removded 'ip' from the dom string
                                   buttons = list("copy", list(extend = "collection",buttons = c("csv", "excel", "pdf"), text = "Save")), # end of buttons custamization
                                   pageLength=19, lengthChange = FALSE, autoWidth = TRUE, 
                                   searchHighlight = TRUE) # end of options
          ) %>% # end of datatable
            formatStyle(names(mass_loss_data.df),  color = 'blue') %>%
            formatStyle('stoichiometry',  color = 'black', backgroundColor = '', fontWeight = 'bold')  
        ) # end of DT render  
      }
      output$text1 <- renderText(paste("Search range:", boundary_bottom, "-", boundary_top, "% weight change!"))

    } else { # this "if" part dials with the stoichiometry matrix!
      
      output$table <- DT::renderDataTable(
        datatable(round(mass_loss_data.df, digits = 1), class="cell-border stripe", 
                  rownames = FALSE,
                  extensions = c('Buttons'),
                  options = list(dom = "Blrt", # removded 'ip' from end of dom string and 'f' after the 'Bl'
                                 buttons = list("copy", list(extend = "collection",buttons = c("csv", "excel", "pdf"), text = "Save")), # end of buttons custamization
                                 pageLength=1, lengthChange = FALSE, autoWidth = TRUE, 
                                 searchHighlight = TRUE) # end of options
        ) %>% # end of datatable
          formatStyle(names(mass_loss_data.df),  color = 'blue')
      ) # end of DT render
      output$text1 <- renderText(paste(""))
    }
    
  }) # end of observeEvent
}

shinyApp(ui, server)