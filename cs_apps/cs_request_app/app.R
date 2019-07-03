library(shiny)
library(tidyverse)
library(shinyalert)
library(shinyjs)
library(gmailr)
library(httr)
library(mailR)
jsReset <- "shinyjs.reset = function() {history.go(0)}"

source('secrets.R')
source("pass_change_mod.R")

login_panel <- function(){
  wellPanel(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Rubik:400,500")
    ),
    useShinyalert(),
    h1("Clinical Solutions Report Request Form"),
    textInput("username", "Username"),
    passwordInput("password", "Password"),
    actionButton("sign_in", "Sign In"),
    actionButton("pass_chg", "Forgot your Password?"),
    style = "width: 500px; max-width: 100%; margin: 10px auto; padding: 20px; font-family: 'Rubik', sans-serif;"
  )
}

main_view <- function(username){
  wellPanel(
    useShinyjs(),
    extendShinyjs(text = jsReset),
    h1("Clinical Solutions Report Request Form"),
    h3("General Report Information"),
    #pre(sprintf("Hi %s, welcome to the machine!", username)),
    selectInput('db',
                "Choose a Database.",
                list("PK USA (Shelbyville)" = "PK USA (Shelbyville)",
                     "PK TN" = "PK TN",
                     "PK MS" = "PK MS",
                     "Vernet" = "Vernet",
                     "Pacers" = "Pacers",
                     "Shelby Materials" = "Shelby Materials",
                     "Bowen" = "Bowen"
                )),
    checkboxGroupInput('report',
                       "Choose Report Type",
                       list("Risk Report" = "Risk Report",
                            "Compliance Report" = "Compliance Report")),
    dateInput("due_date",
              "Due Date",
	      format = "mm-dd-yyyy"),
    radioButtons("spouses",
                 "Include Spouses?",
                 list("Yes" = "Yes",
                      "No" = "No"),
                 selected = "No"),
    textAreaInput('gen_notes', "Provide any other comments or general notes. Specific instructions are greatly appreciated."),
    
    hidden(tags$div(id = "risk_rep",
                    hr(),
                    h3("Risk Report Information"),
                    radioButtons('inactive',
                                 "Do you want to include inactive participants in summary tables?",
                                 list("Yes" = "Yes",
                                      "No" = "No"),
                                 selected = "No"),
                    radioButtons("risk_compare",
                                 "Would you like to compare between two time periods?",
                                 list("Yes" = "Yes",
                                      "No" = "No"),
                                 selected = "No"),
                    dateRangeInput('curr_dates', "Date Range of Interest", format = "mm-dd-yyyy"),
                    hidden(dateRangeInput('prev_dates', "Previous Date Range for Comparison", format = "mm-dd-yyyy")),
                    radioButtons("combo_tables",
                                 "Would you like the risk tables to contain all combinations of changes of risk level?",
                                 list("Yes" = "Yes",
                                      "No" = "No"),
                                 selected = "No"),
                    fileInput('upload', "Upload Risk Report Document", multiple = FALSE)
    )),
    hidden(tags$div(id = "comp_rep",
                    hr(),
                    h3("Compliance Report Information"),
                    dateRangeInput("compli_dates", "Please provide the date range for the Compliance Report.", format = "mm-dd-yyyy"),
                    hr())), 
    
    actionButton("submit", "Submit"),
    shinyjs::hidden(actionButton('new_req', "Request another Report")),
    actionButton("sign_out", "Sign Out"),
    style = "width: 800px; max-width: 100%; margin: 10px auto; padding: 20px;"
  )
}

check_login <- function(username, password){
  if(username %in% names(secrets) && secrets[[username]] == password){
    return(TRUE)
  }
  FALSE
}

.appv <- reactiveValues(
  logged_in = FALSE,
  risk_part = "",
  comp_part = ""
)


view <- fluidPage(
  uiOutput("secret"),
  style = "font-family: 'Quicksand', sans-serif;"
)

controller <- function(input, output){
  observeEvent(input$pass_chg, {
    showModal(
      modalDialog(
        renderUI({
          isolate(local({result = callModule(pass_ser, id='change_password', secret_file_path = "secrets.R")}))
          div(pass_ui('change_password'))
        })
      )
    )
    
    #source('secrets.R')
    #{js$reset()}
  })
  observeEvent(input$sign_in, {
    if(check_login(tolower(input$username), input$password)){
      .appv$logged_in <- TRUE
    } else {
      shinyalert("Oops!", "Either the username or Password is incorrect.
                 Please Try again.", type = "error")
    }
    })
  
  observeEvent(input$sign_out, {
    .appv$logged_in <- FALSE
  })
  
  observe({
    shinyjs::toggle("risk_rep", condition = "Risk Report" %in% input$report)
    shinyjs::toggle("comp_rep", condition = "Compliance Report" %in% input$report)
    shinyjs::toggle('prev_dates', condition = input$risk_compare == "Yes")
  })
  subj <- "REPORT REQUEST - %s - DUE DATE: %s"
  body <- "<u><b>REPORT REQUEST</b></u><br /><br />DATABASE: %s<br />DUE DATE: %s<br />INCLUDE SPOUSES: %s<br />NOTES: %s<br />REQUESTED BY: %s<br />"
  observe({
    if(any(input$report == "Risk Report")){
      .appv$risk_part = "<u><b>RISK REPORT</b></u><br />Inactive Participants: %s<br />Compare between two time periods: %s<br />Current Date Range: %s to %s<br />Previous Date Range: %s to %s<br />Combination Tables: %s<br /><br />
      "
    }
    })
  
  observe({
    if(any(input$report == "Compliance Report")){
      .appv$comp_part <- "<u><b>COMPLIANCE REPORT</b></u><br />Report Date Range: %s to %s <br />"
    }
  })
  
  observeEvent(input$submit, {
    shinyjs::toggle('new_req')
    shinyjs::hide('submit')
    risk = .appv$risk_part
    comp = .appv$comp_part
    if (input$risk_compare == "No"){
      prev_date1 = "NA"
      prev_date2 = "NA"
    } else {
      prev_date1 = input$prev_dates[1]
      prev_date2 = input$prev_dates[2]
    }
    
    
    bod <- paste("<html>",paste(sprintf(body,
                         input$db,
                         input$due_date,
                         input$spouses,
                         input$gen_notes,
                         input$username),
                 sprintf(risk ,
                         input$inactive,
                         input$risk_compare,
                         input$curr_dates[1],
                         input$curr_dates[2],
                         prev_date1,
                         prev_date2,
                         input$combo_tables),
                 sprintf(comp, input$compli_dates[1],input$compli_dates[2]),
                 collapse = "<br />", sep = "<br />"),"</html>", sep = "", collapse = "")
    # options(httr_oob_default = TRUE, httr_oauth_cache=TRUE)
    # gmailr::use_secret_file('clincol-requests.json')
    # #gmail_auth()
    # gmailr::mime() %>% 
    #   to('sam.harbison1@gmail.com') %>% 
    #   from('clinicalsolutions.requests@gmail.com') %>% 
    #   html_body(((bod))) %>% 
    #   subject(sprintf(subj, input$db, input$due_date)) -> msg
    # gmailr::send_message(msg,user_id = 'me')
    send.mail(from = "clinicalsolutions.requests@gmail.com",
              to = c("sam.harbison1@gmail.com",input$username),
              subject = sprintf(subj, input$db, input$due_date),
              body = bod,
              html = TRUE,
              smtp = list(host.name = "aspmx.l.google.com", port = 25),
              #smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "verygoodanalytics@gmail.com", passwd = "lunchbox211.", ssl = TRUE),
              authenticate = FALSE,
              send = TRUE, 
              attach.files = input$upload$datapath)
    shinyalert("Thank You!", "An email has been sent to Sam containing the information you provided.
               
               If he has any questions, he will be in contact with you.", type = "success" )
  })
  
  observeEvent(input$new_req, {
    {js$reset()}
  })
  
  
  
  output$secret <- renderUI({
    if(.appv$logged_in){
      main_view(input$username)
    } else {
      login_panel()
    }
  })
  
  }

shinyApp(view, controller, options = list(port = 5141))
