pass_ui = function(id){
  require(shiny)
  library(shinyalert)
  ns = NS(id)
  tagList(textInput(ns("email"), "Enter email"),
          actionButton(ns("get_code"), "Get Code"))
}

pass_ser = function(input, output, session, secret_file_path){
  require(tidyverse)
  require(shiny)
  library(mailR)
  library(stringi)
  library(shinyalert)
  source(secret_file_path)
  
  jsReset <- "shinyjs.reset = function() {history.go(0)}"
  code = reactive(NULL)
  print(secrets)
  curr_pass = reactive(NULL)
  observeEvent(input$get_code, {
    ns = session$ns
    
    if(tolower(input$email) %in% names(secrets)) {
      code <<- stringi::stri_rand_strings(1, 10, '[A-Z]')
      curr_pass <<- secrets[[tolower(input$email)]]
      print(code)
      send.mail(from = "verygoodanalytics@gmail.com",
                to = tolower(input$email),
                subject = "Reset your password!",
                body = sprintf("<html>Here is the code you need to change your password: %s <br><br> Make sure to cooy and paste the code with no spaces!</html>", code),
                html = TRUE,
                #smtp = list(host.name = "aspmx.l.google.com", port = 25),
                smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "verygoodanalytics@gmail.com", passwd = "lunchbox211.", ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      shinyalert("A code has been sent to you email. You should check your SPAM folder!",
                 "Copy the code and use it in the next window.", type = "info")
      removeModal()
      showModal(
        modalDialog(
          #textInput(ns("curr_pass"), "Current Password"),
          textInput(ns("code_input"), "Enter the code from the email."),
          actionButton(ns("code_submit"), "Submit Code"),
          footer = NULL
        )
      )
    } else {
      shinyalert("The email you provided does not match any we have on record", "Try again", type = "error")
    }
    
  })
  
  observeEvent(input$code_submit, {
    ns = session$ns
    print(c(input$code_input,code))
    if(input$code_input== code){
      removeModal()
      showModal(
        modalDialog(
          passwordInput(ns("new_pass"), "Enter New Password"),
          passwordInput(ns("confirm_pass"), "Confirm New Password"),
          actionButton(ns("save_pass"), "Save New Password")
        )
      )
    } else {
      shinyalert("Something is wrong", "The code you provided is wrong", type = "error")
    }
  })
  
  observeEvent(input$save_pass, {
    ns = session$ns
    if (input$new_pass == input$confirm_pass) {
      removeModal()
      filcon = file(secret_file_path)
      txt = readLines(con = filcon)
      if (strsplit(txt[grep(tolower(input$email), txt)], "'")[[1]][length(strsplit(txt[grep(tolower(input$email), txt)], "'")[[1]])] == ","){
        txt[grep(tolower(input$email), txt)] = sprintf(" '%s' = '%s',", tolower(input$email), input$new_pass)
      } else {
      txt[grep(tolower(input$email), txt)] = sprintf(" '%s' = '%s'", tolower(input$email), input$new_pass)
      }
      writeLines(txt, con = filcon)
      close(filcon)
      #shinyalert("Sick!", "Your password has changed", type = "success")
      showModal(modalDialog(tagList(
        h1("Success! You have changed your password")
      ),
      actionButton(ns("done"), "Done"),
      footer = NULL))
      
      #print(readLines(secret_file_path))
    } else {
      shinyalert("Woooooah",
                 "The two passwords you just entered don't match",
                 type = "alert")
    }
  })
  observeEvent(input$done, {
    ns = session$ns
    source(secret_file_path)
    print(secrets)
    removeModal()
  })
  
  
}
