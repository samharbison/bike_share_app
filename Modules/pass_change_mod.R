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
  
  code = reactive(NULL)
  print(secrets)
  curr_pass = reactive(NULL)
  observeEvent(input$get_code, {
    ns = session$ns
    
    if(input$email %in% names(secrets)) {
      code <<- stringi::stri_rand_strings(1, 10, '[A-Z]')
      curr_pass <<- secrets[[input$email]]
      print(code)
      shinyalert("A code has been sent to your email containing a code.",
                 "YOu should check your SPAM folder!", type = "info")
      removeModal()
      showModal(
        modalDialog(
          textInput(ns("curr_pass"), "Current Password"),
          textInput(ns("code_input"), "Enter code"),
          actionButton(ns("code_submit"), "Submit Code")
        )
      )
    } else {
      shinyalert("The email you provided does not match any we have on record", "Try again", type = "error")
    }
    
  })
  
  observeEvent(input$code_submit, {
    ns = session$ns
    print(c(input$code_input,code))
    if(input$code_input== code & input$curr_pass == curr_pass){
      removeModal()
      showModal(
        modalDialog(
          textInput(ns("new_pass"), "Enter new Password"),
          actionButton(ns("save_pass"), "Save New Password")
        )
      )
    } else {
      shinyalert("Something is wrong", "Either the password or code you provided is wrong", type = "error")
    }
  })
  
  observeEvent(input$save_pass, {
    ns = session$ns
    removeModal()
    txt = readLines(secret_file_path)
    txt[grep(input$email, txt)] = sprintf(" '%s' = '%s'", input$email, input$new_pass)
    writeLines(txt, secret_file_path)
    shinyalert("Great!", "Your password has changed", type = "success")
    showModal(
      modalDialog(
        tagList(h1("Success! You have changed your password"))
      )
    )
    print(readLines(secret_file_path))
  })
  
  
}


ui <- fluidPage(
  useShinyalert(),
  actionButton("change", "change password")
)

server = function(input,output, seession) {
  observeEvent(input$change, {
    showModal(
      modalDialog(
        renderUI({
          isolate(local({result = callModule(pass_ser, id='test', secret_file_path = "secrets.R")}))
          div(pass_ui('test'))
        })
      )
    )
  })
}
shinyApp(ui, server)

