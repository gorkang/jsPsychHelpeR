
# TODO: 

  # Button to create password
    # add_pwd_for_pid_data(pid = 22)

 
  # sync_server_local() SHOULD WORK WITH ENCRYPTED CREDENTIALS!!!
    # THEN DELETE .vault/.credentials AND KEEP .vault/data_encrypted.rds

  # WE NEED TO ACCESS sync_server_local() and get_zip()
    # SHOULD THEY BE IN jsPSychMakeR???
    # jsPSychMakeR:::sync_server_local()
    # jsPSychMakeR:::get_zipl()

  # SEE .vault/encrypt_credentials.R TO UNDERSTAND THE PROCESS
  # https://stackoverflow.com/questions/62251357/simple-password-protection-for-part-of-r-shiny-app-using-hashing
  # APPLY THIS TO ALL THE PROJECTS WHERE WE USE THE SERVER PASSWORD



  source("admin/mysql_extract_tables.R")
  source("admin/mysql_helper_functions.R")
  source("R/helper_functions_minimal.R")
  source("R/sync_server_local.R")
  
  PIDs = c(1:30)
      
      
      

# Shiny app ---------------------------------------------------------------

library(shiny)
library(sodium)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("jsPsychR projects"),
  
  tabsetPanel(
    tabPanel("Download data",
             shiny::hr(),
             # HTML(paste0("WD: ", getwd())),
             uiOutput("pid"),
             shiny::passwordInput("project_password", "Enter project Password"),
             shiny::checkboxInput("admin", "Check if you will use the admin password", value = TRUE),
             shiny::actionButton("btn_download", "Download"),
             shiny::hr(),
             shiny::textOutput("TEXT_download")
             
             ),
  
    tabPanel("Show all project's progress", 
             shiny::hr(),
             shiny::passwordInput("txt_password", "Enter admin Password"),
             shiny::actionButton("btn_action", "Show table"),
             shiny::hr(),
             shiny::textOutput("TEXT"),
             shiny::hr(),
             DT::dataTableOutput("mytable"),
             )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Load encrypted data
  data_encrypted <- readRDS(".vault/data_encrypted.rds")
  
  #Public key (its okay to hardcode it here, public keys are designed for sharing)
  key_public = "39 0a 2a dc b7 1b 82 9d 95 d2 cd 98 d6 32 19 ff 91 33 ce 06 80 88 1e 9d 21 ac 0a 19 9d 1a 70 19"
  
  

# REACTIVE VARIABLES ------------------------------------------------------

  #Create a reactive dataframe to store the unencyrpted data
  reactives <- reactiveValues(data_unencrypted = NULL, 
                              LIST_tables = NULL,
                              DF_table_clean = NULL,
                              PASSWORD_OK = FALSE,
                              MESSAGE = NULL,
                              MESSAGE_download = NULL)


# SHOW table BUTTON --------------------------------------------------------

  
  #Observe submit button (runs once when submit button is clicked)
  observeEvent(input$btn_action, {
    
    #Private key
    key_private <- sha256(charToRaw(input$txt_password))
    
    #Check if private key provided is correct
    if(paste(pubkey(key_private), collapse = " ") == key_public) {
      
      msg = "Correct password"
      reactives$MESSAGE = msg
      showNotification(msg)
      cli::cli_alert_info(msg)
      # showNotification("Correct password")
      reactives$PASSWORD_OK = TRUE
      showModal(modalDialog("Fething mySQL tables... it will take around 10 seconds...", footer = NULL))

      #Unencrypt data and make it available elsewhere
      reactives$data_unencrypted <- unserialize(simple_decrypt(data_encrypted, key_private))
      
      # Extract tables using data_unencrypted
      reactives$LIST_tables = extract_tables(list_credentials = reactives$data_unencrypted, serial_parallel = "parallel") # ~7s
      
      DF_user = reactives$LIST_tables$user
      
      reactives$DF_table_clean = DF_user |> 
        dplyr::group_by(id_protocol) |> 
        dplyr::count(status) |> 
        tidyr::pivot_wider(names_from = status, values_from = n)
      
      removeModal()
      
      
      msg = ""
      reactives$MESSAGE = msg
      
      
      #Print data to the console, so we can check it worked
      # print(reactives$data_unencrypted)
      
    } else {

      msg = "Incorrect password"
      reactives$MESSAGE = msg
      showNotification(msg)
      cli::cli_alert_info(msg)
      
      # showNotification("Incorrect password")

      reactives$PASSWORD_OK = FALSE
      
    }
    
  })



# PID select --------------------------------------------------------------

  output$pid = 
    renderUI({
      
      PID = reactives$DF_table_clean$id_protocol |> unique()
      if(is.null(PID)) PID = 1:100
      
      shiny::selectInput(inputId = "pid", label = "projectID",
                         choices = PID, multiple = FALSE, selectize = TRUE)
      
    })
  
  
# DOWNLOAD BUTTON ---------------------------------------------------------

  observeEvent(input$btn_download, {
    
    
    if (input$admin == FALSE) {
      
      file_public_key = glue::glue(".vault/{input$pid}_public_key.txt")
      
      # CHECK public key exists
      if (file.exists(file_public_key)) {
        
        # SIMPLE password check
        key_private <- sha256(charToRaw(input$project_password))
        key_public = readLines(glue::glue(".vault/{input$pid}_public_key.txt"))
        
      } else {
        key_private = sha256(charToRaw(""))
        key_public = ""
        msg = glue::glue("Public key does not exist for project {input$pid}. Ask your admin about it.")
        reactives$MESSAGE_download = msg
      }
      
      #Check if private key provided is correct
        
      } else if(input$admin == TRUE) {
        
        # SIMPLE password check
        key_private <- sha256(charToRaw(input$project_password))
        key_public = readLines(".vault/data_public_key.txt")
        
      }
      
    
    if(paste(pubkey(key_private), collapse = " ") == key_public) {  
      
        # cli::cli_alert_info("Downloading ZIP for project {input$pid}")
        msg = glue::glue("Downloading ZIP for project {input$pid}... it may take a full minute or two...")
        reactives$MESSAGE_download = msg
        showNotification(msg)
        cli::cli_alert_info(msg)
        
        showModal(modalDialog(msg, footer = NULL))
  
        tictoc::tic()
        get_zip(pid = input$pid, what = "data", where = "~/Downloads/", list_credentials = reactives$data_unencrypted)
        tictoc::toc()
        removeModal()
        
        msg = glue::glue("File {input$pid}.zip downloaded in ~/Downloads/")
        reactives$MESSAGE_download = msg
        # showNotification(msg)
        cli::cli_alert_info(msg)
        
      } else {
        
        msg = "Need to enter a valid password to download data"
        reactives$MESSAGE_download = msg
        showNotification(msg)
        cli::cli_alert_info(msg)
        
      }
      
    
    
  })


# OUTPUTS -----------------------------------------------------------------
  
  output$mytable = DT::renderDataTable({
    DT::datatable(
      reactives$DF_table_clean,
      options = list(pageLength = 50, dom = 'tip'), 
      rownames = FALSE)
  })
  
  output$TEXT = renderText({
    reactives$MESSAGE
  })

  output$TEXT_download = renderText({
    reactives$MESSAGE_download
  })
  

# CLEAN UP ----------------------------------------------------------------

  #Remove all data when session ends
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    rm(list = ls())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
