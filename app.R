#
# This is a Shiny web application. The purpose is to help Adventist Health staff
# with inputting breast cancer data to the Gilead/ICF Excel template.


library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(DT)
library(dplyr)
library(shinyjs)
library(janitor)
library(stringr)
library(openxlsx)
options(shiny.maxRequestSize = 10 * 1024^2)
ui <-  page_navbar(
   title = "Breast Cancer Data Cleaning",
   id = 'nav',
   theme = bs_theme(preset = "cosmo"),
   fillable = 'Drilldown',
   nav_panel('',
             div(class = 'card-container',
                 layout_column_wrap(heights_equal = 'row',max_height = '40%',
                   card(
                      card_header('Upload the UIDs here'),
                      card_body(fileInput("file_filler", label = '',
                                accept = c(
                                   ".xlsx",
                                   ".xls"
                                ),
                                width = '100%'
                            ) # end fileInput
                      ), # end card body,
                      uiOutput('filler_card'),
                      actionButton('submit_filler','Submit',width = '60%'),
                      actionButton('clear_filler','Clear', width = '60%'),
                      textInput("matching_name", "Column used to match data (should be in lowercase):", value = "uid",width = '100%'),
                   ),  # end card 1
                
                card(uiOutput("filler_data"))
                 )
             ), # end div wrap card
             div(class = 'card-container',
             layout_column_wrap(heights_equal = 'row',max_height = '40%',
                     card(
                       card_header('Upload the template here'),
                       card_body(fileInput("file_template", label = '',
                                  accept = c(".xlsx",".xls"),
                                  width = '100%'
                                   ) # end fileInput
                                  ), # end card body,
                           uiOutput('template_data'),
                           actionButton('submit_template','Submit',width = '60%'),
                           actionButton('clear_template','Clear',width = '60%')
                                ), # end card 2
                card(uiOutput("template_data"))
             )
             ),
              # div(class = 'card-container',
              #    layout_column_wrap(heights_equal = 'row',
              #                       card(
              #                          card_header('ID Variable'),
              #                          card_body(
              #                             selectizeInput('filler_vars',
              #                                            label = '',
              #                                            selected = NULL, 
              #                                            choices = NULL)
              #                          ) #end card body
              #                       ),
              #                       card(
              #                          card_header('Variables to transfer to template'),
              #                          card_body(
              #                             selectizeInput('transfer_vars',
              #                                            label = '',
              #                                            selected = NULL,
              #                                            choices = NULL,
              #                                            multiple = TRUE
              #                                            )
              #                          ) #end card body
              #                       ),
              #                       textInput("filename", "Enter name for the new file (without extension)", value = "updated_file"),
              #                       actionButton('upload_patients','Upload new patient data')
              #    ) # end layout_column_wrap
             # ), # end second layout_sidebar
             card(
                #card_header('Download the new data'),
                downloadButton("download", class = "btn-block")
             )
   ) # end nav_panel
) # end page_navbar

server <- function(input,output,session){
   # Reactive expression to read the Excel file
   filler_data <- reactiveVal(NULL)
   
   observeEvent(input$submit_filler,{
      req(input$file_filler)
      
      file1 <- input$file_filler$datapath
      file_ext <- tools::file_ext(input$file_filler$name)
      
      tryCatch({
         filler_data(switch(file_ext,
                     "xlsx" = {
                      read_excel(file1, skip = 0) #skip = as.numeric(input$header_row) - 1)
                     },
                     "RDS" = readRDS(file1),
                     stop("Unsupported file type")
         ))
      }, error = function(e) {
         showNotification("Error reading the file. Please make sure it's a valid Excel.", type = "error")
         filler_data(NULL)
      })
   })
   
   observeEvent(input$clear_filler,{
      filler_data(NULL)
      unlink(input$file_filler$datapath)
      reset(id = '')
   })
   
   # observeEvent(input$submit_filler,{
   #    updateVarSelectizeInput(session = session,
   #                            'filler_vars',
   #                            data = filler_data(),
   #                            selected = 'uid'
   #                            )
   # 
   # })
   
   # Clean filler data names----------------------------------------------------------
   tidied_filler <- reactive({
     req(filler_data()) 
   
     out <- filler_data()|>
            clean_names()|>
        rename(UIN = uid)
      
      if('age' %in% names(out)){
         out <- mutate(out, age = str_remove_all(age,'[[:alpha:]]+')|>str_squish())
      }
      
      out
   })
   
   # Reactive expression to read the Excel file
   template_data <- reactiveVal(NULL)
   
   observeEvent(input$submit_template,{
      req(input$file_template)
      
      file_template <- input$file_template$datapath
      file_ext_template <- tools::file_ext(input$file_template$name)
      
      tryCatch({
         template_data(switch(file_ext_template,
                            "xlsx" = {
                               read_excel(file_template, skip = 3) |>
                                  mutate(across(everything(), \(x)as.character(x)))#skip = as.numeric(input$header_row) - 1)
                            },
                            "RDS" = readRDS(file_template),
                            stop("Unsupported file type")
         ))
      }, error = function(e) {
         showNotification("Error reading the file. Please make sure it's a valid Excel.", type = "error")
         filler_data(NULL)
      })
   })
   
   observeEvent(input$clear_template,{
      template_data(NULL)
      unlink(input$file_template$datapath)
      reset(id = '')
   })
   # observeEvent(input$submit_filler,{
   #    updateVarSelectizeInput(session = session,
   #                            'transfer_vars',
   #                            data = filler_data()
   #    )
   #    
   # })
    output$filler_data <- renderUI({
      req(tidied_filler())
      
      head(tidied_filler(),n=3) |>
          datatable(rownames=FALSE)
      
   })
    
    joined_data <- reactive({
      req(tidied_filler(),template_data()) 
       tmp_filler <- select(tidied_filler(),UIN, age)
       tmp <- rows_insert(template_data(),tmp_filler, by = 'UIN')
       # |>
       #    filter(UIN %in% tidied_filler()$UIN)
       tmp
    })
    
    output$template_data <- renderUI({
       req(joined_data())
       
      head(joined_data()) |>
          datatable(rownames=FALSE)
     #     card(
      #       full_screen = TRUE,
       #      card_header('Matching names', class = "bg-dark")
           #  card_body(paste0(intersect(names(template_data()),names(filler_data())),collapse = ', '), min_height = 150)
          
    })
    
    
    
    # Download -------------------------------------------------------
    output$download <- downloadHandler(
          filename = function() {
             paste0("data-", Sys.Date(), ".xlsx")
          },
          content = function(file) {
             wb <- createWorkbook()
             addWorksheet(wb, "Sheet1")
             writeData(wb, "Sheet1", joined_data())
             saveWorkbook(wb, file, overwrite = TRUE)
          },
          contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
       )
}
# Run the application 
shinyApp(ui = ui, server = server)
