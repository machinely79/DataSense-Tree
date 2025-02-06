
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(
      '
      $(document).ready(function() {
        // Hide the absolutePanel initially
        $("#controls").hide();

        // Toggle the visibility of the absolutePanel when button is clicked
        $("#toggle_button_metrices").click(function() {
          $("#controls").toggle();
        });
      });
      '
    ),
    tags$script(
      '
    $(document).ready(function() {
      // Hide the absolutePanel initially
      $("#controls2").hide();

      // Toggle the visibility of the absolutePanel when button is clicked
      $("#toggle_button_validation").click(function() {
        $("#controls2").toggle();
      });
    });
    '
    ),
  ),
  navbarPage(
    theme = bslib::bs_theme(bootswatch = "superhero"), "DataSense Tree",  
    tabPanel(
             div(img(src='getting_started.svg', height='20', width='20'),"Getting Started"), 

             fluidRow(
               column(width = 6,
                      div(
                        style = "display: flex; justify-content: center; align-items: flex-start; height: 70vh;",
                        tags$img(src = "gif_first_page.gif", width = 500, height ="auto" )
                      )
               ),
               column(width = 6,
                      h3("Getting Started"),
                      glide(
                        height = "500px",
                        screen(
                          div(
                            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                            tags$img(src = "insert_data_screen.png", style = "width: 450px; height: auto; opacity: 0.7;"),
                            HTML("<br>"),
                            h2("Import your data"),
                            p("Import your data and start discovering patterns.")
                          )
                        ),
                        screen(
                          div(
                            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                            tags$img(src = "statistics_table_screen.png", style = "width: 450px; height: auto; opacity: 0.7;"),
                            HTML("<br>"),
                            h2("Basic statistics"),
                            p("Review the basic statistics of your dataset.")
                          )
                        ),
                        screen(
                          div(
                            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                            tags$img(src = "model_train_screen.png", style = "width: 450px; height: auto; opacity: 0.7;"),
                            HTML("<br>"),
                            h2("Training and testing of the model"),
                            p("Interactively select and visualize the best decision tree model.")
                          )
                        ),
                        screen(
                          div(
                            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                            tags$img(src = "insert_pred_data_screen.png", style = "width: 450px; height: auto; opacity: 0.7;"),
                            HTML("<br>"),
                            h2("Import your data for prediction"),
                            p("Import the data for which you want to predict values/labels.")
                          )
                        ),
                        screen(
                          div(
                            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                            tags$img(src = "predictions_table_screen.png", style = "width: 450px; height: auto; opacity: 0.7;"),
                            HTML("<br>"),
                            h2("Get predictions"),
                            p("Review the predictions on the new dataset and export the results.")
                          )
                        ),
                        screen(
                          div(
                            style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%;",
                            tags$img(src = "predictive_scenarios_screen.png", style = "width: 450px; height: auto; opacity: 0.7;"),
                            HTML("<br>"),
                            h2("Test various predictive scenario"),
                            p("Test different implications on the target value/label by interactively changing the values of influential features.")
                          )
                        )
                      )
               )
             )
    ),
    navbarMenu("Data",
               tabPanel(
                 div(img(src='trening_data.svg', height='20', width='20'),"Training Data"), 
                 sidebarLayout(
                   sidebarPanel(
                     p("MODEL TRAINING DATA"),
                     style = "width: 25%; height: 90%; min-height: 90vh; position: fixed; padding-right: 15px; ", 
                     tags$hr(style="border-color: #009999; margin-top: 3px;"),
                     fileInput("file", "Choose File"),
                     
                     shinyjs::hidden(
                       div(p("Data Preview"), style = 'overflow-x: scroll; margin-top: 0; font-size: 14px;',
                           div(class = "preview-table",tableOutput('preview'))
                       )
                     ),
                     br(),
                     uiOutput("delimiter_selection"),
                     br(),
                     fluidRow(
                       column(
                         width = 6,
                         shinyjs::hidden(actionButton("import", "Import"))
                       )
                     ),
                     br(),
                     br(),
                     numericInput("minNumberRow", label = "Display Number of Rows:", value = 15),
                     br(),
                     
                     div(p("Select Columns:", style = "margin-top: 0; font-size: 14px;"),
                         style = "max-height: 200px; min-height: 200px; overflow-y: auto;",
                         uiOutput("columns_select_ui")
                     )
                   ),
                   mainPanel(
                     style = "margin-left: 25%; width: 75%; padding-left: 40px;",
                     tabsetPanel(
                       tabPanel(
                         div(HTML(paste('<img src="table.svg" height="20" width="20" style="vertical-align: middle; margin-right: 4px;" />',
                                        '<span style="vertical-align: middle;">&nbsp;</span>',
                                        "View Training Data"))
                         ),
                         br(),
                         withSpinner(uiOutput("table_1"), type = 2)
                       ),
                       tabPanel(
                         div(HTML(paste('<img src="column_summary.svg" height="25" width="25" style="vertical-align: middle; margin-right: 4px;" />',
                                        '<span style="vertical-align: middle;">&nbsp;</span>',
                                        "Summary Statistics"))
                         ),
                         br(),
                         div(
                           id = "summary_div",
                           style = "border: 2px solid #387a87; padding: 10px; background-color: rgba(123,192,206, 0.7); display: flex; align-items: center;",
                           tags$img(src = "information.svg", width = "30px", height = "30px", style = "margin-right: 10px;"),
                           textOutput("summary_text_output")
                         ),
                         br(),
                         br(),
                         withSpinner(uiOutput("table_2"), type = 2),
                         
                         
                         uiOutput("conditional_elements_help_text") #,
                       ),
                       tabPanel(
                         div(HTML(paste('<img src="nominal.svg" height="25" width="25" style="vertical-align: middle; margin-right: 4px;" />',
                                        '<span style="vertical-align: middle;">&nbsp;</span>',
                                        "Categorical Data"))
                         ),
                         br(),
                         br(),
                         withSpinner(uiOutput("table_3"), type = 2),
                       )
                     )
                   )
                 )
                 
               ),
               tabPanel(
                 div(img(src='prediction.svg', height='20', width='20'),"Prediction Data"), 
                 sidebarLayout(
                   sidebarPanel(
                     p("PREDICTION DATA"),
                     style = "width: 25%; height: 90%; min-height: 90vh; position: fixed; padding-right: 15px;",
                     tags$hr(style="border-color: #009999; margin-top: 3px;"),
                     fileInput("file_pred", "Choose File"),
                     
                     
                     shinyjs::hidden(
                       div(p("Data Preview"), style = 'overflow-x: scroll; margin-top: 0; font-size: 14px;',
                           div(class = "preview-table-pred", tableOutput('preview_pred'))
                       )
                     ),
                     
                     br(),
                     uiOutput("delimiter_selection_pred"),
                     br(),
                     shinyjs::hidden(actionButton("import_pred", "Import")),
                     br(),
                     br(),
                     numericInput("minNumberRow_pred", label = "Display Number of Rows:", value = 15),
                     br(),
                     
                     div(p("Select Columns:", style = "margin-top: 0; font-size: 14px;"),
                         style = "max-height: 200px; min-height: 200px; overflow-y: auto;",
                         uiOutput("columns_select_ui_pred")
                     )
                     
                   ),
                   mainPanel(
                     style = "margin-left: 25%; width: 75%; padding-left: 40px;",
                     tabsetPanel(
                       tabPanel(
                         div(HTML(paste('<img src="table.svg" height="20" width="20" style="vertical-align: middle; margin-right: 4px;" />',
                                        '<span style="vertical-align: middle;">&nbsp;</span>',
                                        "View Prediction Data"))
                         ),
                         
                         br(),
                         withSpinner(uiOutput("table_1_pred"), type = 2)
                         
                       ),
                       tabPanel(
                         div(HTML(paste('<img src="column_summary.svg" height="25" width="25" style="vertical-align: middle; margin-right: 4px;" />',
                                        '<span style="vertical-align: middle;">&nbsp;</span>',
                                        "Summary Statistics"))
                         ),
                         
                         br(),
                         
                         div(
                           id = "summary_div_pred",
                           style = "border: 2px solid #387a87; padding: 10px; background-color: rgba(123,192,206, 0.7); display: flex; align-items: center;",
                           tags$img(src = "information.svg", width = "30px", height = "30px", style = "margin-right: 10px;"),
                           textOutput("summary_text_output_pred")
                         ),
                         
                         
                         br(),
                         br(),
                         withSpinner(uiOutput("table_2_pred"), type = 2),
                         
                         
                         uiOutput("conditional_elements_help_text_pred"),
                         
                         
                       ),
                       tabPanel(
                         div(HTML(paste('<img src="nominal.svg" height="25" width="25" style="vertical-align: middle; margin-right: 4px;" />',
                                        '<span style="vertical-align: middle;">&nbsp;</span>',
                                        "Categorical Data"))
                         ),
                         br(),
                         br(),
                         withSpinner(uiOutput("table_3_pred"), type = 2),
                         
                       )
                     )
                   )
                 )
               )
               
    ),
    navbarMenu("Models",

               tabPanel("Supervised",
                        navlistPanel("",
                                     tabPanel(div(img(src='training.svg', height='30', width='30'),"Training"),
                                              
                                              h5(div(img(src='tuning.svg', height='20', width='20'), "Tuning"), style = "margin-top: 5px;"), 
                                              fluidRow(
                                                column(3,style = "border-left: 1px solid #16525e; border-top: 1px solid #16525e; border-bottom: 1px solid #16525e; border-radius: 5px;",

                                                       selectInput("dependent_var", "Target Feature", NULL),
                                                       br(),
                                                       chooseSliderSkin("Flat", color = "#0a5f70"),
                                                       sliderInput("cp_threshold", "CP Threshold:", min = 0, max = 0.5, step = 0.0005, value = 0.001), 


                                                ),

                                                column(3, style = "border-top: 1px solid #16525e; border-bottom: 1px solid #16525e; border-radius: 5px;",
                                                       sliderInput("max_depth", "Max Depth:", min= 1, max= 30, step =1, value =15),
                                                       sliderInput("minsplit", "Min Split:", min=5, max= 80, step =5, value =  20)

                                                ),

                                                column(3,style = "border-top: 1px solid #16525e; border-bottom: 1px solid #16525e; border-radius: 5px;",
                                                       p(style = "font-size: 14px;","Show Metrics"),
                                                       materialSwitch(
                                                         inputId = "toggle_button_metrices",
                                                         #label = "Metrics Panel",
                                                         value = TRUE,
                                                         status = "primary"
                                                       ),
                                                       p(style = "font-size: 14px;", "Show Cross Validation"),
                                                       materialSwitch(
                                                         inputId = "toggle_button_validation",
                                                         #label = "Metrics Panel",
                                                         value = TRUE,
                                                         status = "primary"
                                                       )
                                                ),
                                                column(3, style = "border-left: 1px solid #16525e; border-right: 1px solid #16525e; border-top: 1px solid #16525e; border-bottom: 1px solid #16525e; border-radius: 5px;",
                                                    
                                                       
                                                       div(
                                                         style = "padding: 10px;  display: flex; align-items: top; color: gray; font-size: 12px;",
                                                         tags$img(src = "information.svg", width = "10px", height = "15px",style = "margin-right: 15px;"),
                                                         textOutput("excluded_feature_info")
                                                         
                                                       ),
                                                       
                                                ) 
                                              ),
                                              br(),
                                                fluidRow(
                                                mainPanel(id = "myMainPanel",
                                                          width=12,
                                                          visNetworkOutput("tree_plot", height = "800px", width = "100%"),

                                                          
                                                          tags$style(HTML('
                                                                        #graphtree_plot > div:nth-child(2)  {
                                                                        background-color: rgb(32, 110, 123) !important;
                                                                        border-radius: 15px !important;
                                                                        color: #cbd5d6 !important;
                                                                        font-family: sans-serif !important;
                                                                        font-size: 12px !important;
                                                                        line-height: 1.42857143 !important;
                                                                        padding: 5px !important;
                                                                        white-space: nowrap !important;
                                                                        }
                                                                      '))
                                                          
                                                )
                                                
                                              ),
                                              
                                              uiOutput("dynamic_content"),
                                              uiOutput("dynamic_content_2")
                                              
                                     ),

                                     tabPanel(div(img(src='predict_model.svg', height='30', width='30'),"Prediction"),

                                              fluidRow(
                                                uiOutput("dynamic_ui_rules")
                                              ),
                                              
                                              br(),
                                              fluidRow(
                                                uiOutput("dynamic_ui_predictions")
                                              ),
                                              
                                     ),
                                     tabPanel(div(img(src='scenarios.svg', height='30', width='30'), "Predictive Scenarios"), 
                                              fluidRow(
                                                column(width = 4, align = "center",
                                                       h6("Feature Scenarios"),
                                                       tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                                       
                                                       br(),
                                                       br(),
                                                       
                                                       tableOutput("selected_values_table"),
                                                ),
                                                column(width = 4, align = "center",
                                                       style = "padding-right: 22px;", 
                                                       h6("Predicted Scenario Result"),
                                                       tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                                       br(),
                                                       br(),
                                                       
                                                       uiOutput("expected_results"),
                                                       
                                                       uiOutput("prescriptiv_plot_UI")
                                                       
                                                ),
                                                column(width = 4, align = "center",   
                                                       h6("Feature Experimentation"),
                                                       tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                                       br(),
                                                       uiOutput("input_generators")
                                                )
                                              )
                                     )
                        )
               )
    ),
  )
)





