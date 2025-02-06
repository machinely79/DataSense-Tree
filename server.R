

shinyServer(function(input, output, session) {
  #Get source code
  source("D:/MyData/Application/scripts/summary_tables.R") 
  source("D:/MyData/Application/scripts/rpart_functions.R") 
  
  
  # Data formatting
  format_data <- function(df) {
    df <- un_factor(df)  # Un-factor strings
    df <- find_and_transform_dates(df)  # Transform columns to dates
    df <- find_and_transform_numerics(df)  # Transform columns to numeric

    # Iterate through columns and check for binary values
    as.data.frame(df)  #from data.table to data.frame
    for (col in names(df)) {
      if (all(df[[col]] %in% c(0, 1))) {
        df[[col]] <- as.logical(df[[col]])  # Convert 0 to FALSE and 1 to TRUE
      }
    }
    return(df)
  }
  
  
  insert_clicked <- reactiveVal(FALSE)  #reactiveVal for insert click     
  
  insert_clicked_pred <- reactiveVal(FALSE)
  

  #Reactive function to preview the first 5 rows of an uploaded CSV or Excel file
  data_preview <- reactive({
    req(input$file)                                         
    if (grepl("\\.csv$", input$file$name)) {
      read.csv(input$file$datapath, nrows = 5, sep = input$delimiter)
    } else if (grepl("\\.xls$|\\.xlsx$", input$file$name)) {
      read_excel(input$file$datapath, sheet = 1, range = "A1:Z5", col_names = TRUE)
    } else {
      as.data.frame()
    }
  })
  
  #Reactive function to preview the first 5 rows of an uploaded CSV or Excel file
  data_preview_pred <- reactive({               
    req(input$file_pred)                                         
    if (grepl("\\.csv$", input$file_pred$name)) {
      read.csv(input$file_pred$datapath, nrows = 5, sep = input$delimiter_pred)
    } else if (grepl("\\.xls$|\\.xlsx$", input$file_pred$name)) {
      read_excel(input$file_pred$datapath, sheet = 1, range = "A1:Z5", col_names = TRUE)
    } else {
      as.data.frame()
    }
  })
  
  

  # Render a UI element (selectInput) for delimiter selection if a CSV file is uploaded
  output$delimiter_selection <- renderUI({
    if (!is.null(input$file) && grepl("\\.csv$", input$file$name)) {
      selectInput("delimiter", "Choose Delimiter", c(",", ";", "\t", "|"))    
    }
  })
  
  # Render a UI element (selectInput) for delimiter selection if a CSV file is uploaded
  output$delimiter_selection_pred <- renderUI({     
    if (!is.null(input$file_pred) && grepl("\\.csv$", input$file_pred$name)) {
      selectInput("delimiter_pred", "Choose Delimiter", c(",", ";", "\t", "|"))    
    }
  })
  

  # Render a UI element (selectInput) to select multiple columns from the uploaded data when a button is clicked
  output$columns_select_ui <- renderUI({
    if (insert_clicked()) {
      selectInput(                                     
        "columns_select", label = NULL,
        choices = colnames(data()),
        selected = colnames(data()),
        multiple = TRUE
      )
    }
  })
  
  # Render a UI element (selectInput) to select multiple columns from the uploaded data when a button is clicked
  output$columns_select_ui_pred <- renderUI({  # THIS
    if (insert_clicked_pred()) {
      selectInput(                                     
        "columns_select_pred", label = NULL,
        choices = colnames(data_pred()),
        selected = colnames(data_pred()),
        multiple = TRUE
      )
    }
  })
  
  # Observe the 'import' event and enable column selection if data is imported, otherwise show a warning notification
  observeEvent(input$import, {
    if (!is.null(input$file) && !is.null(input$file$name) && !is.null(data_preview())) {
      insert_clicked(TRUE)
    }else {
      showNotification("Import the data first!", type = "warning")
      
    }
    
  })
  
  # Observe the 'import' event and enable column selection if data is imported, otherwise show a warning notification
  observeEvent(input$import_pred, { 
    if (!is.null(input$file_pred) && !is.null(input$file_pred$name) && !is.null(data_preview_pred())) {
      insert_clicked_pred(TRUE)
    }else {
      showNotification("Import the data for prediction first!", type = "warning")
      
    }
    
  })
  
  
 
  
  #preview table
  output$preview <- renderUI({
    if (!is.null(input$file) && !insert_clicked()) {
      data <- data_preview()
      if (is.data.frame(data) && ncol(data) > 0) {
        renderTable(data, colnames = TRUE, align = 'l', spacing = 'xs', hover = TRUE, bordered = TRUE, style = "white-space: pre-wrap;",digits = 1)
      } else {
        div("No data available.")
      }
    }
  })
  
  
  #preview table
  output$preview_pred <- renderUI({       
    if (!is.null(input$file_pred) && !insert_clicked_pred()) {
      data_pred <- data_preview_pred()
      if (is.data.frame(data_pred) && ncol(data_pred) > 0) {
        renderTable(data_pred, colnames = TRUE, align = 'l', spacing = 'xs', hover = TRUE, bordered = TRUE, style = "white-space: pre-wrap;")
      } else {
        div("No data available.")
      }
    }
  })
  

  # Reactively load and format the data when the 'import' button is clicked, based on file type (CSV or Excel)
  data <- eventReactive(input$import, {
    req(input$file)
    if (grepl("\\.csv$", input$file$name)) {
      data <- read.csv(input$file$datapath, sep = input$delimiter)
    } else if (grepl("\\.xls$|\\.xlsx$", input$file$name)) {
      data <- read_excel(input$file$datapath, sheet = 1)
    } else {
      data <- NULL
    }
    
    if (!is.null(data)) {
      insert_clicked(TRUE)
      data <- format_data(data)
    }
    
    data
    
  })
  

  # Reactively load and format the data when the 'import' button is clicked, based on file type (CSV or Excel)
  data_pred <- eventReactive(input$import_pred, {
    req(input$file_pred)
    if (grepl("\\.csv$", input$file_pred$name)) {
      data_pred <- read.csv(input$file_pred$datapath, sep = input$delimiter_pred)
    } else if (grepl("\\.xls$|\\.xlsx$", input$file_pred$name)) {
      data_pred <- read_excel(input$file_pred$datapath, sheet = 1)
    } else {
      data_pred <- NULL
    }
    
    if (!is.null(data_pred)) {
      insert_clicked_pred(TRUE)
      data_pred <- format_data(data_pred)  
      
    }
    
    data_pred
    
  })
  
  #Reactively validate and return the minimum number of rows based on user input, ensuring it's at least 1
  min_rows <- reactive({
    validate(
      need(!is.null(input$minNumberRow), "Minimum number of rows must be at least 1. !"),
      need(as.numeric(input$minNumberRow) >= 1, "Minimum number of rows must be at least 1.")
    )

    return(as.numeric(input$minNumberRow))
  })
  
  #Reactively validate and return the minimum number of rows based on user input, ensuring it's at least 1
  min_rows_pred <- reactive({   
    validate(
      need(!is.null(input$minNumberRow_pred), "Minimum number of rows must be at least 1. !"),
      need(as.numeric(input$minNumberRow_pred) >= 1, "Minimum number of rows must be at least 1.")
    )
    
    return(as.numeric(input$minNumberRow_pred))
  })
  
  
  
  # Reactively return the selected columns from the user input
  selected_columns <- reactive({
    input$columns_select
  })
  

  # Reactively return the selected columns from the user input
  selected_columns_pred <- reactive({ 
    input$columns_select_pred
  })
  
  
  # Render a dynamic table based on selected columns and user input, with filtering and sorting options; display the entire dataset if no columns are selected
  output$table_1 <- renderUI({
        selected_cols <- selected_columns()
    
    if (insert_clicked()) {
      if (!is.null(data()) && ncol(data()) > 0) {
        if (!is.null(selected_cols) && length(selected_cols) > 0) {
          valid_selected_columns <- intersect(selected_cols, colnames(data()))
          if (length(valid_selected_columns) > 0) {
            reactable(
              data()[, ..valid_selected_columns, with = FALSE], 
              defaultPageSize = as.numeric(min_rows()),
              filterable = TRUE,
              striped = TRUE,
              searchable = TRUE,
              showSortable = TRUE,
              resizable = TRUE,
              wrap = FALSE,
              bordered = TRUE,
              minRows = 10,
              highlight = TRUE,
              style = list(
                #backgroundColor = "#a3bcc0",
                #color = "#000000"     
              )
            )
          } else {
            div("No valid columns selected.")
          }
        } else {
          reactable(
            data(),
            defaultPageSize = as.numeric(min_rows()),
            filterable = TRUE,
            searchable = TRUE,
            showSortable = TRUE,
            resizable = TRUE,
            wrap = FALSE,
            bordered = TRUE,
            minRows = 10,
            highlight = TRUE,
            style = list(
              #backgroundColor = "#a3bcc0",
              #color = "#000000"     
            )
          )
        }
      } else {
        
        div(
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; font-size: 25px; color: #a4b3b6;",
          tags$img(src = "scientist.svg", style = "width: 700px; height: auto; opacity: 0.3;"),
          HTML("<br>"),
          p("Insert your data and start discovering patterns in a beautifully simple way!")   
        )
      }
    } else {
      div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; font-size: 25px; color: #a4b3b6;",
        tags$img(src = "scientist.svg", style = "width: 700px; height: auto; opacity: 0.3;"),
        HTML("<br>"),
        p("Insert your data and start discovering patterns in a beautifully simple way!")
      )
    }
  })
  
  
  
  # Render a dynamic table for displaying prediction data
  # Based on user input, display selected columns or the entire dataset if no columns are selected
  # Provides filtering, searching, and sorting functionality
  # If no data is available, show a message prompting the user to insert new data for prediction
  # If no columns are selected, display the entire dataset with the specified options
  output$table_1_pred <- renderUI({       
    selected_cols_pred <- selected_columns_pred()
    if (insert_clicked_pred()) {
      if (!is.null(data_pred()) && ncol(data_pred()) > 0) {
        if (!is.null(selected_cols_pred) && length(selected_cols_pred) > 0) {
          valid_selected_columns <- intersect(selected_cols_pred, colnames(data_pred()))
          
          if (length(valid_selected_columns) > 0) {
            reactable(
              data_pred()[, ..valid_selected_columns, with = FALSE], 
              defaultPageSize = as.numeric(min_rows_pred()),
              filterable = TRUE,
              striped = TRUE,
              searchable = TRUE,
              showSortable = TRUE,
              resizable = TRUE,
              wrap = FALSE,
              bordered = TRUE,
              minRows = 10,
              highlight = TRUE,
              style = list(
                #backgroundColor = "#051524",
                #stripedColor = "#051524",
                #highlightColor = "#051524"
                #color = "#000000"
              )
            )
          } else {
            div("No valid columns selected.")
          }
        } else {
          reactable(
            data_pred(),
            defaultPageSize = as.numeric(min_rows_pred()),
            filterable = TRUE,
            searchable = TRUE,
            showSortable = TRUE,
            resizable = TRUE,
            wrap = FALSE,
            bordered = TRUE,
            minRows = 10,
            highlight = TRUE,
            style = list(
              #backgroundColor = "#051524",
              #stripedColor = "#051524",
              #highlightColor = "#051524"
              #color = "#000000"    
            )
          )
        }
      } else {
        
        div(
          style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; font-size: 25px; color: #a4b3b6;",
          tags$img(src = "machine-learning.svg", style = "width: 500px; height: auto; opacity: 0.2;"),
          HTML("<br>"),
          p("Insert your new data and predict the target feature before it happens!")
        )
      }
    } else {
      div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; height: 100%; font-size: 25px; color: #a4b3b6;",
        tags$img(src = "machine-learning.svg", style = "width: 500px; height: auto; opacity: 0.2;"),
        HTML("<br>"),
        p("Insert your new data and predict the target feature before it happens!")
      )
    }
  })
  
  
  # Render a summary table based on selected columns, using the 'generate_summary_table' function sourced from an external file
  # If no columns or data are selected, display a message indicating no data is available
  output$table_2 <- renderUI({
    source("D:/MyData/Application/scripts/summary_tables.R") 
    selected_cols <- selected_columns() 
    if (!is.null(data()) && ncol(data()) > 0) {
       generate_summary_table(data(), selected_cols)  
    } else {
      div("No data available.")
    }

  })
  
  
  # Render a summary table based on selected columns, using the 'generate_summary_table' function sourced from an external file
  # If no columns or data are selected, display a message indicating no data is available
  output$table_2_pred <- renderUI({  
    selected_cols_pred <- selected_columns_pred()
    if (!is.null(data_pred()) && ncol(data_pred()) > 0) {
      generate_summary_table(data_pred(), selected_cols_pred)  
    } else {
      div("No data available.")
    }
    
  })
  
  
  # Render help text conditionally, displaying a note about numerical data calculations if data is available
  # Otherwise, show an empty string if no data is present
  output$conditional_elements_help_text <- renderUI({
      if (!is.null(data()) && ncol(data()) > 0) {
        helpText("Note: - Mean Value, Min Value and Max Value are calculated only for numerical data")
      } else {
        
        ""
      }
  })
  
  
  # Render help text conditionally, displaying a note about numerical data calculations if data is available
  # Otherwise, show an empty string if no data is present
  output$conditional_elements_help_text_pred <- renderUI({   
    if (!is.null(data_pred()) && ncol(data_pred()) > 0) {
      helpText("Note: - Mean Value, Min Value and Max Value are calculated only for numerical data")
    } else {
      
      ""
    }
  })
  
  
  # Toggle the visibility of the summary section based on the presence of data; show it if data is available, otherwise hide it
  observe({
    shinyjs::toggle(id = "summary_div", condition = !is.null(data()) && ncol(data()) > 0)
  })
  
  # Toggle the visibility of the summary section based on the presence of data; show it if data is available, otherwise hide it
  observe({                                      
    shinyjs::toggle(id = "summary_div_pred", condition = !is.null(data_pred()) && ncol(data_pred()) > 0)
  })
  
  
  
  
  # Render summary text based on the selected columns and data available
  # If data is present, count the column types and generate a summary; otherwise, display an empty string
  output$summary_text_output <- renderText({
    if (!is.null(data()) && ncol(data()) > 0) {
      
      selected_cols <- selected_columns() 
      
      new_columns <- data()[, ..selected_cols, drop = FALSE]
      
      column_counts <- count_column_types(new_columns)
      generate_summary_text(column_counts)
    } else {
      ""
    }
  })
  
  
  # Render summary text based on the selected columns and data available
  # If data is present, count the column types and generate a summary; otherwise, display an empty string
  output$summary_text_output_pred <- renderText({    
    if (!is.null(data_pred()) && ncol(data_pred()) > 0) {
      
      selected_cols_pred <- selected_columns_pred()
      
      new_columns <- data_pred()[, ..selected_cols_pred, drop = FALSE]
      
      column_counts <- count_column_types(new_columns)
      generate_summary_text(column_counts)
    } else {
      ""
    }
  })
  
  

  # Render a table based on filtered categorical data; display an alert if no categorical columns are selected or available
  # If data is available, filter the categorical columns and create a reactable table; otherwise, show a "No data available" message
  output$table_3 <- renderUI({
    source("D:/MyData/Application/scripts/summary_tables.R") 
    selected_cols <- selected_columns() 
    if (!is.null(data()) && ncol(data()) > 0) {
      filtered_data <- filter_categorical_columns(data(), selected_cols)
      if (!is.null(filtered_data) && ncol(filtered_data) > 0) {
        create_reactable(filtered_data)
      } else {
        div(
          id = "custom-alert",
          "No categorical data available. Select categorical data type columns"
        )
      }
    }else {
      div("No data available.")
    }
  })
  
  
  # Render a table based on filtered categorical data; display an alert if no categorical columns are selected or available
  # If data is available, filter the categorical columns and create a reactable table; otherwise, show a "No data available" message
  output$table_3_pred <- renderUI({   
    selected_cols_pred <- selected_columns_pred()
    if (!is.null(data_pred()) && ncol(data_pred()) > 0) {
      filtered_data <- filter_categorical_columns(data_pred(), selected_cols_pred)
      if (!is.null(filtered_data) && ncol(filtered_data) > 0) {
        create_reactable(filtered_data)
      } else {
        div(
          id = "custom-alert",
          "No categorical data available. Select categorical data type columns"
        )
      }
    }else {
      div("No data available.")
    }
  })
  
  

  # Observe changes in data and update the select input for the dependent variable with the names of selected columns
  # When data is available, the available column names are set as choices for the dependent variable selection
  observe({
    if (!is.null(data())) {
      selected_cols <- selected_columns()
      temp_data<-data()
      new_columns <- temp_data[, ..selected_cols, drop = FALSE]
      choices <- names(new_columns)
      updateSelectInput(session, "dependent_var", choices = choices)
    }
  })
  

  # Reactively return the value of the complexity parameter (cp_threshold) input
  cp_threshold <- reactive({
    input$cp_threshold
      })
  
  # Reactively return the value of the maximum depth (max_depth) input
  max_depth <-reactive({
    input$max_depth
  })

  # Reactively return the value of the minimum split (minsplit) input
  minsplit_value <-reactive({
    input$minsplit
  })



  
  # Reactively prepare and split data into training and test sets based on the selected dependent variable
  # Exclude irrelevant features and use 'caret' to create a data partition for training (70%) and testing (30%)
  # Return the training data, test data, prediction type, dependent variable, and excluded features
  split_data <- reactive({
    dependent_col <- input$dependent_var
    selected_cols <- selected_columns()
    data_processed <- prepare_data_for_rpart(data())
    excluded_feature<-data_processed$excluded_info
    data_processed <- data_processed$data[, c(selected_cols), drop = FALSE]
    
    set.seed(123)  
    train_index <- createDataPartition(data_processed[[dependent_col]], p = 0.7, list = FALSE)
    train_data <- data_processed[train_index, ]
    test_data <- data_processed[-train_index, ]
    dependent_col_dt_obj <- train_data[[input$dependent_var]]
    prediction_type <- get_prediction_type(dependent_col_dt_obj)
    
    return(list(train_data = train_data, test_data = test_data, 
                prediction_type=prediction_type, dependent_col_dt_obj = dependent_col_dt_obj, 
                excluded_feature = excluded_feature))
  })
  
  
  # Reactively create a decision tree model using the selected dependent variable and input parameters
  # Split the data into training and test sets, then generate predictions and assess variable importance
  # Return the decision tree model, optimal complexity parameter, predictions, and selected features for prescriptive analysis
  tree_data <- reactive({
    if (is.null(data())) return(NULL)
    if (is.null(input$dependent_var)) return(NULL)

    dependent_col <- input$dependent_var
    formula <- as.formula(paste(dependent_col, "~ ."))
    data_split <- split_data()
    train_data <- data_split$train_data
    test_data <- data_split$test_data
    prediction_type <- data_split$prediction_type
    dependent_col_dt_obj<-data_split$dependent_col_dt_obj
    
    cp_threshold_value <- cp_threshold()
    max_depth_value <- max_depth()
    minsplit_value <- minsplit_value()

    method <- NULL
    parms <- NULL
    
    status_dep_var<-analyze_dependent_column(dependent_col_dt_obj)
    
    method<-status_dep_var$method
    parms<-status_dep_var$parms

    tree_model <- create_decision_tree(train_data, formula, method, parms, cp_threshold_value,
                                 max_depth_value, minsplit_value)
    
    optimal_cp <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
    
    optimal_cp <- round(as.numeric(optimal_cp), 4)
    
    predictions <- generate_predictions(tree_model, train_data, test_data, prediction_type, dependent_col)
    
    get_variable_importance<-tree_model$variable.importance
    data_importance_var <- data.frame(Importance = get_variable_importance)
    data_importance_var$Feature <- rownames(data_importance_var)
    rownames(data_importance_var) <- NULL

    sorted_importance <- data_importance_var[order(-data_importance_var$Importance), ]

    top_10_importance <- head(sorted_importance, n = 10)
    
    filter_columns <- intersect(names(train_data), top_10_importance$Feature)

    selected_prescript_data <- train_data[, filter_columns]

    return(list(tree_model = tree_model, optimal_cp=optimal_cp, predictions = predictions, 
                selected_prescript_data=selected_prescript_data))

  })
  


  # Render a decision tree plot using the 'visNetwork' library
  # Generate color data based on the dependent variable and visualize the tree model
  # If an error occurs during visualization, show a notification to adjust slider values for proper model generation
  output$tree_plot <- renderVisNetwork({
    train_data <- split_data()$train_data
    model_type <- tree_data()$predictions$model_type
    dependent_col <- input$dependent_var
    
    colorRange <- c("#eaccb7", "#8f552b") 
    colorYData <- generateColorYData(train_data, dependent_col, colorRange)
    
    tryCatch({
      decision_tree <- tree_data()
      
      if (!is.null(decision_tree)) {
        model_tree <- decision_tree$tree_model  
        tree_visualization(model_tree, model_type, colorYData)
      }
    }, error = function(e) {
      showNotification("To generate the model, adjust the sliders to other value!", type = "warning")
    })
   })



  # Render information about excluded features from the data split
  # If excluded features exist, generate and return a descriptive text about them
  output$excluded_feature_info <- renderText({
    split_data()$excluded_feature
    count <-split_data()$excluded_feature$count
    features_name <-split_data()$excluded_feature$variables
    
    if (!is.null(count)) {
      generated_text <- generate_text_excluded_feature(count, features_name)
  
    } else {
      ""
    }
    return(generated_text)
    
  })



  #Accuracy result on Training Data classification
  output$train_accuracy_output_class <- renderText({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    paste("Accuracy:", round(as.numeric(accuracy_results[["train_accuracy"]][["Accuracy"]]), 1), "%")
    
  })
  
  
  # gauge for train data classification
  output$gauge_train_class = renderGauge({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    gauge_value <- accuracy_results[["train_accuracy"]][["Accuracy"]]
        gauge(value = gauge_value,
          min = 0,
          max = 100,
          sectors = gaugeSectors(success = c(51, 100), warning = c(0, 51),
                                 colors = c("#4dffa6", "#99ffcc")),  
          label = "Accuracy %",
    )
  })
  
  
  #Accuracy result on Test Data classification
  output$test_accuracy_output_class <- renderText({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    paste("Accuracy:", round(as.numeric(accuracy_results[["test_accuracy"]][["Accuracy"]]), 1), "%")
  })
  

  # gauge for test data classification
  output$gauge_test_class = renderGauge({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    gauge_value <- accuracy_results[["test_accuracy"]][["Accuracy"]]
    gauge(value = gauge_value,
          min = 0,
          max = 100,
          sectors = gaugeSectors(success = c(51, 100), warning = c(0, 51),
                                 colors = c("#4dffa6", "#99ffcc")),
          label = "Accuracy %",
    )
  })
  

  #Kappa result on Train Data classification
  output$train_kappa_output_class <- renderText({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    paste("Kappa:", round(as.numeric(accuracy_results$train_kappa) , 1), "%")
  })
  
  #Kappa result on Test Data classification
  output$test_kappa_output_class <- renderText({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    paste("Kappa:", round(as.numeric(accuracy_results$test_kappa), 1), "%")
  })
  

  #Accuracy result on Training Data Regression
  output$train_accuracy_output_regress <- renderText({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    paste("Accuracy: ", round(as.numeric(accuracy_results$train_accuracy), 1), "R²")
  })
  
  
  # gauge for train data Regression
  output$gauge_train_regress = renderGauge({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    gauge_value <- accuracy_results$train_accuracy
    gauge(value = gauge_value,
          min = 0,
          max = 1,
          sectors = gaugeSectors(success = c(0.51, 1), warning = c(0.3, 0.51), danger = c(0.0, 0.3),
                                 colors = c( "#4dffa6", "#99ffcc", "#ccffe6")),
          label = "Accuracy R²",
    )
  })


  #Accuracy result on test Data Regression
  output$test_accuracy_output_regress <- renderText({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    paste("Accuracy: ", round(as.numeric(accuracy_results$test_accuracy), 1), "R²")
  })
  
  
  # gauge for test data Regression
  output$gauge_test_regress = renderGauge({
    tree_results <- tree_data()
    accuracy_results <- tree_results$predictions
    gauge_value <- accuracy_results$test_accuracy
    gauge(value = gauge_value,
          min = 0,
          max = 1,
          sectors = gaugeSectors(success = c(0.51, 1), warning = c(0.3, 0.51), danger = c(0.0, 0.3),
                                 colors = c( "#4dffa6", "#99ffcc", "#ccffe6")),
          label = "Accuracy R²",
    )
  })
  
  
  # Render text displaying the number of records in the training dataset for regression or classification model
  # Access the number of training records from the model's predictions and display it
  output$train_N_output_regress_class <- renderText({
    tree_results <- tree_data()
    N_record <- tree_results$predictions
    paste("Training Data: n= ", round(as.numeric(N_record$N_train), 0), "Records")
  })
  
  
  # Render text displaying the number of records in the test dataset for regression or classification model
  # Access the number of test records from the model's predictions and display it
  output$test_N_output_regress_class <- renderText({
    tree_results <- tree_data()
    N_record <- tree_results$predictions
    paste("Test Data: n= ", round(as.numeric(N_record$N_test), 0), "Records")
  })
  
  
  # Render text displaying the Mean Absolute Error (MAE) for the training dataset in a regression model
  # Access the MAE result from the model's predictions and round it for display
  output$train_mae_output_regress <- renderText({
    tree_results <- tree_data()
    mae_resault <- tree_results$predictions
    paste("Mean Absolute Error: ", round(as.numeric(mae_resault$train_mae), 1))
  })
  
  # Render text displaying the Mean Absolute Error (MAE) for the test dataset in a regression model
  # Access the MAE result from the model's predictions and round it for display
  output$test_mae_output_regress <- renderText({
    tree_results <- tree_data()
    mae_resault <- tree_results$predictions
    paste("Mean Absolute Error: ", round(as.numeric(mae_resault$test_mae), 1))
  })
  

  
  # Render text displaying the Mean Squared Error (MSE) for the training dataset in a regression model
  # Access the MSE result from the model's predictions and round it for display
  output$train_mse_output_regress <- renderText({
    tree_results <- tree_data()
    mse_resault <- tree_results$predictions
    paste("Mean Squared Error: ", round(as.numeric(mse_resault$train_mse), 1))
  })
  
  
  # Render text displaying the Mean Squared Error (MSE) for the test dataset in a regression model
  # Access the MSE result from the model's predictions and round it for display
  output$test_mse_output_regress <- renderText({
    tree_results <- tree_data()
    mse_resault <- tree_results$predictions
    paste("Mean Squared Error: ", round(as.numeric(mse_resault$test_mse), 1))
  })
  

  
  
  # Render a Plotly bar chart displaying feature importance from the decision tree model
  # Plotly is used to create an interactive bar chart showing the importance of each feature
  # The x-axis represents the importance, and the y-axis represents the feature names
  # The chart is customized with specific colors, font sizes, margins, and other layout settings
  output$feature_importance <- renderPlotly({
    tree_results <- tree_data()
    tree_model <- tree_results$tree_model
    data_importance_var <- data.frame(Importance = tree_model$variable.importance)
    data_importance_var$Feature <- rownames(data_importance_var)
    rownames(data_importance_var) <- NULL
    data_importance_var$Importance<-round(as.numeric(data_importance_var$Importance),1)
    plot_ly(
      data = data_importance_var,
      x = ~Importance,
      y = ~Feature,
      type = "bar",
      marker = list(color = c("#7bc0ce")),
      width = 300,  
      height = 400  
    ) %>% 
      layout(
        yaxis = list(
          categoryorder = "total ascending",
          title = "",
          titlefont = list(size = 14, color = "#a4b3b6"),
          tickfont = list(size = 12, color = "#a4b3b6"),
          side = "left" 
        ),
        xaxis = list(
          title = "Importance",
          titlefont = list(size = 14, color = "#a4b3b6"),
          tickfont = list(size = 12, color = "#a4b3b6")
        ),
        paper_bgcolor = "#16525e", 
        plot_bgcolor = "#16525e",  
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 50, b = 10),  
        bargap = 0.4  
      )
    
  })
  
  

  # Render a table displaying the complexity parameter (CP) table for the decision tree model
  # The table shows various statistics (CP, nsplit, relative error, xerror, xstd) from the decision tree's complexity parameter table
  # The table is styled with Bootstrap and custom CSS for a responsive, condensed, and visually appealing format
  # The row and column specifications include background colors, text colors, and borders for better readability
  output$complexity_parameter <- renderUI({
      tree_results <- tree_data()
      tree_model <- tree_results$tree_model
      cptable_df <- as.data.frame(tree_model$cptable)
      num_rows <- nrow(cptable_df)
      table <- cptable_df %>%
        kbl(format = "html", escape = FALSE, col.names = c("CP", "nsplit", "rel error", "xerror", "xstd" )) %>%
        kable_styling(bootstrap_options = c( "hover","responsive","condensed"), full_width = FALSE, font_size = 13) %>%
        row_spec(1:num_rows, background = "#051524", align = "center", color = "#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
        row_spec(0, background = "#0f2437", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
        column_spec(1:2,  color = "#7bc0ce", extra_css = "border: 1px solid #16525e;")
      
      HTML(as.character(table))
  
  })
  

  
  # Render confusion matrix as an interactive heatmap using plotly, with annotations for cell values. 
  # This visualizes the model's performance in terms of true positives, false positives, and other classification metrics.
  output$confusion_matrix_plot <- renderPlotly({
    tree_results <- tree_data()
    conf_matrix <- tree_results$predictions$confusion_matrix

    plot <- plot_ly(
      x = colnames(conf_matrix),
      y = rownames(conf_matrix),
      z = conf_matrix,
      type = "heatmap",
      colorscale = list(c(0, "#b7dce4"), c(1, "#7bc0ce")), 
      showscale = TRUE
    ) %>% layout(

      title = "",
      xaxis = list(
        title = "Predicted", 
        titlefont = list(size = 12, color = "#a4b3b6"),
        tickfont = list(size = 12, color = "#a4b3b6")),
      yaxis = list(
        title = "Actual",
        titlefont = list(size = 12, color = "#a4b3b6"),  
        tickfont = list(size = 12, color = "#a4b3b6"),
        side = "left" 
      ),
      paper_bgcolor = "#16525e",  
      plot_bgcolor = "#16525e",   
      showlegend = FALSE,
      margin = list(l = 10, r = 10, t = 50, b = 10) 
    )
    
    for (i in 1:nrow(conf_matrix)) {
      for (j in 1:ncol(conf_matrix)) {
        plot <- plot %>% add_annotations(
          text = conf_matrix[i, j],
          x = colnames(conf_matrix)[j],
          y = rownames(conf_matrix)[i],
          showarrow = FALSE
        )
      }
    }
    
    plot
  })
  

  
  # display in absolute panel optimal cp value
  output$optimal_cp_values <- renderText({
    tree_results <- tree_data()
    paste("Remark: If you want an optimal model, you can set slider CP Threshold value approx. to:", round(as.numeric(tree_results$optimal_cp), 6))
    
  })
  
  
  # Observe the toggle button event to dynamically render model evaluation metrics for either classification or regression tasks.
  # The UI updates based on the selected model type, showing relevant metrics like accuracy, MAE, MSE, and gauge outputs for both training and test datasets.
  observeEvent(input$toggle_button_metrices, {
    if (input$toggle_button_metrices) {
      
      output$dynamic_content <- renderUI({
        tree_results <- tree_data()
        accuracy_results <- tree_results$predictions
        
        if (accuracy_results$model_type == "Classification") {
          tagList(
            absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                          draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                          width = 600, height = 500,
                          fluidRow(h4("Model Evaluation"),
                                   h5("Classification"),
                                   tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                   column(
                                     width = 6,  
                                     h4("Training"),
                                     div(class = "accuracy-div",
                                         p(class = "accuracy-value", textOutput("train_N_output_regress_class"))),
                                     div(class = "accuracy-div",
                                         p(class = "accuracy-value", textOutput("train_kappa_output_class"))),
                                     br(),
                                     div(class = "accuracy-div",
                                         p(class = "accuracy-value", textOutput("train_accuracy_output_class"))),
                                     gaugeOutput("gauge_train_class"),
                                   ),
                                   column( 
                                     width = 6,  
                                     h4("Test"),
                                     div(class = "accuracy-div",
                                         p(class = "accuracy-value", textOutput("test_N_output_regress_class"))),
                                     div(class = "accuracy-div",
                                         p(class = "accuracy-value", textOutput("test_kappa_output_class"))),
                                     br(),
                                     div(class = "accuracy-div",
                                         p(class = "accuracy-value", textOutput("test_accuracy_output_class"))),
                                     gaugeOutput("gauge_test_class")
                                   )
                          )
            )
          )
          
        } else if (accuracy_results$model_type == "Regression") {
          
          tagList(
            absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                          draggable = TRUE, top = "auto", left = 20, right = "auto", bottom = 20,
                          width = 600, height = 500,
                          fluidRow(h4("Model Evaluation"),
                                   h5("Regression"),
                                   tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                   column( width = 6, 
                                           h4("Training"),
                                           div(class = "accuracy-div",
                                               p(class = "accuracy-value", textOutput("train_N_output_regress_class"))),
                                           
                                           div(class = "accuracy-div",
                                               p(class = "accuracy-value", textOutput("train_mae_output_regress"))),
                                           div(class = "accuracy-div",
                                               p(class = "accuracy-value", textOutput("train_mse_output_regress"))),
                                           
                                           div(class = "accuracy-div",
                                               p(class = "accuracy-value", textOutput("train_accuracy_output_regress"))),
                                           gaugeOutput("gauge_train_regress"),
                                           
                                   ),
                                   column(width = 6, 
                                          h4("Test"),
                                          div(class = "accuracy-div",
                                              p(class = "accuracy-value", textOutput("test_N_output_regress_class"))),
                                          
                                          div(class = "accuracy-div",
                                              p(class = "accuracy-value", textOutput("test_mae_output_regress"))),
                                          div(class = "accuracy-div",
                                              p(class = "accuracy-value", textOutput("test_mse_output_regress"))),
                                          
                                          div(class = "accuracy-div",
                                              p(class = "accuracy-value", textOutput("test_accuracy_output_regress"))),
                                          gaugeOutput("gauge_test_regress"),
                                          
                                   )
                          )
            )
          )
          
          
        }
        
      })
    } else {
      output$dynamic_content <- NULL
    }
  })
  
  
  
  
  # Observe the validation toggle button event to render dynamic content based on the model type (Classification or Regression).
  # It includes displaying cross-validation results such as the complexity parameter (cp), feature importance plots, and confusion matrix (for classification).
  # For regression, it shows relevant metrics with similar structure but for regression-specific results.
  observeEvent(input$toggle_button_validation, {
    if (input$toggle_button_validation) {
      output$dynamic_content_2 <- renderUI({
        tree_results <- tree_data()
        accuracy_results <- tree_results$predictions
        
        if (accuracy_results$model_type == "Classification") {
          tagList(
            absolutePanel(id = "controls2", class = "panel panel-default panel-scroll-vertical", fixed = FALSE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 800, height = 600,
                          fluidRow(h4("Cross Validation"),
                                   h5("Classification"),
                                   tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                   column(width = 6, 
                                          h5("Complexity Parameter (cp)"),
                                          br(),
                                          textOutput("optimal_cp_values"),
                                          br(),
                                          tableOutput("complexity_parameter"),

                                          helpText("CP: Complexity Parameter, n split: Number of Splits, 
                                          rel error: Relative Error, xerror: Cross-Validation Error, 
                                          xstd:Cross-Validation Standard Deviation")
                                   ),
                                   column(width = 6,  
                                          h5("Feature Importance"),
                                          plotlyOutput("feature_importance")
                                          
                                   )
                          ),
                          
                          fluidRow(
                            tags$hr(style = "border-color: #009999; margin-top: 8px;"),
                            h5("Confusion Matrix"),
                            br(),
                            plotlyOutput("confusion_matrix_plot")
                            
                            
                            
                          )
                          
            )
          )
          
        } else if (accuracy_results$model_type == "Regression") {
          
          tagList(
            absolutePanel(id = "controls2",  class = "panel panel-default panel-scroll-vertical" ,fixed = FALSE,
                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                          width = 800, height = 600,
                          fluidRow(h4("Cross Validation"),
                                   h5("Regression"),
                                   tags$hr(style="border-color: #009999; margin-top: 3px;"),
                                   column( width = 6, 
                                           h5("Complexity Parameter (cp)"),
                                           br(),
                                           textOutput("optimal_cp_values"),
                                           br(),
                                           tableOutput("complexity_parameter"),
                                           helpText("CP: Complexity Parameter, n split: Number of Splits, 
                                                     rel error: Relative Error, xerror: Cross-Validation Error, 
                                                     xstd:Cross-Validation Standard Deviation")
                                           
                                   ),
                                   column(width = 6, 
                                          h5("Feature Importance"),
                                          plotlyOutput("feature_importance")
                                          
                                          
                                          
                                   )
                          )
            )
          )
          
          
        }
        
        
      })
    } else {
      output$dynamic_content_2 <- NULL
    }
  })
  
  
  
  
  # cache data for prescriptive
  observeEvent(input$input_generators, {
    updated_data <- tree_data()$selected_prescript_data
    for (col in colnames(updated_data)) {
      input_id <- paste0("input_", col)
      input_value <- input[[input_id]]
      updated_data[, col] <- input_value
    }
  })
  
  
  
  
  # Dynamically generates input controls based on the selected data (numeric or factor columns).
  # For numeric columns, it creates a knob input with a step value based on the minimum difference between data points.
  # For factor columns, it creates a select input with the unique levels as options.
  # The inputs are split into two columns for better layout, and styles are applied to enhance the UI appearance.
  output$input_generators <- renderUI({
    input_list <- lapply(colnames(tree_data()$selected_prescript_data), function(col) {
      col_type <- class(tree_data()$selected_prescript_data[[col]])
      if (col_type %in% c("numeric", "integer")) {
        col_range <- range(tree_data()$selected_prescript_data[[col]], na.rm = TRUE)
        differences <- diff(tree_data()$selected_prescript_data[[col]])
        min_difference <- min(abs(differences))
        step <- min_difference

        knobInput(
          inputId = paste0("input_", col),
          label = col,
          min = col_range[1],
          max = col_range[2],
          value = round(mean(col_range), 0),
          step = step,
          width = "100px",
          fgColor = "#0a5f70",  
          inputColor = "white",
          bgColor = "#a4b3b6",
          fontSize = "13px",
          lineCap = "round"
        )
      } else if (col_type == "factor") {
        unique_levels <- levels(tree_data()$selected_prescript_data[[col]])
        selectInput(
          inputId = paste0("input_", col),
          
          label = col,
          choices = unique_levels
         
        )

      }
    })

    max_outputs_first_column <- 5
    input_list_first_column <- input_list[1:max_outputs_first_column]
    input_list_second_column <- input_list[(max_outputs_first_column + 1):length(input_list)]

    column1 <- column(width = 6, align = "center", style = "border: 1px solid #16525e; border-radius: 5px;", do.call(tagList, input_list_first_column))
    column2 <- column(width = 6, align = "center", style = "border: 1px solid #16525e; border-radius: 5px;", do.call(tagList, input_list_second_column))

    fluidRow(column1, column2)
  })

  
  
  # Creates a reactive object `influental_result_data` to store and update the selected input values.
  influental_result_data <- reactiveValues(selected_values = NULL)
  
  
  # Renders a dynamic table displaying the selected values from user inputs.
  # It first collects the selected values for each column from the `selected_prescript_data` object.
  # The table is styled and formatted using the `kable` package to make it visually appealing.
  # The table contains two columns: "Feature" and "Selected Value", with the selected input values from the UI.
  output$selected_values_table <- renderUI({
    reactive_data_value <- tree_data()$selected_prescript_data
    if (!is.null(reactive_data_value)) {
      selected_values <- sapply(colnames(reactive_data_value), function(col) {
        input_id <- paste0("input_", col)
        input_value <- input[[input_id]]
        input_value
      })
      
      data_from_UI <- data.frame(
        Varijabla = colnames(reactive_data_value),
        Vrijednost = selected_values
      )
      
      rownames(data_from_UI) <- NULL
      influental_result_data$selected_values <- data_from_UI
      
      num_rows <- nrow(data_from_UI)
      
      table <- data_from_UI %>%
        kbl(format = "html", escape = FALSE, col.names = c("Feature", "Selected Value")) %>%
        kable_styling(bootstrap_options = c( "hover","responsive","condensed"), full_width = FALSE, font_size = 13) %>%
        row_spec(1:num_rows, background = "#051524", align = "center", color = "#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
        row_spec(0, background = "#0f2437", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
        
        column_spec(1:2,  color = "#7bc0ce", extra_css = "border: 1px solid #16525e;") 
                    
      HTML(as.character(table))
    }
  }) 
  
  

  
  # Initializes a reactive value 'prescript_prediction' which stores the predictions made for a given input.
  reactive_prescript_predict <- reactiveValues(prescript_prediction = NULL)
  
  
  # Generates the expected prediction results for a given model and user inputs.
  # This UI output dynamically adjusts based on whether the model is a classification or regression model.
  # - For classification: Displays the predicted class with its probability.
  # - For regression: Displays the predicted numeric values rounded to two decimal places.
  # It also ensures the inputs from the user (e.g., selected features and their values) are appropriately used to generate predictions for the first row of the training data.
  output$expected_results <- renderUI({
    influent_df<- influental_result_data$selected_values
    train_df<-split_data()$train_data
    model<-tree_data()$tree_model
    prediction_type <- split_data()$prediction_type
    model_type <- tree_data()$predictions$model_type
    
    if (!is.null(influent_df)) {
      influent_df <- spread(influent_df, key = Varijabla, value = Vrijednost)
      selected_columns_train_df <- train_df %>% select(all_of(names(influent_df)))
      processed_influent_df <- map_data_types_from_UI(selected_columns_train_df, influent_df)
      first_row_train_df <- train_df[1, ]
      cols_to_update_train_df <- intersect(names(first_row_train_df), names(processed_influent_df))
      first_row_train_df[cols_to_update_train_df] <- processed_influent_df[cols_to_update_train_df]
      prescript_prediction<-generate_predictions_prescript(model, first_row_train_df, prediction_type)
      
      if (model_type == "Classification") {
        predictions_class <- prescript_prediction$predictions_df$pred_data 
        predictions_class <- as.data.frame(predictions_class)
        renamed_data <- rename_columns(predictions_class)
        selected_category <- extract_max_row(renamed_data)
        reactive_prescript_predict$selected_category <- selected_category 
        selected_probability <- select_columns(selected_category)

        rownames(selected_probability) <- NULL
        
        num_rows <- nrow(selected_probability)
        num_col<-ncol(selected_probability)
        
        table <- selected_probability %>%
          kbl(format = "html", escape = FALSE) %>% 
          kable_styling(bootstrap_options = c( "strike", "hover","responsive", "condensed"), full_width = FALSE, font_size = 13) %>%
          row_spec(1:num_rows, background = "#051524", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
          row_spec(0, background = "#0f2437", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
          
          column_spec(1:num_col,  color = "#66CDAA",  
                      extra_css = "border: 1px solid #16525e;"
          )
        
        
      } else if (model_type == "Regression") {
        reg_predicted_values<-prescript_prediction$predictions_df$pred_data$Predicted
        reg_predicted_values <- data.frame(reg_predicted_values)
        reg_predicted_values <- reg_predicted_values %>%
          rename("Predicted Value" = "reg_predicted_values")
        reg_predicted_values["Predicted Value"] <- round(reg_predicted_values["Predicted Value"], 2)
        reactive_prescript_predict$reg_predicted_values <- reg_predicted_values 
        num_rows <- nrow(reg_predicted_values)
        num_col<-ncol(reg_predicted_values)
    
        table <- reg_predicted_values %>%
          kbl(format = "html", escape = FALSE) %>%  #col.names = c("Predicted Value")
          kable_styling(bootstrap_options = c( "strike", "hover","responsive", "condensed"), full_width = FALSE, font_size = 13) %>%
          row_spec(1, background = "#051524", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
          row_spec(0, background = "#0f2437", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
         
          column_spec(1, color = "#66CDAA",  extra_css = "border: 1px solid #16525e;") 
      }
      HTML(as.character(table))
      
    }
  })  
  
  
  
  
  # This block creates a Plotly bar chart displaying the predicted class probabilities for a classification model.
  # The chart shows the probability of each category based on the user-specified input.
  # - 'selected_category' holds the category and probability values.
  # - The plot uses `x` for probabilities and `y` for categories.
  # - It is styled with custom colors, margin settings, and background color to match the application's theme.
  output$category_probability <- renderPlotly({
    selected_category <- reactive_prescript_predict$selected_category
    selected_category <- transform_dataframe(selected_category)
    plot_ly(
      data = selected_category,
      x = ~Probability, 
      y = ~Category,   
      type = "bar",
      marker = list(color = c("#66CDAA")),
      width = 180,  
      height = 350  
    ) %>%
      layout(
        title = list(
          text = "Class Probability",
          font = list(size = 12, color = "#a4b3b6")  
        ),
        yaxis = list(
          titlefont = list(size = 12, color = "#a4b3b6"),
          tickfont = list(size = 11, color = "#a4b3b6"),
          side = "left" 
        ),
        xaxis = list(
          title = "Probabilities",
          titlefont = list(size = 12, color = "#a4b3b6"),
          tickfont = list(size = 11, color = "#a4b3b6")
        ),
        paper_bgcolor = "#0f2437",  
        plot_bgcolor = "#0f2437", 
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 50, b = 10),  
        bargap = 0.4  
      )
  })
  
  
  
  output$predicted_prescript_value <- renderPlotly({
    predicted_values_array<-tree_data()$predictions$predictions_df$Train$Predicted 
    reg_predicted_values <- reactive_prescript_predict$reg_predicted_values  
    value <- reg_predicted_values$`Predicted Value`
    min_value <-min(round(as.numeric(predicted_values_array),0))
    max_value <- max(round(as.numeric(predicted_values_array),0))
    
    plot_ly(
      x = c(min_value, value),
      y = c(1, 1),
      type = "scatter",
      mode = "lines+text",
      line = list(width = 80),  
      width = 180,  
      height = 100  
    ) %>% layout(
      title = list(
        text = "Predicted Value",
        font = list(size = 12, color = "#a4b3b6")  
      ),
      xaxis = list(range = c(min_value, max_value), showgrid = FALSE, titlefont = list(size = 12, color = "#a4b3b6"), 
                   tickfont = list(size = 11, color = "#a4b3b6"), title = "", linecolor = "#a4b3b6"),  
      yaxis = list(showticklabels = FALSE, showgrid = FALSE, linecolor = "#a4b3b6"),
      showlegend = FALSE,
      paper_bgcolor = "#0f2437",  
      plot_bgcolor = "#0f2437",
      margin = list(l = 1, r = 4, t = 50, b = 10)
      
    ) %>% 
      add_trace(
        type = "scatter",
        fill = "toself",
        fillcolor = "#66CDAA",     
        line = list(color = "#66CDAA"),
        showlegend = FALSE,
        hoverinfo = "text" 
      ) 
    
    
  })
  
  
  
  # This code generates a Plotly scatter plot to visualize the predicted value for a regression model.
  output$prescriptiv_plot_UI <- renderUI({
    model_type <- tree_data()$predictions$model_type
    dependent_col <- input$dependent_var
    if (model_type == "Classification") {
      div(
        br(),
        tags$hr(style="border-color: #009999; margin-top: 3px;"),
        plotlyOutput("category_probability"),
        helpText(paste("You are working on a predictive scenario for",dependent_col,"target feature:" ))
      )
    } else if (model_type == "Regression") {
      div(
        br(),
        tags$hr(style="border-color: #009999; margin-top: 3px;"),
        plotlyOutput("predicted_prescript_value"),
        helpText(paste("You are working on a predictive scenario for",dependent_col,"target feature:" ))
      )
    }
  })
  
  
  # Initializes a reactive value 'saved_rules' to store the decision tree rules
  saved_rules <- reactiveValues(rules = NULL)
  
  # This code generates and displays a table of decision tree rules for either a classification or regression model.
  output$rulesTable <- renderTable({
    model_type <- tree_data()$predictions$model_type
    model <- tree_data()$tree_model
    
    if (model_type == "Classification") {
      rules <- rpart.rules(model, cover = TRUE, roundint = FALSE, extra = 4)
    } else if (model_type == "Regression") {
      rules <- rpart.rules(model, cover = TRUE, roundint = FALSE)
    }
    
    current_colnames <- colnames(rules)
    current_colnames[1] <- paste("Target Feature -", current_colnames[1], sep = " ")
    current_colnames[3] <- "Rules  ⟶ "
    current_colnames[length(current_colnames)] <- "% Observations in Rule"
    colnames(rules) <- current_colnames
    saved_rules$rules <- rules 
    
    return(rules)
    
  }, colnames = TRUE, align = 'l', spacing = 's', bordered = TRUE, style = "white-space: nowrap;")
  
  
  
  
  #Download Rules table
  output$downloadRules <- downloadHandler(
    filename = function() {
      "myRules.xlsx"
    },
    content = function(file) {
      rules <- saved_rules$rules
      write.xlsx(rules, file)
    }
  )
  
  

  # Initializes a reactive value 'predictions_table' to store the model predictions
  predictions_table <- reactiveValues(predictions = NULL)
  

  
  # This code generates and displays a table with predictions based on the model type (Classification or Regression).
  # It prepares the prediction data by removing the dependent variable and mapping data types, then generates predictions using the trained model.
  # The results are formatted into a styled table, and in case of missing data, a custom alert is shown.
  # Error handling is implemented to manage any issues during prediction generation.
  output$predictionsTable <- renderUI({
    selected_cols <- selected_columns_pred()
    data_prediction <- data_pred()
    data_prediction <- data_prediction[, ..selected_cols, drop = FALSE]
    train_data <- split_data()$train_data
    dependent_col <- input$dependent_var

    train_data <- train_data %>%
      select(-{{dependent_col}})
    
    data_prediction <- as.data.frame(data_prediction)
    data_prediction <- map_data_types(train_data, data_prediction)
    
    model_type <- tree_data()$predictions$model_type
    prediction_type <- split_data()$prediction_type
    model <- tree_data()$tree_model
    
    if (exists("data_prediction") && exists("train_data")) {
      tryCatch({
        predictions <- generate_predictions_prescript(model, data_prediction, prediction_type)
        if(model_type == "Classification"){
          pred_data <- predictions$predictions_df$pred_data
          for (i in 1:ncol(pred_data)) {
            col_name <- colnames(pred_data)[i]
            data_prediction <- bind_cols(data_prediction, pred_data[, i])
            colnames(data_prediction)[ncol(data_prediction)] <- col_name
          }
        }else if (model_type == "Regression"){
          pred_data <- predictions$predictions_df$pred_data
          for (i in 1:ncol(pred_data)) {
            col_name <- colnames(pred_data)[i]
            data_prediction <- bind_cols(data_prediction, pred_data[, i])
            colnames(data_prediction)[ncol(data_prediction)] <- col_name
          }
        }
        
        predictions_table$predictions <- data_prediction
        
        num_rows <- nrow(data_prediction)
        
        table <- data_prediction %>%
          kbl(format = "html", escape = FALSE) %>% 
          kable_styling(bootstrap_options = c( "hover","responsive","condensed"), full_width = FALSE, font_size = 13) %>%
          row_spec(1:num_rows, background = "#051524", align = "center", color = "#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
          row_spec(0, background = "#0f2437", align = "center", color="#7bc0ce", extra_css = "border: 1px solid #16525e;") %>%
          
          column_spec(1:2,  color = "#7bc0ce", extra_css = "border: 1px solid #16525e;") 
        
        HTML(as.character(table))
        
      }, error = function(e) {
       
      })
    }else{
      div(
        id = "custom-alert",
        "You must have both Training Data and Prediction Data!"
      )
    }
    
  })
  
  
  
  #Download Predictions  table
  output$downloadPredictions <- downloadHandler(
    filename = function() {
      "myPredictions.xlsx"
    },
    content = function(file) {
      predictions_df <- predictions_table$predictions
      write.xlsx(predictions_df, file)
    }
  )
  
  

  # This code renders text that explains the decision rules based on the model's output.
  # It checks if the rules table is available and contains data. If so, it calls the 'generate_text_rules' function to generate a descriptive text of the rules.
  # If no rules are available, it returns an empty string.
  output$display_text_rules <- renderText({
    dependent_col <- input$dependent_var
    target_feature <- as.character(dependent_col)
    
    rules_table <- saved_rules$rules 
    
    if (!is.null(rules_table) && ncol(rules_table) > 0) {
      generate_text_rules(rules_table, target_feature)
      
    } else {
      ""
    }
  })
  
  
  
  # This code renders text based on the model's predictions and the selected target feature.
  # It checks if the predictions data is available and contains data. If predictions are available, 
  # it calls either 'generate_text_predictions_class' or 'generate_text_predictions_regress' depending on whether the model type is classification or regression.
  # If no predictions are available, it returns an empty string.
  output$display_text_predictions <- renderText({
    model_type <- tree_data()$predictions$model_type
    dependent_col <- input$dependent_var
    target_feature <- as.character(dependent_col)
    predictions_df <- predictions_table$predictions 
    if (!is.null(predictions_df) && ncol(predictions_df) > 0) {
      if (model_type == "Classification"){
        generate_text_predictions_class(predictions_df, target_feature)
      }else if (model_type == "Regression"){
        generate_text_predictions_regress(predictions_df, target_feature)
      }
    }else {
        ""
    }
    
  })
  
  
  
  # This code renders text about missing columns in the dataset.
  # It first selects the relevant columns for prediction and prepares the prediction data by mapping data types.
  # Then it calls the 'generate_text_missing_columns' function, which generates a textual description of missing columns between the training data and the prediction data.
  output$display_text_missing_columns <- renderText({
    selected_cols <- selected_columns_pred()
    data_prediction <- data_pred()
    data_prediction <- data_prediction[, ..selected_cols, drop = FALSE]
    train_data <- split_data()$train_data
    dependent_col <- input$dependent_var
    train_data <- train_data %>%
      select(-{{dependent_col}})
    data_prediction <- as.data.frame(data_prediction)
    data_prediction <- map_data_types(train_data, data_prediction)
    generate_text_missing_columns(train_data, data_prediction )
  
  })
  
  
  
  
  
  # This code dynamically generates the UI for displaying decision rules based on the training data.
  # It checks if the training data has valid columns and rows. If so, it displays:
  # 1. A section with the decision rules, including a text output with the rules and an informational icon.
  # 2. A download button to export the rules as an Excel file.
  # 3. A table output for showing the actual rules in a scrollable container.
  # If no valid training data is available, an alert is displayed to prompt the user to generate the model first.
  output$dynamic_ui_rules <- renderUI({
    train_data <- split_data()$train_data
    show_rules <- ncol(train_data) > 0 && nrow(train_data) > 0
    
    if(show_rules){
      tagList(
        fluidRow(
          tags$hr(style="border-color: #009999; margin-top: 3px;"),
          column(width = 9, align = "center",
                 h5("Decision Rules"),
                 tags$hr(style="border-color: #009999; margin-top: 3px;"),
                 div(
                   id = "summary_rules_div",
                   style = "border: 2px solid #387a87; padding: 10px; background-color: rgba(123,192,206, 0.7); display: flex; align-items: center;",
                   tags$img(src = "information.svg", width = "30px", height = "30px",style = "margin-right: 10px;"),
                   textOutput("display_text_rules")

                 ),

          ),
          column(width = 3, align = "left", style = "text-align: left; display: flex; flex-direction: column; justify-content: flex-end;",
                 p("Export Excel:", style = "margin-top: 0; font-size: 14px; align-items: center;"),
                 downloadButton("downloadRules", "Rules")
          )
        ),
        fluidRow(
          column(width = 12, align = "center",
                 br(),
                 div(
                   style = 'overflow-x: auto; overflow-y: auto; margin-top: 0; max-height: 400px; font-size: 11px; ',
                   withSpinner(tableOutput("rulesTable"), type = 2),
                   br()
                 )

          )

        )
      )
    }  else {
            div(
              id = "custom-alert",
              "You need to generate the model first!"
            )
       }


  })
  
  
  
  # This code dynamically generates the UI for displaying predictions based on the training and prediction data.
  output$dynamic_ui_predictions <- renderUI({
    selected_cols <- selected_columns_pred()
    data_prediction <- data_pred()
    data_prediction <- data_prediction[, ..selected_cols, drop = FALSE]
    
    train_data <- split_data()$train_data
    dependent_col <- input$dependent_var
    
    train_data <- train_data %>%
      select(-{{dependent_col}})
    
    data_prediction <- as.data.frame(data_prediction)
    data_prediction <- map_data_types(train_data, data_prediction)
    
    show_predictions <- check_differences(train_data, data_prediction)
    show_predictions_2 <- nrow(data_prediction) > 0  && ncol(data_prediction) > 0
    show_predictions_3 <- nrow(train_data) > 0  && ncol(train_data) > 0
    
    if(show_predictions_2 && show_predictions_3){
      
      if(show_predictions){
          tagList(
            fluidRow(
              tags$hr(style="border-color: #009999; margin-top: 3px;"),
              column(width = 9, align = "center",
                     h5("Predictions"),
                     tags$hr(style="border-color: #009999; margin-top: 3px;"),
                     div(
                       id = "summary_predictions_div",
                       style = "border: 2px solid #387a87; padding: 10px; background-color: rgba(123,192,206, 0.7); display: flex; align-items: center;",
                       tags$img(src = "information.svg", width = "30px", height = "30px",style = "margin-right: 10px;"),
                       br(),
                       textOutput("display_text_predictions")

                     ),

              ),
              column(width = 3, align = "left", style = "text-align: left; display: flex; flex-direction: column; justify-content: flex-end;",
                     p("Export Excel:", style = "margin-top: 0; font-size: 14px; align-items: center;"),
                     downloadButton("downloadPredictions", "Predictions"),
              )

            ),
            fluidRow(
              column(width = 12, align = "center",
                     br(),
                     div(
                       style = 'overflow-x: auto; overflow-y: auto; margin-top: 0; max-height: 400px; font-size: 11px; ',
                       withSpinner(uiOutput("predictionsTable"), type = 2)
                       
                        )
              )
            )

          )
        
      }else{
           div(
             id = "custom-alert",
             textOutput("display_text_missing_columns")
           )
        
      }
      
    }else{
      div(
        id = "custom-alert",
        "You must have both Training Data and correct Prediction Data with the same features!"
      )
      
    }

  })
  
  
  
})

  
  
  
  







