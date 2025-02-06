

# This function generates a summary table with descriptive statistics and visualizations for selected columns in a dataset.
# It calculates and displays the following for each column:
# 1. Data type (e.g., Numeric, Categorical, Date/Time)
# 2. Number of valid (non-missing) records
# 3. Number of missing records
# 4. Percentage of missing records
# 5. Mean, minimum, and maximum values (only for numeric columns)
# Additionally, it visualizes the distribution of each column using sparklines:
# - Numeric columns are visualized with a bar chart sparkline showing the distribution.
# - Categorical columns are visualized with a bar chart sparkline displaying the frequency of categories.
# The function then filters the data based on the selected columns and renders the summary table using `reactable`,
# displaying all the above statistics along with the sparklines for each column.
generate_summary_table <- function(data, selected_columns) {
  
  
  # Get data types for each column
  data_types <- sapply(data, function(x) {
    if (is.numeric(x)) {
      "Numeric (1,2,3)"
    } else if (is.character(x)) {
      "Categorical (A,B,C)"
    } else if (is.logical(x)) {
      "Logical (TRUE, FALSE)"
    } else if (is.integer(x)) {
      "Numeric (1,2,3)"
    } else if (is.Date(x) || inherits(x, "POSIXct")){
      "Date/Time"
    } else {
      "Unknown"
    }
  })

  # Calculate the number of valid records for each column
  num_valid <- sapply(data, function(x) sum(complete.cases(x)))

  # Calculate the number of missing records for each column
  num_missing <- sapply(data, function(x) sum(is.na(x)))

  # Calculate the total number of rows in each column
  total_rows <- nrow(data)

  # Calculate the percentage of missing records for each column
  percentage_missing <- round((num_missing / total_rows) * 100, 1)

  calculate_mean <- function(column) {
    if (is.numeric(column)) {
      round(mean(column, na.rm = TRUE), 1)
    } else {
      "-"
    }
  }

  mean_value  <- sapply(data, calculate_mean)

  calculate_min <- function(column) {
    if (is.numeric(column)) {
      min(column, na.rm = TRUE)
    } else {
      "-"
    }
  }

  min_value <- sapply(data, calculate_min)

  calculate_max <- function(column) {
    if (is.numeric(column)) {
      max(column, na.rm = TRUE)
    } else {
      "-"
    }
  }

  max_value <- sapply(data, calculate_max)

 
  
  data_transformed <- data %>%
    gather(key = "Columns", value = "Records") %>%
    group_by(Columns) %>%
    summarise(
      Records = list(Records),
      DataType = first(data_types[match(Columns, names(data_types))]),
      NumValid = first(num_valid[match(Columns, names(num_valid))]),
      NumMissing = first(num_missing[match(Columns, names(num_missing))]),
      PercentMiss = first(percentage_missing[match(Columns, names(percentage_missing))]),
      Mean = first(mean_value[match(Columns, names(mean_value))]),
      Min = first(min_value[match(Columns, names(min_value))]),
      Max = first(max_value[match(Columns, names(max_value))])
    )
  
  
  data_transformed <- data_transformed %>%
    filter(Columns %in% selected_columns)
  

  
  
  generate_sparkline <- function(values, datatype, barWidth = 0.5) {
    if (datatype == "numeric") {
      # Calculate histogram with automatic binning
      hist_data <- hist(values, plot = FALSE)
      
      # Create a bar sparkline with the histogram data
      sparkline(hist_data$counts, type = "bar", width = 200, height = 60, barSpacing = 2, chartRangeMin = 0, barColor = "#009999")
    } else {
      # Create a bar chart sparkline for categorical data
      sparkline(table(values), type = "bar", width = 160, height = 40, arSpacing = 2, barColor ="#009999", barWidth = barWidth)
    }
  }
  
  
    # Determine data types of columns
    col_data_types <- sapply(data_transformed, class)
    
    # Create a list of column definitions
    col_defs <- list(
      DataType = colDef(header = "Data Type", align = "center"),
      NumValid = colDef(header = "Count" , align = "center"),
      NumMissing = colDef(header = "Missing Data", align = "center"),
      PercentMiss = colDef(header = "% Missing", align = "center"),
      Mean = colDef(header = "Mean Value", align = "center"),
      Min = colDef(header = "Min Value", align = "center"),
      Max = colDef(header = "Max Value", align = "center"),
      Records = colDef(header = "Visualization", align = "center", cell = function(values, index, colname) {
        datatype <- col_data_types[[colname]]
        generate_sparkline(values, datatype, barWidth = 20)
      })
    )
    
    # Render the reactable
    reactable(data_transformed, striped = TRUE, resizable = TRUE, wrap = FALSE, defaultPageSize = 15, highlight = TRUE,
              style = list(
                fontFamily = "Helvetica"
              ),
              columns = col_defs)
  
  
  


}




# Function counts the number of different column types in a dataset and returns a summary as a data frame.
# The function also includes the total number of rows and columns in the dataset.
# The result is returned as a data frame with the column type and its count in the dataset.
count_column_types <- function(data) {
  num_rows <- nrow(data)
  num_cols <- ncol(data)
  num_numeric <- sum(sapply(data, is.numeric))
  num_logical <- sum(sapply(data, is.logical))
  num_text <- sum(sapply(data, is.character))
  num_date <- sum(sapply(data, function(x) is.Date(x) || inherits(x, "POSIXct")))
  num_unknown <- sum(sapply(data, function(x) !is.numeric(x) && !is.logical(x) && !is.character(x) && !is.Date(x) && !inherits(x, "POSIXct")))
  
  result <- data.frame(
    ColumnType = c("Numeric", "Logical", "Text", "Date", "Unknown", "Rows", "Total Columns"),
    Count = c(num_numeric, num_logical, num_text, num_date, num_unknown, num_rows, num_cols)
  )
  
  return(result)
}



# Generate summary text 
generate_summary_text <- function(column_counts) {
  num_numeric <- column_counts$Count[column_counts$ColumnType == "Numeric"]
  num_logical <- column_counts$Count[column_counts$ColumnType == "Logical"]
  num_text <- column_counts$Count[column_counts$ColumnType == "Text"]
  num_date <- column_counts$Count[column_counts$ColumnType == "Date"]
  num_unknown <- column_counts$Count[column_counts$ColumnType == "Unknown"]
  num_rows <- column_counts$Count[column_counts$ColumnType == "Rows"]
  num_total_columns <- column_counts$Count[column_counts$ColumnType == "Total Columns"]
  

  summary_text <- paste(
    "In your data set there are",
    paste("a total of", num_rows, "records,", sep = " "),
    paste("and a total of", num_total_columns, "columns.", sep = " "),
    "The data set contains",
    paste(num_numeric, "numeric columns,", sep = " "),
    paste(num_logical, "logical columns,", sep = " "),
    paste(num_text, "categorical columns,", sep = " "),
    paste(num_date, "date/time columns,", sep = " "),
    "and",
    paste(num_unknown, "columns of unknown type.", sep = " ")
    
  )
  
  
  return(summary_text)
}






# This function filters the specified columns from the dataset based on the provided list of selected columns.
# It returns only the categorical columns from the filtered data. Categorical columns are identified as factors, 
# characters, or logical data types.
filter_categorical_columns <- function(data, selected_columns) {
  
  if (!is.null(selected_columns) && length(selected_columns) > 0) {
    #filtered_data <- data[, selected_columns, drop = FALSE]
    filtered_data <- data[, ..selected_columns, drop = FALSE]
    
  } else {
    filtered_data <- data
  }
  
  if (any(sapply(filtered_data, is.numeric))) {
    categorical_columns <- sapply(filtered_data, function(col) is.factor(col) || is.character(col) || is.logical(col))
    #filtered_data <- filtered_data[, categorical_columns, drop = FALSE]
    filtered_data <- filtered_data[, ..categorical_columns, drop = FALSE]
  }
  
  return(filtered_data)
}




# This function calculates a summary for a categorical variable in the dataset.
# It returns a summary table with the frequency count and percentage for each unique category.
calculate_summary <- function(x) {
  data_summary <- data.frame(Category = x) %>%
    count(Category, name = "Frequency") %>%
    mutate(Percent = round((Frequency / sum(Frequency)) * 100, 1)
    ) %>%
    select(Category, Frequency, Percent) %>%
    mutate(Category = ifelse(is.na(Category), "missing", as.character(Category)))
  
  return(data_summary)
}




# This function generates a CSS style for a bar chart (or a progress bar).
# It is used to style bars by applying a linear gradient background and other visual properties.
bar_style <- function(width = 1, fill ="#009899", height = "75%", color = NULL) {  
  position <- paste0(width * 100, "%")
  image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = "#fff"
  )
}




# This function generates a styled reactable table to display summary statistics for a given dataset.
# It first computes the summary for each column in the provided dataset and then combines all the summaries into one table.
create_reactable <- function(filtered_data) {
  data_summary <- lapply(filtered_data, calculate_summary)
  combined_data <- do.call(rbind, data_summary) |>
    tibble::rownames_to_column("Columns") |>
    mutate(Columns = gsub("\\..*", "", Columns))

  reactable(
    combined_data, resizable = TRUE, wrap = FALSE, defaultPageSize = 20, bordered = TRUE, minRows = 10,highlight = TRUE,

    style = list(
      fontFamily = "Helvetica"
    ),

    columns = list(
      Category = colDef(name = "Category"),

      Frequency = colDef(
        style = function(value) {
          bar_style(width = value / max(combined_data$Frequency), fill = "#16525e", color = "#fff")
        },
        align = "left",
        format = colFormat(digits = 0)
      ),

      Percent = colDef(
        style = function(value) {
          bar_style(width = value / max(combined_data$Percent), fill = "#009899", color = "#fff")
        },
        align = "left",
        format = colFormat(digits = 1)
      ),

      Columns = colDef(
        style = JS("function(rowInfo, column, state) {
          const firstSorted = state.sorted[0];
          if (!firstSorted || firstSorted.id === 'Columns') {
            const prevRow = state.pageRows[rowInfo.viewIndex - 1];
            if (prevRow && rowInfo.values['Columns'] === prevRow['Columns']) {
              return { visibility: 'hidden' };
            }
          }
        }")
      )
    ),
    showPagination = TRUE
  )
}



# This function generates a summary text based on the given decision rules table and target feature for the model.
# It calculates the number of terminal nodes and the number of rules in the decision tree model, 
# and generates a descriptive text summarizing these key points.
generate_text_rules <- function(rules_table, target_feature) {

  n_terminal_nodes <- nrow(rules_table)
  n_rules <- nrow(rules_table)
    
  summary_text <- paste(
    "Your selected model with",
    paste(target_feature, "target feature generates", sep = " "),
    paste(n_terminal_nodes, "terminal nodes that produce", sep = " "),
    paste(n_rules, "different rules.", sep = " ")
    
  )
  
  return(summary_text)
}




# This function generates a summary text for classification predictions made by the model.
# It describes the target feature, the unique predicted categories, and the number of records in the prediction data.
generate_text_predictions_class <- function(predictions_table, target_feature) {
  predicted_categories <- unique(predictions_table$Predicted_Class)
  num_records <- nrow(predictions_table)
  
  summary_text <- paste(
          "Your model has generated predictions on a new dataset for the target feature", 
          target_feature, 
          "which has categories", 
          paste(predicted_categories, collapse = ", "), 
          ". Estimates have been generated for", 
          num_records, 
          ifelse(num_records == 1, "record.", "records.")
  )
  
  return(summary_text)
}



# Function generates a summary text for regression predictions made by the model.
# It describes the target feature and the number of records for which predictions were made.
generate_text_predictions_regress <- function(predictions_table, target_feature) {
  num_records <- nrow(predictions_table)
  summary_text <- paste(
    "Your model has generated predictions on a new dataset for the target feature", 
    target_feature, 
    paste(". Estimates have been generated for", num_records), 
    ifelse(num_records == 1, "record.", "records.")
  )
  
  return(summary_text)
}


# This function generates a text summary identifying issues with missing or mismatched columns
# between training data and prediction data, preventing the model from making predictions.
generate_text_missing_columns <- function(df_train, df_prediction) {
  prediction_columns <- colnames(df_prediction)
  training_columns <- colnames(df_train)
  missing_columns <- setdiff(training_columns, prediction_columns)
  extra_columns <- setdiff(prediction_columns, training_columns)
  type_mismatch_columns <- character(0)
  for (col in intersect(training_columns, prediction_columns)) {
    if (!identical(class(df_train[[col]]), class(df_prediction[[col]]))) {
      type_mismatch_columns <- c(type_mismatch_columns, col)
    }
  }
  
  text <- character(0)  
  
  if (length(missing_columns) > 0 && length(type_mismatch_columns) > 0) {
    text <- paste("The model cannot make predictions on Prediction Data because there are missing columns and data types do not match.",
                  "You have missing features (columns):", paste(missing_columns, collapse = ", "),
                  "in Prediction Data. Additionally, the following columns have mismatched data types:",
                  paste(type_mismatch_columns, collapse = ", "))
  } else if (length(missing_columns) > 0) {
    text <- paste("The model cannot make predictions on Prediction Data because there are missing columns.",
                  "You have missing features (columns):", paste(missing_columns, collapse = ", "),
                  "in Prediction Data.")
  } else if (length(type_mismatch_columns) > 0) {
    text <- paste("The model cannot make predictions on Prediction Data because data types do not match.",
                  "The following columns have mismatched data types:",
                  paste(type_mismatch_columns, collapse = ", "))
  } else {
    ""
  }
  
  return(text)
}




# This function checks for differences between training data and prediction data, including 
# missing columns and data type mismatches, and returns a logical value indicating whether
# the datasets are compatible for prediction.
check_differences <- function(df_train, df_prediction) {
  prediction_columns <- colnames(df_prediction)
  training_columns <- colnames(df_train)
  
  missing_columns <- setdiff(training_columns, prediction_columns)
  
  type_mismatch_columns <- character(0)
  
  for (col in intersect(training_columns, prediction_columns)) {
    if (!identical(class(df_train[[col]]), class(df_prediction[[col]]))) {
      type_mismatch_columns <- c(type_mismatch_columns, col)
    }
  }
  
  if (length(missing_columns) == 0 && length(type_mismatch_columns) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



# This function generates a summary text for features that have been excluded from the model training.
generate_text_excluded_feature <- function(count, features_name) {
  if (count > 0) {
    summary_text <- paste(
      "Machinely has automatically excluded",
      count, ifelse(count == 1, "feature", "features"),
      "from model training:", paste(features_name, collapse = ", "), 
      ". Check out these", ifelse(count == 1, "feature!", "features!")
    )
  } else {
    summary_text <- "Machinely has retained all features for model training"
  }
  
  return(summary_text)
}


