
# This function prepares a dataset for use with the rpart algorithm by performing necessary transformations on the data.
prepare_data_for_rpart <- function(data) {
  data_df <- as.data.frame(data)
  
  char_vars <- sapply(data_df, is.character)
  data_df[char_vars] <- lapply(data_df[char_vars], as.factor)
  
  logical_vars <- sapply(data_df, is.logical)
  data_df[logical_vars] <- lapply(data_df[logical_vars], as.factor)
  
  valid_vars <- sapply(data_df, function(col) is.factor(col) || is.numeric(col) || is.integer(col) || is.logical(col))
  selected_vars <- colnames(data_df[valid_vars])
  excluded_vars <- colnames(data_df)[!valid_vars]
  
  for (col_name in selected_vars) {
    if (is.factor(data_df[[col_name]])) {
      num_categories <- length(levels(data_df[[col_name]]))
      if (num_categories > 50) {
        excluded_vars <- c(excluded_vars, col_name)
      }
    }
  }
  
  excluded_info <- list(
    count = length(excluded_vars),
    variables = excluded_vars
  )
  
  data_df <- data_df[, valid_vars, drop = FALSE]
  
  return(list(data = data_df, excluded_info = excluded_info))
}



# Function creates a decision tree model using the `rpart` function in R.
create_decision_tree <- function(data, target_col, method, parms, cp_threshold, max_depth_value, minsplit_value) {
  tree <- rpart(target_col, 
                data = data, 
                method = method, 
                parms = parms, 
                control = rpart.control(cp = cp_threshold, maxdepth = max_depth_value, minsplit = minsplit_value))
  return(tree)
}


# Function generates a color mapping for each unique modality in the target variable, 
# which can be used for visualizing categorical data in plots.
generateColorYData <- function(dataframe, targetVariable, colorRange) {
  if (!(targetVariable %in% colnames(dataframe))) {
    stop("Ciljna varijabla nije pronađena u dataframe-u.")
  }
  
  modalities <- unique(dataframe[[targetVariable]])
  
  numModalities <- length(modalities)
  colors <- colorRampPalette(colors = colorRange)(numModalities)
  
  colorYData <- data.frame(
    modality = modalities,
    color = colors
  )
  
  return(colorYData)
}



# This function visualizes a decision tree based on its type (classification or regression),
# and optionally applies color mappings for the target variable, using the visTree package.
tree_visualization <- function(decision_tree, model_type, colorYData) {
  
  if (model_type == "Classification") {
    
    visTree(decision_tree, main = "", nodesPopSize = TRUE, minNodeSize = 20, 
            maxNodeSize = 40, height = "100%", width = "100%", colorEdges = "#a4b3b6", shapeVar = "dot",
            shapeY = "dot",
            colorVar = "#16525e", 
            colorY = colorYData,
            tooltipDelay = 500,
            nodesFontSize = 14,
            edgesFontSize = 0,
            legend = FALSE,
            
            highlightNearest =  list(enabled = TRUE,
                                     degree = list(from = 50000, to = 0), hover = TRUE,
                                     algorithm = "hierarchical")
    )
    
  } else if (model_type == "Regression"){
    
    visTree(decision_tree, main = "", nodesPopSize = TRUE, minNodeSize = 20, 
            maxNodeSize = 40, height = "100%", width = "100%", colorEdges = "#a4b3b6", shapeVar = "dot",
            shapeY = "dot",
            colorVar = "#16525e",
            colorY =  c("#eaccb7", "#8f552b"),
            tooltipDelay = 500,
            nodesFontSize = 14,
            edgesFontSize = 0,
            legend = FALSE,
            
            highlightNearest =  list(enabled = TRUE,
                                     degree = list(from = 50000, to = 0), hover = TRUE,
                                     algorithm = "hierarchical")
    )
    
    
  }
  
}




# This function determines the prediction type based on the type of the target variable.
# It returns either "class" for classification tasks or "vector" for regression tasks.
get_prediction_type <- function(dependent_col_dt_obj) {
  if (is.factor(dependent_col_dt_obj) || is.logical(dependent_col_dt_obj)) {
    return ("class")
  } else if (is.numeric(dependent_col_dt_obj) || is.integer(dependent_col_dt_obj)) {
    return ("vector")
  } else {
    stop("Unsupported prediction type.")
  }
}





# This function generates predictions using a decision tree model, evaluates its performance, and returns the results.
# It supports both classification and regression tasks and calculates various performance metrics.
generate_predictions <- function(decision_tree, train_data, test_data, prediction_type, dependent_var) {
  # Predviđanja na test podacima
  if (prediction_type == "class") {
    predicted_class_train <- predict(decision_tree, newdata = train_data, type = "class")
    predicted_class_test <- predict(decision_tree, newdata = test_data, type = "class")

    predicted_prob_train <- predict(decision_tree, newdata = train_data, type = "prob")
    predicted_prob_test <- predict(decision_tree, newdata = test_data, type = "prob")
    

    confusion_matrix_train <- confusionMatrix(data = predicted_class_train, reference = train_data[[dependent_var]], mode = "prec_recall")
    confusion_matrix_test <- confusionMatrix(data = predicted_class_test, reference = test_data[[dependent_var]], mode = "prec_recall")
    
    
    confusion_matrix <- table("True Category" = train_data[[dependent_var]], "Predicted Category" = predicted_class_train)
    
    
    train_kappa <- confusion_matrix_train$overall['Kappa']
    test_kappa <- confusion_matrix_test$overall['Kappa']
    
    train_accuracy <- confusion_matrix_train$overall['Accuracy']* 100
    test_accuracy <- confusion_matrix_test$overall['Accuracy'] * 100
   
    
    N_train<-nrow(train_data)
    N_test<-nrow(test_data)
    
      
    model_type <- "Classification"

    predictions_df <- list(
      Train = data.frame(
        Actual = train_data[[dependent_var]],
        Predicted_Class = predicted_class_train,
        Predicted_Prob = predicted_prob_train
      ),
      Test = data.frame(
        Actual = test_data[[dependent_var]],
        Predicted_Class = predicted_class_test,
        Predicted_Prob = predicted_prob_test
      )
    )

  } else if (prediction_type == "vector") {
    predicted_values_train <- predict(decision_tree, newdata = train_data, type = "vector")
    predicted_values_test <- predict(decision_tree, newdata = test_data, type = "vector")

    train_accuracy <- 1 - sum((predicted_values_train - train_data[[dependent_var]])^2) /
      sum((train_data[[dependent_var]] - mean(train_data[[dependent_var]]))^2)

    test_accuracy <- 1 - sum((predicted_values_test - test_data[[dependent_var]])^2) /
      sum((test_data[[dependent_var]] - mean(test_data[[dependent_var]]))^2)
  
    N_train<-nrow(train_data)
    N_test<-nrow(test_data)
    
    train_mae <- mean(abs(predicted_values_train - train_data[[dependent_var]]))
    test_mae <- mean(abs(predicted_values_test - test_data[[dependent_var]]))
    
    train_mse <- mean((predicted_values_train - train_data[[dependent_var]])^2)
    test_mse <- mean((predicted_values_test - test_data[[dependent_var]])^2)
    
    
    model_type <- "Regression"

    predictions_df <- list(
      Train = data.frame(
        Actual = train_data[[dependent_var]],
        Predicted = predicted_values_train
      ),
      Test = data.frame(
        Actual = test_data[[dependent_var]],
        Predicted = predicted_values_test
      )
    )
  } else {
    return(NULL)
  }
  
  if (prediction_type == "class") {
  return(list(predictions_df = predictions_df, train_accuracy = train_accuracy,
              test_accuracy = test_accuracy, model_type = model_type, N_train=N_train, N_test=N_test,
              train_kappa=train_kappa, test_kappa=test_kappa, confusion_matrix=confusion_matrix
              ))
  }else{
    return(list(predictions_df = predictions_df, train_accuracy = train_accuracy,
                test_accuracy = test_accuracy, model_type = model_type, 
                N_train=N_train, N_test=N_test,
                train_mae=train_mae, test_mae=test_mae, train_mse=train_mse,test_mse=test_mse ))
    
    
    
  }
}



# This function analyzes the dependent variable (target column) in a dataset to determine the appropriate model type 
# and the method for splitting in decision tree algorithms based on the data type of the dependent column.
analyze_dependent_column <- function(dependent_col_dt_obj) {
  if (!is.null(dependent_col_dt_obj)) {
    if (is.factor(dependent_col_dt_obj) || is.logical(dependent_col_dt_obj)) {
      method <- "class"
      parms <- list(split = "information")
    } else if (is.numeric(dependent_col_dt_obj) || is.integer(dependent_col_dt_obj)) {
      method <- "anova"
      parms <- list(split = NULL)
    }
    
    return(list(method = method, parms = parms))
  } else {
    return(NULL)
  }
}



# This function maps the data types from a source dataframe to a target dataframe. 
# It ensures that the target dataframe columns have the same data types as the corresponding columns in the source dataframe.
map_data_types_from_UI <- function(source_df, target_df) {
      for (col_name in colnames(source_df)) {
        if (is.factor(source_df[[col_name]])) {
          target_df[[col_name]] <- factor(target_df[[col_name]], levels = levels(source_df[[col_name]]))
        } else {
          target_df[[col_name]] <- as(target_df[[col_name]], class(source_df[[col_name]]))
        }
      }
      return(target_df)
}




# This function maps the data types and column structure from a source dataframe to a target dataframe. 
# It handles missing columns, extra columns, and ensures that the data types of matching columns are consistent between the two dataframes.
map_data_types <- function(source_df, target_df) {
  missing_cols <- setdiff(colnames(source_df), colnames(target_df))
  extra_cols <- setdiff(colnames(target_df), colnames(source_df))
  
  if (length(missing_cols) > 0) {
    for (col_name in missing_cols) {
      target_df[[col_name]] <- NA
    }
  }
  
  if (length(extra_cols) > 0) {
    target_df <- target_df[, !colnames(target_df) %in% extra_cols]
  }
  

  for (col_name in colnames(source_df)) {
    
    if (is.factor(source_df[[col_name]])) {
      target_df[[col_name]] <- factor(target_df[[col_name]], levels = levels(source_df[[col_name]]))
      
    } else if (is.integer(source_df[[col_name]])) {
      target_df[[col_name]] <- as.integer(round(target_df[[col_name]]))
      
    } else if (is.numeric(source_df[[col_name]])) {
      target_df[[col_name]] <- as.numeric(target_df[[col_name]])
      
    } else {
      target_df[[col_name]] <- as(target_df[[col_name]], class(source_df[[col_name]]))
    }
  }
  
  # Brisanje vještački dodatih kolona
  if (length(missing_cols) > 0 || length(extra_cols) > 0) {
    target_df <- target_df[, !colnames(target_df) %in% c(missing_cols, extra_cols)]
  }
  
  return(target_df)
}




# This function generates predictions for a given decision tree model, based on the type of prediction 
# (classification or regression) and input data for prescriptive analysis.
generate_predictions_prescript <- function(decision_tree, data_prescript, prediction_type) {

    if (prediction_type == "class") {
    
    predicted_class <- predict(decision_tree, newdata = data_prescript, type = "class")
    predicted_prob <- predict(decision_tree, newdata = data_prescript, type = "prob")

    
    model_type <- "Classification"
    
    predictions_df <- list(
      pred_data = data.frame(
        Predicted_Class = predicted_class,
        Probability = predicted_prob
      )
    )
    
  } else if (prediction_type == "vector") {
    
    predicted_values <- predict(decision_tree, newdata = data_prescript, type = "vector", interval = "prediction")

    
    model_type <- "Regression"
    
    predictions_df <- list(
      pred_data = data.frame(
        Predicted = predicted_values
      )
    )
  } else {
    return(NULL)
  }
  
  if (prediction_type == "class") {
    return(list(predictions_df = predictions_df, model_type = model_type))
    
  }else{
    return(list(predictions_df = predictions_df, model_type = model_type))
    
    
    
  }
}



# Function renames columns in a dataset, specifically targeting the class column 
# and adding a prefix to probability-related columns for better clarity.
rename_columns <- function(data, class_column_name = "Predicted_Class", prefix = "Probability") {
  colnames(data)[colnames(data) == class_column_name] <- "Predicted Class"
  colnames(data) <- gsub(paste0("^", prefix, "\\."), paste0(prefix, " "), colnames(data))
  
  return(data)
}



# This function extracts the row from a dataset (excluding the first column) 
# that contains the maximum value across the rows.
extract_max_row <- function(df) {
  max_values <- apply(df[, -1], 1, max)  
  max_row_index <- which.max(max_values)  
  return(df[max_row_index, ])
}


# This function selects columns from a dataset based on the maximum numeric value in each column 
# (excluding the first column) and returns a subset of the dataset containing the first column 
# and the column with the highest numeric value.
select_columns <- function(df) {
  numeric_cols <- sapply(df[, -1], as.numeric)
  max_col_index <- which.max(numeric_cols)
  selected_cols <- c(1, max_col_index + 1)
  return(df[, selected_cols, drop = FALSE])
}



# Function transforms a data frame by removing the 'Predicted Class' column, 
# renaming columns that start with 'Probability', and reshaping the data from wide to long format 
# by gathering all columns (except the 'Predicted Class') into a single column with categories and probabilities.
transform_dataframe <- function(df) {
  df <- df %>%
    select(- `Predicted Class`)
  
  names(df) <- sub("^Probability\\s+", "", names(df))
  
  df <- df %>%
    gather(Category, Probability)
  
  return(df)
}




# Function compares two data frames to check if they have the same structure.
# It ensures that both data frames have the same number of columns, identical column names, and matching data types.
compareDataFrames <- function(data1, data2) {
  if (ncol(data1) != ncol(data2)) {
    return(FALSE)
  }
  col_names_data1 <- colnames(data1)
  col_names_data2 <- colnames(data2)
  if (!identical(col_names_data1, col_names_data2)) {
    return(FALSE)
  }
  data_types_data1 <- sapply(data1, class)
  data_types_data2 <- sapply(data2, class)
  if (!identical(data_types_data1, data_types_data2)) {
    return(FALSE)
  }
  return(TRUE)
}



