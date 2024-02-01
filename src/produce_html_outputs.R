library(tidyverse)
library(brms)
library(formattable)
library(kableExtra)  # Add this line

# Custom function to recursively round numeric elements in a list
round_numeric <- function(x, digits = 2) {
  if (is.list(x)) {
    lapply(x, round_numeric, digits = digits)
  } else if (is.numeric(x)) {
    round(x, digits = digits)
  } else {
    x
  }
}

# Function to read a brm model and its summary
read_brms_model <- function(file_path) {
  model <- readRDS(file_path)
  return(list(file_path = file_path, model = model))
}

# Folder containing the brms models with double backslashes
folder_path <- "/home/spinney/scratch/coventure/models/sdq/models"

# List all RDS files in the folder
file_list <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Read all brms models
brms_models <- map(file_list, read_brms_model)

# Create a directory to store the HTML files with double backslashes
html_output_dir <- "/home/spinney/scratch/coventure/models/sdq/html_output"
dir.create(html_output_dir, showWarnings = FALSE)

# Generate HTML files for each model
walk(brms_models, function(brms_model) {
  model_name <- tools::file_path_sans_ext(basename(brms_model$file_path))
  
  fixed_effects <- fixef(brms_model$model)
  
  # Create a data frame for the fixed effects
  fixed_effects_df <- as.data.frame(fixed_effects)
  
  # Create a kableExtra table for the fixed effects
  kable_obj <- kable(fixed_effects_df, format = "html", caption = model_name) %>%
    kable_styling(full_width = FALSE) %>%
    column_spec(1:ncol(fixed_effects_df), bold = TRUE, border_right = TRUE)
  
  # Save the kableExtra table to an HTML file if it doesn't exist
  html_file_path <- file.path(html_output_dir, paste0(model_name, "_summary.html"))
  if (!file.exists(html_file_path)) {
    save_kable(kable_obj, html_file_path)
    cat("HTML file saved:", html_file_path, "\n")
  } else {
    cat("HTML file already exists, skipping:", html_file_path, "\n")
  }
})
