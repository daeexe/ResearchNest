library(dplyr)
library(lubridate)

# 1. Check for Missing Values
check_missing_values <- function(data) {
  missing_summary <- sapply(data, function(col) sum(is.na(col)))
  missing_summary_df <- data.frame(Column = names(missing_summary), Missing_Values = missing_summary)
  missing_summary_df <- missing_summary_df %>% filter(Missing_Values > 0)
  if (nrow(missing_summary_df) == 0) {
    message("No missing values detected.")
  } else {
    message("Missing values summary:")
    print(missing_summary_df)
  }
}

# 2. Check for Incorrect Data Types
check_data_types <- function(data) {
  type_summary <- sapply(data, class)
  message("Data types summary:")
  print(type_summary)
}

# 3. Check for Duplicates
check_duplicates <- function(data) {
  duplicate_count <- nrow(data) - nrow(distinct(data))
  if (duplicate_count > 0) {
    message(paste("There are", duplicate_count, "duplicate rows in the dataset."))
  } else {
    message("No duplicate rows found.")
  }
}

# 4. Check for Outliers (Numerical Columns Only)
check_outliers <- function(data) {
  numeric_columns <- data %>% select_if(is.numeric)
  outliers <- sapply(numeric_columns, function(col) {
    q1 <- quantile(col, 0.25, na.rm = TRUE)
    q3 <- quantile(col, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    sum(col < (q1 - 1.5 * iqr) | col > (q3 + 1.5 * iqr), na.rm = TRUE)
  })
  outliers_summary_df <- data.frame(Column = names(outliers), Outliers = outliers)
  outliers_summary_df <- outliers_summary_df %>% filter(Outliers > 0)
  if (nrow(outliers_summary_df) == 0) {
    message("No outliers detected.")
  } else {
    message("Outliers summary:")
    print(outliers_summary_df)
  }
}

# 5. Check Date Consistency (For columns expected to be Dates)
check_date_consistency <- function(data, date_columns) {
  for (col in date_columns) {
    if (!all(is.na(data[[col]]) | !is.na(dmy(data[[col]])))) {
      message(paste("Inconsistent date formats found in column:", col))
    } else {
      message(paste("All dates in column", col, "are consistent."))
    }
  }
}

# 6. Check for Out-of-Range Values (Specific Columns)
check_value_ranges <- function(data, column, min_val, max_val) {
  out_of_range <- data %>% filter(data[[column]] < min_val | data[[column]] > max_val)
  if (nrow(out_of_range) > 0) {
    message(paste("Out-of-range values detected in column", column, "with", nrow(out_of_range), "rows."))
    print(out_of_range)
  } else {
    message(paste("All values in column", column, "are within the specified range."))
  }
}

# 7. Run All Checks
run_all_checks <- function(data, date_columns = NULL, range_checks = list()) {
  message("Running data quality checks...")
  check_missing_values(data)
  check_data_types(data)
  check_duplicates(data)
  check_outliers(data)
  if (!is.null(date_columns)) check_date_consistency(data, date_columns)
  for (range_check in range_checks) {
    check_value_ranges(data, range_check$column, range_check$min_val, range_check$max_val)
  }
  message("Data quality checks completed.")
}

date_columns = c("TIME_PERIOD") # Specify date columns
range_checks = list(list(column = "OBS_VALUE", min_val = 0, max_val = 100))
run_all_checks(data, date_columns, range_checks)
