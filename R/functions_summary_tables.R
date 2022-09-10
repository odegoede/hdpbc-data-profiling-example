## Functions for summary tables

####
## Summarizing missing and unique values

#' missing_summary(): Get a summary table of missing values by column in your data.frame.
#' 
#' Required inputs: the data.frame you want summarized.
missing_summary <- function(df) {
  
  res <- df %>%
    dlookr::diagnose() %>%
    mutate(missing_percent = round(missing_percent, digits = 3)) %>%
    select(variables, missing_count, missing_percent)
  
}



#' unique_summary(): Get a summary table of unique values by column in your data.frame.
#' 
#' Required inputs: the data.frame you want summarized.
unique_summary <- function(df) {
  
  res <- df %>%
    dlookr::diagnose() %>%
    mutate(unique_percent = round((unique_rate * 100), digits = 3)) %>%
    select(variables, unique_count, unique_percent)
  
}



####
## Most rare and most common categorical variables

#' get_most_common(): Get the top n most common values in each categorical column.
#' 
#' Returns a summary table of the most common values for each categorical column, including how many times they occur in the data.frame.
#' Required input: the data.frame, as well as how many top values you want displayed (top_num), and a vector of 
#' any columns you want to exclude (exclude)
get_most_common <- function(df, top_num = 5, exclude = NA) {
  
  if (!is.na(exclude)) {
    res <- df %>%
      select(-all_of(exclude)) %>%
      diagnose_category() %>%
      group_by(variables) %>%
      slice_min(n = top_num, order_by = rank)
  }
  else {
    res <- df %>%
      diagnose_category() %>%
      group_by(variables) %>%
      slice_min(n = top_num, order_by = rank)
  }
  
  return(res)
  
}



#' get_most_rare(): Get the top n most rare values in each categorical column.
#' 
#' Returns a summary table of the most rare values for each categorical column, including how many times they occur in the data.frame.
#' Required input: the data.frame, as well as how many top values you want displayed (top_num), and a vector of 
#' any columns you want to exclude (exclude)
get_most_rare <- function(df, top_num = 5, exclude = NA) {
  
  if (!is.na(exclude)) {
    res <- df %>%
      select(-all_of(exclude)) %>%
      diagnose_category() %>%
      group_by(variables) %>%
      slice_max(n = top_num, order_by = rank)
  }
  else {
    res <- df %>%
      diagnose_category() %>%
      group_by(variables) %>%
      slice_max(n = top_num, order_by = rank)
  }
  
  return(res)
  
}



####
## Hierarchy checks of categorical variables

#' get_hierarchy_summary_table(): For a pair of categorical variables where one variable assigns the other into broader categories,
#' display a summary of these hierarchical category relationships. 
#' Called by get_multicategory_values(), and can also be called on its own. 
#' 
#' Returns a summary table of which values are assigned to which categories.
#' Required input: the data.frame to check, column name of the fine-grained values (value_var), column name of the category assignments
#' for these fine-grained values (cat_var).
get_hierarchy_summary_table <- function(df = NULL, value_var = NULL, cat_var = NULL) {
  
  res <- df %>%
    group_by(across(all_of(c(value_var, cat_var)))) %>%
    tally() %>%
    arrange(all_of(value_var))
  
  return(res)
  
}



#' get_multicategory_values(): For a pair of categorical vairables where one variable assigns the other into broader categories,
#' search for any values that have been assigned to >1 category in the data.
#' 
#' Returns a statement of which values have been assigned to more than one category in the data. This is a data quality check:
#' for hierachical variables, we'd expect each value to have only one category assignment. The results can be reported either as
#' a vector of values which have been assigned to more than one category (if as_vector = TRUE), or just messages in the console of
#' which categories a multi-category variable has been assigned to (if as_vector = FALSE).
#' Required input: the data.frame to check, column name of the fine-grained values (value_var), column name of the category assignments
#' for these fine-grained values (cat_var). Also, a TRUE/FALSE for whether you want the results returned as a vector or printed mesages (as_vector).
get_multicategory_values <- function(df = NULL, value_var = NULL, cat_var = NULL, as_vector = TRUE) {
  
  hierarchy_summary <- get_hierarchy_summary_table(df = df, value_var = value_var, cat_var = cat_var)
  
  multicat_summary <- hierarchy_summary %>% filter(n() > 1)
  
  multicat_vals <- multicat_summary %>% pull(all_of(value_var)) %>% unique()
  
  if (as_vector) {
    return(multicat_vals)
  }
  else {
    for (i in multicat_vals) {
      cats <- multicat_summary %>% filter(.data[[value_var]] == i) %>% pull(all_of(cat_var)) %>% paste(collapse = ", ")
      answer <- paste(value_var, "value", i, "is assigned to", cat_var, "category values:", cats)
      message(answer)
    }
  }
  
}

