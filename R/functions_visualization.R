## Functions for data visualization

####
## Missing/unique/outlier


#' muo_table_prep(): a preparatory function for profiling_bar_plot().
#' 
muo_table_prep <- function(df, type, num_rows) {
  
  facet_size <- ceiling(ncol(df)/num_rows)
  
  if (type == "missing" | type == "unique") {
    table_plot <- diagnose(df) %>%
      mutate(unique_percent = unique_rate * 100) %>%
      mutate(across(ends_with("percent"), round, digits = 3))
  }
  
  else if (type == "outlier") {
    table_plot <- diagnose_outlier(df) %>%
      filter(outliers_cnt > 0) %>%
      arrange(desc(outliers_cnt)) %>%
      rename(val_count = outliers_cnt)
  }
  
  if (type == "missing") {
    table_plot <- table_plot %>% arrange(desc(missing_count)) %>%
      mutate(facet_var = rep(c(1:num_rows), each = facet_size)[c(1:ncol(df))]) %>%
      rename(val_count = missing_count)
  }
  else if (type == "unique") {
    table_plot <- table_plot %>% arrange(desc(unique_count)) %>%
      mutate(facet_var = rep(c(1:num_rows), each = facet_size)[c(1:ncol(df))]) %>%
      rename(val_count = unique_count)
  }
  
  return(table_plot)

}



#' profiling_bar_plot(): Create a summary bar plot of missing, unique, or outlier values in a data.frame.
#' 
#' Required inputs: the data.frame, the type of bar plot you want based on the way you want to examine the data (allowable
#' values are "missing", "unique", or "outlier"), whether the plot should have labels with the missing/unique/outlier count (labels). 
#' If there are a lot of variables in the data.frame, may want to split the plot into several rows, with facet = TRUE and providing
#' a number for num_rows.
profiling_bar_plot <- function(df, type = c("missing", "unique", "outlier"), labels = TRUE, facet = FALSE, num_rows = 2) {
  
  table_plot <- muo_table_prep(df, type = type, num_rows = num_rows)

  if (type == "missing") {
    g <- table_plot %>%
      ggplot(aes(x = fct_inorder(variables), y = missing_percent)) +
        geom_col()
  }
  else if (type == "unique") {
    g <- table_plot %>%
      ggplot(aes(x = fct_inorder(variables), y = unique_percent)) +
      geom_col()
  }
  else if (type == "outlier") {
    g <- table_plot %>%
      ggplot(aes(x = fct_inorder(variables), y = outliers_ratio)) +
      geom_col()
  }

  g <- g + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 105)) +
      xlab(NULL) + ylab(paste(type, "percent")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  if (labels) {
    g <- g + geom_label(aes(label = val_count), vjust = -0.5)
  }

  if (facet) {
    g <- g + facet_wrap(vars(facet_var), scales = "free_x", nrow = num_rows) +
      theme(strip.background = element_blank(), strip.text = element_blank())
  }

  return(g)
  
}



####
## Frequencies

### Frequencies - Dates

#' make_date_plot(): Create a bar plot summarizing the date range covered of a date variable.
#' Called by plot_bar_date().
#' 
#' Required input: the data.frame, and the column name of the date variable you want to plot.
make_date_plot <- function(df, date_var) {
  
  plot_dat <- df %>% select(all_of(date_var))
  plot_dat$date_col <- df %>% pull(all_of(date_var)) %>% date()
  
  g <- plot_dat %>% ggplot(aes(x = date_col)) +
    geom_bar() +
    scale_x_date() +
    ggtitle(date_var) +
    theme_bw()
  
  return(g)

}


#' plot_bar_date(): loop through all date variables in a data.frame and run make_date_plot() function.
#' 
#' Required input: the data.frame, and a vector of the column names for the date variables you want to plot.
plot_bar_date <- function(df, date_vars) {

  if (length(date_vars) == 0) {
    date_vars <- names(df)
  }
  
  plist <- lapply(date_vars, make_date_plot, df = df)
  
  names(plist) <- date_vars
 
  for (d in date_vars) {
    print(plist[d])
  }

}



### Frequencies - Continuous

#' make_cont_plot(): Make a density plot of the distribution of a single numeric variable.
#' Called by plot_cont_density().
#' 
#' Required input: the data.frame, and the column name of the numeric variable you want to plot.
#' For numeric variables with a few extreme outliers, you can opt to limit the x-axis of the plot with the limit argument.
#' Specifying limit (needs to be between 0 and 1) limits the x axis to the "limit"-th quantile of the data, times two.
make_cont_plot <- function(df, num_var, limit) {
  
  g <- df %>% ggplot(aes(x = .data[[num_var]])) +
    geom_line(stat = "density", lwd = 0.75) + 
    ggtitle(num_var) +
    theme_bw()
  
  if (limit < 1) {
    x_min <- df %>% 
      pull(num_var) %>% 
      min()
    x_max <- df %>%
      pull(num_var) %>%
      quantile(probs = limit, na.rm = TRUE) %>%
      as.numeric() %>%
      multiply_by(2)
    g <- g + coord_cartesian(xlim = c(x_min, x_max))
  }
  
  return(g)
  
}


#' plot_cont_density(): loop through all date variables in a data.frame and run make_date_plot() function.
#' 
#' Required input: the data.frame, and a vector of the column names for the numeric variables you want to plot.
#' For numeric variables with a few extreme outliers, you can opt to limit the x-axis of the plot with the limit argument.
#' Specifying limit (needs to be between 0 and 1) limits the x axis to the "limit"-th quantile of the data, times two.
plot_cont_density <- function(df, num_vars, limit = 1) {
  
  if (length(num_vars) == 0) {
    num_vars <- df %>%
      select(-where(is.POSIXt)) %>%
      select(where(is.double)) %>%
      names()
  }
  
  plist <- lapply(num_vars, make_cont_plot, df = df, limit = limit)
  
  names(plist) <- num_vars
  
  for (i in num_vars) {
    print(plist[i])
  }
  
}

