# install pacman package if not already installed
if (!require(pacman)) {
  install.packages("pacman")
}

# load required packages with pacman p_load function
pacman::p_load(tidyverse, gmodels)


#function to check if data is discrete
check_discrete <- \(data, cutoff = 10){
  discrete = vector()
  
  if (is.vector(data)) {
    data <- na.omit(data)
    discrete <- length(unique(data)) <= cutoff | is.factor(data)
  }else{
    warning("Unsure if grouping variable/s are discrete")
  }
  
  return(discrete)
}

#function to check to see if grouping variables provided to summariser function are likely discrete. If not provide warning. 
are_cols_discrete <- function(data, cols, i) {
  
  df <-
    data %>% 
    select(all_of(cols))
  
  discrete <- vector()
  count = 1
  for (i in df) {
    discrete[count] <- check_discrete(i, cutoff = 10)
    count = count + 1
  }
  
  if(!all(discrete, na.rm = T)){
    warning("The grouping variable/s you have selected may be of continuous type")
  }
}



# function to summarise data
summariser <- \(data, group_cols = NULL, digits = 2, incl_ci = T, confidence = 0.95, na_rm = T,
  incl_se = T,
  incl_min = T,
  incl_max = T,
  incl_med = F,
  incl_n_na = F,
  wide = T,
  ...){
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe")
  }
  
  are_cols_discrete(data, group_cols)

  stats <- list(
    mean = ~ mean(.x, na.rm = na_rm),
    median = ~ median(.x, na.rm = na_rm),
    sd = ~ sd(.x, na.rm = na_rm),
    min = ~ min(.x, na.rm = na_rm),
    max = ~ max(.x, na.rm = na_rm),
    se = ~ gmodels::ci(.x, confidence = confidence, na.rm = na_rm)[4],
    lowci = ~ gmodels::ci(.x, confidence = confidence, na.rm = na_rm)[2],
    hici = ~ gmodels::ci(.x, confidence = confidence, na.rm = na_rm)[3],
    total_n = ~ n(),
    n = ~ sum(!is.na(.x)),
    na_n = ~ sum(is.na(.x))
  )

  d1 <- data %>%
    group_by(across(all_of(group_cols))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        stats,
        .names = "{.col}-{.fn}"
      ),
      .groups = "drop"
    ) %>%
    ungroup()

  out <-
    d1 %>%
    pivot_longer(
      cols = !group_cols,
      names_to = c("var", "stat"),
      names_sep = "-",
      values_to = "value"
    ) %>%
    mutate(value = round(value, digits)) %>%
    mutate(across(all_of(group_cols), as_factor))

  if (incl_ci == F) {
    out <-
      out %>%
      filter(!grepl("ci", stat))
  }

  if (incl_se == F) {
    out <-
      out %>%
      filter(!grepl("se", stat))
  }

  if (incl_min == F) {
    out <-
      out %>%
      filter(!grepl("min", stat))
  }

  if (incl_max == F) {
    out <-
      out %>%
      filter(!grepl("max", stat))
  }

  if (incl_med == F) {
    out <-
      out %>%
      filter(!grepl("median", stat))
  }

  if (incl_n_na == F) {
    out <-
      out %>%
      filter(!grepl("n_na", stat))
  }

  if(wide){
    out <-
      out %>%
      pivot_wider(
        names_from = stat,
        values_from = value
      ) %>%
      arrange(var)
  }


  return(out)
}