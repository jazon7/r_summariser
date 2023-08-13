#install pacman package if not already installed
if (!require(pacman)) {

install.packages("pacman")  

}

#load required packages with pacman p_load function
pacman::p_load(tidyverse, gmodels)


#function to summarise data
summariser <- \(data, group_cols = NULL, digits = 2, incl_ci = T,
  incl_se = T,
  incl_min = T,
  incl_max = T,
  incl_med = F,
  incl_n_na = F,
  ...)

{
  
  stats <- list(
    mean = ~ mean(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE),
    min = ~ min(.x, na.rm = TRUE),
    max = ~ max(.x, na.rm = TRUE),
    se = ~ gmodels::ci(.x)[4],
    lowci = ~ gmodels::ci(.x)[2],
    hici = ~ gmodels::ci(.x)[3],
    n = ~ n(),
    n_na = ~ sum(is.na(.x))
  )

  d1 <- data %>%
    group_by(across(all_of(group_cols))) %>% 
    dplyr::summarise(
      dplyr::across(
        dplyr::where(is.numeric),
        stats,
        .names = "{.col}-{.fn}"
      )
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
    mutate(value = round(value, digits)) 

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
  
  return(out)
}


#test function

test <-
  summariser(data = mtcars,
             group_cols = c("cyl"))

test1 <- 
  summariser(data = mtcars %>% select(cyl, mpg, hp), 
             group_cols = c("cyl")
             )


#pivot wider
test1 %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(n_label = str_glue("n={n}"))

test1 %>% 

  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate(n_label = str_glue("n={n}")) %>% 
  filter(var == 'mpg') %>%
  ggplot(aes(x = as_factor(cyl), y = mean, fill = as_factor(cyl))) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=lowci, ymax=hici), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label = n_label),
            nudge_y = -5 , nudge_x = -0
  ) +
  xlab("Cyl") +
  ggpubr::theme_pubr() +
  theme(legend.position = 'none')


  