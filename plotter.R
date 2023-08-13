#install pacman package if not already installed
if (!require(pacman)) {
  
  install.packages("pacman")  
  
}

#load required packages with pacman p_load function
pacman::p_load(tidyverse, ggpubr, rstatix)


#using ggpubr automatic
mtcars %>% 
  ggbarplot(., "cyl", "mpg",
            fill = "cyl", color = "cyl",
            palette = get_palette(palette = 'jco', k = 3),
            label = T,
            lab.pos = 'in',
            lab.col = "white",
            add = "mean_ci",
            lab.nb.digits = 2, 
            facet.by = c("am")) +
  theme(legend.position = 'none') +
  stat_compare_means(method = "anova") +                                         # Global p-value
  stat_compare_means(method = "t.test", 
                     ref.group = "8", label = "p.signif",
                     label.y = c(32))


#using ggpubr manual
stat <- 
  mtcars %>%
  group_by(am) %>%
  rstatix::t_test(mpg ~ cyl) %>%
  add_xy_position(x = "cyl")

num_colours <- 
  mtcars %>% 
  select(cyl) %>% 
  distinct() %>% 
  nrow()

mtcars %>% 
  ggbarplot(., "cyl", "mpg",
            fill = "cyl", color = "cyl",
            palette = get_palette(palette = 'jco', k = 3),
            label = T,
            lab.pos = 'in',
            lab.col = "white",
            add = "mean_ci",
            lab.nb.digits = 2, 
            facet.by = c("am")) +
  theme(legend.position = 'none') +
  stat_pvalue_manual(stat, label = "p.adj.signif", 
                     tip.length = 0.01, 
                     bracket.nudge.y = 1)







nice_bar_chart <- \(data, x_col, y_col, group_cols = NULL, palette = 'jco',
                    b_nudge = 0, ...) {
  
  xcol = deparse(substitute(x_col))
  ycol = deparse(substitute(y_col))
  
  
  num_colours <- 
    data %>% 
    select({{x_col}}) %>% 
    distinct() %>% 
    nrow()
  
  stat <- 
    data %>%
    group_by(across(all_of(group_cols))) %>% 
    rstatix::t_test(as.formula(paste(ycol, '~', xcol)), paired = FALSE) %>% 
    rstatix::add_xy_position(x = xcol)
  
  plot <-
    data %>% 
    ggbarplot(., xcol, ycol,
              fill = xcol, color = xcol,
              palette = get_palette(palette = palette, k = num_colours),
              label = T,
              lab.pos = 'in',
              lab.col = "white",
              add = "mean_ci",
              lab.nb.digits = 2, 
              facet.by = group_cols,
              ...) +
    theme(legend.position = 'none',
          ...) +
    stat_pvalue_manual(stat, label = "p.adj.signif", 
                       tip.length = 0.01, 
                       bracket.nudge.y = b_nudge)
  
  
  list <- list(stat = stat, plot = plot)
  
  
  return(list)
}

#test function
mtcars %>% 
  nice_bar_chart(., x_col = cyl, y_col = hp, 
                 b_nudge = -120, 
                 axis.text = element_text(size = 16))
