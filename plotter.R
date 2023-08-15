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

nice_bar_chart <- \(data, x_col, y_col, group_cols = NULL, palette = 'jco',
                    b_nudge = 0, y_breaks= NULL, y_limits= NULL, label = F,
                    lb_vjust = -10, ...) {
  
  xcol = deparse(substitute(x_col))
  ycol = deparse(substitute(y_col))
  
  
  num_colours <- 
    data %>% 
    select({{x_col}}) %>% 
    distinct() %>% 
    nrow()
  
  normality <-
  data %>% 
  group_by(across(all_of(group_cols))) %>%  
  shapiro_test(., vars = ycol)
  
  stat2 <-
    data %>% 
    ggpubr::compare_means(
      as.formula(paste(ycol, '~', xcol)) , data = ., group.by = group_cols,
      method = "t.test", 
      p.adjust.method = "bonferroni", 
      paired = FALSE)
  
  # stat <- 
  #   data %>%
  #   group_by(across(all_of(group_cols))) %>% 
  #   rstatix::t_test(as.formula(paste(ycol, '~', xcol)), paired = FALSE) %>% 
  #   rstatix::add_xy_position(x = xcol)
  
  x_label <- str_to_title(xcol)
  y_label <- str_to_title(ycol)
  
  y_max <- data %>% 
                 group_by(across(all_of(group_cols))) %>%  
                 summarise(max = max({{y_col}})) %>%
                 pull())
  
  print(y_max)
  
  plot <-
    data %>% 
    ggbarplot(., 
              xcol, 
              ycol,
              fill = xcol, color = "black",
              palette = get_palette(palette = palette, k = num_colours),
              label = label,
              sort.val = c("desc"),
              size = 1,
              add = "mean_ci",
              lab.nb.digits = 2, 
              facet.by = group_cols,
              xlab = x_label,
              ylab = y_label,
              lab.vjust = lb_vjust,
              ...) +
    theme(legend.position = 'none',
          ...) +
    scale_y_continuous(breaks = scales::pretty_breaks(y_breaks),
                       limits = y_limits) +
    stat_pvalue_manual(stat2,
                       y.position = y_max,
                       label = "p.adj", 
                       bracket.nudge.y = b_nudge)
  
  
  list <- list(normality = normality, stat = stat2, plot = plot)
  
  
  return(list)
}

#test function
mtcars %>% 
  nice_bar_chart(., 
                 x_col = cyl, 
                 y_col = mpg,
                 b_nudge = 0, 
                 axis.text = element_text(size = 16),
                 y_breaks = 5,
                 label = T,
                 lb_vjust = 5)

mtcars %>%
  group_by(across(all_of("cyl"))) %>%  
               summarise(max = max(mpg, na.rm = T)) %>% 
  pull(max)

                         