source("summariser.R")
source("plotter.R")

test1 <- 
  mtcars %>% 
  summariser(group_cols = c("cyl"))

test2 <- 
  mtcars %>% 
  summariser()

test3 <- 
  ToothGrowth %>% 
  summariser(group_cols = c("supp","dose"))

test1 %>% 
  mutate(n_label = str_glue("n={n}")) %>% 
  filter(var == 'mpg') %>%
  ggplot(aes(x = cyl, y = mean, fill = cyl)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(ymin=lowci, ymax=hici), width=.2,
                position=position_dodge(.9)) +
  geom_text(aes(label = n_label),
            nudge_y = -5 , nudge_x = -0
  ) +
  labs(title="title", 
       x="X", 
       y = "Y")+
  scale_fill_manual(values=c('orange','lightgray',"steelblue"))+
  ggpubr::theme_pubr()
  

test1 %>% 
  mutate(n_label = str_glue("n={n}")) %>% 
  filter(var == 'mpg') %>% 
  ggbarplot(., x = "cyl", y = "mean")

ggplot(df3, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9))


ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_ci",
          color = "supp", palette = "jco", 
          position = position_dodge(0.8))+
  stat_compare_means(aes(group = supp), label = "p.signif", label.y = 29)


ggbarplot(ToothGrowth, x = "dose", y = "len", add = "mean_se")+
  stat_compare_means() +  # Global p-value
  stat_compare_means(label = "p.signif",
                     label.y = c(22, 29))
