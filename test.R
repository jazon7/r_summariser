source("summariser.R")
source("plotter.R")


mtcars %>% 
  summariser()

df <- ToothGrowth
df$len[1] <- NA

df %>% 
  mutate(dose = as_factor(dose)) %>% 
  mutate(supp = as_factor(supp)) %>% 
  summariser()






