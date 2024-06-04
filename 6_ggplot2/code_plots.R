library(ggplot2)
library(dplyr)

theme_set(theme_bw())

data = datasets::airquality

# Grouped / stacked barplots 
data %>%
  mutate(temp_factor = ifelse(Temp > 80, 'above 80', 'below 80')) %>%
  ggplot( mapping = aes(x = Month, y = Ozone, fill = temp_factor)) +
  geom_col()
ggsave('projects/ggplot/1.png', dpi=600)

data %>%
  mutate(temp_factor = ifelse(Temp > 80, 'above 80', 'below 80')) %>%
  ggplot( mapping = aes(x = Month, y = Ozone, fill = temp_factor)) +
  geom_col(position="dodge")
ggsave('projects/ggplot/2.png', dpi=600)

# histogram 
ggplot(data, aes(x= Temp)) +
  geom_histogram()
ggsave('projects/ggplot/3.png', dpi=600)

# density
ggplot(data, aes(x= Temp, fill = as.factor(Month))) +
  geom_density()
ggsave('projects/ggplot/4.png', dpi=600)

# facet
ggplot(data, aes(x= Temp, fill = as.factor(Month))) +
  geom_density() +
  facet_wrap(vars(Month))
ggsave('projects/ggplot/5.png', dpi=600)

# violin / boxplot 
ggplot(data, aes(x= as.factor(Month), y=Temp, fill = as.factor(Month ))) +
  geom_violin()
ggsave('projects/ggplot/6.png', dpi=600)

ggplot(data, aes(x= as.factor(Month), y=Temp, fill = as.factor(Month ))) +
  geom_boxplot()
ggsave('projects/ggplot/7.png', dpi=600)

# Colors 
data %>%
  ggplot(aes( x= Temp, y = Solar.R, color = Ozone)) +
  geom_point (size = 3)
ggsave('projects/ggplot/8.png', dpi=600)

data %>%  
  ggplot(aes( x= Temp, y = Solar.R, color = Ozone)) +  
  geom_point (size = 3) +  
  scale_colour_gradient(low="lightgrey", high="blue")
ggsave('projects/ggplot/9.png', dpi=600)

data_month = data %>%
  mutate(month = ifelse(Month == 5, 'May', 
                        ifelse(Month == 6, 'June', 
                               ifelse(Month == 7, 'July', 
                                      ifelse(Month == 8, 'August', 'September')))))



