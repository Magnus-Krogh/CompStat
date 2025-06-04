library(tidyverse)

data <- titanic_train %>% 
  drop_na() %>%
  filter(Fare != 0) %>%
  mutate(Pclass = as.factor(Pclass))
View(data)

data %>%
  ggplot(aes(Age, log(Fare), group = Survived, color = Survived)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE, aes(color = Survived)) +
  theme_minimal()
min(data$Fare)

class(data$Pclass)
