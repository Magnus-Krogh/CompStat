library(tidyverse)

data <- titanic_train %>% 
  drop_na() %>%
  filter(Fare != 0) %>%
  mutate(Pclass = as.factor(Pclass)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  mutate(Survived = as.factor(Survived))
View(data)

data %>%
  ggplot(aes(Age, log(Fare), group = Survived, color = Survived)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE, aes(color = Survived)) +
  theme_minimal()

