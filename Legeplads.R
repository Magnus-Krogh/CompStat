library("tidyverse")
library("tidymodels")
library("titanic")

View(titanic_train)

head(titanic_train, n = 6)

data <- titanic_train %>% 
  drop_na() %>%
  filter(Fare != 0) %>%
  mutate(Pclass = as.factor(Pclass)) %>%
  mutate(Sex = as.factor(Sex)) %>%
  mutate(Embarked = as.factor(Embarked)) %>%
  mutate(Survived = as.factor(Survived)) %>%
  mutate(LogFare = log(Fare))
View(data)

data %>%
  ggplot(aes(Age, LogFare, group = Survived, color = Survived)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = lm, se = FALSE, aes(color = Survived)) +
  theme_minimal()

lr_mod <- 
  logistic_reg() %>%
  set_engine("glm")

rec <- recipe(Survived ~ Pclass + Sex + Age + LogFare + Embarked, data = data)

wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(rec)
wflow

data_split <- initial_split(data, prop = 3/4)
train_data <- training(data_split)
test_data <- testing(data_split)


fit <- wflow %>% fit(data = train_data)
fit %>% extract_fit_parsnip() %>% tidy()
titanic_aug <- augment(fit, test_data)
titanic_aug %>% roc_curve(truth = Survived, .pred_0) %>% autoplot() 
titanic_aug %>% roc_auc(truth = Survived, .pred_0)

test <- randomForest(Survived ~ Pclass + Sex + Age + LogFare + Embarked, data = train_data, ntree = 5000)
predictions <- predict(test, newdata = test_data, type = "prob")[, 2]
rf.roc <- roc(test_data$Survived, predictions, levels = rev(levels(test_data$Survived)))
auc(rf.roc)
plot.roc(rf.roc)
