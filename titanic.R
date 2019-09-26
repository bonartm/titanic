library(ggplot2)
library(dplyr)

# read in the data and define factor variables
titanic <- readr::read_csv("titanic.csv") %>%
  mutate_at(vars(Pclass, Sex), as.factor)


# apply logistic regression model
form <- Survived ~ Pclass + Sex + Age + Pclass:Sex + Pclass:Age + Sex:Age
m <- glm(form, data = titanic, family = "binomial")

# create artificial new data
dat <- expand.grid(Sex = c("male", "female"), 
                   Pclass = as.factor(c(1, 2, 3)), 
                   Age = 0:70)

# predict the new data on linear scale together with se
pred <- predict(m, newdata = dat, type = "link", se.fit = TRUE)

# add prediction, upper, lower CI to the new data
# transform the prediction to [0,1]
dat <- dat %>%
  mutate(Survived = plogis(pred$fit),
         Upper = plogis(pred$fit + 1.96*pred$se.fit),
         Lower =  plogis(pred$fit - 1.96*pred$se.fit))


# define some labels
facet_labels <- c("1" = "first class", "2" = "second class", "3" = "third class")
subtitle <- paste0("Logistic regression model equation: ", format(form), 
                   "\n95% confidence intervals, N: ", m$df.null + 1)

# plot the data
ggplot(dat, aes(x = Age, y = Survived, 
                color = Sex, fill = Sex, 
                ymin = Lower, ymax = Upper)) + 
  geom_line() + 
  geom_ribbon(linetype = 0, alpha = 0.2) + 
  facet_grid(~ Pclass, labeller = as_labeller(facet_labels)) + 
  ggtitle("Survival rates on the titanic", subtitle) + 
  theme(legend.position = "top", 
        legend.title=element_blank(), 
        legend.justification = "left") + 
  ggthemes::theme_gdocs() + 
  ggsave("titanic.png", width = 10, height = 5)

