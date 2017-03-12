# All Exercise Instructions: ==================================================================

## Exercise: least squares regression
## ────────────────────────────────────────
##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

# Exercise Instructions step-by-step, each step followed by the code =============================

  ## Exercise: least squares regression

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro).

# Setup ====
getwd()
list.files("dataSets")
states.data <- readRDS("dataSets/states.rds")

##   1. Examine/plot the data before fitting the model =========================================

str(states.data)
head(states.data)
head(subset(states.data, select = c("energy", "metro")))

# this is how lesson does it
## subset of states.data with variables energy and metro, all rows
states.data1 <- subset(states.data, select = c("metro", "energy"))
head(states.data1)
str(states.data1)
cor(states.data1)
cor(na.omit(states.data1))

##   2. Print and interpret the model `summary' ==============================================
# (I do it this way as per the lesson protocol, but then change it later to simplify)
model1 <- lm(states.data$energy ~ states.data$metro, data = states.data1)
summary(model1)

cor(states.data1)
cor(na.omit(states.data1))

# R2 not too large

##   3. `plot' the model to look for deviations from modeling assumptions =====================

plot(states.data1) # the exercise demonstrates using plot()

# ggplot2 version
library(ggplot2)
ggplot(states.data1, aes(metro, energy)) + geom_point()

# and
ggplot(states.data1, aes(metro, energy)) + geom_point() +
  ggtitle("Energy Consumption per Person \n Based on Metropolitan Area Population") +
  xlab("Metropolitan Area Population, %") + ylab("Per Capita Energy Consumption, BTU") +
  theme(plot.title = element_text(hjust = 0.5)) # this centers the title

# another way
ggplot(states.data1, aes(metro, energy)) + geom_point() +
  labs(title = "Energy Consumption per Person \n Based on Metropolitan Area Population",
       x = "Metropolitan Area Population, %", y = "Per Capita Energy Consumption, BTU") +
       theme(plot.title = element_text(hjust = 0.5))
  
##   Select one or more additional predictors to add to your model and ========================
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

head(states.data) # to see variables again

model3 <- lm(energy ~ metro, data = states.data) # this is the same as model1. Redone this
summary(model3)                                  # way in order to do anova test later

model4 <- lm(energy ~ metro + region + toxic + green, data = states.data)
summary(model4)

# checking for multicollinearity. Can someone please comment if this approach is appropriate?
cor(na.omit(subset(states.data, select = c("toxic", "green")))) # appears okay
cor(na.omit(subset(states.data, select = c("metro", "green"))))
cor(na.omit(subset(states.data, select = c("toxic", "metro"))))

model5 <- lm(energy ~ metro + toxic + green, data = states.data)
summary(model5) # metro loses significance when toxic and green are added. However, the
                # significance of metro and green are much higher

model6 <- lm(energy ~ toxic + green, data = states.data)
summary(model6) # and Ajd-R2 increases when metro removed

model7 <- lm(energy ~ metro + pop + area + density + waste + miles + toxic + green, data = states.data)
summary(model7)

# It seems that model 6 is the strongest. "metro" has no significance, and the model's
# Adjusted-R2 increases when metro is removed.

confint(model6)
hist(residuals(model6))

plot(model6, which = c(1, 2))

# comparing using the anova function

# this renders an error message:
anova(model3, model6)

# this little code makes it better:
model3 <- update(model3, data=na.omit(states.data)) # why does this make the anova test work?
anova(model3, model6)

coef(summary(model6)) # same as summary but removes significance levels, R2

# Model6 regresses toxic and green against energy. It appears significantly better than
# model1, which has only metro as the predictor. Model6 has a higher R2 and adj-R2,
# and its predictor variables have much more significant p-values.

## Exercise: interactions and factors =========================================================
## ────────────────────────────────────────
##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 (Least Squares =======
##      Regression) by generating an interaction term and testing the interaction.

head(states.data) # to see variables

# this appeared to be the best model in Exercise 1 ("original"):
model6 <- lm(energy ~ toxic + green, data = states.data)
summary(model6)

# this model increases adj-R2 from .7521 to .7793
model.interaction3 <- lm(energy ~ toxic*density + green, data = states.data)
summary(model.interaction3)

# this model increases adj-R2 from .7521 to .8477. interaction term sig at ***; significance
# of component terms decreases
model.interaction4 <- lm(energy ~ toxic*pop + green, data = states.data)
summary(model.interaction4)

##   2. Try adding region to the model. Are there significant differences ======================
##      across the four regions?

# model6 (original)
model6 <- lm(energy ~ toxic + green, data = states.data)
summary(model6)

# ensure region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)

# region only model. doesn't seem to do much
model.region <- lm(energy ~ region, data = states.data)
summary(model.region)

# show the results
coef(summary(model.region))
anova(model.region)
summary(model.region)

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(energy ~ C(region, base = 4),
                data = states.data)))
# change the coding scheme
coef(summary(lm(energy ~ C(region, contr.helmert),
                data = states.data)))
# ---

# original model + region. doesn't seem to do much
model6.region <- lm(energy ~ toxic + green + region, data = states.data)
summary(model6.region)

# interaction model + region

# step 1: show original model
model6 <- lm(energy ~ toxic + green + region, data = states.data)
# step 2: show interaction model: model.interaction4 (improved with interaction term)
model.interaction4 <- lm(energy ~ toxic*pop + green, data = states.data)
# step3: add region
model.interaction4.region <- lm(energy ~ toxic*pop + green + region, data = states.data)
summary(model.interaction4.region) # doesn't seem to do much

# region does not seem to be a significant predictor of energy.

# best models appear to be: ====

# model6 (original linear model)
model6 <- lm(energy ~ toxic + green + region, data = states.data)
summary(model6)

# and

# model.interaction4 (original linear model, with pop added as interaction variable to toxic)
model.interaction4 <- lm(energy ~ toxic*pop + green, data = states.data)
summary(model.interaction4)