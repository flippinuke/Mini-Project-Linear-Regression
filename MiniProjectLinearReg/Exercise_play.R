## Exercise: least squares regression

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro).

# Setup ========================================================================================
getwd()
list.files("dataSets")
states.data <- readRDS("dataSets/states.rds")

##   1. Examine/plot the data before fitting the model =========================================

nrow(states.data)
class(states.data)
str(states.data)
head(states.data)
head(states.data$energy)
head(states.data$metro)

# this doesn't work
head(states.data$energy, states.data$metro)
# but this does
head(subset(states.data, select = c("energy", "metro")))

# this is how lesson does it
## subset of states.data with variables energy and metro, all rows
states.data1 <- subset(states.data, select = c("energy", "metro"))
head(states.data1)
str(states.data1)
cor(states.data1)
cor(na.omit(states.data1))

##   2. Print and interpret the model `summary' ==============================================
# (I do it this way as per the lesson protocol, but then change it to make it simpler)
model1 <- lm(states.data$energy ~ states.data$metro, data = states.data1)
summary(model1)

cor(states.data1)
cor(na.omit(states.data1))

# R2 not too large

# Note: na.omit in the lm() code does not seem to matter. likely already incorporated
# model2 <- lm(states.data$energy ~ states.data$metro, data = na.omit(states.data1))
# summary(model2)

##   3. `plot' the model to look for deviations from modeling assumptions =====================
# (Plot the data before fitting models)
# (Plot the data to look for multivariate outliers, non-linear
# relationships etc.)

plot(states.data1)

# ggplot2 version
library(ggplot2)
ggplot(states.data1, aes(energy, metro)) + geom_point()

##   Select one or more additional predictors to add to your model and ========================
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

head(states.data) # to see variables again

model3 <- lm(energy ~ metro, data = states.data) # this model added afterwards in order
summary(model3)                                  # to do anova test later

model4 <- lm(energy ~ metro + region + toxic + green, data = states.data)
summary(model4)

# checking for multicollinearity(?)
cor(na.omit(subset(states.data, select = c("toxic", "green")))) # seems okay

model5 <- lm(energy ~ metro + toxic + green, data = states.data)
summary(model5)

model6 <- lm(energy ~ toxic + green, data = states.data)
summary(model6)

model7 <- lm(energy ~ metro + pop + area + density + waste + miles + toxic + green, data = states.data)
summary(model7)

# It seems that model 6 is the best model. "metro" has no significance, and the model's
# Adjusted-R2 increases when metro is removed. Now just to find out what toxic and
# green mean ...

confint(model6)
hist(residuals(model6))

plot(model6, which = c(1, 2))

# comparing using the anova function

# this renders an error message:
anova(model3, model6)

# this little code makes it better:
model3 <- update(model3, data=na.omit(states.data)) # why does this work in making the anova test work?
anova(model3, model6)
# why?

coef(summary(model6)) # same as summary but removes significance levels, R2

# Model6 regresses toxic and green against energy. It appears significantly better than
# model1, which has only metro as the predictor. Model6 has a higher R2 and adj-R2,
# and its predictor variables have much more significant p-values.

## Modeling interactions and categorical (play - this will be done again below) =================
## ─────────────────────────

##   Interactions allow us to assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

##   -  but what would I pick as the interaction variable?

model.interaction <- lm(energy ~ metro*region, data = states.data)

# show the results
coef(summary(model.interaction))
summary(model.interaction)

# all variables are significant or almost significant. R2 is .3126, adj-R2 is .1981
# However, we did this with categorical variable (region), not sure if that is the best approach

# Let's try again with non-catorical variables, say density?

head(states.data)
model.interaction2 <-lm(energy ~ metro*density, data = states.data)

# show the results
coef(summary(model.interaction2))
summary(model.interaction2)
cor(na.omit(subset(states.data, select = c("metro", "density"))))
# not really getting much

##   I will try to predict energy from region, a categorical variable.
##   Note: need to make sure R does not think the categorical
##   variable is numeric.

##   Note: I am doing the following only because the exercise follows these steps:

# make sure R knows it's categorical
str(states.data$region)
states.data$region <- factor(states.data$region)

model.region <- lm(energy ~ region, data = states.data)

# show the results
coef(summary(model.region))
anova(model.region)
summary(model.region)

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(energy ~ C(region, base=4),
                data = states.data)))
# change the coding scheme
coef(summary(lm(energy ~ C(region, contr.helmert),
                data = states.data)))

summary(lm(energy ~ C(region, base=4),
                data = states.data))

summary(lm(energy ~ C(region, contr.helmert),
                data = states.data))

# End this play#


## Final exercise (the two above exercises in "play" but with additional instruction) =========

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by ====================
##      generating an interaction term and testing the interaction.

head(states.data)
# this appeared to be the best model in exercises one (we'll call it 'original'):
model6 <- lm(energy ~ toxic + green, data = states.data)
summary(model6)

# this model increases adj-R2 from .7521 to .7793. Not huge but maybe something?
model.interaction3 <- lm(energy ~ toxic*density + green, data = states.data)
summary(model.interaction3)

# this model increases adj-R2 again, from .7793 to .8477. Even more but now toxic isn't sig
# (but won't remove toxic because it is significant in the interaction term)
model.interaction4 <- lm(energy ~ toxic*pop + green, data = states.data)
summary(model.interaction4)

##   2. Try adding region to the model. Are there significant differences ======================
##      across the four regions?
##   (am I supposed to add region to the original model (model6, or the more recent model?)
##   (will try both)

# model6 (original)
model6 <- lm(energy ~ toxic + green, data = states.data)
summary(model6)

# ensure region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)

# -- redoing - adding stuff here:

# region only model
model.region <- lm(energy ~ region, data = states.data)

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
# play
summary(lm(energy ~ C(region, contr.helmert) + green + toxic, data = states.data))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

?contrasts
?contr.treatment
?relevel

# original model + region. doesn't seem to do much
model6.region <- lm(energy ~ toxic + green + region, data = states.data)
summary(model6.region)

# final interaction model + region

# step 1: show original model
model6 <- lm(energy ~ toxic + green + region, data = states.data)
# step 2: show interaction model: model.interaction4 (improved with interaction term)
model.interaction4 <- lm(energy ~ toxic*pop + green, data = states.data)
# step3: add region
model.interaction4.region <- lm(energy ~ toxic*pop + green + region, data = states.data)
summary(model.interaction4.region) # doesn't seem to do much

# region does not seem to be a significant predictor of energy. very interesting, given that
# some regions are hit with the seasons a bit hard (I would expect regionN. East and
# regionMidwest to have higher energy use than say, regionSouth)

# best models appear to be: ==================================================================
# model6 (original linear model)
model6 <- lm(energy ~ toxic + green + region, data = states.data)
summary(model6)
# and
# model.interaction4 (original linear model, with pop added as interaction variable to pop)
model.interaction4 <- lm(energy ~ toxic*pop + green, data = states.data)
summary(model.interaction4)

ggplot(states.data1, aes(energy, metro)) + geom_point() + ggtitle("Energy vs. Metro")
