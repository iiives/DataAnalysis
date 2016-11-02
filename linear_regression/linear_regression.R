#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod) # confidence Interval
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

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

### Predicting energy consumed per capita (energy) from percentage of residents living in metropolitan areas

####1. Examine/plot the data before fitting the model

library("dpylr")
glimpse (states.data)
states.info

# summary of energy and metro columns, all rows
sts.nrg.met <- subset(states.data, select = c("energy", "metro"))
summary(sts.nrg.met)

#remove na entries
#sts.nrg.met.c <- complete.cases(sts.nrg.met)
#class(sts.nrg.met.c)  # gives a boolean output

sts.nrg.met.c <- na.omit(sts.nrg.met)
summary(sts.nrg.met.c)

# correlation between energy and metro
cor(sts.nrg.met.c)


#Plot the data to look for multivariate outliers, non-linear relationships etc.
plot(sts.nrg.met.c)



####2. Print and interpret the model summary

#Base model with metro:
mod1 <- lm(energy ~ metro, data = na.omit(states.data))
summary (mod1)


####3. plot the model to look for deviations from modeling assumptions

#Plot model
plot(mod1)


#From the summary: 

#* Linear equation is Energy Per capita = 449.8382 - 1.6526(metro%).
#* R-Squared is 0.097 which is positive yet very low suggesting a very weak relation.



### Predicting energy consumed per capita (energy) 

##Reflecting back to the subject, 'Predicting energy consumed per capita (energy)', choosing other independent variables, the following are in context and applausible with correlation to be investigated:
##* metro% x pop - a better variable of persons to feed into the per capita dependent variable
##* density 
##* income - level of income may dictate affordability of energy use
##* waste
##* toxic
##* green - emission from greenhouse gases is the direct output of energy station production
##* house
##* senate
##* college - more knowledgable person may affect energy consumption
##* high


######Model 2 - adding metro*pop as a variable - since metro is a percentage of population

#Add data column "metPop"
states.data$metPop <- states.data$metro * states.data$pop

# Build mod2
mod2 <- lm(energy ~ metPop, data = na.omit(states.data))
summary(mod2)

#From the summary: 

#* Linear equation is Energy Per capita = 3.601e+02 - -4.147e-08(metro% x pop).
#* R-Squared is 0.033 which is a weaker relation.


######Model 3 - adding all variables

# Build mod3
mod3 <- lm(energy ~ metro + area + density + waste + miles + toxic + green + house + senate + csat + vsat + msat + percent + income + high + college , data = na.omit(states.data))
summary (mod3)


#From the summary: 

#* Linear equation has 3 significant variables - area, toxic & green
#* R-Squared is 0.8101 which has an increased relation.
#* R-Adjusted is 0.7121 .


#Plot model
plot(mod3)


######Model 4 - removing education and senate , house related variables

# Build mod4
mod4 <- lm(energy ~ metro + pop + area + density + waste + 
    miles + toxic + green + income, data = na.omit(states.data))
summary (mod4)


#From the summary: 

#* Linear equation has 2 significant variables - toxic & green.
#* R-Squared is 0.791 which has an increased relation.
#* R-Adjusted is 0.7415 .

#Plot model
plot(mod4)



######Model 5 - only placing the three significant variables

# Build mod5
mod5 <- lm(energy ~ metro + area + toxic + green , data = na.omit(states.data))
summary (mod5)


#From the summary: 

#* Linear equation has 3 significant variables - area, toxic & green.
#* R-Squared is 0.7815 which has an increased relation.
#* R-Adjusted is 0.7612 .


#Plot model
plot(mod5)


######Model 6 - only placing the 4 lowest p-value significant variables +

# Build mod6
mod6 <- lm(energy ~ metro + area + toxic + green + college, data = na.omit(states.data))
summary (mod6)


#From the summary: 

#* Linear equation has 3 significant variables - area, toxic & green.
#* R-Squared is 0.7873 which has an increased relation.
#* R-Adjusted is 0.762 .


#Plot model
plot(mod6)



######Model 7 - only placing the 5 lowest p-value significant variables +

# Build mod7
mod7 <- lm(energy ~ metro + area + toxic + green + college + percent, data = na.omit(states.data))
summary (mod7)

#From the summary: 

#* Linear equation has 4 significant variables - area, toxic , green & college.
#* R-Squared is 0.801 which has an increased relation.
#* R-Adjusted is 0.7719 .


#Plot model
plot(mod7)



###Comparing models 
##mod1 (metro only), mod3 (all variables) & mod7 (selected 5 p-value < 0.5)


SSE_1 = sum((mod1$residuals)^2 )
SSE_3 = sum((mod3$residuals)^2 )
SSE_7 = sum((mod7$residuals)^2 )

RMSE_1 = sqrt(SSE_1 / nrow(clean))
RMSE_3 = sqrt(SSE_3 / nrow(clean))
RMSE_7 = sqrt(SSE_7 / nrow(clean))

SSE_1  #580411.5
SSE_3  #122700.4 - all variables
SSE_7  #127928.1 - 5 variables

RMSE_1  #109.9632 - metro only , R-Squared is 0.097     , R-Adjusted is 0.0775
RMSE_3  #50.55946 - all variables , R-Squared is 0.8101 , R-Adjusted is 0.7121
RMSE_7  #51.62528 - 5 variables , R-Squared is 0.801    , R-Adjusted is 0.7719


anova(mod1, mod7) # p-value = 1.841e-12 *** is significant

#Ans4-- Compared to only viewing metropolitan % to predict energy consumption  per capita, 
#-- it is better that area + toxic + green + college + percent is added to better predict 
#-- energy consumption per capita. 
#-- By the ANOVA results shows that that is a significant difference comparing mod1 to mod7. 
#-- Meanwhile the RMSE shows a more stronger relationship model can be seen with likely
#-- an average of 51 points error margin in the prediction.





## Interactions and factors
## ══════════════════════════

## Modeling interactions
## ─────────────────────────

##   Interactions allow us assess the extent to which the association
##   between one predictor and the outcome depends on a second predictor.
##   For example: Does the association between expense and SAT scores
##   depend on the median income in the state?

  #Add the interaction to the model
sat.expense.by.percent <- lm(csat ~ expense*income,
                             data=states.data) 
#Show the results
  coef(summary(sat.expense.by.percent)) # show regression coefficients table

## Regression with categorical predictors
## ──────────────────────────────────────────

##   Let's try to predict SAT scores from region, a categorical variable.
##   Note that you must make sure R does not think your categorical
##   variable is numeric.

# make sure R knows region is categorical
str(states.data$region)
states.data$region <- factor(states.data$region)
#Add region to the model
sat.region <- lm(csat ~ region,
                 data=states.data) 
#Show the results
coef(summary(sat.region)) # show regression coefficients table
anova(sat.region) # show ANOVA table

##   Again, *make sure to tell R which variables are categorical by
##   converting them to factors!*

## Setting factor reference groups and contrasts
## ─────────────────────────────────────────────────

##   In the previous example we use the default contrasts for region. The
##   default in R is treatment contrasts, with the first level as the
##   reference. We can change the reference group or use another coding
##   scheme using the `C' function.

# print default contrasts
contrasts(states.data$region)
# change the reference group
coef(summary(lm(csat ~ C(region, base=4),
                data=states.data)))
# change the coding scheme
coef(summary(lm(csat ~ C(region, contr.helmert),
                data=states.data)))

##   See also `?contrasts', `?contr.treatment', and `?relevel'.

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?


###### Q1:Model 7 - adding interaction on toxic & green with pop as those figures are percentages of pop

# Build mod7
mod7i <- lm(energy ~ metro + area + toxic*pop + green*pop + college + percent, data = na.omit(states.data))
summary (mod7i)


#Ans1--From the summary: 

#--* Linear equation has 2 significant variables - toxic:pop & green.
#--* R-Squared is 0.8711 which has an increased relation by 0.70 .
#--* R-Adjusted is 0.8405 whichis an increase of 0.11 .


###### Q2: Model 7 - adding region

# make region categorical
str(states.data$region)
states.data$region <- factor(states.data$region)

# print default contrasts
contrasts(states.data$region)

#Add region to the mod7
# change the coding group
# use contr.sum as all each category is not ordered and that all mean of each element is compared to the overall mean
mod7i.region <- lm(energy ~ C(region, contr.sum) + metro + area + toxic*pop + green*pop + college + percent, data = na.omit(states.data) ) 

#Show the results
coef(summary(mod7i.region)) # show regression coefficients table
anova(mod7i.region) # show ANOVA table

#plot model
plot(mod7i.region)


#Ans2--There are 4 regions - West ; N.East ; South ; Midwest .
#--In the regression model, 'West' presents to be a significant variable in the equation.

