library(tidyverse)
library(ggplot2)  ## for the graphics
library(olsrr)    ## regression
library(car)      ## vif() function
library(usmap)    ## easy US Map 
library(stargazer)## nice formatted tables in a text file



## working notes
#
#   ## get unified gov't dummy variable
#   ## get racial indicator variable (losing meaning in just highly populated areas)
#
# 
#


## leonardo
setwd("C:\\Users\\lyr14\\OneDrive\\Desktop\\Project2") 

## cody
setwd("C:\\Users\\ricke\\OneDrive\\Documents\\R\\Project2")



###
### DATA SETS 
###############################################################################

## read correlates of state policy into a dataframe
correlates <- read.csv("correlatesofstatepolicyprojectv2_2.csv")


## renaming year and state to match Frank_Education
colnames(correlates)[colnames(correlates) == "ï..year"] <- "Year"
colnames(correlates)[colnames(correlates) == "state"] <- "State"


## read education information ino a dataframe
education <- read.csv("Frank_Education_2015.csv")
education <- subset(education, select = -st) ## removing just "st" column



##
## We were losing "New Mexico" in the merge
## because there was an extra space after
## New Mexico in education data frame  
## 
education$State[education$State == "New Mexico "] <- "New Mexico" 


## merge the two together based on year and state.
##
## this cuts out anything before 1940 and after 2015. 
##
correlates <- merge(correlates, education, by=c("State","Year"))


###
### vARIABLES DECLARATION AND CLEANING
###############################################################################


## depedendant variable
###########################################

## DEPENDENT: Atkinson Index
##
##    atkin_index 
##
## this is a measure of income inequality
## closer to one, higher inequality
## 1917 - 2013

atkinson <- correlates$atkin_index



## indepedendant variables
###########################################

## INDEPENDENT : Renewable Energy Consumption
##
##    cons_renewable (continuous)
##
## this is in billions and trillions BTU
## to convert -> divide billions by 1000
##
## Years that need to be converted: 1960 - 2009 (billions to trillions)
## Years that are already in trillions: 2010 - 2017
## 1960 - 2017


correlates$cons_renewable <- 
  as.numeric(correlates$cons_renewable) ## changing all to numbers


## Converting billions to trillions
correlates$cons_renewable[correlates$Year <= 2009] <-     
  (correlates$cons_renewable[correlates$Year <= 2009] / 1000) 

renew_consum <- correlates$cons_renewable


## INDEPENDENT : Pro-Environment Opinion
##    
##    pro_environment (continuous)
## 
## estimated proportion of citizenry who believe we 
## are spending "too little" on the environment 
## 1973 - 2012

pro_environment <- correlates$pro_environment

plot(pro_environment, atkinson)



## INDEPENDENT  : Economic Status
##
##    hincomemed (continuous)
##
## median of all houshold income. CPI-U-RS adjsuted dollars. 
##  1984 - 2011

economic_status <- as.numeric(correlates$hincomemed)

plot(economic_status, atkinson)


## INDEPENDENT : Innovative Policies
##
##    innovatescore_boehmkeskinner
##
##  adoption of new policies sooner than other states 
##  based on 180 policies passed at state level during years
##  1913 - 2010

innovate_score <- correlates$innovatescore_boehmkeskinner


## INDEPENDENT  : Measure of Liberalism of Policies - IDEOLOGY MEASURE
##
##    pollib_median
##
##  Yearly measure of policy liberalism of states, based
##  on a dynamic latent-variable model of 148 polices over
##  1936 - 2014

ideology <- correlates$pollib_median

## INDEPENDENT  : Region
##
##    region
##
##  Whether or not the state is
##  1: South , 2: West, 3: Midwest , 4: Northeast
##  1929 - 2012

region <- correlates$region


## INDEPENDENT  : College Graduation Rates
##
##    College
##
##  
##  Percentage of college graduates
##  1940 - 2015

## missing data was dots, recodd as missing
correlates$College[correlates$College == "."]  <- NA 

## reads in as a class "character", change this to numeric
college <- as.numeric(correlates$College)


###############################################################################
########################################################                      #
## INDEPENDENT  : Expenditures on Education           ##  less data than year #
##                                                    ##  ranges in codebook  #
##                                                    ##                      #
##    exp_education                                   ##                      #
##                                                    ##    chose to use      #
##  General Expenditure on Education                  ##    other measures of #
##                                                    ##  education spending  #
##  1942 - 2016                                       ##                      #
edu_expense <- correlates$exp_education               ##     2012 - 2015      #
########################################################                      #
###############################################################################



###############################################################################
########################################################                      #
## INDEPENDENT  : Expenditures Other / Unallocable    ##  less data than year #
##                                                    ##  ranges in codebook  #
##    exp_other_and_unallocable                       ##                      #
##                                                    ##                      #
##                                                    ##       chose to       # 
##  General Expenditure on Other or Unallocable Funds ##  use other measures  #
##  1942 - 2016                                       ##    of expense        #
##                                                    ##                      #
other_expense <- correlates$exp_other_and_unallocable ##      2012 - 2015     #
########################################################                      #
###############################################################################



## INDEPENDENT  : Spending on Public Education per Pupil
##
##    z_education_expenditures_per_pup
##
##  
##  Per capita, spending on education per pupil this is in dollars
## 
##  1940 - 2009, missing a few years here and there
public_education <- correlates$z_education_expenditures_per_pup


## INDEPENDENT  : Subsidy amount per student for higher education
##
##     z_education_higher_edu_spending
##
##  
##  Amount of college subsidy per student in dollars 
## 
##  1988 - 2013
college_subsidy <- correlates$z_education_higher_edu_spending


## INDEPENDENT  : Change in Corporate Tax Revenue
##
##    change_corptax 
##
##  
##  Yearly percentage of change in corporate tax revenue 
## controlled for impact of tax policy
##  1995 - 2014

corp_tax <- correlates$change_corptax



## INDEPENDENT  : Party of the Governor
##
##    govparty_a
##
##  Party of the Governor
##  0 is republic, 1 is democrat, .5 is non-major party, 
##   could be a fraction if governor party changes
##  1937 - 2011
gov_party <- correlates$govparty_a

gov_party[gov_party != 1 & gov_party != 0 & gov_party != .5] <- NA


## INDEPENDENT  : Divided Government
##
##    divded_gov
##
##  whether or not the government is controlled by a single party 
##  0 for yes
##   1 for no
##  1937 - 2011

divided_govt <- correlates$divided_gov

## INDEPENDENT  : Nonwhite Population
##
##    nonwhite
##
##  proportion of the state that is nonwhite
##   
## 1974 - 2011

nonwhite <- correlates$nonwhite


###
### Building Dataset for Model
###############################################################################

## identifier variables for the data. 
year <- correlates$Year
state <- correlates$State
## after looking at the years of the data
model_data <- data.frame(year,
                         state,
                         atkinson,
                         public_education,
                         corp_tax,
                         gov_party,
                         college_subsidy, 
                         college,
                         region,
                         renew_consum,
                         pro_environment,
                         economic_status,
                         innovate_score,
                         ideology,
                         divided_govt,
                         nonwhite)


## 
##    Trimming the years we are not looking at out of the data
## 
model_data <- subset(model_data, model_data$year > 1994  & year < 2011)




###
###  Creating the table of summary statistics
###############################################################################

#
# Function that takes in a variable and gives summary statistics
#
#                               Takes: variable
#
#                               Returns: vector with 
#                                 mean, median, min, 
#                                 max, standard deviation, 
#                                 NA count
##
sum.stats <- function(variable) {
  mean <- round(mean(variable, na.rm = T), 2)
  median <- round(median(variable, na.rm = T), 2)
  min <- round(min(variable, na.rm = T), 2)
  max <- round(max(variable, na.rm = T), 2)
  sd <- round(sd(variable, na.rm =T), 2)
  missing <- sum(is.na(variable))
  stats <- c(mean, median,  min, max, sd, missing)
  return (stats)
}


##
## names of the columns in the summary statistic table
##
colnames <- c("Mean", "Median",  "Min", "Max", "SD", "Missing")

##
## names of the variables themselves in summary statistic table 
##
rownames <- c("atkinson", "public_education", "corp_tax", "gov_party", 
              "college_subsidy", "college", "region", "renew_consum",
              "pro_environment", "economic_status", "innovate_score", 
              "ideology", "divided_govt", "nonwhite")

##
## Put all the summary stats into one table 
##
summary_statistics <- rbind(sum.stats(model_data$atkinson), 
                                       sum.stats(model_data$public_education),
                                       sum.stats(model_data$corp_tax),
                                       sum.stats(model_data$gov_party),
                                       sum.stats(model_data$college_subsidy),
                                       sum.stats(model_data$college), 
                                       sum.stats(model_data$region),
                                       sum.stats(model_data$renew_consum),
                                       sum.stats(model_data$pro_environment),
                                       sum.stats(model_data$economic_status),
                                       sum.stats(model_data$innovate_score),
                                       sum.stats(model_data$ideology),
                                       sum.stats(model_data$divided_govt),
                                       sum.stats(model_data$nonwhite))

##
## change the row and column names of the summary stats table
##
colnames(summary_statistics) <- colnames
rownames(summary_statistics) <- rownames


###
### REGRESSION MODELS 
###############################################################################
model_data$region <- as.factor(model_data$region)
#########################################
### Variables giving issues
# + 
# +  edu_expense -> only 3 years of data
# +  other_expense -> "" "" ""  ""  "" 
# +  
#########################################


## model 1
## 
## IVs: renewable energy consumption / pro environment opinion / region
## 
## 
## 
##  renew_consum is in trillions BTU ( unit of heat )
##  pro_environment is proportion of voters think 
##  "don't spend enough" on environment issues
##
##
## HYPOTHESIS
## In comparison of states, 
##      a higher consumption of renewable energy and 
##          a higher pro-environment option will result 
##                  in a decrease in economic inequality. 
##
##

Model1 <- lm(atkinson ~  pro_environment + renew_consum + region, 
             data = model_data)
summary(Model1)

plot(Model1$fitted.values, Model1$residuals)

vif(Model1)


## model 2
## 
## IVs: liberal policies / education level / party of the governor
## 
## 
## 
## HYPOTHESIS
## In comparison of states, 
##                  those that have a greater liberal ideology 
##                  will show a decrease in economic inequality. 
## 
##
Model2<- lm(atkinson ~ gov_party + ideology + innovate_score + divided_govt
            , data = model_data)
summary(Model2)

plot(Model2$fitted.values, Model2$residuals)

vif(Model2)


## model 3
## 
## IVs: economic_status / change in corporate tax revenue / education spending
##    
## 
## HYPOTHESIS
## In comparison of states, 
##          the higher a state spends on education and 
##            the higher percent of college graduates 
##            the lower the economic inequality will be
## 
##
Model3 <- lm(atkinson ~ economic_status + corp_tax + public_education 
             + college_subsidy + college + nonwhite, data = model_data)
summary(Model3)

plot(Model3$fitted.values, Model3$residuals)

vif(Model3)


## model 4
##
## IVS:  liberal policies / education level / party of the governor
##       renewable energy consumption / pro environment opinion / region
##       economic_status / change in corporate tax revenue / education spending
##  
##  RESEARCH QUESTION 
##  How socio-economic factors such as different 
##           education policy, renewable energy policy, 
##              and political ideology influence economic inequality
##  
##  
##  
Model4 <- lm(atkinson ~  economic_status + corp_tax  + pro_environment 
             + renew_consum + college + ideology + innovate_score
             + gov_party + region + public_education
             + college_subsidy + nonwhite + divided_govt, data = model_data)
summary(Model4)

## checking for normally scattered data points
##
## this is a good indicator assumptions 1 - 4 are met
## 
plot(Model4$fitted.values, Model4$residuals)

## checking for multicollinearity
##
# VIF = 2 means the variance is twice what it would have been 
# if multicollinearity were not present 
# (or,if you had excluded that particular IV) 
## VIF > 4 means strong multicollinearity
##
##
vif(Model4)


## model 5
##
## IVS: edu expense, other expense
##
##  These independent variables were of interest, but
##    the amount of missing data (all but 2012 - 2015) made it 
##      difficult to fit with the rest of our historically data.
##
Model5 <- lm(atkinson ~ edu_expense + other_expense)
summary(Model5)



###
### BUILDING TABLE OF OUTPUT FOR MODELS 1 - 4 
###############################################################################

stargazer(Model1, Model2, Model3, Model4, 
          type = "text",
          dep.var.labels = c("Atkinson Index"),
          title = "Summary of Regressions", 
          digits = 8,
          out = "models.txt"
        # covariate.labels = c(... list of IVs here ...,)
          )

###
### GRAPHICS 
###############################################################################

## mean of atkinson scores 
## this aggregates the atkinson score by state, and then finds the mean
mean_atkinsons <- aggregate(model_data$atkinson~model_data$state, FUN=mean)
colnames(mean_atkinsons) <- c("state", "atkinson")

## map of the states and their mean atkinson score overtime
plot_usmap(data = mean_atkinsons, values = "atkinson",  color = "steelblue4", 
           labels=T) + 
  scale_fill_continuous( low = "white", high = "steelblue4", 
                         name = "Atkinson Index") + 
  theme(legend.position = "left") + 
  theme(panel.background = element_rect(colour = "black")) + 
  labs(title = "Mean Atkinson Index, 1995 - 2010")




##
## Function to graph each of the variables. 
##                                    Takes: variable,
##                                          x-axis label,
##                                          title 
##
##                            Returns: ggplot2 plot with 
##                                    line of best fit,
##                                     input on x, atkinson on y
##
variable.plot <- function(variable, xlabel,title){
  g <- ggplot(model_data, aes(variable, atkinson)) +
    geom_point() +
    stat_smooth(method = lm) +
    labs(x = xlabel, y = "Atkinson Index") +
    ggtitle(title)
  
  return (g)
}

##
## All of our independent variables are plotted 
## starting here
##
public_education_plot <- variable.plot(
  model_data$public_education, 
  "Dollars spent on education per pupil",
  "Public Education Spending and Atkinson")

public_education_plot



corp_tax_plot <- variable.plot(
  model_data$corp_tax, 
  "Yearly Change in Corporate Tax Revenue (%)",
  "Corporate Tax Revenue and Atkinson Index")
corp_tax_plot


gov_party_plot <- variable.plot(
  model_data$gov_party, 
  "Party of Governor",
  "Party of Governor and Atkinson Index")
gov_party_plot


college_subsidy_plot <- variable.plot(
  model_data$college_subsidy,
  "Amount of College Subsidy per Student",
  "College Subsidies and Atkinson Index")
college_subsidy_plot

college_plot <- variable.plot(
  model_data$college, 
  "Percentage of College Graduates",
  "College Graduates and Atkinson Index")
college_plot

region_plot <- variable.plot(
  model_data$region, 
  "Region of United States",
  "Atkinson Index by Region")
region_plot

renew_consum_plot <- variable.plot(
  model_data$renew_consum,
  "Consumption of Renewable Energy (trillions of BTUs)",
  "Renewable Energy Consumption and Atkinson Index")
renew_consum_plot

pro_environment_plot <- variable.plot(
  model_data$pro_environment,
  "Population That Believes Spending 'Too Little' On Environemnt (%)",
  "Pro-Environment Population and Atkinson Index")
pro_environment_plot

economic_status_plot <- variable.plot(
  model_data$economic_status, 
  "Household Median Income",
  "Household Median Income and Atkinson Index")
economic_status_plot

innovate_score_plot <- variable.plot(
  model_data$innovate_score, 
  "Liklihood to Adopt New Policies Faster Than Other States", 
  "Boehmke-Skinner Innovate Score and Atkinson Index")
innovate_score_plot

ideology_plot <- variable.plot(
  model_data$ideology, 
  "Median Liberal Score",
  "Measure of Liberalism of a State and Atkinson Index")
ideology_plot

divided_plot <- variable.plot(
  model_data$divided_govt,
  "Divided Government (0: Unified, 1: Divided)",
  "Government Party Unity and Atkinson Index"
)

nonwhite_plot <- variable.plot(
  model_data$nonwhite,
  "Proportion of Nonwhite Residents",
  "Nonwhite Population and Atkinson Index"
)



## plotting line of best fit with a model 
Model1_plot <- ggplot(model_data, aes(pro_environment + renew_consum 
                                      + as.numeric(region), atkinson)) +
  geom_point() +
  stat_smooth(method = lm) +
  ggtitle("Model 1 Fitted Plot") +
  labs(x = "Independent Variables(pro_environment + renew_consum + region)",
         y = "Atkinson Index Score") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
Model1_plot




Model2_plot <- ggplot(model_data, aes(gov_party + ideology 
                                      + innovate_score + college +
                                        divided_govt, atkinson)) +
  geom_point() +
  stat_smooth(method = lm) +
  ggtitle("Model 2 Fitted Plot") +
  labs(x = "Independent Variables(gov_party + ideology + innovate_score  + divided_govt)",
       y = "Atkinson Index Score") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
Model2_plot




Model3_plot <- ggplot(model_data, aes(economic_status + corp_tax 
                                      + public_education  + college_subsidy + college + nonwhite,
                                      atkinson)) +
  geom_point() +
  stat_smooth(method = lm) +
  ggtitle("Model 3 Fitted Plot") +
  labs(x = "Independent Variables(economic_status + corp_tax + public_education 
       + college_subsidy + college + nonwhite population )",
       y = "Atkinson Index Score") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
Model3_plot




Model4_plot <- ggplot(model_data, aes(gov_party + college_subsidy
                                      + ideology + innovate_score 
                                      + college + pro_environment
                                      + renew_consum + economic_status 
                                      + corp_tax + public_education
                                      + as.numeric(region), atkinson)) +
  geom_point() +
  stat_smooth(method = lm)+
  ggtitle("Model 4 Fitted Plot") +
  labs(x = "Independent Variables (aggregation)",
       y = "Atkinson Index Score") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
Model4_plot