
# Project - survival_analysis
# Sub-Project - 
# File name - 01_Setup.R
# Purpose - Initial set up of RStudio for the project
# Date created - 26-Nov-2023

""" Notes
Data Source: https://github.com/lbraglia/suanselete3/tree/master
Data Name  : Addict

Description (as lifted from the file)
 Survival times in days of heroin addicts
 from entry to a clinic until departure.

 Data provided by John Caplehorn,
 c/- The University of Sydney,
     Dept of Public Health.

 Column 1 = ID of subject
        2 = Clinic (1 or 2)
        3 = status (0=censored, 1=endpoint)
        4 = survival time (days)
        5 = prison record?
        6 = methodone dose (mg/day)
"""
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)     # this is needed on some PCs to increase memory allowance, but has no impact on macs.

## ---------------------------

## Installing the missing package 'survminer'
# install.packages("survminer")

## Loading packages
library(survival)
library(dplyr)
library(survminer)

## ---------------------------

## Loading the data
addicts <- read.table(file = "Data/addicts.dat", skip = 19)
names(addicts) <- c('id', 'clinic', 'status', 'survt', 'prison', 'dose')

## ---------------------------------------------------------------------

# Structure of addicts table
str(addicts)

# Summary statistic on all columns of addicts table
summary(addicts)

## Dimensions of addicts table
dim(addicts)
#### 238 rows, 6 columns

# Frequency table by each column

## Prison history
table(addicts$prison)

## Clinic
table(addicts$clinic)

## Status
table(addicts$status)

## Methadone Dose
table(addicts$dose)

## Summary Statistic Again - For Dose and Survival Time
summary(addicts[,c("survt","dose")])

## Check for NA
sum(is.na.data.frame(addicts))

## ---------------------------------------------------------------------

## Building first curve
### No stratification
#### Surv function creates a survival object
#### Survfit function fits a survival curve on the specified survival object 
#### with respect to the strata specified
fit_1 <- survfit(Surv(survt, status) ~ 1, data = addicts)

## Key results of the model
print(fit_1)

## Summary of the model - Prints the entire life table
summary(fit_1)

## Creating a dataframe of the result
d <- data.frame(time      = fit_1$time,
                n.risk    = fit_1$n.risk,
                n.event   = fit_1$n.event,
                n.censor  = fit_1$n.censor,
                surv      = fit_1$surv,
                upper     = fit_1$upper,
                lower     = fit_1$lower
)

## Printing some top results of the data frame created
head(d)

## Removing the object 'd'
rm("d")

## Creating a survival plot
ggsurvplot(fit_1,
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv" # Specify median survival
)


## ---------------------------------------------------------------------

## Building second curve
### Stratify by clinic
fit_2 <- survfit(Surv(survt, status) ~ clinic, data = addicts)

## Key results of the model
print(fit_2)

## Summary of the model - Prints the entire life table
summary(fit_2)

## Structure of the survival object
str(fit_2)

## Getting the names of the strata
names(fit_2$strata)
### Couldn't figure out how to map strata name to the value

## Creating a dataframe of the result
lifetable_2 <- data.frame(time      = fit_2$time,
                          n.risk    = fit_2$n.risk,
                          n.event   = fit_2$n.event,
                          n.censor  = fit_2$n.censor,
                          surv      = fit_2$surv,
                          upper     = fit_2$upper,
                          lower     = fit_2$lower,
                          strata    = fit_2$strata
)

## Printing some top results of the data frame created
head(lifetable_2)
### Turns out this was not needed

## Creating a survival plot
ggsurvplot(fit_2,
           pval = TRUE, 
           conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF")
)






