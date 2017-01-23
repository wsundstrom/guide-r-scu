#===================================================
#   Data Analysis Tutorial: Replicated 538 analysis
#===================================================

# For context see original fivethirtyeight article
# https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/

# Adapted from https://mran.microsoft.com/web/packages/fivethirtyeight/vignettes/bechdel.html
# edited by Michael Kevane 1/22/2017

# Description: Bechdel website codes movies on scale if
# there are two females, who have conversation, not about men 
# 538 correlated Bechdel score with budget and gross receipts
# Do movies with female leads generate lower box office returns?

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR folder)
setwd("/Users/verysmart/data")

# Remember only need to install once, after that hashtag out the install
#install.packages("fivethirtyeight")
#install.packages("ggthemes")
#install.packages("broom")
library(fivethirtyeight)
library(dplyr)
library(ggplot2)
library(knitr)
library(magrittr)
library(broom)
library(stringr)
library(ggthemes)
library(scales)
library(doBy)
library(stargazer)
library(sandwich)


# Turn off scientific notation
options(scipen = 99)
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
  }

#==============================================================================
#   2. Data section
#==============================================================================

# Load data that accompanies fivethirtyeight package
data("bechdel")

# put dataset in R dataframe format
bechdel <- data.frame(bechdel)

# Focus on films from 1990 to 2013
bechdel90_13 <- bechdel %>% filter(between(year, 1990, 2013))

# Create international gross only and return on investment (ROI) columns 
# and add to bechdel_90_13 data frame as new variables
# the "mutate" function creates new variables
bechdel90_13 %<>% 
  mutate(int_only = intgross_2013 - domgross_2013,
         roi_total = intgross_2013 / budget_2013,
         roi_dom = domgross_2013 / budget_2013,
         roi_int = int_only / budget_2013)

# Create generous variable
bechdel90_13 %<>%
  mutate(generous = ifelse(test = clean_test %in% c("ok", "dubious"),
                           yes = TRUE,
                           no = FALSE))

# Crosstab of generous and binary, measure of if passes Bechdel test
with(bechdel90_13, table(generous, binary))


#==============================================================================
#   3. Analysis section
#==============================================================================

# Median ROI and budget based on categories

# This is the way R programmers do
budget_by_binary <- bechdel90_13 %>% 
  group_by(binary) %>% 
summarize(median_budget = median(budget_2013, na.rm = TRUE))
budget_by_binary

ROI_by_binary <- bechdel90_13 %>% 
  group_by(binary) %>% 
  summarize(median_ROI = median(roi_total, na.rm = TRUE))
ROI_by_binary

bechdel90_13 %>% 
  summarize(`Median Overall Return on Investment` = median(roi_total, na.rm = TRUE))

# This is the way social scientists do it
# Standard descriptive statistics for all numerical variables in the data
median(bechdel90_13$roi_total , na.rm=TRUE)

summaryBy(budget_2013 +roi_total~ binary,  data=bechdel90_13 , FUN=c(median), na.rm=TRUE)

stargazer(bechdel90_13, type="text", median=TRUE,
          digits=2, title="Bechdel data set")

t.test(bechdel90_13$roi_total ~ bechdel90_13$binary)  
t.test(bechdel90_13$roi_dom ~ bechdel90_13$binary)  
t.test(bechdel90_13$roi_int ~ bechdel90_13$binary)  

# Plot budget, international gross, ROI, and their logarithms
ggplot(data = bechdel90_13, mapping = aes(x = budget)) +
  geom_histogram(color = "white", bins = 20) +
  labs(title = "Histogram of budget")

ggplot(data = bechdel90_13, 
  mapping = aes(x = log(budget_2013), y = log(intgross_2013), 
  color = binary)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# How programmers do regressions
gross_vs_budget_binary <- lm(log(intgross_2013) ~ log(budget_2013) + factor(binary), 
                      data = bechdel90_13)
tidy(gross_vs_budget_binary)
# How social scientists do regressions
reg1 <- lm(log(domgross_2013) ~ log(budget_2013) , 
           data = bechdel90_13)
reg2 <- lm(log(domgross_2013) ~ log(budget_2013) + factor(binary), 
           data = bechdel90_13)
reg3 <- lm(log(intgross_2013) ~ log(budget_2013) + factor(binary), 
           data = bechdel90_13)
reg4 <- lm(log(intgross_2013) ~ log(budget_2013) + factor(binary)+ log(domgross_2013), 
           data = bechdel90_13)

# With correct standard errors and column labels to help the reader
stargazer(reg1, reg2, reg3, reg4,  
          se=list(cse(reg1),cse(reg2),cse(reg3),cse(reg4)), 
          title="Regression Results", type="text", 
          column.labels=c("Dom gross", "Dom gross", "Int gross", "Int gross"),
          df=FALSE, digits=3)


# Replicating the graphic that appeared in the news article

# median ROI for domestic that pass
passes_bechtel_rom <- bechdel90_13 %>% 
  filter(generous == TRUE) %>% 
  summarize(median_roi = median(roi_dom, na.rm = TRUE))
# median ROI for domestic that by group wherer Bechdel is not disputed
median_groups_dom <- bechdel90_13 %>% 
  filter(clean_test %in% c("men", "notalk", "nowomen")) %>% 
  group_by(clean_test) %>% 
  summarize(median_roi = median(roi_dom, na.rm = TRUE))
# join with a title
pass_bech_rom <- data_frame(clean_test = "pass", 
                  median_roi = passes_bechtel_rom$median_roi)
# join with a new variable for the group
med_groups_dom_full <- bind_rows(pass_bech_rom, median_groups_dom) %>% 
  mutate(group = "U.S. and Canada")

# Do same thing for international
passes_bechtel_int <- bechdel90_13 %>% 
  filter(generous == TRUE) %>% 
  summarize(median_roi = median(roi_int, na.rm = TRUE))
median_groups_int <- bechdel90_13 %>% 
  filter(clean_test %in% c("men", "notalk", "nowomen")) %>% 
  group_by(clean_test) %>% 
  summarize(median_roi = median(roi_int, na.rm = TRUE))
pass_bech_int <- data_frame(clean_test = "pass", 
                  median_roi = passes_bechtel_int$median_roi)
med_groups_int_full <- bind_rows(pass_bech_int, median_groups_int) %>% 
  mutate(group = "International")

# merge domestic and international and change labels to be good lablels
med_groups <- bind_rows(med_groups_dom_full, med_groups_int_full) %>% 
  mutate(clean_test = str_replace_all(clean_test, 
                                      "pass",
                                      "Passes Bechdel Test"),
         clean_test = str_replace_all(clean_test, "men",
                                      "Women only talk about men"),
         clean_test = str_replace_all(clean_test, "notalk",
                                      "Women don't talk to each other"),
         clean_test = str_replace_all(clean_test, "nowoWomen only talk about men",
                                      "Fewer than two women"))
med_groups %<>% mutate(clean_test = factor(clean_test, 
                                 levels = c("Fewer than two women", 
                                            "Women don't talk to each other",
                                            "Women only talk about men",
                                            "Passes Bechdel Test"))) %>% 
  mutate(group = factor(group, levels = c("U.S. and Canada", "International"))) %>% 
  mutate(median_roi_dol = dollar(median_roi))

# Plotting the resultsd
ggplot(data = med_groups, mapping = aes(x = clean_test, y = median_roi, 
       fill = group)) +
  geom_bar(stat = "identity") +  #this makes bar chart have height = value, and not = count of obs in the group bins
  facet_wrap(~ group) +
  coord_flip() +
  labs(title = "Dollars Earned for Every Dollar Spent", subtitle = "2013 dollars")+
  scale_fill_fivethirtyeight() +
  theme_fivethirtyeight()