# Using and finding missing values
# When working with missing data, there are a couple of commands that you should be familiar with - firstly, you should be able to identify if there are any missing values, and where these are.
# 
# Using the any_na() and are_na() tools, identify which values are missing.

# Create x, a vector, with values NA, NaN, Inf, ".", and "missing"
x <- c(NA,NaN, Inf, ".", "missing")

# Use any_na() and are_na() on to explore the missings
install.packages("tidyverse")
install.packages("naniar")
library(tidyverse)
library(naniar)


any_na(x)
are_na(x)

# How many missing values are there?
#   One of the first things that you will want to check with a new dataset is if there are any missing missing values, and how many there are.
# 
# You could use are_na() to and count up the missing values, but the most efficient way to count missings is to use the n_miss() function. This will tell you the total number of missing values in the data.
# 
# You can then find the percent of missing values in the data with the pct_miss function. This will tell you the percentage of missing values in the data.
# 
# You can also find the complement to these - how many complete values there are - using n_complete and pct_complete.


# Use n_miss() to count the total number of missing values in dat_hw
n_miss(dat_hw)

# Use n_miss() on dat_hw$weight to count the total number of missing values
n_miss(dat_hw$weight)

# Use n_complete() on dat_hw to count the total number of complete values
n_complete(dat_hw)

# Use n_complete() on dat_hw$weight to count the total number of complete values
n_complete(dat_hw$weight)

# Use prop_miss() and prop_complete() on dat_hw to count the total number of missing values in each of the variables
prop_miss(dat_hw)
prop_complete(dat_hw)


# Working with missing values
# R stores missing values as NA, which have some special behavior. Now that you can define missing data and understand how R stores missing values, can you predict what will happen when we operate with some missing values?
#   
#   What is the output of the following four commands in R? Try them out in the code console to test them before you submit your answer.

1 + NA
NA+NA
NA | TRUE
NA | FALSE

# NA NA TRUE NA

# Summarizing missingness
# Now that you understand the behavior of missing values in R, and how to count them, let's scale up our summaries for cases (rows) and variables, using miss_var_summary() and miss_case_summary(), and also explore how they can be applied for groups in a dataframe, using the group_by function from dplyr.

# Summarize missingness in each variable of the `airquality` dataset
miss_var_summary(airquality)

# Summarize missingness in each case of the `airquality` dataset
miss_case_summary(airquality)

# Return the summary of missingness in each variable, 
# grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_var_summary()

# Return the summary of missingness in each case, 
# grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_case_summary()

# Tabulating Missingness
# The summaries of missingness we just calculated give us the number and percentage of missing observations for the cases and variables.
# 
# Another way to summarize missingness is by tabulating the number of times that there are 0, 1, 2, 3, missings in a variable, or in a case.
# 
# In this exercise we are going to tabulate the number of missings in each case and variable using miss_var_table() and miss_case_table(), and also combine these summaries with the the group_by operator from dplyr. to explore the summaries over a grouping variable in the dataset.

# Tabulate missingness in each variable and case of the `airquality` dataset
miss_var_table(airquality)
miss_case_table(airquality)

# Tabulate the missingness in each variable, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_var_table()

# Tabulate of missingness in each case, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_case_table()


# Other summaries of missingness
# Some summaries of missingness are particularly useful for different types of data. For example, miss_var_span() and miss_var_run().
# 
# miss_var_span() calculates the number of missing values in a specified variable for a repeating span. This is really useful in time series data, to look for weekly (7 day) patterns of missingness.
# 
# miss_var_run() calculates the number of "runs" or "streaks" of missingness. This is useful to find unusual patterns of missingness, for example, you might find a repeating pattern of 5 complete and 5 missings.
# 
# Both miss_var_span() and miss_var_run() work with the group_by operator from dplyr.

# Calculate the summaries for each run of missingness for the variable, hourly_counts
miss_var_run(pedestrian, var = hourly_counts)

# Calculate the summaries for each span of missingness, 
# for a span of 4000, for the variable hourly_counts
miss_var_span(pedestrian, var = hourly_counts, span_every = 4000)

# For each `month` variable, calculate the run of missingness for hourly_counts
pedestrian %>% group_by(month) %>% miss_var_run(var = hourly_counts)

# For each `month` variable, calculate the span of missingness 
# of a span of 2000, for the variable hourly_counts
pedestrian %>% group_by(month) %>% miss_var_span(var = hourly_counts, span_every = 2000)

# Your first missing data visualizations
# It can be difficult to get a handle on where the missing values are in your data, and here is where visualization can really help.
# 
# The function vis_miss() creates an overview visualization of the missingness in the data. It also has options to cluster rows based on missingness, using cluster = TRUE; as well as options for sorting the columns, from most missing to least missing (sort_miss = TRUE).

# Visualize all of the missingness in the `riskfactors`  dataset
vis_miss(riskfactors)

# Visualize and cluster all of the missingness in the `riskfactors` dataset
vis_miss(riskfactors, cluster = TRUE)

# visualize and sort the columns by missingness in the `riskfactors` dataset
vis_miss(riskfactors, sort_miss = TRUE)

# Visualizing missing cases and variables
# To get a clear picture of the missingness across variables and cases, use gg_miss_var() and gg_miss_case(). These are the visual counterpart to miss_var_summary() and miss_case_summary().
# 
# These can be split up into multiple plots with one for each category by choosing a variable to facet by.

# Visualize the number of missings in cases using `gg_miss_case()`
gg_miss_case(riskfactors)

# Explore the number of missings in cases using `gg_miss_case()` 
# and facet by the variable `education`
gg_miss_case(riskfactors, facet = education)

# Visualize the number of missings in variables using `gg_miss_var()`
gg_miss_var(riskfactors)

# Explore the number of missings in variables using `gg_miss_var()` 
# and facet by the variable `education`
gg_miss_var(riskfactors, facet = education)

# Visualizing missingness patterns
# Let's practice a few different ways to visualize patterns of missingness using:
# 
# gg_miss_upset() to give an overall pattern of missingness.
# gg_miss_fct() for a dataset that has a factor of interest: marriage.
# and gg_miss_span() to explore the missingness in a time series dataset.
# What do you notice with the missingness and the faceting in the data?

# Using the airquality dataset, explore the missingness pattern using gg_miss_upset()
gg_miss_upset(airquality)

# With the riskfactors dataset, explore how the missingness changes across the marital variable using gg_miss_fct()
gg_miss_fct(x = riskfactors, fct = marital)

# Using the pedestrian dataset, explore how the missingness of hourly_counts changes over a span of 3000 
gg_miss_span(pedestrian, var = hourly_counts, span_every = 3000)

# Using the pedestrian dataset, explore the impact of month by faceting by month
# and explore how missingness changes for a span of 1000
gg_miss_span(pedestrian, var = hourly_counts, span_every = 1000, facet = month)

