# Creating shadow matrix data
# Missing data can be tricky to think about, as they don't usually proclaim themselves for you, and instead hide amongst the weeds of the data.
# 
# One way to help expose missing values is to change the way we think about the data - by thinking about every single data value being missing or not missing.
# 
# The as_shadow() function in R transforms a dataframe into a shadow matrix, a special data format where the values are either missing (NA), or Not Missing (!NA).
# 
# The column names of a shadow matrix are the same as the data, but have a suffix added _NA.
# 
# To keep track of and compare data values to their missingness state, use the bind_shadow() function. Having data in this format, with the shadow matrix column bound to the regular data is called nabular data.

# Create shadow matrix data with `as_shadow()`
as_shadow(oceanbuoys)

# Create nabular data by binding the shadow to the data with `bind_shadow()`
bind_shadow(oceanbuoys)

# Bind only the variables with missing values by using bind_shadow(only_miss = TRUE)
bind_shadow(oceanbuoys, only_miss = TRUE)

# Performing grouped summaries of missingness
# Now that you can create nabular data, let's use it to explore the data. Let's calculate summary statistics based on the missingness of another variable.
# 
# To do this we are going to use the following steps:
#   
#   First, bind_shadow() turns the data into nabular data.
# 
# Next, perform some summaries on the data using group_by() and summarize() to calculate the mean and standard deviation, using the mean() and sd() functions.

# `bind_shadow()` and `group_by()` humidity missingness (`humidity_NA`)
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew), # calculate mean of wind_ew
            wind_ew_sd = sd(wind_ew)) # calculate standard deviation of wind_ew

# Repeat this, but calculating summaries for wind north south (`wind_ns`).
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarize(wind_ns_mean = mean(wind_ns),
            wind_ns_sd = sd(wind_ns))

# Further exploring more combinations of missingness
# It can be useful to get a bit of extra information about the number of cases in each missing condition.
# 
# In this exercise, we are going to add information about the number of observed cases using n() inside the summarize() function.
# 
# We will then add an additional level of grouping by looking at the combination of humidity being missing (humidity_NA) and air temperature being missing (air_temp_c_NA).

# Summarize wind_ew by the missingness of `air_temp_c_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

# Summarize wind_ew by missingness of `air_temp_c_NA` and `humidity_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA, humidity_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

# Nabular data and filling by missingness
# Summary statistics are useful to calculate, but as they say, a picture tells you a thousand words.
# 
# In this exercise, we are going to explore how you can use nabular data to explore the variation in a variable by the missingness of another.
# 
# We are going to use the oceanbuoys dataset from naniar.

# First explore the missingness structure of `oceanbuoys` using `vis_miss()`
vis_miss(oceanbuoys)

# Explore the distribution of `wind_ew` for the missingness  
# of `air_temp_c_NA` using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew, 
             color = air_temp_c_NA)) + 
  geom_density()

# Explore the distribution of sea temperature for the  
# missingness of humidity (humidity_NA) using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = sea_temp_c,
             color = humidity_NA)) + 
  geom_density()

# Nabular data and summarising by missingness
# In this exercise, we are going to explore how to use nabular data to explore the variation in a variable by the missingness of another.
# 
# We are going to use the oceanbuoys dataset from naniar, and then create multiple plots of the data using facets.
# 
# This allows you to explore different layers of missingness.

# Explore the distribution of wind east west (wind_ew) for the missingness of air temperature 
# using geom_density() and faceting by the missingness of air temperature (air_temp_c_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Build upon this visualization by coloring by the missingness of humidity (humidity_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew,
             color = humidity_NA)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Explore variation by missingness: box plots
# Previous exercises use nabular data along with density plots to explore the variation in a variable by the missingness of another.
# 
# We are going to use the oceanbuoys dataset from naniar, using box plots instead of facets or others to explore different layers of missingness.

# Explore the distribution of wind east west (`wind_ew`) for  
# the missingness of air temperature using  `geom_boxplot()`
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot()

# Build upon this visualization by faceting by the missingness of humidity (`humidity_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot() + 
  facet_wrap(~humidity_NA)

# Exploring missing data with scatter plots
# Missing values in a scatter plot in ggplot2 are removed by default, with a warning.
# 
# We can display missing values in a scatter plot, using geom_miss_point() - a special ggplot2 geom that shifts the missing values into the plot, displaying them 10% below the minimum of the variable.
# 
# Let's practice using this visualization with the oceanbuoys dataset.

# Explore the missingness in wind and air temperature, and  
# display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point()

# Explore the missingness in humidity and air temperature,  
# and display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point()

# Using facets to explore missingness
# Because geom_miss_point() is a ggplot geom, you can use it with ggplot2 features like faceting.
# 
# This means we can rapidly explore the missingness and stay within the familiar bounds of ggplot2.

# Explore the missingness in wind and air temperature, and display the 
# missingness using `geom_miss_point()`. Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Explore the missingness in humidity and air temperature, and display the 
# missingness using `geom_miss_point()` Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Faceting to explore missingness (multiple plots)
# Another useful technique with geommisspoint() is to explore the missingness by creating multiple plots.
# 
# Just as we have done in the previous exercises, we can use the nabular data to help us create additional faceted plots.
# 
# We can even create multiple faceted plots according to values in the data, such as year, and features of the data, such as missingness.

# Use geom_miss_point() and facet_wrap to explore how the missingness  
# in wind_ew and air_temp_c is different for missingness of humidity
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~humidity_NA)

# Use geom_miss_point() and facet_grid to explore how the missingness in wind_ew and air_temp_c 
# is different for missingness of humidity AND by year - by using `facet_grid(humidity_NA ~ year)`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_grid(humidity_NA~year)
