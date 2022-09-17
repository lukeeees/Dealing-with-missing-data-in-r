# Impute data below range with nabular data
# We want to keep track of values we imputed. If we don't, it is very difficult to assess how good the imputed values are.
# 
# We are going to practice imputing data and recreate visualizations in the previous set of exercises by imputing values below the range of the data.
# 
# This is a very useful way to help further explore missingness, and also provides the framework for imputing missing values.
# 
# First, we are going to impute the data below the range using impute_below_all(), and then visualize the data. We notice that although we can see where the missing values are in this instance, we need some way to track them. The track missing data programming pattern can help with this.

# Impute the oceanbuoys data below the range using `impute_below`.
ocean_imp <- impute_below_all(oceanbuoys)

# Visualize the new missing values
ggplot(ocean_imp,
       aes(x = wind_ew, y = air_temp_c)) + 
  geom_point()

# Impute and track data with `bind_shadow`, `impute_below_all`, and `add_label_shadow`
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>%
  add_label_shadow()

# Look at the imputed values
ocean_imp_track

# Visualize imputed values in a scatter plot
# Now, let's recreate one of the previous plots we saw in chapter three that used geom_miss_point().
# 
# To do this, we need to impute the data below the range of the data. This is a special kind of imputation to explore the data. This imputation will illustrate what we need to practice: how to track missing values. To impute the data below the range of the data, we use the function impute_below_all().

# Impute and track the missing values
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>%
  add_label_shadow()

# Visualize the missingness in wind and air temperature,  
# coloring missing air temp values with air_temp_c_NA
ggplot(ocean_imp_track,
       aes(x = wind_ew, y = air_temp_c, color = air_temp_c_NA)) + 
  geom_point()

# Visualize humidity and air temp, coloring any missing cases using the variable any_missing
ggplot(ocean_imp_track,
       aes(x = humidity, y = air_temp_c, color = any_missing)) + 
  geom_point()

# Create histogram of imputed data
# Now that we can recreate the first visualization of geom_miss_point(), let's explore how we can apply this to other exploratory tasks.
# 
# One useful task is to evaluate the number of missings in a given variable using a histogram. We can do this using the ocean_imp_track dataset we created in the last exercise, which is loaded into this session.

# Explore the values of air_temp_c, visualizing the amount of missings with `air_temp_c_NA`.
p <- ggplot(ocean_imp_track, aes(x = air_temp_c, fill = air_temp_c_NA)) + geom_histogram()

# Expore the missings in humidity using humidity_NA
p2 <- ggplot(ocean_imp_track, aes(x = humidity, fill = humidity_NA)) + geom_histogram()

# Explore the missings in air_temp_c according to year, using `facet_wrap(~year)`.
p + facet_wrap(~year)

# Explore the missings in humidity according to year, using `facet_wrap(~year)`.
p2 + facet_wrap(~year)

# Evaluating bad imputations
# In order to evaluate imputations, it helps to know what something bad looks like. To explore this, let's look at a typically bad imputation method: imputing using the mean value.
# 
# In this exercise we are going to explore how the mean imputation method works using a box plot, using the oceanbuoys dataset.

# Impute the mean value and track the imputations 
ocean_imp_mean <- bind_shadow(oceanbuoys) %>% 
  impute_mean_all() %>% 
  add_label_shadow()

# Explore the mean values in humidity in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = humidity_NA, y = humidity)) + 
  geom_boxplot()

# Explore the values in air temperature in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c_NA, y = air_temp_c)) + 
  geom_boxplot()

# Evaluating imputations: The scale
# While the mean imputation might not look so bad when we compare it using a box plot, it is important to get a sense of the variation in the data. This is why it is important to explore how the scale and spread of imputed values changes compared to the data.
# 
# One way to evaluate the appropriateness of the scale of the imputations is to use a scatter plot to explore whether or not the values are appropriate.

# Explore imputations in air temperature and humidity,  
# coloring by the variable, any_missing
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point()

# Explore imputations in air temperature and humidity,  
# coloring by the variable, any_missing, and faceting by year
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) +
  geom_point() + 
  facet_wrap(~year)

# Evaluating imputations: Across many variables
# So far, we have covered ways to look at individual variables or pairs of variables and their imputed values. However, sometimes you want to look at imputations for many variables. To do this, you need to perform some data munging and re-arranging. This lesson covers how to perform this data wrangling, which can get a little bit hairy when considering its usage in nabular data. The function, shadow_long() gets the data into the right shape for these kinds of visualizations.

# Gather the imputed data 
ocean_imp_mean_gather <- shadow_long(ocean_imp_mean,
                                     humidity,
                                     air_temp_c)
# Inspect the data
ocean_imp_mean_gather

# Explore the imputations in a histogram 
ggplot(ocean_imp_mean_gather, 
       aes(x = value, fill = value_NA)) + 
  geom_histogram() + 
  facet_wrap(~variable)

# Using simputation to impute data
# There are many imputation packages in R. We are going to focus on using the simputation package, which provides a simple, powerful interface into performing imputations.
# 
# Building a good imputation model is super important, but it is a complex topic - there is as much to building a good imputation model as there is for building a good statistical model. In this course, we are going to focus on how to evaluate imputations.
# 
# First, we are going to look at using impute_lm() function, which imputes values according to a specified linear model.
# 
# In this exercise, we are going to apply the previous assessment techniques to data with impute_lm(), and then build upon this imputation method in subsequent lessons.

# Impute humidity and air temperature using wind_ew and wind_ns, and track missing values
ocean_imp_lm_wind <- oceanbuoys %>% 
  bind_shadow() %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns) %>% 
  impute_lm(humidity ~ wind_ew + wind_ns) %>%
  add_label_shadow()

# Plot the imputed values for air_temp_c and humidity, colored by missingness
ggplot(ocean_imp_lm_wind, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) +
  geom_point()

# Evaluating and comparing imputations
# When you build up an imputation model, it's a good idea to compare it to another method. In this lesson, we are going to compare the previously imputed dataset created using impute_lm() to the mean imputed dataset. Both of these datasets are included in this exercise as ocean_imp_lm_wind and ocean_imp_mean respectively.

# Bind the models together 
bound_models <- bind_rows(mean = ocean_imp_mean,
                          lm_wind = ocean_imp_lm_wind,
                          .id = "imp_model")

# Inspect the values of air_temp and humidity as a scatter plot
ggplot(bound_models,
       aes(x = air_temp_c,
           y = humidity,
           color = any_missing)) +
  geom_point() + 
  facet_wrap(~imp_model)

# Evaluating imputations (many models & variables)
# When you build up an imputation model, it's a good idea to compare it to another method.
# 
# In this lesson, we are going to get you to add a final imputation model that contains an extra useful piece of information that helps explain some of the variation in the data. You are then going to compare the values, as previously done in the last lesson.

# Build a model adding year to the outcome
ocean_imp_lm_wind_year <- bind_shadow(oceanbuoys) %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year) %>%
  impute_lm(humidity ~ wind_ew + wind_ns + year) %>%
  add_label_shadow()

# Bind the mean, lm_wind, and lm_wind_year models together
bound_models <- bind_rows(mean = ocean_imp_mean,
                          lm_wind = ocean_imp_lm_wind,
                          lm_wind_year = ocean_imp_lm_wind_year,
                          .id = "imp_model")

# Explore air_temp and humidity, coloring by any missings, and faceting by imputation model
ggplot(bound_models, aes(x = air_temp_c, y = humidity, color = any_missing)) +
  geom_point() + facet_wrap(~imp_model)

# Combining and comparing many imputation models
# To evaluate the different imputation methods, we need to put them into a single dataframe. Next, you will compare three different approaches to handling missing data using the dataset, oceanbuoys.
# 
# The first method is using only the completed cases and is loaded as ocean_cc.
# The second method is imputing values using a linear model with predictions made using wind and is loaded as ocean_imp_lm_wind.
# You will create the third imputed dataset, ocean_imp_lm_all, using a linear model and impute the variables sea_temp_c, air_temp_c, and humidity using the variables wind_ew, wind_ns, year, latitude, longitude.
# 
# You will then bind all of the datasets together (ocean_cc, ocean_imp_lm_wind, and ocean_imp_lm_all), calling it bound_models.
# Create an imputed dataset using a linear models

ocean_imp_lm_all <- bind_shadow(oceanbuoys) %>%
  add_label_shadow() %>%
  impute_lm(sea_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
  impute_lm(humidity ~ wind_ew + wind_ns + year + latitude + longitude)

# Bind the datasets
bound_models <- bind_rows(cc = ocean_cc,
                          imp_lm_wind = ocean_imp_lm_wind,
                          imp_lm_all = ocean_imp_lm_all,
                          .id = "imp_model")
# Look at the models
bound_models

# Evaluating the different parameters in the model
# We are imputing our data for a reason - we want to analyze the data!
#   
#   In this example, we are interested in predicting sea temperature, so we will build a linear model predicting sea temperature.
# 
# We will fit this model to each of the datasets we created and then explore the coefficients in the data.
# 
# The objects from the previous lesson (ocean_cc, ocean_imp_lm_wind, ocean_imp_lm_all, and bound_models) are loaded into the workspace.

# Create the model summary for each dataset
model_summary <- bound_models %>% 
  group_by(imp_model) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(sea_temp_c ~ air_temp_c + humidity + year, data = .)),
         res = map(mod, residuals),
         pred = map(mod, predict),
         tidy = map(mod, tidy))

# Explore the coefficients in the model
model_summary %>% 
  select(imp_model,tidy) %>% 
  unnest()
best_model <-  "imp_lm_all"

