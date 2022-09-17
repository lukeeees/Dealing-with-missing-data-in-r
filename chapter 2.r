# Using miss_scan_count
# You have a dataset with missing values coded as "N/A", "missing", and "na". But before we go ahead and start replacing these with NA, we should get an idea of how big the problem is.
# 
# Use miss_scan_count to count the possible missings in the dataset, pacman, a dataset of pacman scores, containing three columns:
#   
#   year: the year that person made that score.
# initial: the initials of the person.
# score: the scores of that person.

# Explore the strange missing values "N/A"
miss_scan_count(data = pacman, search = list("N/A"))

# Explore the strange missing values "missing"
miss_scan_count(data = pacman, search = list("missing"))

# Explore the strange missing values "na"
miss_scan_count(data = pacman, search = list("na"))

# Explore the strange missing values " " (a single space)
miss_scan_count(data = pacman, search = list(" "))

# Explore all of the strange missing values, "N/A", "missing", "na", " "
miss_scan_count(data = pacman, search = list("N/A", "missing","na", " "))
# 
# Using replace_with_na
# Following on from the previous dataset, we now know that we have a few strange missing values.
# 
# Now, we are going to do something about it, and replace these values with missings (e.g. NA) using the function replace_with_na().

# Print the top of the pacman data using `head()`
head(pacman)

# Replace the strange missing values "N/A", "na", and  
# "missing" with `NA` for the variables, year, and score
pacman_clean <- replace_with_na(pacman, replace = list(year = c("N/A", "na", "missing"),
                                                       score = c("N/A", "na", "missing")))

# Test if `pacman_clean` still has these values in it?
miss_scan_count(pacman_clean, search = list("N/A", "na", "missing"))

# Using replace_with_na scoped variants
# To reduce code repetition when replacing values with NA, use the "scoped variants" of replace_with_na():
#   
#   replace_with_na_at()
# replace_with_na_if()
# replace_with_na_all()
# The syntax of replacement looks like this:
#   
#   ~.x == "N/A"
# This replaces all cases that are equal to "N/A".
# 
# ~.x %in% c("N/A", "missing", "na", " ")
# Replaces all cases that have "N/A", "missing", "na", or " ".

# Use `replace_with_na_at()` to replace with NA
replace_with_na_at(pacman,
                   .vars = c("year", "month", "day"), 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_if()` to replace with NA the character values using `is.character`
replace_with_na_if(pacman,
                   .predicate = is.character, 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_all()` to replace with NA
replace_with_na_all(pacman, ~.x %in% c("N/A", "missing", "na", " "))


# Fix implicit missings using complete()
# We are going to explore a new dataset, frogger.
# 
# This dataset contains 4 scores per player recorded at different times: morning, afternoon, evening, and late_night.
# 
# Every player should have played 4 games, one at each of these times, but it looks like not every player completed all of these games.
# 
# Use the complete() function to make these implicit missing values explicit

# Print the frogger data to have a look at it
frogger

# Use `complete()` on the `time` and `name` variables to  
# make implicit missing values explicit
frogger_tidy <- frogger %>% complete(time, name)

# Fix explicit missings using fill()
# One type of missing value that can be obvious to deal with is where the first entry of a group is given, but subsequent entries are marked NA.
# 
# These missing values often result from empty values in spreadsheets to avoid entering multiple names multiple times; as well as for "human readability".
# 
# This type of problem can be solved by using the fill() function from the tidyr package.

# Print the frogger data to have a look at it
frogger

# Use `fill()` to fill down the name variable in the frogger dataset
frogger %>% fill(name)

# Using complete() and fill() together
# Now let's put it together!
# 
# Use complete() and fill() together to fix explicit and implicitly missing values in the frogger dataset.

# Print the frogger data to have a look at it
frogger

# Correctly fill() and complete() missing values so that our dataset becomes sensible

frogger %>%
  fill(name) %>%
  complete(name, time)

# Differences between MCAR and MAR
# We need to make certain assumptions about our data when we move further into analysis.
# 
# Which of the following answers about MCAR and MAR is TRUE?

# Generally, deleting observations is safer for MCAR data than deleting observations for MAR data.

# Exploring missingness dependence
# To learn about the structure of the missingness in data, you can explore how sorting changes how missingness is presented.
# 
# For the oceanbuoys dataset, explore the missingness with vis_miss(), and then arrange by a few different variables
# 
# This is not a definitive process, but it will get you started to ask the right questions of your data. We explore more powerful techniques in the next chapter.

# Arrange by year
oceanbuoys %>% arrange(year) %>% vis_miss()

# Arrange by latitude
oceanbuoys %>% arrange(latitude) %>% vis_miss()

# Arrange by wind_ew (wind east west)
oceanbuoys %>% arrange(wind_ew) %>% vis_miss()

# Further exploring missingness dependence
# Using the information from earlier on the oceanbuoys dataset, which of these statements makes the most appropriate statement on the missingness type?
#   
#   Try using gg_miss_var(), and gg_miss_case(), faceting by year to get more information. For example:
#   
#   library(naniar)
# gg_miss_var(oceanbuoys, facet = year)

# Data is MAR: year and location are both important for explaining missingness

