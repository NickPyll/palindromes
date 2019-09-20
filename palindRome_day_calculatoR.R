library(magrittr)
library(dplyr)

## How many palindromes in a year?
## Which year in the current century has the most palindromes?

## Defining date as (month, day, year)
## No spaces 
## 2 digit year
## No leading zeroes for month or day

# Function to reverse text
strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

# Sequence of dates
df <- data.frame(date = seq.Date(as.Date("2000-01-01"), as.Date("2099-12-31"), by = "1 day"))

# Flag palindromes
df %<>%
  mutate(year = format(date, "%Y"),
         character_date = paste0(
           sub("^0+", "", format(date, "%m")),
           sub("^0+", "", format(date, "%d")),
           format(date, "%y")),
         reverse_character_date = strReverse(character_date),
         palindrome = case_when(
           character_date == reverse_character_date ~ 1,
           TRUE ~ 0))

# how many palindrome days?
df %>%
  summarise(sum = sum(palindrome))

# So it looks like the only years without palindrome days are due to issues with leading zeroes.
# eg 2020 will not have a palindrome because, by definition, there will never be a day in February that starts with a '0'. Also, there is no 20th month

# what percent of days are palindrome days?
df %>%
  summarise(mean = mean(palindrome))

# Aggregate at year level
df_year <-
  df %>%
  group_by(year) %>%
  summarise(count_palindrome_days = sum(palindrome),
            has_palindrome = max(palindrome))

# How many years don't have a palindrome at all?
table(df_year$has_palindrome)

# histogram
hist(df_year$count_palindrome_days)
table(df_year$count_palindrome_days)

# What are the most exciting upcoming palindrome years?
df_year %>%
  arrange(desc(count_palindrome_days)) %>%
  filter(as.numeric(year) > 2019) %>%
  head()

# What are the most boring upcoming palindrome years?
df_year %>%
  arrange(as.numeric(year)) %>%
  filter(count_palindrome_days == 0,
         as.numeric(year) > 2019) %>%
  head()
